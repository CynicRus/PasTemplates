{
  Copyright (c) 2026 Aleksandr Vorobev aka CynicRus, CynicRus@gmail.com

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its
     contributors may be used to endorse or promote products derived from
     this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit TemplateExec;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  SysUtils, Classes, Contnrs, uComplex, TypInfo, Rtti,
  TemplateParser, TemplateRuntime;

type
  { TExecutor }

  TTemplateName = class(TCustomAttribute)
  public
    Name: string;
    constructor Create(const AName: string);
  end;

  TExecutor = class
  private
    FWriter: TStream;
    FTmpl: TTemplate;
    FCtx: TTemplateContext;
    FRtti: TRttiContext;

  type
    TVariable = record
      Name: string;
      Value: TValue;
    end;

  var
    FVars: array of TVariable;
    FDot: TValue;

    procedure WriteStr(const S: string);
    procedure ErrorF(const Fmt: string; const Args: array of const);

    procedure PushVar(const Name: string; const V: TValue);
    function VarValue(const Name: string): TValue;
    procedure SetVar(const Name: string; const V: TValue);

    function TryGetMember(const Receiver: TValue; const Name: string;
      out OutV: TValue): boolean;
    function TryIndex(const Receiver: TValue; const Key: TValue;
      out OutV: TValue): boolean;
    function ZeroValueOfType(ATyp: PTypeInfo): TValue;

    function IsTrue(const V: TValue; out Ok: boolean): boolean;
    function Printable(const V: TValue; out S: string): boolean;

    function EvalPipeline(const Dot: TValue; Pipe: TPipeNode): TValue;
    function EvalCommand(const Dot: TValue; Cmd: TCommandNode;
      const Final: TValue; HasFinal: boolean): TValue;
    function EvalArg(const Dot: TValue; N: TNode): TValue;
    function EvalFieldChain(const Dot, Receiver: TValue;
      const Idents: array of string): TValue;
    function EvalFunction(const Name: string; const Args: array of TValue): TValue;

    procedure Walk(const Dot: TValue; Node: TNode);
    procedure WalkList(const Dot: TValue; List: TListNode);
    procedure WalkIfWith(Node: TBranchNode; IsWith: boolean; const Dot: TValue);
    procedure WalkRange(Node: TRangeNode; const Dot: TValue);
    procedure WalkTemplate(Node: TTemplateNode; const Dot: TValue);
  public
    constructor Create(ATemplate: TTemplate; AWriter: TStream; const Data: TValue);
    destructor Destroy; override;
    procedure Run;
  end;

function ExecuteToString(T: TTemplate; const Data: TValue): string;
function SliceIdents(const A: array of string; FromIndex: SizeInt): TStringArray;

implementation

type
  EWalkBreak = class(Exception);
  EWalkContinue = class(Exception);

constructor TTemplateName.Create(const AName: string);
begin
  inherited Create;
  Name := AName;
end;

{ Helpers }

procedure TExecutor.WriteStr(const S: string);
var
  U: utf8string;
begin
  if FWriter = nil then Exit;
  if S = '' then Exit;
  U := UTF8Encode(S);
  if Length(U) > 0 then
    FWriter.WriteBuffer(U[1], Length(U));
end;

procedure TExecutor.ErrorF(const Fmt: string; const Args: array of const);
begin
  raise ETemplateExec.CreateFmt('template: %s: %s', [FTmpl.Name, Format(Fmt, Args)]);
end;

constructor TExecutor.Create(ATemplate: TTemplate; AWriter: TStream; const Data: TValue);
begin
  inherited Create;
  FTmpl := ATemplate;
  FCtx := ATemplate.Context;
  FWriter := AWriter;
  FRtti := TRttiContext.Create;

  FDot := Data;

  SetLength(FVars, 1);
  FVars[0].Name := '$';
  FVars[0].Value := FDot;
end;

destructor TExecutor.Destroy;
begin
  FRtti.Free;
  inherited;
end;

procedure TExecutor.PushVar(const Name: string; const V: TValue);
begin
  SetLength(FVars, Length(FVars) + 1);
  FVars[High(FVars)].Name := Name;
  FVars[High(FVars)].Value := V;
end;

function TExecutor.VarValue(const Name: string): TValue;
var
  i: integer;
begin
  for i := High(FVars) downto 0 do
    if SameText(FVars[i].Name, Name) then Exit(FVars[i].Value);
  ErrorF('undefined variable: %s', [Name]);
end;

procedure TExecutor.SetVar(const Name: string; const V: TValue);
var
  i: integer;
begin
  for i := High(FVars) downto 0 do
    if SameText(FVars[i].Name, Name) then
    begin
      FVars[i].Value := V;
      Exit;
    end;
  ErrorF('undefined variable: %s', [Name]);
end;

function TExecutor.ZeroValueOfType(ATyp: PTypeInfo): TValue;
var
  TD: PTypeData;
begin
  if ATyp = nil then
    Exit(TValue.Empty);

  case ATyp^.Kind of
    tkInteger:
      Result := 0;

    tkInt64:
      Result := int64(0);

    tkQWord:
      Result := QWord(0);

    tkFloat:
    begin
      TD := GetTypeData(ATyp);
      case TD^.FloatType of
        ftSingle: Result := single(0.0);
        ftDouble: Result := double(0.0);
        ftExtended: Result := extended(0.0);
        ftCurr: Result := currency(0.0);
        else
          Result := double(0.0);
      end;
    end;

    tkEnumeration, tkBool:
    begin
      if (ATyp = TypeInfo(boolean)) or (ATyp = TypeInfo(bytebool)) or
        (ATyp = TypeInfo(wordbool)) or (ATyp = TypeInfo(longbool)) then
        Result := False
      else
        Result := TValue.FromOrdinal(ATyp, 0);
    end;

    tkAString, tkUString, tkWString, tkLString, tkSString:
      Result := '';

    tkChar:
      Result := ansichar(#0);

    tkWChar:
      Result := widechar(#0);

    tkClass, tkInterface, tkPointer:
      Result := TValue.Empty;

    tkSet:
      TValue.Make(nil, ATyp, Result);

    else
      Result := TValue.Empty;
  end;
end;

function TExecutor.TryIndex(const Receiver: TValue; const Key: TValue;
  out OutV: TValue): boolean;
var
  V: TValue;
  i: int64;
  S: string;
  ArrLen: SizeInt;
begin
  Result := False;
  OutV := TValue.Empty;
  V := Receiver;

  if V.IsEmpty then Exit;

  // Dynamic array
  if V.Kind = tkDynArray then
  begin
    if Key.TryAsOrdinal(i) then
    begin
      try
        ArrLen := V.GetArrayLength;
        if (i >= 0) and (i < ArrLen) then
        begin
          OutV := V.GetArrayElement(i);
          Exit(True);
        end;
      except
      end;
    end;
  end;

  // Static array
  if V.IsArray and not V.IsOpenArray then
  begin
    if Key.TryAsOrdinal(i) then
    begin
      try
        ArrLen := V.GetArrayLength;
        if (i >= 0) and (i < ArrLen) then
        begin
          OutV := V.GetArrayElement(i);
          Exit(True);
        end;
      except
      end;
    end;
  end;

  // String indexing
  if (V.Kind in [tkAString, tkUString, tkWString, tkLString, tkSString]) then
  begin
    if Key.TryAsOrdinal(i) then
    begin
      S := V.AsString;
      if (i >= 0) and (i < Length(S)) then
      begin
        OutV := S[i + 1];
        Exit(True);
      end;
    end;
  end;

  Result := False;
end;

function TExecutor.TryGetMember(const Receiver: TValue; const Name: string;
  out OutV: TValue): boolean;
var
  V: TValue;
  T: TRttiType;
  P: TRttiProperty;
  F: TRttiField;
  Obj: TObject;
  PI: PPropInfo;
begin
  Result := False;
  OutV := TValue.Empty;
  V := Receiver;

  if V.IsEmpty then Exit;

  // Class/Object
  if V.Kind = tkClass then
  begin
    Obj := V.AsObject;
    if Obj = nil then Exit(False);

    // TypInfo published properties
    PI := GetPropInfo(Obj, Name);
    if PI <> nil then
    begin
      try
        case PI^.PropType^.Kind of
          tkInteger: OutV := GetOrdProp(Obj, PI);
          tkInt64: OutV := GetInt64Prop(Obj, PI);
          tkFloat: OutV := GetFloatProp(Obj, PI);
          tkBool: OutV := GetOrdProp(Obj, PI) <> 0;
          tkAString, tkUString, tkWString, tkLString, tkSString:
            OutV := GetStrProp(Obj, PI);
          tkClass: OutV := GetObjectProp(Obj, PI);
          else
            Exit(False);
        end;
        Exit(True);
      except
        Exit(False);
      end;
    end;

    // RTTI public fields/properties
    T := FRtti.GetType(Obj.ClassType);
    if T <> nil then
    begin
      P := T.GetProperty(Name);
      if (P <> nil) and P.IsReadable then
      begin
        try
          OutV := P.GetValue(Obj);
          Exit(True);
        except
          Exit(False);
        end;
      end;

      F := T.GetField(Name);
      if F <> nil then
      begin
        try
          OutV := F.GetValue(Obj);
          Exit(True);
        except
          Exit(False);
        end;
      end;
    end;

    Exit(False);
  end;

  // Record
  if V.Kind = tkRecord then
  begin
    T := FRtti.GetType(V.TypeInfo);
    if T <> nil then
    begin
      P := T.GetProperty(Name);
      if (P <> nil) and P.IsReadable then
      begin
        try
          OutV := P.GetValue(V.GetReferenceToRawData);
          Exit(True);
        except
          Exit(False);
        end;
      end;

      F := T.GetField(Name);
      if F <> nil then
      begin
        try
          OutV := F.GetValue(V.GetReferenceToRawData);
          Exit(True);
        except
          Exit(False);
        end;
      end;
    end;
    Exit(False);
  end;
  Result := False;
end;

function TExecutor.IsTrue(const V: TValue; out Ok: boolean): boolean;
var
  k: TTypeKind;
  OrdVal: int64;
begin
  Ok := True;
  if V.IsEmpty then Exit(False);

  k := V.Kind;
  case k of
    tkBool:
      Exit(V.AsBoolean);

    tkInteger, tkInt64, tkQWord:
    begin
      if V.TryAsOrdinal(OrdVal) then
        Exit(OrdVal <> 0)
      else
        Exit(False);
    end;

    tkFloat:
      Exit(V.AsExtended <> 0);

    tkAString, tkUString, tkWString, tkLString, tkSString:
      Exit(V.AsString <> '');

    tkClass:
      Exit(V.AsObject <> nil);

    tkInterface:
      Exit(V.AsInterface <> nil);

    tkRecord:
      Exit(True);

    tkPointer:
      Exit(V.AsPointer <> nil);
    else
      Ok := False;
      Exit(False);
  end;
end;

function TExecutor.Printable(const V: TValue; out S: string): boolean;
begin
  if V.IsEmpty then
  begin
    S := '<no value>';
    Exit(True);
  end;

  if V.Kind in [tkAString, tkUString, tkWString, tkLString, tkSString] then
  begin
    S := V.AsString;
    Exit(True);
  end;

  try
    S := V.ToString;
    Result := True;
  except
    Result := False;
  end;
end;

function TExecutor.EvalArg(const Dot: TValue; N: TNode): TValue;
begin
  if N = nil then Exit(TValue.Empty);

  case N.TypeNum of
    NodeDot: Exit(Dot);
    NodeNil: Exit(TValue.Empty);
    NodeString: Result := TStringNode(N).Text;
    NodeBool: Result := TBoolNode(N).True;
    NodeNumber:
    begin
      if TNumberNode(N).IsInt then Exit(TNumberNode(N).int64);
      if TNumberNode(N).IsUint then Exit(TNumberNode(N).uint64);
      if TNumberNode(N).IsFloat then Exit(TNumberNode(N).Float64);
      if TNumberNode(N).IsComplex then
        Exit(TValue.From(TypeInfo(TNumberNode(N).Complex128),
          @TNumberNode(N).Complex128));
      // fallback: try float field
      Exit(TNumberNode(N).Float64);
    end;
    NodeVariable:
    begin
      Result := VarValue(TVariableNode(N).Ident[0]);
      if Length(TVariableNode(N).Ident) > 1 then
        Result := EvalFieldChain(Dot, Result, SliceIdents(TVariableNode(N).Ident, 1));
      Exit;
    end;
    NodeField: Exit(EvalFieldChain(Dot, Dot, TFieldNode(N).Ident));
    NodeChain:
    begin
      Result := EvalArg(Dot, TChainNode(N).Node);
      Exit(EvalFieldChain(Dot, Result, TChainNode(N).Field));
    end;
    NodePipe: Exit(EvalPipeline(Dot, TPipeNode(N)));
    NodeIdentifier: Exit(EvalFunction(TIdentifierNode(N).Ident, []));
    else
      ErrorF('can''t handle arg %s', [N.ToString]);
  end;
end;

function TExecutor.EvalFunction(const Name: string; const Args: array of TValue): TValue;
var
  Fn: TTemplateFunc;
  i: integer;
  S, Fmt: string;
  NewArgs: array of TValue;

  function IsMissing(const V: TValue): boolean; inline;
  begin
    Result := V.IsEmpty;
  end;

  function AsBoolLoose(const V: TValue): boolean;
  var
    ok: boolean;
  begin
    Result := IsTrue(V, ok);
    if not ok then ErrorF('value has no meaningful truth value', []);
  end;

  function IsNumericKind(K: TTypeKind): boolean; inline;
  begin
    Result := K in [tkInteger, tkInt64, tkQWord, tkFloat, tkEnumeration, tkBool];
  end;

  function IsStringKind(K: TTypeKind): boolean; inline;
  begin
    Result := K in [tkAString, tkUString, tkWString, tkLString, tkSString];
  end;

  function TryAsExtended(const V: TValue; out E: extended): boolean;
  var
    i64: int64;
  begin
    if V.IsEmpty then Exit(False);
    if V.Kind = tkFloat then
    begin
      E := V.AsExtended;
      Exit(True);
    end;
    if V.TryAsOrdinal(i64) then
    begin
      E := i64;
      Exit(True);
    end;
    if V.Kind = tkEnumeration then
    begin
      E := V.AsOrdinal;
      Exit(True);
    end;
    Result := False;
  end;

  function CmpTValue(const A, B: TValue): integer;
  var
    aE, bE: extended;
    aStr, bStr: string;
  begin
    if A.IsEmpty and B.IsEmpty then Exit(0);
    if A.IsEmpty then Exit(-1);
    if B.IsEmpty then Exit(1);

    // numeric promotion
    if IsNumericKind(A.Kind) and IsNumericKind(B.Kind) and
      TryAsExtended(A, aE) and TryAsExtended(B, bE) then
    begin
      if aE < bE then Exit(-1);
      if aE > bE then Exit(1);
      Exit(0);
    end;

    // string
    if IsStringKind(A.Kind) and IsStringKind(B.Kind) then
    begin
      aStr := A.AsString;
      bStr := B.AsString;
      Exit(CompareStr(aStr, bStr));
    end;

    // bool direct
    if (A.Kind = tkBool) and (B.Kind = tkBool) then
    begin
      if A.AsBoolean = B.AsBoolean then Exit(0)
      else if not A.AsBoolean and B.AsBoolean then Exit(-1)
      else
        Exit(1);
    end;

    ErrorF('can''t compare values', []);
  end;

  function BuiltinLen(const V: TValue): integer;
  begin
    if V.IsEmpty then Exit(0);
    case V.Kind of
      tkAString, tkUString, tkWString, tkLString, tkSString:
        Exit(Length(V.AsString));
      tkClass:
      begin
        if V.AsObject is TStrings then
          Exit(TStrings(V.AsObject).Count);
        Exit(0);
      end;
      tkDynArray:
      begin
        try
          Exit(V.GetArrayLength);
        except
          Exit(0);
        end;
      end;
      else
        Exit(0);
    end;
  end;

  function BuiltinIndex(const Base, Key: TValue): TValue;
  var
    outV: TValue;
  begin
    if TryIndex(Base, Key, outV) then Exit(outV);

    if (Key.Kind in [tkAString, tkUString, tkWString, tkLString, tkSString]) then
    begin
      if TryGetMember(Base, Key.AsString, outV) then
        Exit(outV);
    end;

    Exit(TValue.Empty);
  end;

begin
  // Builtins
  if SameText(Name, 'and') then
  begin
    if Length(Args) = 0 then Exit(TValue.Empty);
    for i := 0 to High(Args) do
      if not AsBoolLoose(Args[i]) then Exit(Args[i]);
    Exit(Args[High(Args)]);
  end;

  if SameText(Name, 'or') then
  begin
    if Length(Args) = 0 then Exit(TValue.Empty);
    for i := 0 to High(Args) do
      if AsBoolLoose(Args[i]) then Exit(Args[i]);
    Exit(Args[High(Args)]);
  end;

  if SameText(Name, 'not') then
  begin
    if Length(Args) <> 1 then ErrorF('wrong number of args for not', []);
    Result := not AsBoolLoose(Args[0]);
    Exit;
  end;

  if SameText(Name, 'len') then
  begin
    if Length(Args) <> 1 then ErrorF('wrong number of args for len', []);
    Result := BuiltinLen(Args[0]);
    Exit;
  end;

  if SameText(Name, 'index') then
  begin
    if Length(Args) < 2 then ErrorF('wrong number of args for index', []);
    Result := BuiltinIndex(Args[0], Args[1]);
    if IsMissing(Result) then
    begin
      case FCtx.MissingKey of
        mkError: ErrorF('index: missing key', []);
        mkZero, mkInvalid: Exit(TValue.Empty);
      end;
    end;
    Exit;
  end;

  if SameText(Name, 'eq') then
  begin
    if Length(Args) < 2 then ErrorF('wrong number of args for eq', []);
    for i := 1 to High(Args) do
      if CmpTValue(Args[0], Args[i]) <> 0 then
      begin
        Result := False;
        Exit;
      end;
    Result := True;
    Exit;
  end;

  if SameText(Name, 'ne') then
  begin
    if Length(Args) <> 2 then ErrorF('wrong number of args for ne', []);
    Result := CmpTValue(Args[0], Args[1]) <> 0;
    Exit;
  end;

  if SameText(Name, 'lt') then
  begin
    if Length(Args) <> 2 then ErrorF('wrong number of args for lt', []);
    Result := CmpTValue(Args[0], Args[1]) < 0;
    Exit;
  end;

  if SameText(Name, 'le') then
  begin
    if Length(Args) <> 2 then ErrorF('wrong number of args for le', []);
    Result := CmpTValue(Args[0], Args[1]) <= 0;
    Exit;
  end;

  if SameText(Name, 'gt') then
  begin
    if Length(Args) <> 2 then ErrorF('wrong number of args for gt', []);
    Result := CmpTValue(Args[0], Args[1]) > 0;
    Exit;
  end;

  if SameText(Name, 'ge') then
  begin
    if Length(Args) <> 2 then ErrorF('wrong number of args for ge', []);
    Result := CmpTValue(Args[0], Args[1]) >= 0;
    Exit;
  end;

  if SameText(Name, 'print') or SameText(Name, 'println') then
  begin
    S := '';
    for i := 0 to High(Args) do
      S := S + Args[i].ToString;
    if SameText(Name, 'println') then S := S + LineEnding;
    Result := S;
    Exit;
  end;

  if SameText(Name, 'printf') then
  begin
    if Length(Args) = 0 then
    begin
      Result := '';
      Exit;
    end;

    Fmt := Args[0].ToString;
    //FIXME
    // Simple implementation - for a full implementation we need to parse the format
    S := Fmt;
    Result := S;
    Exit;
  end;

  if SameText(Name, 'call') then
  begin
    if Length(Args) < 1 then ErrorF('wrong number of args for call', []);
    if not (Args[0].Kind in [tkAString, tkUString, tkWString, tkLString, tkSString]) then
      ErrorF('call: first arg must be function name', []);
    S := Args[0].AsString;

    if not FCtx.FuncMap.TryGet(S, Fn) then
      ErrorF('"%s" is not a defined function', [S]);


    SetLength(NewArgs, Length(Args) - 1);
    for i := 1 to High(Args) do
      NewArgs[i - 1] := Args[i];

    Exit(Fn(NewArgs));
  end;

  // User-defined
  if not FCtx.FuncMap.TryGet(Name, Fn) then
    ErrorF('"%s" is not a defined function', [Name]);

  Result := Fn(Args);
end;

function TExecutor.EvalFieldChain(const Dot, Receiver: TValue;
  const Idents: array of string): TValue;
var
  i: integer;
  V: TValue;
  Ok: boolean;
  KeyVal: TValue;
begin
  Result := Receiver;
  for i := 0 to High(Idents) do
  begin
    if Result.IsEmpty then
    begin
      case FCtx.MissingKey of
        mkError: ErrorF('missing key: "%s"', [Idents[i]]);
        mkZero: Exit(TValue.Empty);
        mkInvalid: Exit(TValue.Empty);
      end;
    end;

    Ok := TryGetMember(Result, Idents[i], V);
    if Ok then
    begin
      Result := V;
      Continue;
    end;

    KeyVal := TValue.From(TypeInfo(Idents[i]), @Idents[i]);
    if TryIndex(Result, KeyVal, V) then
    begin
      Result := V;
      Continue;
    end;

    case FCtx.MissingKey of
      mkError: ErrorF('map has no entry for key "%s"', [Idents[i]]);
      mkZero: Exit(TValue.Empty);
      mkInvalid: Exit(TValue.Empty);
    end;
  end;
end;

function TExecutor.EvalCommand(const Dot: TValue; Cmd: TCommandNode;
  const Final: TValue; HasFinal: boolean): TValue;
var
  First: TNode;
  Args: array of TValue;
  i, n: integer;
begin
  if (Cmd = nil) or (Length(Cmd.Args) = 0) then Exit(TValue.Empty);

  First := Cmd.Args[0];

  case First.TypeNum of
    NodeIdentifier:
    begin
      n := Length(Cmd.Args) - 1 + Ord(HasFinal);
      SetLength(Args, n);
      n := 0;
      for i := 1 to High(Cmd.Args) do
      begin
        Args[n] := EvalArg(Dot, Cmd.Args[i]);
        Inc(n);
      end;
      if HasFinal then
      begin
        Args[n] := Final;
        Inc(n);
      end;
      SetLength(Args, n);
      Exit(EvalFunction(TIdentifierNode(First).Ident, Args));
    end;

    NodeField:
      Exit(EvalFieldChain(Dot, Dot, TFieldNode(First).Ident));

    NodeVariable:
    begin
      Result := VarValue(TVariableNode(First).Ident[0]);
      if Length(TVariableNode(First).Ident) > 1 then
        Result := EvalFieldChain(Dot, Result, SliceIdents(
          TVariableNode(First).Ident, 1));
      Exit;
    end;

    NodePipe:
    begin
      if HasFinal then ErrorF('parenthesized pipeline can''t take final value', []);
      Exit(EvalPipeline(Dot, TPipeNode(First)));
    end;

    NodeChain:
    begin
      Result := EvalArg(Dot, TChainNode(First).Node);
      Exit(EvalFieldChain(Dot, Result, TChainNode(First).Field));
    end;

    NodeDot: Exit(Dot);
    NodeString: Result := TStringNode(First).Text;
    NodeNumber:
    begin
      with TNumberNode(First) do
      begin
        if IsInt then Exit(int64);
        if IsUint then Exit(uint64);
        if IsFloat then Exit(Float64);
        if IsComplex then
          Exit(TValue.From(TypeInfo(Complex128), @Complex128));
        Exit(Float64);
      end;
    end;
    NodeBool: Result := TBoolNode(First).True;
    else
      ErrorF('can''t evaluate command %s', [First.ToString]);
  end;
end;

function TExecutor.EvalPipeline(const Dot: TValue; Pipe: TPipeNode): TValue;
var
  i: integer;
  V: TValue;
  HasFinal: boolean;
begin
  if Pipe = nil then Exit(TValue.Empty);
  V := TValue.Empty;
  HasFinal := False;

  for i := 0 to High(Pipe.Cmds) do
  begin
    V := EvalCommand(Dot, Pipe.Cmds[i], V, HasFinal);
    HasFinal := True;
  end;

  if Length(Pipe.Decl) > 0 then
  begin
    if Pipe.IsAssign then
      SetVar(Pipe.Decl[0].Ident[0], V)
    else
      PushVar(Pipe.Decl[0].Ident[0], V);
  end;

  Result := V;
end;

procedure TExecutor.WalkIfWith(Node: TBranchNode; IsWith: boolean; const Dot: TValue);
var
  V: TValue;
  Truth, Ok: boolean;
begin
  V := EvalPipeline(Dot, Node.Pipe);
  Truth := IsTrue(V, Ok);
  if not Ok then ErrorF('if/with can''t use %s', [V.ToString]);

  if Truth then
  begin
    if IsWith then
      WalkList(V, Node.List)
    else
      WalkList(Dot, Node.List);
  end
  else if Node.ElseList <> nil then
    WalkList(Dot, Node.ElseList);
end;

procedure TExecutor.WalkRange(Node: TRangeNode; const Dot: TValue);
var
  V: TValue;
  i: integer;
  Elem: TValue;
  S: string;
  ArrLen: SizeInt;
  Obj: TObject;
  L: TFPList;
  HL: TFPHashObjectList;
  Strs: TStrings;
begin
  V := EvalPipeline(Dot, Node.Pipe);
  if V.IsEmpty then
  begin
    if Node.ElseList <> nil then
      WalkList(Dot, Node.ElseList);
    Exit;
  end;

  try
    // Static array
    if V.IsArray and not V.IsOpenArray then
    begin
      ArrLen := V.GetArrayLength;
      if ArrLen = 0 then
      begin
        if Node.ElseList <> nil then WalkList(Dot, Node.ElseList);
        Exit;
      end;
      for i := 0 to ArrLen - 1 do
      begin
        Elem := V.GetArrayElement(i);
        if Length(Node.Pipe.Decl) > 0 then
        begin
          if Node.Pipe.IsAssign then SetVar(Node.Pipe.Decl[0].Ident[0], Elem)
          else
            PushVar(Node.Pipe.Decl[0].Ident[0], Elem);
        end;
        try
          WalkList(Elem, Node.List);
        except
          on EWalkContinue do Continue;
        end;
      end;
      Exit;
    end;

    // Dynamic array
    if V.Kind = tkDynArray then
    begin
      ArrLen := V.GetArrayLength;
      if ArrLen = 0 then
      begin
        if Node.ElseList <> nil then
          WalkList(Dot, Node.ElseList);
        Exit;
      end;

      for i := 0 to ArrLen - 1 do
      begin
        Elem := V.GetArrayElement(i);
        if Length(Node.Pipe.Decl) > 0 then
        begin
          if Node.Pipe.IsAssign then
            SetVar(Node.Pipe.Decl[0].Ident[0], Elem)
          else
            PushVar(Node.Pipe.Decl[0].Ident[0], Elem);
        end;
        try
          WalkList(Elem, Node.List);
        except
          on EWalkContinue do Continue;
        end;
      end;
      Exit;
    end;

    // String iteration
    if (V.Kind in [tkAString, tkUString, tkWString, tkLString, tkSString]) then
    begin
      S := V.AsString;
      if S = '' then
      begin
        if Node.ElseList <> nil then
          WalkList(Dot, Node.ElseList);
        Exit;
      end;

      for i := 1 to Length(S) do
      begin
        Elem := S[i];
        if Length(Node.Pipe.Decl) > 0 then
        begin
          if Node.Pipe.IsAssign then
            SetVar(Node.Pipe.Decl[0].Ident[0], Elem)
          else
            PushVar(Node.Pipe.Decl[0].Ident[0], Elem);
        end;
        try
          WalkList(Elem, Node.List);
        except
          on EWalkContinue do Continue;
        end;
      end;
      Exit;
    end;

    // Class-based containers
    if V.Kind = tkClass then
    begin
      Obj := V.AsObject;
      if Obj = nil then
      begin
        if Node.ElseList <> nil then WalkList(Dot, Node.ElseList);
        Exit;
      end;

      // TStrings
      if Obj is TStrings then
      begin
        Strs := TStrings(Obj);
        if Strs.Count = 0 then
        begin
          if Node.ElseList <> nil then WalkList(Dot, Node.ElseList);
          Exit;
        end;
        for i := 0 to Strs.Count - 1 do
        begin
          Elem := Strs[i];
          if Length(Node.Pipe.Decl) > 0 then
          begin
            if Node.Pipe.IsAssign then SetVar(Node.Pipe.Decl[0].Ident[0], Elem)
            else
              PushVar(Node.Pipe.Decl[0].Ident[0], Elem);
          end;
          try
            WalkList(Elem, Node.List);
          except
            on EWalkContinue do Continue;
          end;
        end;
        Exit;
      end;

      // TFPHashObjectList: iterate Values (objects). If you need key too - extend decl logic later.
      if Obj is TFPHashObjectList then
      begin
        HL := TFPHashObjectList(Obj);
        if HL.Count = 0 then
        begin
          if Node.ElseList <> nil then WalkList(Dot, Node.ElseList);
          Exit;
        end;
        for i := 0 to HL.Count - 1 do
        begin
          Elem := HL.Items[i];
          if Length(Node.Pipe.Decl) > 0 then
          begin
            if Node.Pipe.IsAssign then SetVar(Node.Pipe.Decl[0].Ident[0], Elem)
            else
              PushVar(Node.Pipe.Decl[0].Ident[0], Elem);
          end;
          try
            WalkList(Elem, Node.List);
          except
            on EWalkContinue do Continue;
          end;
        end;
        Exit;
      end;

      // TFPList / descendants
      if Obj is TFPList then
      begin
        L := TFPList(Obj);
        if L.Count = 0 then
        begin
          if Node.ElseList <> nil then WalkList(Dot, Node.ElseList);
          Exit;
        end;
        for i := 0 to L.Count - 1 do
        begin
          if TypeInfo(Pointer) = nil then
            Elem := PtrUInt(L[i])
          else
            TValue.Make(L[i], TypeInfo(Pointer), Elem);
          if Length(Node.Pipe.Decl) > 0 then
          begin
            if Node.Pipe.IsAssign then SetVar(Node.Pipe.Decl[0].Ident[0], Elem)
            else
              PushVar(Node.Pipe.Decl[0].Ident[0], Elem);
          end;
          try
            WalkList(Elem, Node.List);
          except
            on EWalkContinue do Continue;
          end;
        end;
        Exit;
      end;
    end;

    // Default: unsupported
    if Node.ElseList <> nil then
      WalkList(Dot, Node.ElseList)
    else
      ErrorF('range can''t iterate over %s', [V.ToString]);
  except
    on E: EWalkBreak do ;
  end;
end;

procedure TExecutor.WalkTemplate(Node: TTemplateNode; const Dot: TValue);
var
  Tmpl: TTemplate;
  NewDot: TValue;
  Exec: TExecutor;
begin
  Tmpl := FTmpl.Lookup(Node.Name);
  if Tmpl = nil then
    ErrorF('template "%s" not defined', [Node.Name]);

  if Node.Pipe <> nil then
    NewDot := EvalPipeline(Dot, Node.Pipe)
  else
    NewDot := Dot;

  Exec := TExecutor.Create(Tmpl, FWriter, NewDot);
  try
    Exec.Run;
  finally
    Exec.Free;
  end;
end;

procedure TExecutor.WalkList(const Dot: TValue; List: TListNode);
var
  i: integer;
begin
  if List = nil then Exit;
  for i := 0 to List.Nodes.Count - 1 do
    Walk(Dot, TNode(List.Nodes[i]));
end;

procedure TExecutor.Walk(const Dot: TValue; Node: TNode);
var
  V: TValue;
  S: string;
begin
  if Node = nil then Exit;

  case Node.TypeNum of
    NodeList:
      WalkList(Dot, TListNode(Node));

    NodeText:
      WriteStr(TTextNode(Node).Text);

    NodeAction:
    begin
      V := EvalPipeline(Dot, TActionNode(Node).Pipe);
      if Length(TActionNode(Node).Pipe.Decl) = 0 then
      begin
        if not Printable(V, S) then
          ErrorF('can''t print %s', [Node.ToString]);
        WriteStr(S);
      end;
    end;

    NodeIf:
      WalkIfWith(TIfNode(Node), False, Dot);

    NodeWith:
      WalkIfWith(TWithNode(Node), True, Dot);

    NodeRange:
      WalkRange(TRangeNode(Node), Dot);

    NodeTemplate:
      WalkTemplate(TTemplateNode(Node), Dot);

    NodeComment:
      ;

    NodeBreak, NodeContinue:
    begin
      if Node.TypeNum = NodeBreak then
        raise EWalkBreak.Create('')
      else
        raise EWalkContinue.Create('');
    end;
    else
      ErrorF('unknown node type', []);
  end;
end;

procedure TExecutor.Run;
begin
  if (FTmpl = nil) or (FTmpl.Tree = nil) or (FTmpl.Tree.Root = nil) then
    ErrorF('"%s" is an incomplete or empty template', [FTmpl.Name]);
  Walk(FDot, FTmpl.Tree.Root);
end;

function ExecuteToString(T: TTemplate; const Data: TValue): string;
var
  SS: TStringStream;
  Exec: TExecutor;
begin
  SS := TStringStream.Create('');
  try
    Exec := TExecutor.Create(T, SS, Data);
    try
      Exec.Run;
    finally
      Exec.Free;
    end;
    Result := SS.DataString;
  finally
    SS.Free;
  end;
end;

function SliceIdents(const A: array of string; FromIndex: SizeInt): TStringArray;
var
  i, n: SizeInt;
begin
  n := Length(A) - FromIndex;
  if n <= 0 then Exit(nil);
  SetLength(Result, n);
  for i := 0 to n - 1 do
    Result[i] := A[FromIndex + i];
end;

end.
