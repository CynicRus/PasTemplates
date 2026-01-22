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

unit TemplateRuntime;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  SysUtils, Classes, fgl, Rtti, TypInfo,
  TemplateParser;

type
  ETemplateExec = class(Exception);
  ETemplateParse = class(Exception);


  TTemplateFunc = reference to function(const Args: array of TValue): TValue;

  TMissingKeyAction = (mkInvalid, mkZero, mkError);

  TValueBox = class(TInterfacedObject)
  public
    V: TValue;
    constructor Create(const AV: TValue);
  end;

  { for passing arrays of funcs }
  TFuncDef = record
    Name: string;
    Func: TTemplateFunc;
  end;

  TTemplate = class;
  TTemplateArray = specialize TArray<TTemplate>;
  TFuncDefArray = specialize TArray<TFuncDef>;

  { TFuncMap: map name->function }
  TFuncMap = class(specialize TFPGMap<string, TTemplateFunc>)
  public
    constructor Create;
    procedure AddFunc(const Name: string; const Func: TTemplateFunc);
    function TryGet(const Name: string; out Fn: TTemplateFunc): boolean;
  end;

  TTemplateContext = class
  private
    FFuncs: TFuncMap;
    FTemplates: TStringList;
    FLock: TRTLCriticalSection;
    FMissingKey: TMissingKeyAction;
    function GetTemplateUnlocked(const Name: string): TTemplate;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    function Lookup(const Name: string): TTemplate;
    procedure Associate(ATemplate: TTemplate);
    function Templates: TTemplateArray;

    procedure DefineFuncs(const Map: array of TFuncDef);

    function BuiltinsNames: TStrings;
    procedure SetOption(const Opt: string);
    property MissingKey: TMissingKeyAction read FMissingKey write FMissingKey;
    property FuncMap: TFuncMap read FFuncs;
  end;

  { TTemplate }

  TTemplate = class
  private
    FName: string;
    FTree: TTree;
    FContext: TTemplateContext;
    FLeftDelim: string;
    FRightDelim: string;
    procedure Init;
  public
    constructor Create(const AName: string); overload;
    constructor Create(const AName: string; ACtx: TTemplateContext); overload;
    destructor Destroy; override;

    function Name: string;
    function New(const AName: string): TTemplate;
    function Delims(const Left, Right: string): TTemplate;

    function Funcs(const Map: TFuncDefArray): TTemplate;

    function Option(const Opt: array of string): TTemplate;

    function Parse(const Text: string): TTemplate;
    function AddParseTree(const AName: string; Tree: TTree): TTemplate;
    function Lookup(const AName: string): TTemplate;
    function Clone: TTemplate;

    procedure Execute(Output: TStrings; Data: TObject); overload;
    procedure Execute(Output: TStrings; const Data: TValue); overload;
    procedure ExecuteStream(Writer: TStream; const Data: TValue);
    function ExecuteToString(const Data: TValue): string;

    property Tree: TTree read FTree;
    property Context: TTemplateContext read FContext;
  end;

  { TTemplateHelper }

  TTemplateHelper = class helper for TTemplate
  public
    function HTMLEscape(const S: string): string;
    function ParseHTML(const Text: string): TTemplate; // auto-escaping parse
    function ParseFiles(const FileNames: TStringArray): TTemplate;
    function ParseFile(const FileName: string): TTemplate;
  end;

function NewTemplate(const Name: string): TTemplate;
function New(const Name: string): TTemplate;
function ReadFileToString(const FileName: string): string;

implementation

uses
  TemplateExec;

  { TValueBox }

constructor TValueBox.Create(const AV: TValue);
begin
  inherited Create;
  V := AV;
end;

{ TFuncMap }

constructor TFuncMap.Create;
begin
  inherited Create;
  Sorted := True;
end;

procedure TFuncMap.AddFunc(const Name: string; const Func: TTemplateFunc);
var
  i: integer;
begin
  i := IndexOf(Name);
  if i >= 0 then
    Data[i] := Func
  else
    Add(Name, Func);
end;

function TFuncMap.TryGet(const Name: string; out Fn: TTemplateFunc): boolean;
var
  i: integer;
begin
  i := IndexOf(Name);
  Result := i >= 0;
  if Result then Fn := Data[i]
  else
    Fn := nil;
end;

{ TTemplateContext }

constructor TTemplateContext.Create;
begin
  inherited Create;
  FFuncs := TFuncMap.Create;
  FTemplates := TStringList.Create;
  FTemplates.Sorted := True;
  FTemplates.Duplicates := dupIgnore;
  InitCriticalSection(FLock);
  FMissingKey := mkInvalid;
end;

destructor TTemplateContext.Destroy;
begin
  DoneCriticalSection(FLock);
  FTemplates.Free;
  FFuncs.Free;
  inherited Destroy;
end;

procedure TTemplateContext.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TTemplateContext.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

function TTemplateContext.GetTemplateUnlocked(const Name: string): TTemplate;
var
  i: integer;
begin
  i := FTemplates.IndexOf(Name);
  if i >= 0 then Result := TTemplate(FTemplates.Objects[i])
  else
    Result := nil;
end;

function TTemplateContext.Lookup(const Name: string): TTemplate;
begin
  Lock;
  try
    Result := GetTemplateUnlocked(Name);
  finally
    Unlock;
  end;
end;

procedure TTemplateContext.Associate(ATemplate: TTemplate);
var
  i: integer;
begin
  Lock;
  try
    i := FTemplates.IndexOf(ATemplate.Name);
    if i >= 0 then
      FTemplates.Objects[i] := ATemplate
    else
      FTemplates.AddObject(ATemplate.Name, ATemplate);
  finally
    Unlock;
  end;
end;

function TTemplateContext.Templates: TTemplateArray;
var
  i: integer;
begin
  Lock;
  try
    SetLength(Result, FTemplates.Count);
    for i := 0 to FTemplates.Count - 1 do
      Result[i] := TTemplate(FTemplates.Objects[i]);
  finally
    Unlock;
  end;
end;

procedure TTemplateContext.DefineFuncs(const Map: array of TFuncDef);
var
  i: integer;
begin
  for i := Low(Map) to High(Map) do
    FFuncs.AddFunc(Map[i].Name, Map[i].Func);
end;

function TTemplateContext.BuiltinsNames: TStrings;
var
  S: TStringList;
begin
  { go/text/template builtins + common comparisons }
  S := TStringList.Create;
  S.Add('and');
  S.Add('or');
  S.Add('not');
  S.Add('call');
  S.Add('len');
  S.Add('index');
  S.Add('eq');
  S.Add('ne');
  S.Add('lt');
  S.Add('le');
  S.Add('gt');
  S.Add('ge');
  S.Add('print');
  S.Add('printf');
  S.Add('println');
  Result := S;
end;

procedure TTemplateContext.SetOption(const Opt: string);
var
  Key, Value: string;
  P: SizeInt;
begin
  if Opt = '' then raise ETemplateExec.Create('empty option string');
  P := Pos('=', Opt);
  if P <= 0 then raise ETemplateExec.Create('unrecognized option: ' + Opt);

  Key := Trim(Copy(Opt, 1, P - 1));
  Value := Trim(Copy(Opt, P + 1, MaxInt));

  if SameText(Key, 'missingkey') then
  begin
    if SameText(Value, 'default') or SameText(Value, 'invalid') then
      FMissingKey := mkInvalid
    else if SameText(Value, 'zero') then FMissingKey := mkZero
    else if SameText(Value, 'error') then FMissingKey := mkError
    else
      raise ETemplateExec.Create('unrecognized option: ' + Opt);
    Exit;
  end;

  raise ETemplateExec.Create('unrecognized option: ' + Opt);
end;

{ TTemplate }

constructor TTemplate.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FTree := nil;
  FContext := nil;
  FLeftDelim := '{{';
  FRightDelim := '}}';
  Init;
end;

constructor TTemplate.Create(const AName: string; ACtx: TTemplateContext);
begin
  inherited Create;
  FName := AName;
  FTree := nil;
  FContext := ACtx;
  FLeftDelim := '{{';
  FRightDelim := '}}';
  Init;
end;

destructor TTemplate.Destroy;
begin
  FTree.Free;
  inherited Destroy;
end;

procedure TTemplate.Init;
begin
  if FContext = nil then
    FContext := TTemplateContext.Create;
  FContext.Associate(Self);
end;

function TTemplate.Name: string;
begin
  Result := FName;
end;

function TTemplate.New(const AName: string): TTemplate;
begin
  Init;
  Result := TTemplate.Create(AName, FContext);
  Result.FLeftDelim := FLeftDelim;
  Result.FRightDelim := FRightDelim;
end;

function TTemplate.Delims(const Left, Right: string): TTemplate;
begin
  Init;
  if Left <> '' then FLeftDelim := Left
  else
    FLeftDelim := '{{';
  if Right <> '' then FRightDelim := Right
  else
    FRightDelim := '}}';
  Result := Self;
end;

function TTemplate.Funcs(const Map: TFuncDefArray): TTemplate;
begin
  Init;
  FContext.DefineFuncs(Map);
  Result := Self;
end;

function TTemplate.Option(const Opt: array of string): TTemplate;
var
  s: string;
begin
  Init;
  for s in Opt do FContext.SetOption(s);
  Result := Self;
end;

function TTemplate.Parse(const Text: string): TTemplate;
var
  TreeSet: TTreeSet;
  FuncNames, Builtins: TStrings;
  ParsedMain: TTree;
  i: integer;
  T: TTree;
begin
  Init;

  TreeSet := TTreeSet.Create;
  try
    { parse needs only names; give a TStringList snapshot }
    FuncNames := TStringList.Create;
    for i := 0 to FContext.FuncMap.Count - 1 do
      FuncNames.Add(FContext.FuncMap.Keys[i]);

    Builtins := FContext.BuiltinsNames;
    try
      ParsedMain := TTree.Create(FName, [FuncNames, Builtins], False);
      ParsedMain.Mode := ParsedMain.Mode;
      ParsedMain.Parse(Text, FLeftDelim, FRightDelim, TreeSet, [FuncNames, Builtins]);

      TreeSet.AddObject(FName, ParsedMain);

      for i := 0 to TreeSet.Count - 1 do
      begin
        T := TTree(TreeSet.Objects[i]);
        AddParseTree(TreeSet[i], T);
        TreeSet.Objects[i] := nil;
      end;
    finally
      Builtins.Free;
      FuncNames.Free;
    end;
  finally
    TreeSet.Free;
  end;

  Result := Self;
end;

function TTemplate.AddParseTree(const AName: string; Tree: TTree): TTemplate;
var
  NT: TTemplate;
  Existing: TTemplate;
begin
  Init;

  if AName = FName then
    NT := Self
  else
    NT := New(AName);

  Existing := FContext.Lookup(AName);
  if (Existing <> nil) and (Existing.Tree <> nil) and IsEmptyTree(Tree.Root) then
    Exit(NT);

  if NT.FTree <> Tree then
  begin
    NT.FTree.Free;
    NT.FTree := Tree;
  end;

  FContext.Associate(NT);
  Result := NT;
end;

function TTemplate.Lookup(const AName: string): TTemplate;
begin
  if FContext = nil then Exit(nil);
  Result := FContext.Lookup(AName);
end;

function TTemplate.Clone: TTemplate;
var
  Ts: TTemplateArray;
  i: integer;
  NewCtx: TTemplateContext;
  Src, Dst: TTemplate;
begin
  Init;

  NewCtx := TTemplateContext.Create;
  NewCtx.MissingKey := FContext.MissingKey;

  for i := 0 to FContext.FuncMap.Count - 1 do
    NewCtx.FuncMap.AddFunc(FContext.FuncMap.Keys[i], FContext.FuncMap.Data[i]);

  Result := TTemplate.Create(FName, NewCtx);
  Result.FLeftDelim := FLeftDelim;
  Result.FRightDelim := FRightDelim;
  Result.FTree := FTree;

  Ts := FContext.Templates;
  for Src in Ts do
  begin
    if Src = nil then Continue;
    if Src.Name = FName then Continue;
    Dst := Result.New(Src.Name);
    Dst.FTree := Src.Tree;
    NewCtx.Associate(Dst);
  end;
end;

procedure TTemplate.ExecuteStream(Writer: TStream; const Data: TValue);
begin
  { delegate to TemplateExec }
  with TExecutor.Create(Self, Writer, Data) do
  try
    Run;
  finally
    Free;
  end;
end;

function TTemplate.ExecuteToString(const Data: TValue): string;
begin
  Result := TemplateExec.ExecuteToString(Self, Data);
end;

function NewTemplate(const Name: string): TTemplate;
begin
  Result := TTemplate.Create(Name);
end;

function New(const Name: string): TTemplate;
begin
  Result := NewTemplate(Name);
end;

function ReadFileToString(const FileName: string): string;
var
  Stream: TStringStream;
begin
  if not FileExists(FileName) then
    raise ETemplateExec.Create('Template file not found: ' + FileName);
  Stream := TStringStream.Create;
  try
    Stream.LoadFromFile(FileName);
    Result := StringReplace(Stream.DataString, sLineBreak, '', [rfReplaceAll]);
    if (Result <> '') then
    begin
      if Result.EndsWith(LineEnding) then
        System.Delete(Result, Length(Result) - Length(LineEnding) +
          1, Length(LineEnding))
      else if Result.EndsWith(#10) then
        System.Delete(Result, Length(Result), 1);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TTemplate.Execute(Output: TStrings; Data: TObject);
var
  V: TValue;
begin
  TValue.Make(@Data, TypeInfo(Data), V);
  Execute(Output, V);
end;

procedure TTemplate.Execute(Output: TStrings; const Data: TValue);
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  try
    ExecuteStream(SS, Data);
    Output.Text := SS.DataString;
  finally
    SS.Free;
  end;
end;

{ TTemplateHelper }

function TTemplateHelper.HTMLEscape(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);
end;

function TTemplateHelper.ParseHTML(const Text: string): TTemplate;
begin
  Self.Init;
  Self.FContext.FuncMap.AddFunc('html',
    function(const Args: array of TValue): TValue
  begin
    if Length(Args) = 0 then Exit('');
    Result := Self.HTMLEscape(Args[0].AsString);
  end
    );
  Result := Self.Parse(Text);
end;

function LoadFileAsString(const FN: string): string;
var
  FS: TFileStream;
  SS: TStringStream;
begin
  FS := TFileStream.Create(FN, fmOpenRead or fmShareDenyNone);
  try
    SS := TStringStream.Create('');
    try
      SS.CopyFrom(FS, FS.Size);
      Result := SS.DataString;
    finally
      SS.Free;
    end;
  finally
    FS.Free;
  end;
end;

function TTemplateHelper.ParseFiles(const FileNames: TStringArray): TTemplate;
var
  i: integer;
  Content, Name: string;
  SL: TStringList;
begin
  Result := Self;
  SL := TStringList.Create;
  try
    for i := 0 to High(FileNames) do
    begin
      SL.Clear;
      SL.LoadFromFile(FileNames[i]);
      Content := SL.Text;
      Content := StringReplace(Content, sLineBreak, '', [rfReplaceAll]);
      Content := TrimRight(Content);
      Name := ChangeFileExt(ExtractFileName(FileNames[i]), '');
      if i = 0 then
        Result.Parse(Content)
      else
        Result.New(Name).Parse(Content);
    end;
  finally
    SL.Free;
  end;
end;

function TTemplateHelper.ParseFile(const FileName: string): TTemplate;
begin
  Result := ParseFiles([FileName]);
end;

end.
