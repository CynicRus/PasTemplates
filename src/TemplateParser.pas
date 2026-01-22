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

unit TemplateParser;

{$mode objfpc}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Contnrs, Math, StrUtils, uComplex, LazUTF8;

type
  ETemplateError = class(Exception);

  TItemType = (
    itemError, itemBool, itemChar, itemCharConstant, itemComment,
    itemComplex, itemAssign, itemDeclare, itemEOF, itemField,
    itemIdentifier, itemLeftDelim, itemLeftParen, itemNumber, itemPipe,
    itemRawString, itemRightDelim, itemRightParen, itemSpace,
    itemString, itemText, itemVariable,
    itemKeyword, itemBlock, itemBreak, itemContinue, itemDot,
    itemDefine, itemElse, itemEnd, itemIf, itemNil, itemRange,
    itemTemplate, itemWith
    );

  TPos = nativeint;

type
  TStringDynArray = array of string;

const
  { Mode flags }
  ParseComments = 1 shl 0;
  SkipFuncCheck = 1 shl 1;

type
  TItem = record
    Typ: TItemType;
    Pos: TPos;
    Val: string;
    Line: integer;
    function ToString: string;
  end;

  TNodeType = (
    NodeText, NodeAction, NodeBool, NodeChain, NodeCommand, NodeDot,
    nodeElse, nodeEnd, NodeField, NodeIdentifier, NodeIf, NodeList,
    NodeNil, NodeNumber, NodePipe, NodeRange, NodeString, NodeTemplate,
    NodeVariable, NodeWith, NodeComment, NodeBreak, NodeContinue
    );

  TNode = class;
  TTree = class;
  TLexer = class;
  TCommandNode = class;

  { TreeSet owns trees (releases Objects) }
  TTreeSet = class(TStringList)
  public
    constructor Create;
  end;

  TNode = class
  private
    FTree: TTree;
    FPos: TPos;
    FNodeType: TNodeType;
  public
    constructor Create(ATree: TTree; APos: TPos; AType: TNodeType); virtual;
    function TypeNum: TNodeType; virtual;
    function ToString: string; virtual;
    function Copy: TNode; virtual; abstract;
    function Position: TPos; virtual;
    function Tree: TTree; virtual;
    procedure WriteTo(Strings: TStrings); virtual; abstract;
  end;

  TListNode = class(TNode)
  public
    Nodes: TFPList;
    constructor Create(ATree: TTree; APos: TPos);
    destructor Destroy; override;
    procedure Append(N: TNode);
    function CopyList: TListNode;
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TTextNode = class(TNode)
  public
    Text: string;
    constructor Create(ATree: TTree; APos: TPos; AText: string);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TPipeNode = class;//forward

  TActionNode = class(TNode)
  public
    Line: integer;
    Pipe: TPipeNode;
    constructor Create(ATree: TTree; APos: TPos; ALine: integer; APipe: TPipeNode);
    destructor Destroy; override;
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TIdentifierNode = class(TNode)
  public
    Ident: string;
    constructor Create(ATree: TTree; APos: TPos; const AIdent: string);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TVariableNode = class(TNode)
  public
    Ident: array of string;
    constructor Create(ATree: TTree; APos: TPos; AIdent: string);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TDotNode = class(TNode)
  public
    function TypeNum: TNodeType; override;
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TNilNode = class(TNode)
  public
    function TypeNum: TNodeType; override;
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TFieldNode = class(TNode)
  public
    Ident: array of string;
    constructor Create(ATree: TTree; APos: TPos; AIdent: string);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TChainNode = class(TNode)
  public
    Node: TNode;
    Field: array of string;
    constructor Create(ATree: TTree; APos: TPos; ANode: TNode);
    destructor Destroy; override;
    procedure Add(AField: string);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TBoolNode = class(TNode)
  public
    True: boolean;
    constructor Create(ATree: TTree; APos: TPos; ATrue: boolean);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TNumberNode = class(TNode)
  public
    IsInt, IsUint, IsFloat, IsComplex: boolean;
    int64: int64;
    uint64: QWord;
    Float64: double;
    Complex128: Complex;
    Text: string;
    constructor Create(ATree: TTree; APos: TPos; AText: string;
      ATyp: TItemType = itemNumber);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TStringNode = class(TNode)
  public
    Quoted, Text: string;
    constructor Create(ATree: TTree; APos: TPos; AQuoted, AText: string);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TPipeNode = class(TNode)
  public
    Line: integer;
    IsAssign: boolean;
    Decl: array of TVariableNode;
    Cmds: array of TCommandNode;
    constructor Create(ATree: TTree; APos: TPos; ALine: integer;
      Vars: array of TVariableNode);
    destructor Destroy; override;
    procedure Append(Cmd: TCommandNode);
    function CopyPipe: TPipeNode;
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TCommandNode = class(TNode)
  public
    Args: array of TNode;
    destructor Destroy; override;
    procedure Append(Arg: TNode);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TBranchNode = class(TNode)
  public
    Line: integer;
    Pipe: TPipeNode;
    List: TListNode;
    ElseList: TListNode;
    constructor Create(ATree: TTree; APos: TPos; AType: TNodeType;
      ALine: integer; APipe: TPipeNode; AList, AElseList: TListNode);
    destructor Destroy; override;
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TIfNode = class(TBranchNode)
  public
    constructor Create(ATree: TTree; APos: TPos; ALine: integer;
      APipe: TPipeNode; AList, AElseList: TListNode);
    function Copy: TNode; override;
  end;

  TRangeNode = class(TBranchNode)
  public
    constructor Create(ATree: TTree; APos: TPos; ALine: integer;
      APipe: TPipeNode; AList, AElseList: TListNode);
    function Copy: TNode; override;
  end;

  TWithNode = class(TBranchNode)
  public
    constructor Create(ATree: TTree; APos: TPos; ALine: integer;
      APipe: TPipeNode; AList, AElseList: TListNode);
    function Copy: TNode; override;
  end;

  TTemplateNode = class(TNode)
  public
    Line: integer;
    Name: string;
    Pipe: TPipeNode;
    constructor Create(ATree: TTree; APos: TPos; ALine: integer;
      AName: string; APipe: TPipeNode);
    destructor Destroy; override;
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TBreakNode = class(TNode)
  public
    Line: integer;
    constructor Create(ATree: TTree; APos: TPos; ALine: integer);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TContinueNode = class(TNode)
  public
    Line: integer;
    constructor Create(ATree: TTree; APos: TPos; ALine: integer);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TEndNode = class(TNode)
  public
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TElseNode = class(TNode)
  public
    Line: integer;
    function TypeNum: TNodeType; override;
    constructor Create(ATree: TTree; APos: TPos; ALine: integer);
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TCommentNode = class(TNode)
  public
    Text: string;
    constructor Create(ATree: TTree; APos: TPos; const AText: string);
    function TypeNum: TNodeType; override;
    function Copy: TNode; override;
    procedure WriteTo(Strings: TStrings); override;
  end;

  TTree = class
  private
    FName: string;
    FParseName: string;
    FRoot: TListNode;
    FText: string;
    FFuncs: TFPList;
    FLexer: TLexer;
    FToken: array[0..2] of TItem;
    FPeekCount: integer;
    FVars: array of string;
    FTreeSet: TTreeSet;
    FActionLine: integer;
    FRangeDepth: integer;
    FStackDepth: integer;
    FMode: integer;
    FOwnsFuncs: boolean;

    procedure SetRoot(ARoot: TListNode);
    procedure ErrorF(const Msg: string);
    procedure ErrorFmt(const Fmt: string; const Args: array of const);
    procedure Expect(ATyp: TItemType; Context: string);
    function ExpectOneOf(A1, A2: TItemType; Context: string): TItem;
    procedure Unexpected(AToken: TItem; Context: string);
    procedure StartParse(Funcs: TFPList; ALexer: TLexer; ATreeSet: TTreeSet);
    procedure StopParse;
    procedure AddToTreeSet;
    procedure ParseDefinition;
    function ItemList(out Next: TNode): TListNode;
    function TextOrAction: TNode;
    function Action: TNode;
    function Pipeline(Context: string; EndTyp: TItemType): TPipeNode;
    procedure CheckPipeline(Pipe: TPipeNode; const Context: string);
    function Command: TCommandNode;
    function Operand: TNode;
    function Term: TNode;
    function ParseControl(Context: string): TNode;
    function IfControl: TNode;
    function RangeControl: TNode;
    function WithControl: TNode;
    function EndControl: TNode;
    function ElseControl: TNode;
    function BlockControl: TNode;
    function TemplateControl: TNode;
    function BreakControl(Pos: TPos; Line: integer): TNode;
    function ContinueControl(Pos: TPos; Line: integer): TNode;
    function ParseTemplateName(Token: TItem; Context: string): string;

    function UseVar(Pos: TPos; Name: string): TNode;
    procedure PopVars(N: integer);
    function HasFunction(Name: string): boolean;

    function Next: TItem;
    procedure Backup;
    procedure Backup2(T1: TItem);
    procedure Backup3(T2, T1: TItem);
    function Peek: TItem;
    function NextNonSpace: TItem;
    function PeekNonSpace: TItem;

    function NewList(Pos: TPos): TListNode;
    function NewText(Pos: TPos; Text: string): TTextNode;
    function NewAction(Pos: TPos; Line: integer; Pipe: TPipeNode): TActionNode;
    function NewPipe(Pos: TPos; Line: integer; Vars: array of TVariableNode): TPipeNode;
    function NewCommand(Pos: TPos): TCommandNode;
    function NewIdentifier(Str: string): TIdentifierNode;
    function NewVariable(Pos: TPos; Ident: string): TVariableNode;
    function NewDot(Pos: TPos): TDotNode;
    function NewNil(Pos: TPos): TNilNode;
    function NewField(Pos: TPos; Ident: string): TFieldNode;
    function NewChain(Pos: TPos; Node: TNode): TChainNode;
    function NewBool(Pos: TPos; True: boolean): TBoolNode;
    function NewNumber(Pos: TPos; Text: string; Typ: TItemType): TNumberNode;
    function NewString(Pos: TPos; Quoted, Text: string): TStringNode;
    function NewEnd(Pos: TPos): TEndNode;
    function NewElse(Pos: TPos; Line: integer): TElseNode;
    function NewIf(Pos: TPos; Line: integer; Pipe: TPipeNode;
      List, ElseList: TListNode): TIfNode;
    function NewRange(Pos: TPos; Line: integer; Pipe: TPipeNode;
      List, ElseList: TListNode): TRangeNode;
    function NewWith(Pos: TPos; Line: integer; Pipe: TPipeNode;
      List, ElseList: TListNode): TWithNode;
    function NewTemplate(Pos: TPos; Line: integer; Name: string;
      Pipe: TPipeNode): TTemplateNode;
    function NewBreak(Pos: TPos; Line: integer): TBreakNode;
    function NewContinue(Pos: TPos; Line: integer): TContinueNode;
    function NewComment(Pos: TPos; const Text: string): TCommentNode;

    procedure ClearActionLine;
  public
    constructor Create(AName: string; Funcs: array of TStrings;
      const OwnsFuncs: boolean = False);
    destructor Destroy; override;

    function Parse(AText, LeftDelim, RightDelim: string; TreeSet: TTreeSet;
      Funcs: array of TStrings): TTree;
    function Copy: TTree;

    property Name: string read FName write FName;
    property ParseName: string read FParseName write FParseName;
    property Root: TListNode read FRoot write SetRoot;
    property Mode: integer read FMode write FMode;
  end;

  { TLexer }
  TLexState = (
    lsText, lsLeftDelim, lsComment, lsRightDelim, lsInsideAction,
    lsSpace, lsIdentifier, lsField, lsVariable, lsChar, lsNumber,
    lsQuote, lsRawQuote, lsDone
    );

  TLexer = class
  private
    FName: string;
    FInput: string;
    FLeftDelim: string;
    FRightDelim: string;
    FPos, FStart: TPos;
    FAtEOF: boolean;
    FParenDepth: integer;
    FLine, FStartLine: integer;
    FItem: TItem;
    FInsideAction: boolean;
    FOptions: record
      EmitComment, BreakOK, ContinueOK: boolean;
      end;

    function Emit(ATyp: TItemType): TLexState;
    procedure EmitItem(AItem: TItem);
    function ErrorF(const Fmt: string): TLexState;
    procedure Ignore;
    function Accept(Valid: string): boolean;
    procedure AcceptRun(Valid: string);
    function NextRune: cardinal;
    procedure BackupRune;
    function PeekRune: cardinal;
    procedure AtRightDelim(out Delim, TrimSpaces: boolean);
    function HasLeftTrimMarker(const S: string): boolean;
    function HasRightTrimMarker(const S: string): boolean;

    function LexText: TLexState;
    function LexLeftDelim: TLexState;
    function LexComment: TLexState;
    function LexRightDelim: TLexState;
    function LexInsideAction: TLexState;
    function LexSpace: TLexState;
    function LexIdentifier: TLexState;
    function LexField: TLexState;
    function LexVariable: TLexState;
    function LexFieldOrVariable(Typ: TItemType): TLexState;
    function AtTerminator: boolean;
    function LexChar: TLexState;
    function LexNumber: TLexState;
    function LexQuote: TLexState;
    function LexRawQuote: TLexState;
  public
    constructor Create(AName, AInput, ALeft, ARight: string);
    function NextItem: TItem;
  end;

function IsEmptyTree(N: TNode): boolean;
function ParseComplexGoLiteral(const S: string; out C: Complex): boolean;
function ParseFloatInvariant(const S: string; out V: double): boolean;
function DecodeUnicodeEscapeSequences(const S: string): string;
function UTF8DecodeEscapeSequences(const S: string): string;
function UTF8PrevCharIndex(const S: string; Index: SizeInt): SizeInt;

implementation

const
  leftDelim = '{{';
  rightDelim = '}}';
  leftComment = '/*';
  rightComment = '*/';
  spaceChars = ' '#9#13#10;
  trimMarker = '-';
  trimMarkerLen = 2;

{ Helpers }
function IsSpace(r: cardinal): boolean;
begin
  Result := (r = Ord(' ')) or (r = 9) or (r = 13) or (r = 10);
end;

function IsAlphaNumeric(r: cardinal): boolean;
begin
  Result := CharInSet(Chr(r), ['a'..'z', 'A'..'Z', '0'..'9', '_']);
end;

function IsHexDigit(C: char): boolean; inline;
begin
  Result := (C >= '0') and (C <= '9') or (C >= 'a') and (C <= 'f') or
    (C >= 'A') and (C <= 'F');
end;

function IsDigit(C: char): boolean; inline;
begin
  Result := (C >= '0') and (C <= '9');
end;

procedure AppendStrings(var Arr: TStringDynArray; const Parts: TStringArray);
var
  OldLen, I: integer;
begin
  OldLen := Length(Arr);
  SetLength(Arr, OldLen + Length(Parts));
  for I := 0 to High(Parts) do
    Arr[OldLen + I] := Parts[I];
end;

function SplitFieldAccess(const FieldTok: string): TStringArray;
  { FieldTok like ".A.B" or ".A" or "." (dot token is itemDot, but be defensive) }
var
  S: string;
begin
  S := FieldTok;
  if (S <> '') and (S[1] = '.') then Delete(S, 1, 1);
  if S = '' then
    SetLength(Result, 0)
  else
    Result := S.Split(['.']);
end;

procedure AppendIdentParts(var Arr: TStringDynArray; const Parts: TStringArray);
begin
  AppendStrings(Arr, Parts);
end;

procedure AppendIdentFromFieldToken(var Arr: TStringDynArray; const FieldTok: string);
var
  Parts: TStringArray;
begin
  Parts := SplitFieldAccess(FieldTok);
  AppendIdentParts(Arr, Parts);
end;

function RightTrimLength(const S: string): TPos;
var
  I: integer;
begin
  I := Length(S);
  while (I > 0) and IsSpace(Ord(S[I])) do Dec(I);
  Result := Length(S) - I;
end;

function LeftTrimLength(const S: string): TPos;
var
  I: integer;
begin
  I := 1;
  while (I <= Length(S)) and IsSpace(Ord(S[I])) do Inc(I);
  Result := I - 1;
end;

function IsEmptyTree(N: TNode): boolean;
var
  I: integer;
  L: TListNode;
  T: TTextNode;
begin
  if N = nil then Exit(True);

  case N.TypeNum of
    NodeComment:
      Exit(True);

    NodeText:
    begin
      T := TTextNode(N);
      Exit(Trim(T.Text) = '');
    end;

    NodeList:
    begin
      L := TListNode(N);
      for I := 0 to L.Nodes.Count - 1 do
        if not IsEmptyTree(TNode(L.Nodes[I])) then Exit(False);
      Exit(True);
    end;
    else
      Exit(False);
  end;
end;

{ TItem }
function TItem.ToString: string;
begin
  if Typ = itemEOF then Exit('EOF');
  if Typ = itemError then Exit(Val);
  if Typ > itemKeyword then Exit('<' + Val + '>');
  if Length(Val) > 10 then Exit('''' + Copy(Val, 1, 10) + '...''');
  Result := '''' + Val + '''';
end;

{ TNode }
constructor TNode.Create(ATree: TTree; APos: TPos; AType: TNodeType);
begin
  inherited Create;
  FTree := ATree;
  FPos := APos;
  FNodeType := AType;
end;

function TNode.TypeNum: TNodeType;
begin
  Result := FNodeType;
end;

function TNode.ToString: string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LineBreak := '';
    WriteTo(SL);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TNode.Position: TPos;
begin
  Result := FPos;
end;

function TNode.Tree: TTree;
begin
  Result := FTree;
end;

{ TListNode }
constructor TListNode.Create(ATree: TTree; APos: TPos);
begin
  inherited Create(ATree, APos, NodeList);
  Nodes := TFPList.Create;
end;

destructor TListNode.Destroy;
var
  I: integer;
begin
  for I := 0 to Nodes.Count - 1 do
    if Assigned(Nodes[I]) then TNode(Nodes[I]).Free;
  Nodes.Free;
  inherited Destroy;
end;

procedure TListNode.Append(N: TNode);
begin
  if Assigned(N) then Nodes.Add(N);
end;

function TListNode.CopyList: TListNode;
var
  I: integer;
  N: TNode;
begin
  Result := TListNode.Create(FTree, FPos);
  for I := 0 to Nodes.Count - 1 do
  begin
    N := TNode(Nodes[I]);
    if Assigned(N) then Result.Append(N.Copy);
  end;
end;

function TListNode.Copy: TNode;
begin
  Result := CopyList;
end;

procedure TListNode.WriteTo(Strings: TStrings);
var
  I: integer;
  N: TNode;
begin
  for I := 0 to Nodes.Count - 1 do
  begin
    N := TNode(Nodes[I]);
    if Assigned(N) then N.WriteTo(Strings);
  end;
end;

{ TTextNode }
constructor TTextNode.Create(ATree: TTree; APos: TPos; AText: string);
begin
  inherited Create(ATree, APos, NodeText);
  Text := AText;
end;

function TTextNode.Copy: TNode;
begin
  Result := TTextNode.Create(FTree, FPos, Text);
end;

procedure TTextNode.WriteTo(Strings: TStrings);
begin
  Strings.Add(Text);
end;

{ TActionNode }
constructor TActionNode.Create(ATree: TTree; APos: TPos; ALine: integer;
  APipe: TPipeNode);
begin
  inherited Create(ATree, APos, NodeAction);
  Line := ALine;
  Pipe := APipe;
end;

destructor TActionNode.Destroy;
begin
  Pipe.Free;
  inherited;
end;

function TActionNode.Copy: TNode;
begin
  Result := TActionNode.Create(FTree, FPos, Line, TPipeNode(Pipe.CopyPipe));
end;

procedure TActionNode.WriteTo(Strings: TStrings);
begin
  Strings.Add('{{');
  if Assigned(Pipe) then
    Pipe.WriteTo(Strings);
  Strings.Add('}}');
end;

{ TIdentifierNode }
constructor TIdentifierNode.Create(ATree: TTree; APos: TPos; const AIdent: string);
begin
  inherited Create(ATree, APos, NodeIdentifier);
  Ident := AIdent;
end;

function TIdentifierNode.Copy: TNode;
begin
  Result := TIdentifierNode.Create(FTree, FPos, Ident);
end;

procedure TIdentifierNode.WriteTo(Strings: TStrings);
begin
  Strings.Add(Ident);
end;

{ TVariableNode }
constructor TVariableNode.Create(ATree: TTree; APos: TPos; AIdent: string);
begin
  inherited Create(ATree, APos, NodeVariable);
  Ident := AIdent.Split(['.']);
end;

function TVariableNode.Copy: TNode;
var
  N: TVariableNode;
  I: integer;
begin
  N := TVariableNode.Create(FTree, FPos, '');
  SetLength(N.Ident, Length(Self.Ident));
  for I := 0 to High(Self.Ident) do N.Ident[I] := Self.Ident[I];
  Result := N;
end;

procedure TVariableNode.WriteTo(Strings: TStrings);
var
  I: integer;
  S: string;
begin
  S := '';
  for I := 0 to High(Ident) do
  begin
    if I > 0 then S := S + '.';
    S := S + Ident[I];
  end;
  Strings.Add(S);
end;

{ TDotNode }
function TDotNode.TypeNum: TNodeType;
begin
  Result := NodeDot;
end;

function TDotNode.Copy: TNode;
begin
  Result := TDotNode.Create(FTree, FPos, NodeDot);
end;

procedure TDotNode.WriteTo(Strings: TStrings);
begin
  Strings.Add('.');
end;

{ TNilNode }
function TNilNode.TypeNum: TNodeType;
begin
  Result := NodeNil;
end;

function TNilNode.Copy: TNode;
begin
  Result := TNilNode.Create(FTree, FPos, NodeNil);
end;

procedure TNilNode.WriteTo(Strings: TStrings);
begin
  Strings.Add('nil');
end;

{ TFieldNode }
constructor TFieldNode.Create(ATree: TTree; APos: TPos; AIdent: string);
var
  Parts: TStringArray;
begin
  inherited Create(ATree, APos, NodeField);
  if Length(AIdent) > 0 then
    Delete(AIdent, 1, 1);
  Parts := AIdent.Split(['.']);
  Ident := Parts;
end;

function TFieldNode.Copy: TNode;
var
  N: TFieldNode;
  I: integer;
begin
  N := TFieldNode.Create(FTree, FPos, '');
  SetLength(N.Ident, Length(Self.Ident));
  for I := 0 to High(Self.Ident) do N.Ident[I] := Self.Ident[I];
  Result := N;
end;

procedure TFieldNode.WriteTo(Strings: TStrings);
var
  I: integer;
  S: string;
begin
  S := '';
  for I := 0 to High(Ident) do
    S := S + '.' + Ident[I];
  Strings.Add(S);
end;

{ TChainNode }
constructor TChainNode.Create(ATree: TTree; APos: TPos; ANode: TNode);
begin
  inherited Create(ATree, APos, NodeChain);
  Node := ANode;
end;

destructor TChainNode.Destroy;
begin
  Node.Free;
  inherited;
end;

procedure TChainNode.Add(AField: string);
begin
  if Length(AField) = 0 then Exit;
  if AField[1] = '.' then Delete(AField, 1, 1);
  SetLength(Field, Length(Field) + 1);
  Field[High(Field)] := AField;
end;

function TChainNode.Copy: TNode;
var
  N: TChainNode;
  I: integer;
begin
  N := TChainNode.Create(FTree, FPos, Node.Copy);
  SetLength(N.Field, Length(Self.Field));
  for I := 0 to High(Self.Field) do N.Field[I] := Self.Field[I];
  Result := N;
end;

procedure TChainNode.WriteTo(Strings: TStrings);
var
  I: integer;
begin
  Node.WriteTo(Strings);
  for I := 0 to High(Field) do
    Strings.Append('.' + Field[I]);
end;

{ TBoolNode }
constructor TBoolNode.Create(ATree: TTree; APos: TPos; ATrue: boolean);
begin
  inherited Create(ATree, APos, NodeBool);
  Self.True := ATrue;
end;

function TBoolNode.Copy: TNode;
begin
  Result := TBoolNode.Create(FTree, FPos, True);
end;

procedure TBoolNode.WriteTo(Strings: TStrings);
begin
  if True then Strings.Add('true')
  else
    Strings.Add('false');
end;

{ TNumberNode }


constructor TNumberNode.Create(ATree: TTree; APos: TPos; AText: string; ATyp: TItemType);
var
  C: Complex;
  V: double;
  FS: TFormatSettings;
  T: string;
  I64: int64;
  U64: QWord;
begin
  inherited Create(ATree, APos, NodeNumber);
  Text := AText;

  if ParseComplexGoLiteral(AText, C) then
  begin
    IsComplex := True;
    Complex128 := C;
    Exit;
  end;

  // remove '_' like Go allows
  T := StringReplace(AText, '_', '', [rfReplaceAll]);

  // If it has '.' or exponent markers -> float (also hex-float 0x..p.. is float)
  if (Pos('.', T) > 0) or (Pos('e', LowerCase(T)) > 0) or
    (Pos('p', LowerCase(T)) > 0) then
  begin
    if ParseFloatInvariant(T, V) then
    begin
      IsFloat := True;
      Float64 := V;
      Exit;
    end;
  end;

  // Try signed integer
  if TryStrToInt64(T, I64) then
  begin
    IsInt := True;
    int64 := I64;
    Exit;
  end;

  // Try unsigned integer (Go has uint but in templates ideal constants become int-ish;
  // still store it)
  if (Length(T) > 0) and (T[1] <> '-') and TryStrToQWord(T, U64) then
  begin
    IsUint := True;
    uint64 := U64;
    Exit;
  end;

  // Fallback: float
  if ParseFloatInvariant(T, V) then
  begin
    IsFloat := True;
    Float64 := V;
    Exit;
  end;
end;

function TNumberNode.Copy: TNode;
begin
  Result := TNumberNode.Create(FTree, FPos, Text);
  with TNumberNode(Result) do
  begin
    IsInt := Self.IsInt;
    IsUint := Self.IsUint;
    IsFloat := Self.IsFloat;
    IsComplex := Self.IsComplex;
    int64 := Self.int64;
    uint64 := Self.uint64;
    Float64 := Self.Float64;
    Complex128 := Self.Complex128;
    Text := Self.Text;
  end;
end;

procedure TNumberNode.WriteTo(Strings: TStrings);
begin
  Strings.Add(Text);
end;

{ TStringNode }
constructor TStringNode.Create(ATree: TTree; APos: TPos; AQuoted, AText: string);
begin
  inherited Create(ATree, APos, NodeString);
  Quoted := AQuoted;
  Text := AText;
end;

function TStringNode.Copy: TNode;
begin
  Result := TStringNode.Create(FTree, FPos, Quoted, Text);
end;

procedure TStringNode.WriteTo(Strings: TStrings);
begin
  Strings.Add(Quoted);
end;

{ TPipeNode }
constructor TPipeNode.Create(ATree: TTree; APos: TPos; ALine: integer;
  Vars: array of TVariableNode);
var
  I: integer;
begin
  inherited Create(ATree, APos, NodePipe);
  Line := ALine;
  SetLength(Decl, Length(Vars));
  for I := 0 to High(Vars) do Decl[I] := Vars[I];
end;

destructor TPipeNode.Destroy;
var
  I: integer;
begin
  for I := 0 to High(Decl) do Decl[I].Free;
  for I := 0 to High(Cmds) do Cmds[I].Free;
  inherited;
end;

procedure TPipeNode.Append(Cmd: TCommandNode);
begin
  SetLength(Cmds, Length(Cmds) + 1);
  Cmds[High(Cmds)] := Cmd;
end;

function TPipeNode.CopyPipe: TPipeNode;
var
  I: integer;
  N: TPipeNode;
begin
  N := TPipeNode.Create(FTree, FPos, Line, []);
  N.IsAssign := Self.IsAssign;
  SetLength(N.Decl, Length(Self.Decl));
  for I := 0 to High(Self.Decl) do N.Decl[I] := TVariableNode(Self.Decl[I].Copy);
  for I := 0 to High(Self.Cmds) do N.Append(TCommandNode(Self.Cmds[I].Copy));
  Result := N;
end;

function TPipeNode.Copy: TNode;
begin
  Result := CopyPipe;
end;

procedure TPipeNode.WriteTo(Strings: TStrings);
var
  I: integer;
  S: string;
begin
  if Length(Decl) > 0 then
  begin
    S := '';
    for I := 0 to High(Decl) do
    begin
      if I > 0 then S := S + ', ';
      Decl[I].WriteTo(Strings);
      S := S + Strings[Strings.Count - 1];
      Strings.Delete(Strings.Count - 1);
    end;
    if IsAssign then S := S + ' = '
    else
      S := S + ' := ';
    Strings.Add(S);
  end;
  for I := 0 to High(Cmds) do
  begin
    if I > 0 then Strings.Add(' | ');
    Cmds[I].WriteTo(Strings);
  end;
end;

{ TCommandNode }
destructor TCommandNode.Destroy;
var
  I: integer;
begin
  for I := 0 to High(Args) do Args[I].Free;
  inherited;
end;

procedure TCommandNode.Append(Arg: TNode);
begin
  SetLength(Args, Length(Args) + 1);
  Args[High(Args)] := Arg;
end;

function TCommandNode.Copy: TNode;
var
  I: integer;
  N: TCommandNode;
begin
  N := TCommandNode.Create(FTree, FPos, NodeCommand);
  SetLength(N.Args, Length(Self.Args));
  for I := 0 to High(Self.Args) do N.Args[I] := Self.Args[I].Copy;
  Result := N;
end;

procedure TCommandNode.WriteTo(Strings: TStrings);
var
  I: integer;
  S: string;
begin
  S := '';
  for I := 0 to High(Args) do
  begin
    if I > 0 then S := S + ' ';
    Args[I].WriteTo(Strings);
    if Strings.Count > 0 then
    begin
      S := S + Strings[Strings.Count - 1];
      Strings.Delete(Strings.Count - 1);
    end;
  end;
  Strings.Add(S);
end;

{ TBranchNode }
constructor TBranchNode.Create(ATree: TTree; APos: TPos; AType: TNodeType;
  ALine: integer; APipe: TPipeNode; AList, AElseList: TListNode);
begin
  inherited Create(ATree, APos, AType);
  Line := ALine;
  Pipe := APipe;
  List := AList;
  ElseList := AElseList;
end;

destructor TBranchNode.Destroy;
begin
  Pipe.Free;
  List.Free;
  ElseList.Free;
  inherited;
end;

function TBranchNode.Copy: TNode;
begin
  Result := nil;
end;

procedure TBranchNode.WriteTo(Strings: TStrings);
var
  Name: string;
begin
  Name := '';
  case TypeNum of
    NodeIf: Name := 'if';
    NodeRange: Name := 'range';
    NodeWith: Name := 'with';
  end;
  Strings.Add('{{' + Name + ' ');
  if Assigned(Pipe) then Pipe.WriteTo(Strings);
  Strings.Add('}}');
  if Assigned(List) then List.WriteTo(Strings);
  if Assigned(ElseList) then
  begin
    Strings.Add('{{else}}');
    ElseList.WriteTo(Strings);
  end;
  Strings.Add('{{end}}');
end;

{ TIfNode }
constructor TIfNode.Create(ATree: TTree; APos: TPos; ALine: integer;
  APipe: TPipeNode; AList, AElseList: TListNode);
begin
  inherited Create(ATree, APos, NodeIf, ALine, APipe, AList, AElseList);
end;

function TIfNode.Copy: TNode;
var
  NewElse: TListNode;
begin
  NewElse := nil;
  if Assigned(ElseList) then
    NewElse := TListNode(ElseList.CopyList);
  Result := TIfNode.Create(FTree, FPos, Line, TPipeNode(Pipe.CopyPipe),
    TListNode(List.CopyList), NewElse);
end;

{ TRangeNode }
constructor TRangeNode.Create(ATree: TTree; APos: TPos; ALine: integer;
  APipe: TPipeNode; AList, AElseList: TListNode);
begin
  inherited Create(ATree, APos, NodeRange, ALine, APipe, AList, AElseList);
end;

function TRangeNode.Copy: TNode;
var
  NewElse: TListNode;
begin
  NewElse := nil;
  if Assigned(ElseList) then
    NewElse := TListNode(ElseList.CopyList);
  Result := TRangeNode.Create(FTree, FPos, Line, TPipeNode(Pipe.CopyPipe),
    TListNode(List.CopyList), NewElse);
end;

{ TWithNode }
constructor TWithNode.Create(ATree: TTree; APos: TPos; ALine: integer;
  APipe: TPipeNode; AList, AElseList: TListNode);
begin
  inherited Create(ATree, APos, NodeWith, ALine, APipe, AList, AElseList);
end;

function TWithNode.Copy: TNode;
var
  NewElse: TListNode;
begin
  NewElse := nil;
  if Assigned(ElseList) then
    NewElse := TListNode(ElseList.CopyList);
  Result := TWithNode.Create(FTree, FPos, Line, TPipeNode(Pipe.CopyPipe),
    TListNode(List.CopyList), NewElse);
end;

{ TTemplateNode }
constructor TTemplateNode.Create(ATree: TTree; APos: TPos; ALine: integer;
  AName: string; APipe: TPipeNode);
begin
  inherited Create(ATree, APos, NodeTemplate);
  Line := ALine;
  Name := AName;
  Pipe := APipe;
end;

destructor TTemplateNode.Destroy;
begin
  Pipe.Free;
  inherited;
end;

function TTemplateNode.Copy: TNode;
var
  NewPipe: TPipeNode;
begin
  NewPipe := nil;
  if Assigned(Pipe) then
    NewPipe := TPipeNode(Pipe.CopyPipe);
  Result := TTemplateNode.Create(FTree, FPos, Line, Name, NewPipe);
end;

procedure TTemplateNode.WriteTo(Strings: TStrings);
begin
  Strings.Add('{{template "' + Name + '"');
  if Assigned(Pipe) then
  begin
    Strings.Add(' ');
    Pipe.WriteTo(Strings);
  end;
  Strings.Add('}}');
end;

{ TBreakNode }
constructor TBreakNode.Create(ATree: TTree; APos: TPos; ALine: integer);
begin
  inherited Create(ATree, APos, NodeBreak);
  Line := ALine;
end;

function TBreakNode.Copy: TNode;
begin
  Result := TBreakNode.Create(FTree, FPos, Line);
end;

procedure TBreakNode.WriteTo(Strings: TStrings);
begin
  Strings.Add('{{break}}');
end;

{ TContinueNode }
constructor TContinueNode.Create(ATree: TTree; APos: TPos; ALine: integer);
begin
  inherited Create(ATree, APos, NodeContinue);
  Line := ALine;
end;

function TContinueNode.Copy: TNode;
begin
  Result := TContinueNode.Create(FTree, FPos, Line);
end;

procedure TContinueNode.WriteTo(Strings: TStrings);
begin
  Strings.Add('{{continue}}');
end;

{ TEndNode }
function TEndNode.Copy: TNode;
begin
  Result := TEndNode.Create(FTree, FPos, NodeEnd);
end;

procedure TEndNode.WriteTo(Strings: TStrings);
begin
  Strings.Add('{{end}}');
end;

{ TElseNode }
function TElseNode.TypeNum: TNodeType;
begin
  Result := nodeElse;
end;

constructor TElseNode.Create(ATree: TTree; APos: TPos; ALine: integer);
begin
  inherited Create(ATree, APos, nodeElse);
  Line := ALine;
end;

function TElseNode.Copy: TNode;
begin
  Result := TElseNode.Create(FTree, FPos, Line);
end;

procedure TElseNode.WriteTo(Strings: TStrings);
begin
  Strings.Add('{{else}}');
end;

{ TTreeSet }
constructor TTreeSet.Create;
begin
  inherited Create;
  Sorted := False;
  Duplicates := dupIgnore;
  OwnsObjects := True;
end;

{ TCommentNode }
constructor TCommentNode.Create(ATree: TTree; APos: TPos; const AText: string);
begin
  inherited Create(ATree, APos, NodeComment);
  Text := AText;
end;

function TCommentNode.TypeNum: TNodeType;
begin
  Result := NodeComment;
end;

function TCommentNode.Copy: TNode;
begin
  Result := TCommentNode.Create(FTree, FPos, Text);
end;

procedure TCommentNode.WriteTo(Strings: TStrings);
begin
  Strings.Add(Text);
end;

{ TLexer }
constructor TLexer.Create(AName, AInput, ALeft, ARight: string);
begin
  FName := AName;
  FInput := AInput;
  FLeftDelim := IfThen(ALeft = '', '{{', ALeft);
  FRightDelim := IfThen(ARight = '', '}}', ARight);
  FLine := 1;
  FStartLine := 1;
  FPos := 0;
  FStart := 0;
  FInsideAction := False;
end;

function TLexer.NextRune: cardinal;
var
  Len, NewPos: integer;
  Ch: string;
begin
  if FPos >= Length(FInput) then
  begin
    FAtEOF := True;
    Exit(cardinal(-1));
  end;

  { FPos is byte index (0..Length), Lazarus strings are 1-based }
  NewPos := FPos + 1;
  Ch := UTF8Copy(FInput, NewPos, 1);  { one UTF-8 codepoint as string }
  Len := Length(Ch);                 { bytes count }
  if Len <= 0 then
  begin
    FAtEOF := True;
    Exit(cardinal(-1));
  end;

  Result := UTF8CharacterToUnicode(PChar(Ch), Len);
  Inc(FPos, Len);
  if Result = 10 then Inc(FLine);
end;

procedure TLexer.BackupRune;
var
  PrevPos: integer;
  Ch: string;
  Len: integer;
begin
  if FPos > 0 then
  begin
    { Move back by 1 UTF-8 codepoint (byte-wise position kept in FPos) }
    PrevPos := UTF8PrevCharIndex(FInput, FPos + 1); { returns 1..Length+1 }
    if PrevPos < 1 then PrevPos := 1;
    Ch := Copy(FInput, PrevPos, (FPos + 1) - PrevPos);
    Len := Length(Ch);
    FPos := PrevPos - 1;
    if (Len > 0) and (Ch[Len] = #10) then Dec(FLine);
    FAtEOF := False;
  end;
end;

function TLexer.PeekRune: cardinal;
begin
  Result := NextRune;
  BackupRune;
end;

procedure TLexer.EmitItem(AItem: TItem);
begin
  FItem := AItem;
end;

function TLexer.Emit(ATyp: TItemType): TLexState;
var
  I: TItem;
begin
  I.Typ := ATyp;
  I.Pos := FStart;
  I.Val := Copy(FInput, FStart + 1, FPos - FStart);
  I.Line := FStartLine;
  FItem := I;
  FStart := FPos;
  FStartLine := FLine;
  Result := lsDone;
end;

function TLexer.ErrorF(const Fmt: string): TLexState;
begin
  FItem.Typ := itemError;
  FItem.Pos := FStart;
  FItem.Val := Fmt;
  FItem.Line := FStartLine;
  FStart := 0;
  FPos := 0;
  FInput := '';
  Result := lsDone;
end;

procedure TLexer.Ignore;
var
  S: string;
  I: integer;
begin
  if FPos > FStart then
  begin
    S := Copy(FInput, FStart + 1, FPos - FStart);
    for I := 1 to Length(S) do
      if S[I] = #10 then Inc(FLine);
  end;
  FStart := FPos;
  FStartLine := FLine;
end;

function TLexer.Accept(Valid: string): boolean;
var
  Ch: char;
begin
  if FPos >= Length(FInput) then
    Exit(False);

  Ch := FInput[FPos + 1];
  if Pos(Ch, Valid) > 0 then
  begin
    Inc(FPos);
    if Ch = #10 then Inc(FLine);
    Result := True;
  end
  else
    Result := False;
end;

procedure TLexer.AcceptRun(Valid: string);
var
  Ch: char;
begin
  while FPos < Length(FInput) do
  begin
    Ch := FInput[FPos + 1];
    if Pos(Ch, Valid) = 0 then
      Break;
    Inc(FPos);
    if Ch = #10 then Inc(FLine);
  end;
end;

function TLexer.NextItem: TItem;
var
  State: TLexState;
begin
  FItem.Typ := itemEOF;
  FItem.Pos := FPos;
  FItem.Val := 'EOF';
  FItem.Line := FStartLine;

  if FInsideAction then
    State := lsInsideAction
  else
    State := lsText;

  while True do
  begin
    case State of
      lsText: State := LexText;
      lsLeftDelim: State := LexLeftDelim;
      lsComment: State := LexComment;
      lsRightDelim: State := LexRightDelim;
      lsInsideAction: State := LexInsideAction;
      lsSpace: State := LexSpace;
      lsIdentifier: State := LexIdentifier;
      lsField: State := LexField;
      lsVariable: State := LexVariable;
      lsChar: State := LexChar;
      lsNumber: State := LexNumber;
      lsQuote: State := LexQuote;
      lsRawQuote: State := LexRawQuote;
      lsDone:
        Exit(FItem);
      else
        Exit(FItem);
    end;
  end;
end;

function TLexer.HasLeftTrimMarker(const S: string): boolean;
begin
  Result := (Length(S) >= 2) and (S[1] = trimMarker) and IsSpace(Ord(S[2]));
end;

function TLexer.HasRightTrimMarker(const S: string): boolean;
begin
  Result := (Length(S) >= 2) and IsSpace(Ord(S[1])) and (S[2] = trimMarker);
end;

procedure TLexer.AtRightDelim(out Delim, TrimSpaces: boolean);
var
  Rem: string;
begin
  Rem := Copy(FInput, FPos + 1, MaxInt);
  if HasRightTrimMarker(Rem) and StartsStr(FRightDelim,
    Copy(Rem, 1 + trimMarkerLen, MaxInt)) then
  begin
    Delim := True;
    TrimSpaces := True;
    Exit;
  end;
  if StartsStr(FRightDelim, Rem) then
  begin
    Delim := True;
    TrimSpaces := False;
    Exit;
  end;
  Delim := False;
  TrimSpaces := False;
end;

function TLexer.LexText: TLexState;
var
  Sub, Rem: string;
  X: SizeInt;
  DelimEnd: TPos;
  TrimLen: TPos;
  I: TItem;
begin
  Sub := Copy(FInput, FPos + 1, MaxInt);
  X := System.Pos(FLeftDelim, Sub);

  if X > 0 then
  begin
    if X > 1 then
    begin
      Inc(FPos, X - 1);
      TrimLen := 0;
      DelimEnd := FPos + Length(FLeftDelim);
      Rem := Copy(FInput, DelimEnd + 1, MaxInt);
      if HasLeftTrimMarker(Rem) then
        TrimLen := RightTrimLength(Copy(FInput, FStart + 1, FPos - FStart));

      Dec(FPos, TrimLen);

      I.Typ := itemText;
      I.Pos := FStart;
      I.Val := Copy(FInput, FStart + 1, FPos - FStart);
      I.Line := FStartLine;
      FItem := I;

      Inc(FPos, TrimLen);
      Ignore;

      if Length(FItem.Val) > 0 then
        Exit(lsDone);
    end;
    Exit(lsLeftDelim);
  end;

  FPos := Length(FInput);
  if FPos > FStart then
    Exit(Emit(itemText))
  else
    Exit(Emit(itemEOF));
end;

function TLexer.LexLeftDelim: TLexState;
var
  TrimSpace: boolean;
  AfterMarker: TPos;
  Rem: string;
begin
  Inc(FPos, Length(FLeftDelim));
  Rem := Copy(FInput, FPos + 1, MaxInt);
  TrimSpace := HasLeftTrimMarker(Rem);
  AfterMarker := 0;
  if TrimSpace then AfterMarker := trimMarkerLen;

  if StartsStr(leftComment, Copy(FInput, FPos + AfterMarker + 1, MaxInt)) then
  begin
    Inc(FPos, AfterMarker);
    Ignore;
    Exit(lsComment);
  end;

  Result := Emit(itemLeftDelim);
  FInsideAction := True;
  Inc(FPos, AfterMarker);
  Ignore;
  FParenDepth := 0;
end;

function TLexer.LexComment: TLexState;
var
  X: SizeInt;
  Delim, TrimSpace: boolean;
begin
  Inc(FPos, Length(leftComment));
  X := System.Pos(rightComment, Copy(FInput, FPos + 1, MaxInt));
  if X = 0 then
    Exit(ErrorF('unclosed comment'));

  Inc(FPos, X - 1 + Length(rightComment));

  AtRightDelim(Delim, TrimSpace);
  if not Delim then
    Exit(ErrorF('comment ends before closing delimiter'));

  Emit(itemComment);

  if TrimSpace then
  begin
    Inc(FPos, trimMarkerLen);
    Ignore;
  end;

  Inc(FPos, Length(FRightDelim));

  if TrimSpace then
  begin
    Inc(FPos, LeftTrimLength(Copy(FInput, FPos + 1, MaxInt)));
  end;
  Ignore;

  if FOptions.EmitComment then
    Exit(lsDone)
  else
    Exit(lsText);
end;

function TLexer.LexRightDelim: TLexState;
var
  Delim, TrimSpace: boolean;
begin
  AtRightDelim(Delim, TrimSpace);
  if not Delim then
    Exit(ErrorF('internal error: lexRightDelim without delim'));

  if TrimSpace then
  begin
    Inc(FPos, trimMarkerLen);
    Ignore;
  end;

  Inc(FPos, Length(FRightDelim));
  Result := Emit(itemRightDelim);

  if TrimSpace then
  begin
    Inc(FPos, LeftTrimLength(Copy(FInput, FPos + 1, MaxInt)));
    Ignore;
  end;

  FInsideAction := False;
end;

function TLexer.LexInsideAction: TLexState;
var
  R: cardinal;
  D, TS: boolean;
  Ch: char;
begin
  AtRightDelim(D, TS);
  if D then
  begin
    if FParenDepth = 0 then Exit(lsRightDelim)
    else
      Exit(ErrorF('unclosed left paren'));
  end;

  if FPos >= Length(FInput) then
    Exit(ErrorF('unclosed action'));

  Ch := FInput[FPos + 1];
  R := Ord(Ch);

  case Ch of
    ' ', #9, #13, #10:
      Exit(lsSpace);
    '=':
    begin
      Inc(FPos);
      Exit(Emit(itemAssign));
    end;
    ':':
    begin
      Inc(FPos);
      if (FPos < Length(FInput)) and (FInput[FPos + 1] = '=') then
      begin
        Inc(FPos);
        Exit(Emit(itemDeclare));
      end
      else
        Exit(ErrorF('expected :='));
    end;
    '|':
    begin
      Inc(FPos);
      Exit(Emit(itemPipe));
    end;
    '"': Exit(lsQuote);
    '`': Exit(lsRawQuote);
    '$': Exit(lsVariable);
    '''': Exit(lsChar);
    '.':
    begin
      if (FPos + 1 < Length(FInput)) and not (FInput[FPos + 2] in ['0'..'9']) then
        Exit(lsField);
      Exit(lsNumber);
    end;
    '+', '-', '0'..'9':
      Exit(lsNumber);
    '(':
    begin
      Inc(FParenDepth);
      Inc(FPos);
      Exit(Emit(itemLeftParen));
    end;
    ')':
    begin
      Dec(FParenDepth);
      if FParenDepth < 0 then
        Exit(ErrorF('unexpected right paren'));
      Inc(FPos);
      Exit(Emit(itemRightParen));
    end;
    else
      if IsAlphaNumeric(R) then
        Exit(lsIdentifier);
      if R <= 127 then
      begin
        Inc(FPos);
        Exit(Emit(itemChar));
      end;
      Exit(ErrorF('unrecognized character'));
  end;
end;

function TLexer.LexSpace: TLexState;
var
  NumSpaces: integer;
  Ch: char;
begin
  NumSpaces := 0;
  while FPos < Length(FInput) do
  begin
    Ch := FInput[FPos + 1];
    if not IsSpace(Ord(Ch)) then Break;
    Inc(FPos);
    if Ch = #10 then Inc(FLine);
    Inc(NumSpaces);
  end;

  if HasRightTrimMarker(Copy(FInput, FPos - NumSpaces + 1, Length(FInput))) and
    StartsStr(FRightDelim, Copy(FInput, FPos + 2, Length(FInput))) then
  begin
    Dec(FPos);
    if NumSpaces = 1 then Exit(lsRightDelim);
  end;
  Result := Emit(itemSpace);
end;

function TLexer.LexIdentifier: TLexState;
var
  word: string;
  Ch: char;
begin
  while FPos < Length(FInput) do
  begin
    Ch := FInput[FPos + 1];
    if not IsAlphaNumeric(Ord(Ch)) then Break;
    Inc(FPos);
  end;

  word := Copy(FInput, FStart + 1, FPos - FStart);

  if word = '.' then Emit(itemField)
  else if word = 'block' then Emit(itemBlock)
  else if word = 'break' then
    if FOptions.BreakOK then Emit(itemBreak)
    else
      Emit(itemIdentifier)
  else if word = 'continue' then
    if FOptions.ContinueOK then Emit(itemContinue)
    else
      Emit(itemIdentifier)
  else if word = 'define' then Emit(itemDefine)
  else if word = 'else' then Emit(itemElse)
  else if word = 'end' then Emit(itemEnd)
  else if word = 'if' then Emit(itemIf)
  else if word = 'range' then Emit(itemRange)
  else if word = 'nil' then Emit(itemNil)
  else if word = 'template' then Emit(itemTemplate)
  else if word = 'with' then Emit(itemWith)
  else if (word = 'true') or (word = 'false') then Emit(itemBool)
  else
    Emit(itemIdentifier);
  Result := lsDone;
end;

function TLexer.LexField: TLexState;
begin
  Result := LexFieldOrVariable(itemField);
end;

function TLexer.LexVariable: TLexState;
begin
  Inc(FPos); // Skip $   if AtTerminator then Exit(Emit(itemVariable));
  Result := LexFieldOrVariable(itemVariable);
end;

function TLexer.LexFieldOrVariable(Typ: TItemType): TLexState;
var
  Ch: char;
begin
  if Typ = itemField then
    Inc(FPos); // Skip .

  if AtTerminator then
  begin
    if Typ = itemVariable then Emit(itemVariable)
    else
      Emit(itemDot);
    Exit(lsDone);
  end;

  while FPos < Length(FInput) do
  begin
    Ch := FInput[FPos + 1];
    if not IsAlphaNumeric(Ord(Ch)) then Break;
    Inc(FPos);
  end;

  if not AtTerminator then Exit(ErrorF('bad character'));
  Emit(Typ);
  Result := lsDone;
end;

function TLexer.AtTerminator: boolean;
var
  Ch: char;
begin
  if FPos >= Length(FInput) then
    Exit(True);

  Ch := FInput[FPos + 1];
  if IsSpace(Ord(Ch)) then Exit(True);

  case Ch of
    '.', ',', '|', ':', ')', '(': Exit(True);
  end;

  Result := StartsStr(FRightDelim, Copy(FInput, FPos + 1, Length(FInput)));
end;

function TLexer.LexChar: TLexState;
var
  Ch: char;
begin
  Inc(FPos); // Skip opening '

  while FPos < Length(FInput) do
  begin
    Ch := FInput[FPos + 1];
    Inc(FPos);

    if Ch = '''' then
    begin
      Emit(itemCharConstant);
      Exit(lsDone);
    end;

    if Ch = '\' then
    begin
      if FPos >= Length(FInput) then
        Exit(ErrorF('unterminated char constant'));
      Inc(FPos); // Skip escaped char
    end;
  end;

  Exit(ErrorF('unterminated char constant'));
end;

function TLexer.LexNumber: TLexState;
var
  P, Save, AfterFirst: TPos;

  function AtEnd(Q: TPos): boolean; inline;
  begin
    Result := Q >= Length(FInput);
  end;

  function PeekCh(Q: TPos): char; inline;
  begin
    if Q < Length(FInput) then Result := FInput[Q + 1]
    else
      Result := #0;
  end;

  procedure ErrBad;
  begin
    raise ETemplateError.Create('bad number syntax');
    // will be caught by ErrorF wrapper? we use Exit(ErrorF(...)) instead below
  end;

  function ScanDigits(var Q: TPos; AllowUnderscore: boolean; ValidBase: string): boolean;
  { scan 1+ digits from ValidBase, allowing '_' between digits.
    returns True if at least one digit consumed.
    never allows leading or trailing '_' or '__' }
  var
    HadDigit: boolean;
    PrevUnd: boolean;
    C: char;
  begin
    HadDigit := False;
    PrevUnd := False;
    while Q < Length(FInput) do
    begin
      C := FInput[Q + 1];
      if (AllowUnderscore) and (C = '_') then
      begin
        if (not HadDigit) or PrevUnd then Break;
        PrevUnd := True;
        Inc(Q);
        Continue;
      end;
      if Pos(C, ValidBase) > 0 then
      begin
        HadDigit := True;
        PrevUnd := False;
        Inc(Q);
        Continue;
      end;
      Break;
    end;
    if PrevUnd then Dec(Q); { back off trailing '_' }
    Result := HadDigit;
  end;

  function ScanDecimalOrFloat(var Q: TPos): boolean;
  { Scan Go-style decimal int or float (with '_' allowed).
    Accepts:
      digits [ '.' [digits] ] [e[+|-]digits]
      '.' digits [e...]
    Returns True if it consumed a valid numeric prefix. }
  var
    StartQ: TPos;
    HadInt, HadFrac: boolean;
  begin
    StartQ := Q;
    HadInt := ScanDigits(Q, True, '0123456789');

    HadFrac := False;
    if (Q < Length(FInput)) and (FInput[Q + 1] = '.') then
    begin
      Inc(Q);
      HadFrac := ScanDigits(Q, True, '0123456789');
    end;

    if not HadInt and not HadFrac then
    begin
      Q := StartQ;
      Exit(False);
    end;

    if (Q < Length(FInput)) and (FInput[Q + 1] in ['e', 'E']) then
    begin
      Inc(Q);
      if (Q < Length(FInput)) and (FInput[Q + 1] in ['+', '-']) then Inc(Q);
      if not ScanDigits(Q, True, '0123456789') then Exit(False);
    end;
    Result := True;
  end;

  function ScanBaseInt(var Q: TPos; const Digits: string): boolean;
  begin
    Result := ScanDigits(Q, True, Digits);
  end;

  function ScanHexMantissa(var Q: TPos): boolean;
    { hex float mantissa: hexDigits [ '.' [hexDigits] ] or '.' hexDigits }
  var
    StartQ: TPos;
    HadA, HadB: boolean;
  begin
    StartQ := Q;
    HadA := ScanDigits(Q, True, '0123456789abcdefABCDEF');
    HadB := False;
    if (Q < Length(FInput)) and (FInput[Q + 1] = '.') then
    begin
      Inc(Q);
      HadB := ScanDigits(Q, True, '0123456789abcdefABCDEF');
    end;
    if not HadA and not HadB then
    begin
      Q := StartQ;
      Exit(False);
    end;
    Result := True;
  end;

  function ScanHexFloat(var Q: TPos): boolean;
    { Requires 0x/0X already consumed. Must have mantissa then 'p' exponent. }
  begin
    if not ScanHexMantissa(Q) then Exit(False);
    if not ((Q < Length(FInput)) and (FInput[Q + 1] in ['p', 'P'])) then Exit(False);
    Inc(Q);
    if (Q < Length(FInput)) and (FInput[Q + 1] in ['+', '-']) then Inc(Q);
    if not ScanDigits(Q, True, '0123456789') then Exit(False);
    Result := True;
  end;

  function IsTerminatorAfterNumber(Q: TPos): boolean;
  begin
    if Q >= Length(FInput) then Exit(True);
    Result := not IsAlphaNumeric(Ord(FInput[Q + 1]));
  end;

  function TryScanOneNumber(var Q: TPos): boolean;
  { scans a single Go numeric literal (int/float/hex-float),
    starting at current lexer FPos (byte index in ASCII range here).
    sign is handled outside.
  }
  var
    StartQ: TPos;
    C1, C2: char;
  begin
    StartQ := Q;
    if AtEnd(Q) then Exit(False);

    C1 := PeekCh(Q);
    C2 := PeekCh(Q + 1);

    if (C1 = '0') and ((C2 = 'x') or (C2 = 'X')) then
    begin
      Inc(Q, 2);
      { hex int OR hex float }
      if ScanHexFloat(Q) then Exit(True);
      { if not float, must be hex int digits }
      if not ScanBaseInt(Q, '0123456789abcdefABCDEF') then
      begin
        Q := StartQ;
        Exit(False);
      end;
      Exit(True);
    end;

    if (C1 = '0') and ((C2 = 'o') or (C2 = 'O')) then
    begin
      Inc(Q, 2);
      if not ScanBaseInt(Q, '01234567') then
      begin
        Q := StartQ;
        Exit(False);
      end;
      Exit(True);
    end;

    if (C1 = '0') and ((C2 = 'b') or (C2 = 'B')) then
    begin
      Inc(Q, 2);
      if not ScanBaseInt(Q, '01') then
      begin
        Q := StartQ;
        Exit(False);
      end;
      Exit(True);
    end;

    { decimal int/float: includes leading '.' form }
    Result := ScanDecimalOrFloat(Q);
  end;

begin
  { Go-compatible scanNumber, including:
    0x/0o/0b, '_' separators, hex-float (0x...p...),
    and complex tokens like: 2i, 1+2i, 1-2i, 0x1p-2+3i }

  P := FPos;

  { optional leading sign }
  if (P < Length(FInput)) and (FInput[P + 1] in ['+', '-']) then Inc(P);
  AfterFirst := P;

  if not TryScanOneNumber(P) then
    Exit(ErrorF('bad number syntax'));

  { optional: imaginary suffix => make it itemNumber (later parsed) }
  if (P < Length(FInput)) and (FInput[P + 1] = 'i') then
  begin
    Inc(P);
    FPos := P;
    if not IsTerminatorAfterNumber(FPos) then Exit(ErrorF('bad number syntax'));
    Emit(itemNumber);
    Exit(lsDone);
  end;

  { optional: complex form: <num> (+|-) <num> i }
  Save := P;
  if (P < Length(FInput)) and (FInput[P + 1] in ['+', '-']) then
  begin
    Inc(P);
    if not TryScanOneNumber(P) then
      P := Save
    else if (P < Length(FInput)) and (FInput[P + 1] = 'i') then
    begin
      Inc(P);
      FPos := P;
      if not IsTerminatorAfterNumber(FPos) then Exit(ErrorF('bad number syntax'));
      Emit(itemComplex);
      Exit(lsDone);
    end
    else
      P := Save;
  end;

  { plain number }
  FPos := Save;
  if not IsTerminatorAfterNumber(FPos) then Exit(ErrorF('bad number syntax'));
  Emit(itemNumber);
  Result := lsDone;
end;

function TLexer.LexQuote: TLexState;
var
  Ch: char;
begin
  Inc(FPos); // Skip opening "

  while FPos < Length(FInput) do
  begin
    Ch := FInput[FPos + 1];
    Inc(FPos);

    if Ch = '"' then
    begin
      Emit(itemString);
      Exit(lsDone);
    end;

    if Ch = '\' then
    begin
      if FPos >= Length(FInput) then
        Exit(ErrorF('unterminated string'));
      Inc(FPos); // Skip escaped char
    end;
  end;

  Exit(ErrorF('unterminated string'));
end;

function TLexer.LexRawQuote: TLexState;
var
  Ch: char;
begin
  Inc(FPos); // Skip opening `

  while FPos < Length(FInput) do
  begin
    Ch := FInput[FPos + 1];
    Inc(FPos);

    if Ch = '`' then
    begin
      Emit(itemRawString);
      Exit(lsDone);
    end;
  end;

  Exit(ErrorF('unterminated raw string'));
end;


{ TTree }

constructor TTree.Create(AName: string; Funcs: array of TStrings;
  const OwnsFuncs: boolean);
var
  F: TStrings;
begin
  inherited Create;
  FOwnsFuncs := OwnsFuncs;
  FName := AName;
  FFuncs := TFPList.Create;
  for F in Funcs do
    FFuncs.Add(F);
end;

destructor TTree.Destroy;
begin
  if Assigned(FRoot) then
    FRoot.Free;
  if FOwnsFuncs and Assigned(FFuncs) then
    FFuncs.Free;

  inherited Destroy;
end;

function TTree.Parse(AText, LeftDelim, RightDelim: string; TreeSet: TTreeSet;
  Funcs: array of TStrings): TTree;
var
  i: integer;
  T: TItem;
  NewT: TTree;
  N: TNode;
begin
  FParseName := FName;
  FText := AText;
  FLexer := TLexer.Create(FName, AText, LeftDelim, RightDelim);

  if Length(Funcs) > 0 then
  begin
    FFuncs.Clear;
    for i := Low(Funcs) to High(Funcs) do
      FFuncs.Add(Funcs[i]);
  end;

  FTreeSet := TreeSet;
  StartParse(FFuncs, FLexer, TreeSet);

  try
    FRoot := NewList(Peek.Pos);
    while Peek.Typ <> itemEOF do
    begin
      if Peek.Typ = itemLeftDelim then
      begin
        T := Next;
        if NextNonSpace.Typ = itemDefine then
        begin
          NewT := TTree.Create('definition', []);
          NewT.FText := FText;
          NewT.ParseName := ParseName;
          NewT.StartParse(FFuncs, FLexer, FTreeSet);
          NewT.ParseDefinition;
          Continue;
        end;
        Backup2(T);
      end;

      N := TextOrAction;
      if Assigned(N) then
      begin
        if (N.TypeNum = nodeEnd) or (N.TypeNum = nodeElse) then
          ErrorF('unexpected ' + N.ToString);
        FRoot.Append(N);
      end;
    end;

    Result := Self;
  finally
    StopParse;
    FLexer.Free;
    FLexer := nil;
  end;
end;

procedure TTree.StartParse(Funcs: TFPList; ALexer: TLexer; ATreeSet: TTreeSet);
begin
  FRoot := nil;
  FLexer := ALexer;
  SetLength(FVars, 1);
  FVars[0] := '$';
  FFuncs := Funcs;
  FTreeSet := ATreeSet;
  FStackDepth := 0;
  FLexer.FOptions.EmitComment := (FMode and ParseComments) <> 0;
  FLexer.FOptions.BreakOK := not HasFunction('break');
  FLexer.FOptions.ContinueOK := not HasFunction('continue');
end;

procedure TTree.StopParse;
begin
  FLexer := nil;
  FVars := nil;
  FTreeSet := nil;
end;

{ Token handling }
function TTree.Next: TItem;
begin
  if FPeekCount > 0 then
  begin
    Dec(FPeekCount);
    Result := FToken[FPeekCount];
  end
  else
  begin
    FToken[0] := FLexer.NextItem;
    Result := FToken[0];
  end;
end;

procedure TTree.Backup;
begin
  Inc(FPeekCount);
end;

procedure TTree.Backup2(T1: TItem);
begin
  FToken[1] := T1;
  FPeekCount := 2;
end;

procedure TTree.Backup3(T2, T1: TItem);
begin
  FToken[1] := T1;
  FToken[2] := T2;
  FPeekCount := 3;
end;

function TTree.Peek: TItem;
begin
  if FPeekCount > 0 then
    Result := FToken[FPeekCount - 1]
  else
  begin
    FPeekCount := 1;
    FToken[0] := FLexer.NextItem;
    Result := FToken[0];
  end;
end;

function TTree.NextNonSpace: TItem;
begin
  repeat
    Result := Next;
  until Result.Typ <> itemSpace;
end;

function TTree.PeekNonSpace: TItem;
begin
  Result := NextNonSpace;
  Backup;
end;

{ Error handling }
procedure TTree.ErrorF(const Msg: string);
var
  Full: string;
begin
  FRoot := nil;
  Full := SysUtils.Format('template: %s:%d: %s', [FParseName, FToken[0].Line, Msg]);
  raise ETemplateError.Create(Full);
end;

procedure TTree.ErrorFmt(const Fmt: string; const Args: array of const);
var
  Msg: string;
begin
  Msg := SysUtils.Format(Fmt, Args);
  ErrorF(Msg);
end;

procedure TTree.Expect(ATyp: TItemType; Context: string);
var
  Token: TItem;
begin
  Token := NextNonSpace;
  if Token.Typ <> ATyp then Unexpected(Token, Context);
end;

function TTree.ExpectOneOf(A1, A2: TItemType; Context: string): TItem;
begin
  Result := NextNonSpace;
  if (Result.Typ <> A1) and (Result.Typ <> A2) then Unexpected(Result, Context);
end;

procedure TTree.Unexpected(AToken: TItem; Context: string);
begin
  if AToken.Typ = itemError then
    ErrorF(AToken.Val)
  else
    ErrorF('unexpected ' + AToken.ToString + ' in ' + Context);
end;

{ Parsing logic }
function TTree.TextOrAction: TNode;
var
  Token: TItem;
begin
  Result := nil;
  Token := NextNonSpace;
  case Token.Typ of
    itemText: Result := NewText(Token.Pos, Token.Val);
    itemLeftDelim:
    begin
      FActionLine := Token.Line;
      Result := Action;
      ClearActionLine;
    end;
    itemComment:
    begin
      if (FMode and ParseComments) <> 0 then
        Result := NewComment(Token.Pos, Token.Val)
      else
        Result := nil;
    end;
    else
      Unexpected(Token, 'input');
  end;
end;

function TTree.Action: TNode;
var
  Token: TItem;
begin
  Token := NextNonSpace;
  case Token.Typ of
    itemBlock: Result := BlockControl;
    itemBreak: Result := BreakControl(Token.Pos, Token.Line);
    itemContinue: Result := ContinueControl(Token.Pos, Token.Line);
    itemElse: Result := ElseControl;
    itemEnd: Result := EndControl;
    itemIf: Result := IfControl;
    itemRange: Result := RangeControl;
    itemTemplate: Result := TemplateControl;
    itemWith: Result := WithControl;
    else
    begin
      Backup;
      Token := Peek;
      Result := NewAction(Token.Pos, Token.Line, Pipeline('command', itemRightDelim));
    end;
  end;
end;

function TTree.Pipeline(Context: string; EndTyp: TItemType): TPipeNode;
label
  decls;
var
  Token, TokenAfterVariable, NextTok, VTok: TItem;
begin
  Token := PeekNonSpace;
  Result := NewPipe(Token.Pos, Token.Line, []);

  decls:
    if PeekNonSpace.Typ = itemVariable then
    begin
      VTok := Next;
      TokenAfterVariable := Peek;
      NextTok := PeekNonSpace;

      if (NextTok.Typ = itemAssign) or (NextTok.Typ = itemDeclare) then
      begin
        Result.IsAssign := NextTok.Typ = itemAssign;
        NextNonSpace;
        SetLength(Result.Decl, Length(Result.Decl) + 1);
        Result.Decl[High(Result.Decl)] := NewVariable(VTok.Pos, VTok.Val);
        SetLength(FVars, Length(FVars) + 1);
        FVars[High(FVars)] := VTok.Val;
      end
      else if (NextTok.Typ = itemChar) and (NextTok.Val = ',') then
      begin
        NextNonSpace;
        SetLength(Result.Decl, Length(Result.Decl) + 1);
        Result.Decl[High(Result.Decl)] := NewVariable(VTok.Pos, VTok.Val);
        SetLength(FVars, Length(FVars) + 1);
        FVars[High(FVars)] := VTok.Val;

        if (Context = 'range') and (Length(Result.Decl) < 2) then
        begin
          case PeekNonSpace.Typ of
            itemVariable, itemRightDelim, itemRightParen:
              goto decls;
            else
              ErrorF('range can only initialize variables');
          end;
        end;
        ErrorF('too many declarations in ' + Context);
      end
      else if TokenAfterVariable.Typ = itemSpace then
      begin
        Backup3(VTok, TokenAfterVariable);
      end
      else
      begin
        Backup2(VTok);
      end;
    end;

  while True do
  begin
    Token := NextNonSpace;
    if Token.Typ = EndTyp then
    begin
      CheckPipeline(Result, Context);
      Exit;
    end;

    case Token.Typ of
      itemBool, itemCharConstant, itemComplex, itemDot, itemField, itemIdentifier,
      itemNumber, itemNil, itemRawString, itemString, itemVariable, itemLeftParen:
      begin
        Backup;
        Result.Append(Command);
      end;
      else
        Unexpected(Token, Context);
    end;
  end;
end;

procedure TTree.CheckPipeline(Pipe: TPipeNode; const Context: string);
var
  I: integer;
  C: TCommandNode;
  FirstArgType: TNodeType;
begin
  if (Pipe = nil) or (Length(Pipe.Cmds) = 0) then
    ErrorF('missing value for ' + Context);

  for I := 1 to High(Pipe.Cmds) do
  begin
    C := Pipe.Cmds[I];
    if (C = nil) or (Length(C.Args) = 0) then Continue;
    FirstArgType := C.Args[0].TypeNum;
    case FirstArgType of
      NodeBool, NodeDot, NodeNil, NodeNumber, NodeString:
        ErrorF(Format('non executable command in pipeline stage %d', [I + 1]));
    end;
  end;
end;

function TTree.Command: TCommandNode;
var
  Token: TItem;
  Op: TNode;
begin
  Result := NewCommand(PeekNonSpace.Pos);
  while True do
  begin
    PeekNonSpace;
    Op := Operand;
    if Assigned(Op) then Result.Append(Op);

    Token := Next;
    case Token.Typ of
      itemSpace: Continue;
      itemRightDelim, itemRightParen: Backup;
      itemPipe: ;
      else
        Unexpected(Token, 'operand');
    end;
    Break;
  end;

  if Length(Result.Args) = 0 then ErrorF('empty command');
end;

function TTree.Operand: TNode;
var
  Node: TNode;
  Chain: TChainNode;
  T0: TItem;
  Tok: TItem;
begin
  Result := nil;
  Node := Term;
  if not Assigned(Node) then Exit;

  if Peek.Typ = itemField then
  begin
    { Go compat:
      - if Node=Field/Variable: increment Ident (don't create ChainNode)
      - if Node=literal/dot/nil: error "unexpected . after term ..."
      - otherwise: ChainNode as usual }
    case Node.TypeNum of
      NodeField:
      begin
        while Peek.Typ = itemField do
        begin
          Tok := Next;
          AppendIdentFromFieldToken(TFieldNode(Node).Ident, Tok.Val);
        end;
        Result := Node;
      end;
      NodeVariable:
      begin
        while Peek.Typ = itemField do
        begin
          Tok := Next;
          AppendIdentFromFieldToken(TVariableNode(Node).Ident, Tok.Val);
        end;
        Result := Node;
      end;
      NodeBool, NodeString, NodeNumber, NodeNil, NodeDot:
      begin
        T0 := Next; { consume . }
        ErrorFmt('unexpected %s after term %s', [T0.ToString, Node.ToString]);
      end;
      else
      begin
        Chain := NewChain(Peek.Pos, Node);
        while Peek.Typ = itemField do
          Chain.Add(Next.Val);
        Result := Chain;
      end;
    end;
  end
  else
    Result := Node;
end;

function TTree.Term: TNode;
var
  Token: TItem;
  S: string;
begin
  Result := nil;
  Token := NextNonSpace;
  case Token.Typ of
    itemIdentifier:
    begin
      if ((FMode and SkipFuncCheck) = 0) and (not HasFunction(Token.Val)) then
        ErrorFmt('function %s not defined', [AnsiQuotedStr(Token.Val, '"')]);
      Result := NewIdentifier(Token.Val);
    end;
    itemDot: Result := NewDot(Token.Pos);
    itemNil: Result := NewNil(Token.Pos);
    itemVariable: Result := UseVar(Token.Pos, Token.Val);
    itemField: Result := NewField(Token.Pos, Token.Val);
    itemBool: Result := NewBool(Token.Pos, Token.Val = 'true');
    itemNumber, itemComplex: Result := NewNumber(Token.Pos, Token.Val, Token.Typ);
    itemLeftParen:
    begin
      if FStackDepth >= 10000 then ErrorF('max expression depth exceeded');
      Inc(FStackDepth);
      Result := Pipeline('parenthesized pipeline', itemRightParen);
      Dec(FStackDepth);
    end;
    itemString, itemRawString:
    begin
      S := Token.Val;
      if (Length(S) >= 2) then
      begin
        if (S[1] = '"') and (S[Length(S)] = '"') then
          S := UTF8DecodeEscapeSequences(System.Copy(S, 2, Length(S) - 2))
        else if (S[1] = '`') and (S[Length(S)] = '`') then
          S := System.Copy(S, 2, Length(S) - 2);
      end;
      Result := NewString(Token.Pos, Token.Val, S);
    end;
    else
    begin
      Backup;
      Exit(nil);
    end;
  end;
end;

{ Control structures }
function TTree.ParseControl(Context: string): TNode;
var
  Pipe: TPipeNode;
  List, ElseList: TListNode;
  NextNode: TNode;
  OrigVarLen: integer;
begin
  OrigVarLen := Length(FVars);
  ElseList := nil;

  try
    Pipe := Pipeline(Context, itemRightDelim);

    if Context = 'range' then Inc(FRangeDepth);
    List := ItemList(NextNode);
    if Context = 'range' then Dec(FRangeDepth);

    case NextNode.TypeNum of
      nodeEnd: ;
      nodeElse:
      begin
        if (Context = 'if') and (Peek.Typ = itemIf) then
        begin
          Next;
          ElseList := NewList(NextNode.Position);
          ElseList.Append(IfControl);
        end
        else if (Context = 'with') and (Peek.Typ = itemWith) then
        begin
          Next;
          ElseList := NewList(NextNode.Position);
          ElseList.Append(WithControl);
        end
        else
        begin
          ElseList := ItemList(NextNode);
          if NextNode.TypeNum <> nodeEnd then
            ErrorF('expected end; found ' + NextNode.ToString);
        end;
      end;
      else
        ErrorF('unexpected ' + NextNode.ToString + ' in ' + Context);
    end;

    case Context of
      'if': Result := NewIf(Pipe.Position, Pipe.Line, Pipe, List, ElseList);
      'range': Result := NewRange(Pipe.Position, Pipe.Line, Pipe, List, ElseList);
      'with': Result := NewWith(Pipe.Position, Pipe.Line, Pipe, List, ElseList);
      else
        Result := nil;
    end;

  finally
    PopVars(OrigVarLen);
  end;
end;

function TTree.ItemList(out Next: TNode): TListNode;
var
  N: TNode;
begin
  Result := NewList(PeekNonSpace.Pos);
  while PeekNonSpace.Typ <> itemEOF do
  begin
    N := TextOrAction;
    if Assigned(N) then
    begin
      if (N.TypeNum = nodeEnd) or (N.TypeNum = nodeElse) then
      begin
        Next := N;
        Exit;
      end;
      Result.Append(N);
    end;
  end;
  ErrorF('unexpected EOF');
end;

function TTree.IfControl: TNode;
begin
  Result := ParseControl('if');
end;

function TTree.RangeControl: TNode;
begin
  Result := ParseControl('range');
end;

function TTree.WithControl: TNode;
begin
  Result := ParseControl('with');
end;

function TTree.EndControl: TNode;
begin
  Expect(itemRightDelim, 'end');
  Result := NewEnd(FToken[0].Pos);
end;

function TTree.ElseControl: TNode;
var
  PeekTok, Tok: TItem;
begin
  PeekTok := PeekNonSpace;
  if (PeekTok.Typ = itemIf) or (PeekTok.Typ = itemWith) then
    Exit(NewElse(PeekTok.Pos, PeekTok.Line));

  Tok := NextNonSpace;
  if Tok.Typ <> itemRightDelim then Unexpected(Tok, 'else');
  Result := NewElse(Tok.Pos, Tok.Line);
end;

function TTree.BlockControl: TNode;
var
  Token: TItem;
  AName: string;
  Pipe: TPipeNode;
  BlockTree: TTree;
  NextNode: TNode;
begin
  Token := NextNonSpace;
  AName := ParseTemplateName(Token, 'block clause');

  (* In Go templates, block has optional pipeline.
    Syntax: {{block "name" pipeline}}...{{end}}
    Also allowed: {{block "name"}}...{{end}} (implicit dot)
  *)
  Pipe := nil;
  if NextNonSpace.Typ <> itemRightDelim then
  begin
    Backup;
    Pipe := Pipeline('block clause', itemRightDelim);
  end;

  { Like in go: create a new template Tree with a name, parse the body up to {{end}},
    add to TreeSet, return the TemplateNode that calls this template }
  BlockTree := TTree.Create(AName, []);
  BlockTree.FText := FText;
  BlockTree.ParseName := ParseName;
  BlockTree.Mode := Self.Mode;
  BlockTree.StartParse(FFuncs, FLexer, FTreeSet);
  BlockTree.FRoot := BlockTree.ItemList(NextNode);
  if NextNode.TypeNum <> nodeEnd then
    ErrorF('unexpected ' + NextNode.ToString + ' in block clause');
  BlockTree.AddToTreeSet;
  BlockTree.StopParse;

  Result := NewTemplate(Token.Pos, Token.Line, AName, Pipe);
end;

function TTree.TemplateControl: TNode;
var
  Token: TItem;
  AName: string;
  Pipe: TPipeNode;
begin
  Token := NextNonSpace;
  AName := ParseTemplateName(Token, 'template clause');
  Pipe := nil;
  if NextNonSpace.Typ <> itemRightDelim then
  begin
    Backup;
    Pipe := Pipeline('template clause', itemRightDelim);
  end;
  Result := NewTemplate(Token.Pos, Token.Line, AName, Pipe);
end;

function TTree.ParseTemplateName(Token: TItem; Context: string): string;
var
  S: string;
begin
  { Go strconv.Unquote : support "..." with escape and raw `...` }
  S := Token.Val;
  if Length(S) < 2 then Exit(S);

  if (S[1] = '`') and (S[Length(S)] = '`') then
    Exit(System.Copy(S, 2, Length(S) - 2));

  if (S[1] = '"') and (S[Length(S)] = '"') then
  begin
    try
      Result := SysUtils.AnsiDequotedStr(S, '"'); { remove quotes, keep escapes }
    except
      on E: Exception do
        ErrorFmt('%s', [E.Message]);
    end;
    { Now unescape like Go basic escapes; Lazarus: UTF8DecodeEscapeSequences does \n,\t,\",\x..,\u.... }
    try
      Result := UTF8DecodeEscapeSequences(Result);
    except
      on E: Exception do
        ErrorFmt('%s', [E.Message]);
    end;
    Exit;
  end;

  Result := S;
end;

function TTree.BreakControl(Pos: TPos; Line: integer): TNode;
begin
  if NextNonSpace.Typ <> itemRightDelim then Unexpected(Next, '{{break}}');
  if FRangeDepth = 0 then ErrorF('{{break}} outside {{range}}');
  Result := NewBreak(Pos, Line);
end;

function TTree.ContinueControl(Pos: TPos; Line: integer): TNode;
begin
  if NextNonSpace.Typ <> itemRightDelim then Unexpected(Next, '{{continue}}');
  if FRangeDepth = 0 then ErrorF('{{continue}} outside {{range}}');
  Result := NewContinue(Pos, Line);
end;

procedure TTree.ParseDefinition;
const
  Context = 'define clause';
var
  NameTok: TItem;
  AName: string;
  NextNode: TNode;
begin
  NameTok := ExpectOneOf(itemString, itemRawString, Context);

  AName := ParseTemplateName(NameTok, Context);
  FName := AName;

  Expect(itemRightDelim, Context);

  FRoot := ItemList(NextNode);
  if NextNode.TypeNum <> nodeEnd then
    ErrorF('unexpected ' + NextNode.ToString + ' in ' + Context);

  AddToTreeSet;
  FVars := nil;
  FTreeSet := nil;
end;

procedure TTree.AddToTreeSet;
var
  Idx: integer;
  Existing: TTree;
begin
  if FTreeSet = nil then Exit;

  Idx := FTreeSet.IndexOf(FName);
  if Idx < 0 then
  begin
    FTreeSet.AddObject(FName, Self);
    Exit;
  end;

  Existing := TTree(FTreeSet.Objects[Idx]);
  if (Existing = nil) or IsEmptyTree(Existing.Root) then
  begin
    FTreeSet.Objects[Idx] := Self;
    Exit;
  end;

  if not IsEmptyTree(FRoot) then
    ErrorF('template: multiple definition of template "' + FName + '"');
end;

{ Helpers }
function TTree.UseVar(Pos: TPos; Name: string): TNode;
var
  I: integer;
  V: TVariableNode;
begin
  V := NewVariable(Pos, Name);
  for I := 0 to High(FVars) do
    if FVars[I] = V.Ident[0] then Exit(V);
  ErrorF('undefined variable "' + V.Ident[0] + '"');
  Result := nil;
end;

procedure TTree.PopVars(N: integer);
begin
  SetLength(FVars, N);
end;

function TTree.HasFunction(Name: string): boolean;
var
  I: integer;
  Map: TStrings;
begin
  Result := False;
  for I := 0 to FFuncs.Count - 1 do
  begin
    Map := TStrings(FFuncs[I]);
    if Assigned(Map) and (Map.IndexOf(Name) >= 0) then Exit(True);
  end;
end;

{ Factories }
function TTree.NewList(Pos: TPos): TListNode;
begin
  Result := TListNode.Create(Self, Pos);
end;

function TTree.NewText(Pos: TPos; Text: string): TTextNode;
begin
  Result := TTextNode.Create(Self, Pos, Text);
end;

function TTree.NewAction(Pos: TPos; Line: integer; Pipe: TPipeNode): TActionNode;
begin
  Result := TActionNode.Create(Self, Pos, Line, Pipe);
end;

function TTree.NewPipe(Pos: TPos; Line: integer;
  Vars: array of TVariableNode): TPipeNode;
begin
  Result := TPipeNode.Create(Self, Pos, Line, Vars);
end;

function TTree.NewCommand(Pos: TPos): TCommandNode;
begin
  Result := TCommandNode.Create(Self, Pos, NodeCommand);
end;

function TTree.NewIdentifier(Str: string): TIdentifierNode;
begin
  Result := TIdentifierNode.Create(Self, 0, Str);
end;

function TTree.NewVariable(Pos: TPos; Ident: string): TVariableNode;
begin
  Result := TVariableNode.Create(Self, Pos, Ident);
end;

function TTree.NewDot(Pos: TPos): TDotNode;
begin
  Result := TDotNode.Create(Self, Pos, NodeDot);
end;

function TTree.NewNil(Pos: TPos): TNilNode;
begin
  Result := TNilNode.Create(Self, Pos, NodeNil);
end;

function TTree.NewField(Pos: TPos; Ident: string): TFieldNode;
begin
  Result := TFieldNode.Create(Self, Pos, Ident);
end;

function TTree.NewChain(Pos: TPos; Node: TNode): TChainNode;
begin
  Result := TChainNode.Create(Self, Pos, Node);
end;

function TTree.NewBool(Pos: TPos; True: boolean): TBoolNode;
begin
  Result := TBoolNode.Create(Self, Pos, True);
end;

function TTree.NewNumber(Pos: TPos; Text: string; Typ: TItemType): TNumberNode;
begin
  Result := TNumberNode.Create(Self, Pos, Text, Typ);
end;

function TTree.NewString(Pos: TPos; Quoted, Text: string): TStringNode;
begin
  Result := TStringNode.Create(Self, Pos, Quoted, Text);
end;

function TTree.NewEnd(Pos: TPos): TEndNode;
begin
  Result := TEndNode.Create(Self, Pos, NodeEnd);
end;

function TTree.NewElse(Pos: TPos; Line: integer): TElseNode;
begin
  Result := TElseNode.Create(Self, Pos, Line);
end;

function TTree.NewIf(Pos: TPos; Line: integer; Pipe: TPipeNode;
  List, ElseList: TListNode): TIfNode;
begin
  Result := TIfNode.Create(Self, Pos, Line, Pipe, List, ElseList);
end;

function TTree.NewRange(Pos: TPos; Line: integer; Pipe: TPipeNode;
  List, ElseList: TListNode): TRangeNode;
begin
  Result := TRangeNode.Create(Self, Pos, Line, Pipe, List, ElseList);
end;

function TTree.NewWith(Pos: TPos; Line: integer; Pipe: TPipeNode;
  List, ElseList: TListNode): TWithNode;
begin
  Result := TWithNode.Create(Self, Pos, Line, Pipe, List, ElseList);
end;

function TTree.NewTemplate(Pos: TPos; Line: integer; Name: string;
  Pipe: TPipeNode): TTemplateNode;
begin
  Result := TTemplateNode.Create(Self, Pos, Line, Name, Pipe);
end;

function TTree.NewBreak(Pos: TPos; Line: integer): TBreakNode;
begin
  Result := TBreakNode.Create(Self, Pos, Line);
end;

function TTree.NewContinue(Pos: TPos; Line: integer): TContinueNode;
begin
  Result := TContinueNode.Create(Self, Pos, Line);
end;

function TTree.NewComment(Pos: TPos; const Text: string): TCommentNode;
begin
  Result := TCommentNode.Create(Self, Pos, Text);
end;

procedure TTree.ClearActionLine;
begin
  FActionLine := 0;
end;

procedure TTree.SetRoot(ARoot: TListNode);
begin
  if Assigned(FRoot) then
    FRoot.Free;
  FRoot := ARoot;
end;

function TTree.Copy: TTree;
begin
  Result := TTree.Create(Name, []);
  Result.ParseName := ParseName;
  if Assigned(Root) then
    Result.Root := Root.CopyList;
  Result.FText := FText;
end;

{ Additional Number Parsing Helpers }

function ParseFloatInvariant(const S: string; out V: double): boolean;
var
  FS: TFormatSettings;
  T: string;
  PPos, I, DotPos: integer;
  Mant, ExpS: string;
  ExpV: integer;
  MantV: extended;
  FracDigits: integer;
  Ch: char;
  Nib: integer;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  T := StringReplace(S, '_', '', [rfReplaceAll]);

  { Support Go hex-floats like 0x1.2p-3 by converting to decimal Double manually. }
  if (Length(T) >= 3) and (T[1] = '0') and ((T[2] = 'x') or (T[2] = 'X')) and
    (Pos('p', LowerCase(T)) > 0) then
  begin
    try
      V := StrToFloat(FormatFloat('0.#############################E+0', Math.Power(2, 0))
        , FS); // dummy init
    except
    end;
    { Minimal hex-float parser }
    // Parse: 0x<MANT>p<EXP>, mant is hex with optional '.', exp is decimal with sign.
    // We'll compute: value = mantissa * 2^exp, with mantissa in base16.

    PPos := Pos('p', LowerCase(T));
    Mant := Copy(T, 3, PPos - 3);
    ExpS := Copy(T, PPos + 1, MaxInt);
    if not TryStrToInt(ExpS, ExpV) then Exit(False);

    DotPos := Pos('.', Mant);
    FracDigits := 0;
    if DotPos > 0 then
    begin
      FracDigits := Length(Mant) - DotPos;
      Delete(Mant, DotPos, 1);
    end;
    if Mant = '' then Exit(False);

    MantV := 0;
    for I := 1 to Length(Mant) do
    begin
      Ch := Mant[I];
      if (Ch >= '0') and (Ch <= '9') then Nib := Ord(Ch) - Ord('0')
      else if (Ch >= 'a') and (Ch <= 'f') then Nib := 10 + Ord(Ch) - Ord('a')
      else if (Ch >= 'A') and (Ch <= 'F') then Nib := 10 + Ord(Ch) - Ord('A')
      else
        Exit(False);
      MantV := MantV * 16 + Nib;
    end;
    { adjust for fractional hex digits: each hex digit is 4 bits => /16^FracDigits = *2^(-4*FracDigits) }
    V := double(MantV * Power(2, ExpV - 4 * FracDigits));
    Exit(True);
  end;

  Result := TryStrToFloat(T, V, FS);
end;

function ParseComplexGoLiteral(const S: string; out C: Complex): boolean;
{ Supports:
  - "2i", ".5i", "1e2i"
  - "0x1p-2i"
  - "1+2i", "1-2i" (without spaces, like a lexer token)
  - "0x1p-2+0x1.8p+1i"
}
var
  IPos: integer;
  Mid: integer;
  ReS, ImS: string;
  ReV, ImV: double;
begin
  Result := False;

  if (S = '') then Exit;

  IPos := LastDelimiter('i', S);
  if (IPos <> Length(S)) then Exit; // must end with i

  // we look for the last + or - (not at the beginning and not immediately after e/E)
  Mid := 0;
  for IPos := Length(S) - 1 downto 2 do
  begin
    if (S[IPos] in ['+', '-']) and not (S[IPos - 1] in ['e', 'E', 'p', 'P']) then
    begin
      Mid := IPos;
      Break;
    end;
  end;

  if Mid = 0 then
  begin
    // purely imaginary: "<num>i"
    ImS := Copy(S, 1, Length(S) - 1);
    if not ParseFloatInvariant(ImS, ImV) then Exit;
    C.re := 0;
    C.im := ImV;
    Exit(True);
  end;

  ReS := Copy(S, 1, Mid - 1);
  ImS := Copy(S, Mid, (Length(S) - 1) - Mid + 1); // includes sign, excludes trailing i

  if not ParseFloatInvariant(ReS, ReV) then Exit;
  if not ParseFloatInvariant(ImS, ImV) then Exit;

  C.re := ReV;
  C.im := ImV;
  Result := True;
end;

{UTF8 Helpers}

function DecodeUnicodeEscapeSequences(const S: string): string;
var
  I, J: integer;
  HexCode: string;
  CodePoint: cardinal;
  UTF8Char: string;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    if (S[I] = '\') and (I + 1 <= Length(S)) and (S[I + 1] = 'u') then
    begin
      // Potential Unicode escape sequence (\uXXXX)
      if I + 5 <= Length(S) then
      begin
        HexCode := Copy(S, I + 2, 4);
        // Check if the next 4 characters are valid hex digits
        if TryStrToInt('$' + HexCode, integer(CodePoint)) then
        begin
          // Convert the codepoint to its UTF-8 representation
          UTF8Char := '';
          UnicodeToUTF8(CodePoint, @UTF8Char[1]);
          Result := Result + UTF8Char;
          I := I + 6; // Skip the \uXXXX sequence
          Continue;
        end;
      end;
    end;

    // If not an escape sequence, or invalid, append the character as is
    Result := Result + S[I];
    Inc(I);
  end;
end;

function UTF8PrevCharIndex(const S: string; Index: SizeInt): SizeInt;
var
  L: SizeInt;
begin
  L := Length(S);

  if L = 0 then Exit(0);

  { Allow Index = L+1 (cursor after last byte) }
  if Index > L + 1 then Index := L + 1;

  { If already at/before start, no previous char }
  if Index <= 1 then Exit(0);

  { Step to previous byte then rewind over UTF-8 continuation bytes (10xxxxxx) }
  Dec(Index);
  while (Index > 1) and ((Ord(S[Index]) and $C0) = $80) do
    Dec(Index);

  Result := Index;
end;

function UTF8DecodeEscapeSequences(const S: string): string;
var
  i, L, Start: SizeInt;

  function HexVal(C: char): integer; inline;
  begin
    case C of
      '0'..'9': Result := Ord(C) - Ord('0');
      'a'..'f': Result := 10 + (Ord(C) - Ord('a'));
      'A'..'F': Result := 10 + (Ord(C) - Ord('A'));
      else
        Result := -1;
    end;
  end;

  function AppendCodePointUTF8(var R: string; CodePoint: cardinal): boolean;
  begin
    try
      R := R + UnicodeToUTF8(CodePoint);
      Result := True;
    except
      Result := False;
    end;
  end;

  function ReadHexN(var P: SizeInt; N: integer; out V: cardinal): boolean;
  var
    k: integer;
    hv: integer;
  begin
    V := 0;
    if P + N - 1 > L then Exit(False);
    for k := 1 to N do
    begin
      hv := HexVal(S[P]);
      if hv < 0 then Exit(False);
      V := (V shl 4) or cardinal(hv);
      Inc(P);
    end;
    Result := True;
  end;

var
  R: string;
  ch: char;
  cp, cp2: cardinal;
  p: SizeInt;
  ok: boolean;
begin
  L := Length(S);
  if L = 0 then Exit('');

  R := '';
  SetLength(R, 0);

  i := 1;
  while i <= L do
  begin
    ch := S[i];
    if ch <> '\' then
    begin
      R := R + ch;
      Inc(i);
      Continue;
    end;

    { Backslash sequence }
    if i = L then
    begin
      { trailing '\' -> keep as-is }
      R := R + '\';
      Break;
    end;

    Inc(i);
    ch := S[i];

    case ch of
      '\': begin
        R := R + '\';
        Inc(i);
      end;
      '"': begin
        R := R + '"';
        Inc(i);
      end;
      '''': begin
        R := R + '''';
        Inc(i);
      end;
      'n': begin
        R := R + #10;
        Inc(i);
      end;
      'r': begin
        R := R + #13;
        Inc(i);
      end;
      't': begin
        R := R + #9;
        Inc(i);
      end;
      'b': begin
        R := R + #8;
        Inc(i);
      end;
      'f': begin
        R := R + #12;
        Inc(i);
      end;
      'a': begin
        R := R + #7;
        Inc(i);
      end;
      'v': begin
        R := R + #11;
        Inc(i);
      end;

      'x': begin
        { \xHH (exactly 2 hex digits). If invalid -> keep literally }
        p := i + 1;
        ok := ReadHexN(p, 2, cp);
        if ok then
        begin
          R := R + Chr(byte(cp));
          i := p;
        end
        else
        begin
          R := R + '\x';
          Inc(i);
        end;
      end;

      'u': begin
        { \uHHHH (4 hex digits); supports surrogate pairs \uD800..\uDBFF + \uDC00..\uDFFF }
        p := i + 1;
        ok := ReadHexN(p, 4, cp);
        if not ok then
        begin
          R := R + '\u';
          Inc(i);
          Continue;
        end;

        { surrogate pair? }
        if (cp >= $D800) and (cp <= $DBFF) then
        begin
          { need following \uXXXX }
          if (p + 1 <= L) and (S[p] = '\') and (S[p + 1] = 'u') then
          begin
            p := p + 2;
            if ReadHexN(p, 4, cp2) and (cp2 >= $DC00) and (cp2 <= $DFFF) then
            begin
              cp := $10000 + ((cp - $D800) shl 10) + (cp2 - $DC00);
              if AppendCodePointUTF8(R, cp) then
              begin
                i := p;
                Continue;
              end;
            end;
          end;

          { invalid surrogate pair -> keep original literally }
          R := R + '\u' + Copy(S, i + 1, 4);
          i := i + 1 + 4;
        end
        else
        begin
          if AppendCodePointUTF8(R, cp) then
          begin
            i := p;
          end
          else
          begin
            { fallback: keep literally }
            R := R + '\u' + Copy(S, i + 1, 4);
            i := i + 1 + 4;
          end;
        end;
      end;

      'U': begin
        { \U00HHHHHH (8 hex digits), common in some formats }
        p := i + 1;
        ok := ReadHexN(p, 8, cp);
        if ok and (cp <= $10FFFF) then
        begin
          if AppendCodePointUTF8(R, cp) then
            i := p
          else
          begin
            R := R + '\U' + Copy(S, i + 1, 8);
            i := i + 1 + 8;
          end;
        end
        else
        begin
          R := R + '\U';
          Inc(i);
        end;
      end;

      '0'..'7': begin
        { octal escape: \OOO (up to 3 digits, like C). }
        cp := 0;
        p := i;
        Start := p;
        while (p <= L) and (p < Start + 3) and (S[p] in ['0'..'7']) do
        begin
          cp := (cp shl 3) + cardinal(Ord(S[p]) - Ord('0'));
          Inc(p);
        end;
        R := R + Chr(byte(cp and $FF));
        i := p;
      end;

      else
      { unknown escape -> keep the escaped char as-is (drops backslash) or keep both?
        Here: keep backslash + char to avoid data loss. }
        R := R + '\' + ch;
        Inc(i);
    end;
  end;

  Result := R;
end;

end.
