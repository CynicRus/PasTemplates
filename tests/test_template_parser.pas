unit test_template_parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry,
  TemplateRuntime,TemplateParser, Rtti;

type
  TTestParser = class(TTestCase)
  published
    procedure Test_Define_CreatesNamedTemplates;
    procedure Test_Block_CreatesTemplateAndReturnsTemplateNode;
    procedure Test_ParseComments_ModeOff_DropsComments;
    procedure Test_ParseComments_ModeOn_EmitsComments;
    procedure Test_WhitespaceTrim_Markers;
    procedure Test_Parse_Error_UndefinedFunction_WhenSkipFuncCheckOff;
  end;

implementation

procedure TTestParser.Test_Define_CreatesNamedTemplates;
var
  T: TTemplate;
begin
  T := New('main');
  T.Parse('{{define "a"}}A{{end}}{{define "b"}}B{{end}}X');
  AssertNotNull(T.Lookup('a'));
  AssertNotNull(T.Lookup('b'));
end;

procedure TTestParser.Test_Block_CreatesTemplateAndReturnsTemplateNode;
var
  T: TTemplate;
begin
  T := New('main');
  // block creates template "card", and the node itself is {{template "card" ...}}
  T.Parse('before:{{block "card" .}}CARD{{end}}:after');
  AssertNotNull(T.Lookup('card'));
end;

procedure TTestParser.Test_ParseComments_ModeOff_DropsComments;
var
  T: TTemplate;
begin
  T := New('main');
  // ParseComments is not enabled by default => comments do not affect the output
  T.Parse('A{{/* hi */}}B');
  AssertEquals('AB', T.ExecuteToString(TValue.Empty));
end;

procedure TTestParser.Test_ParseComments_ModeOn_EmitsComments;
var
  TreeSet: TTreeSet;
  FuncNames, Builtins: TStrings;
  MainTree: TTree;
  T: TTemplate;
begin
  // We're testing Tree.Mode here, since TemplateRuntime.Parse doesn't expose it externally.
  // Therefore, we parse directly using the tree.
  T := New('main');

  TreeSet := TTreeSet.Create;
  FuncNames := TStringList.Create;
  Builtins := T.Context.BuiltinsNames;
  try
    MainTree := TTree.Create('main', [FuncNames, Builtins], False);
    MainTree.Mode := ParseComments; // enable
    MainTree.Parse('A{{/* hi */}}B', '{{', '}}', TreeSet, [FuncNames, Builtins]);

    // The comment will become a node, but the NodeComment executor ignores the print,
    // therefore the output is still AB. The important fact is that the parser doesn't "eat" it as an error..
    T.AddParseTree('main', MainTree);
    AssertEquals('AB', T.ExecuteToString(TValue.Empty));
  finally
    Builtins.Free;
    FuncNames.Free;
    TreeSet.Free;
  end;
end;

procedure TTestParser.Test_WhitespaceTrim_Markers;
var
  T: TTemplate;
begin
  T := New('main');
  // left trim: "{{- " eats spaces to the left of '{{'
  // right trim: " -}}" eats spaces to the right of '}}'
  T.Parse('A   {{- "X" -}}   B');
  AssertEquals('AXB', T.ExecuteToString(TValue.Empty));
end;

procedure TTestParser.Test_Parse_Error_UndefinedFunction_WhenSkipFuncCheckOff;
var
  T: TTemplate;
begin
  T := New('main');
  try
    T.Parse('{{noSuchFunc 1}}');
    Fail('Expected parse error');
  except
    on E: Exception do
      AssertTrue(Pos('function', LowerCase(E.Message)) > 0);
  end;
end;

initialization
  RegisterTest(TTestParser);
end.
