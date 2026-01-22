unit test_template_exec_controlflow;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, Rtti,
  TemplateRuntime;

type
  TBox = class
  private
    FX: Integer;
  published
    property X: Integer read FX write FX;
  end;

  TTestControlFlow = class(TTestCase)
  published
    procedure Test_If_Else_ElseIf;
    procedure Test_With_ChangesDot;
    procedure Test_Pipeline_AssignAndDeclare;
    procedure Test_ParenthesizedPipeline_AsArg;
  end;

implementation

procedure TTestControlFlow.Test_If_Else_ElseIf;
var
  T: TTemplate;
  B: TBox;
begin
  T := New('t');
  T.Parse('{{if gt .X 10}}big{{else if gt .X 5}}mid{{else}}small{{end}}');

  B := TBox.Create;
  try
    B.X := 3;
    AssertEquals('small', T.ExecuteToString(TValue.From(TypeInfo(B),@B)));
    B.X := 7;
    AssertEquals('mid', T.ExecuteToString(TValue.From(TypeInfo(B),@B)));
    B.X := 11;
    AssertEquals('big', T.ExecuteToString(TValue.From(TypeInfo(B),@B)));
  finally
    B.Free;
  end;
end;

procedure TTestControlFlow.Test_With_ChangesDot;
var
  T: TTemplate;
  B: TBox;
begin
  T := New('t');
  T.Parse('{{with .}}{{.X}}{{end}}');

  B := TBox.Create;
  try
    B.X := 42;
    AssertEquals('42', T.ExecuteToString(TValue.From(TypeInfo(B),@B)));
  finally
    B.Free;
  end;
end;

procedure TTestControlFlow.Test_Pipeline_AssignAndDeclare;
var
  T: TTemplate;
begin
  T := New('t');
  // declare
  T.Parse('{{$x := 3}}{{$x}}');
  AssertEquals('3', T.ExecuteToString(TValue.Empty));

  // assign
  T.Parse('{{$x := 3}}{{$x = 5}}{{$x}}');
  AssertEquals('5', T.ExecuteToString(TValue.Empty));
end;

procedure TTestControlFlow.Test_ParenthesizedPipeline_AsArg;
var
  T: TTemplate;
begin
  T := New('t');
  // (pipeline) as argument {{and (gt 2 1) "ok"}}
  T.Parse('{{and (gt 2 1) "ok"}}');
  AssertEquals('ok', T.ExecuteToString(TValue.Empty));
end;

initialization
  RegisterTest(TTestControlFlow);
end.
