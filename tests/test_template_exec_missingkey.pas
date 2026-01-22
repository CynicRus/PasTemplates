unit test_template_exec_missingkey;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, Rtti,
  TemplateRuntime;

type
  TTestMissingKey = class(TTestCase)
  published
    procedure Test_MissingKey_Default_PrintsNoValue;
    procedure Test_MissingKey_Zero_PrintsNoValue;
    procedure Test_MissingKey_Error_Raises;
    procedure Test_Index_MissingKey_Error_Raises;
  end;

implementation

procedure TTestMissingKey.Test_MissingKey_Default_PrintsNoValue;
var
  T: TTemplate;
begin
  T := New('t');
  T.Option(['missingkey=default']);
  T.Parse('x={{.Nope}}');
  AssertEquals('x=<no value>', T.ExecuteToString(TValue.Empty));
end;

procedure TTestMissingKey.Test_MissingKey_Zero_PrintsNoValue;
var
  T: TTemplate;
begin
  T := New('t');
  T.Option(['missingkey=zero']);
  T.Parse('x={{.Nope}}');
  AssertEquals('x=<no value>', T.ExecuteToString(TValue.Empty));
end;

procedure TTestMissingKey.Test_MissingKey_Error_Raises;
var
  T: TTemplate;
begin
  T := New('t');
  T.Option(['missingkey=error']);
  T.Parse('{{.Nope}}');
  try
    T.ExecuteToString(TValue.Empty);
    Fail('Expected error');
  except
    on E: Exception do
      AssertTrue(Pos('missing', LowerCase(E.Message)) > 0);
  end;
end;

procedure TTestMissingKey.Test_Index_MissingKey_Error_Raises;
var
  T: TTemplate;
begin
  T := New('t');
  T.Option(['missingkey=error']);
  T.Parse('{{index "abc" 99}}');
  try
    T.ExecuteToString(TValue.Empty);
    Fail('Expected error');
  except
    on E: Exception do
      AssertTrue(Pos('index', LowerCase(E.Message)) > 0);
  end;
end;

initialization
  RegisterTest(TTestMissingKey);
end.