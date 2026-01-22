unit test_template_core;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, Rtti,
  TemplateRuntime;

type
  { Simple data with published property for TypInfo and public field/property for RTTI }
  TInner = class
  private
    FTitle: string;
  published
    property Title: string read FTitle write FTitle;

  end;

  TMyData = class
  private
    FName: string;
    FValue: integer;
    FInner: TInner;
  public
    PublicField: integer;
    PublicProp: string;
  published
    property Name: string read FName write FName;
    property Value: integer read FValue write FValue;
    property Inner: TInner read FInner write FInner;

  end;

function ExecToStr(T: TTemplate; const Data: TValue): string;

type
  TTestCore = class(TTestCase)
  published
    procedure Test_New_Delims_Option_Funcs_BasicExecute;
    procedure Test_Clone_CopiesTemplatesAndFuncsAndMissingKey;
  end;

implementation

function ExecToStr(T: TTemplate; const Data: TValue): string;
begin
  Result := T.ExecuteToString(Data);
end;

procedure TTestCore.Test_New_Delims_Option_Funcs_BasicExecute;
var
  Tmpl: TTemplate;
  Data: TMyData;
  V: TValue;
  Fns: TFuncDefArray;
begin
  Tmpl := New('main');

  SetLength(Fns, 2);
  Fns[0].Name := 'upper';
  Fns[0].Func := function(const Args: array of TValue): TValue
  begin
    Result := UpperCase(Args[0].AsString);
  end;

  Fns[1].Name := 'add';
  Fns[1].Func := function(const Args: array of TValue): TValue
  begin
    Result := Args[0].AsInteger + Args[1].AsInteger;
  end;

  Tmpl.Funcs(Fns);
  Tmpl.Option(['missingkey=default']);
  Tmpl.Delims('<<', '>>');

  Tmpl.Parse('Hello <<.Name | upper>>; sum=<<add .Value 2>>');

  Data := TMyData.Create;
  try
    Data.Name := 'world';
    Data.Value := 10;

    V := TValue.From(TypeInfo(Data),@Data);
    AssertEquals('Hello WORLD; sum=12', ExecToStr(Tmpl, V));
  finally
    Data.Free;
  end;
end;

procedure TTestCore.Test_Clone_CopiesTemplatesAndFuncsAndMissingKey;
var
  T0, T1: TTemplate;
  Fns: TFuncDefArray;
  V: TValue;
begin
  T0 := New('main');

  SetLength(Fns, 1);
  Fns[0].Name := 'up';
  Fns[0].Func := function(const Args: array of TValue): TValue
  begin
    Result := UpperCase(Args[0].AsString);
  end;

  T0.Funcs(Fns);

  T0.Option(['missingkey=error']);
  T0.Parse('{{define "sub"}}Sub={{.X}}{{end}}Main={{"a" | up}};{{template "sub" .}}');

  T1 := T0.Clone;

  AssertTrue(T1 <> T0);
  AssertNotNull(T1.Lookup('sub'));

  // missingkey=error must be preserved
  V := TValue.From(TypeInfo(TObject(nil)),nil);
  try
    // sub requires .X, but there is no data => error
    T1.ExecuteToString(V);
    Fail('Expected exception');
  except
    on E: Exception do
      AssertTrue(Pos('missing key', LowerCase(E.Message)) > 0);
  end;
end;

initialization
  RegisterTest(TTestCore);
end.
