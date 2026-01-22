unit test_template_exec_builtins;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, Rtti,
  TemplateRuntime;

type
  TTestBuiltins = class(TTestCase)
  published
    procedure Test_And_Or_Not;
    procedure Test_Len_String_Array_TStrings;
    procedure Test_Index_String_Array_Member;
    procedure Test_Comparisons_Numeric_String_Bool;
    procedure Test_Print_Println;
    procedure Test_Printf_IsStub;
    procedure Test_Call_UserFunctionByName;
  end;

implementation

procedure TTestBuiltins.Test_And_Or_Not;
var
  T: TTemplate;
begin
  T := New('t');
  T.Parse('{{and true 1 "x"}}|{{and true 0 "x"}}|{{or false "" "x"}}|{{not true}}');
  AssertEquals('x|0|x|false', LowerCase(T.ExecuteToString(TValue.Empty)));
end;

procedure TTestBuiltins.Test_Len_String_Array_TStrings;
var
  T: TTemplate;
  SL: TStringList;
  V: TValue;
  Arr: array of integer;
begin
  T := New('t');

  SL := TStringList.Create;
  try
    SL.Add('a');
    SL.Add('b');
    V := TValue.From(TypeInfo(SL), @SL);

    SetLength(Arr, 3);
    Arr[0] := 1;
    Arr[1] := 2;
    Arr[2] := 3;

    T.Parse('{{len "abcd"}}|{{len .}}');
    AssertEquals('4|2', T.ExecuteToString(V));

    T.Parse('{{len .}}');
    AssertEquals('3', T.ExecuteToString(TValue.From(TypeInfo(Arr), @Arr)));
  finally
    SL.Free;
  end;
end;

type
  TObj = class
  private
    FName: string;
  published
    property Name: string read FName write FName;

  end;

procedure TTestBuiltins.Test_Index_String_Array_Member;
var
  T: TTemplate;
  Arr: array of string;
  Obj: TObject;
  O: TObj;
begin
  T := New('t');

  // string index
  T.Parse('{{index "abc" 1}}');
  AssertEquals('b', T.ExecuteToString(TValue.Empty));

  // array index
  SetLength(Arr, 2);
  Arr[0] := 'x';
  Arr[1] := 'y';
  T.Parse('{{index . 0}}-{{index . 1}}');
  AssertEquals('x-y', T.ExecuteToString(TValue.From(TypeInfo(Arr), @Arr)));

  // member access via index "Name"
  O := TObj.Create;
  try
    O.Name := 'zz';
    T.Parse('{{index . "Name"}}');
    AssertEquals('zz', T.ExecuteToString(TValue.From(TypeInfo(O), @O)));
  finally
    O.Free;
  end;
end;

procedure TTestBuiltins.Test_Comparisons_Numeric_String_Bool;
var
  T: TTemplate;
begin
  T := New('t');
  T.Parse('{{eq 1 1 1}}|{{ne 1 2}}|{{lt 1 2}}|{{le 2 2}}|{{gt "b" "a"}}|{{ge true false}}');
  AssertEquals('TRUE|TRUE|TRUE|TRUE|TRUE|TRUE', UpperCase(T.ExecuteToString(TValue.Empty)));
end;

procedure TTestBuiltins.Test_Print_Println;
var
  T: TTemplate;
begin
  T := New('t');
  T.Parse('{{print "a" 1}}|{{println "b"}}');
  AssertEquals('a1|b' + LineEnding, T.ExecuteToString(TValue.Empty));
end;

procedure TTestBuiltins.Test_Printf_IsStub;
var
  T: TTemplate;
  S: string;
begin
  T := New('t');
  S:= 'x=%d';
  // printf now just returns the format (see TemplateExec.EvalFunction)
  T.Parse('{{printf "x=%d" 10}}');
  AssertEquals(S, T.ExecuteToString(TValue.Empty));
end;

procedure TTestBuiltins.Test_Call_UserFunctionByName;
var
  T: TTemplate;
  Fns: TFuncDefArray;
begin
  T := New('t');
  SetLength(Fns, 1);
  Fns[0].Name := 'add';
  Fns[0].Func := function(const Args: array of TValue): TValue
  begin
    Result := Args[0].AsInteger + Args[1].AsInteger;
  end;

  T.Funcs(Fns);

  T.Parse('{{call "add" 2 3}}');
  AssertEquals('5', T.ExecuteToString(TValue.Empty));
end;

initialization
  RegisterTest(TTestBuiltins);
end.
