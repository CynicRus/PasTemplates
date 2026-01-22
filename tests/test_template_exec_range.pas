unit test_template_exec_range;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Contnrs, fpcunit, testregistry, Rtti,
  TemplateRuntime;

type
  TTestRange = class(TTestCase)
  published
    procedure Test_Range_DynArray;
    procedure Test_Range_String;
    procedure Test_Range_TStrings;
    procedure Test_Range_TFPList_Pointers;
    procedure Test_Range_TFPHashObjectList_Objects;
    procedure Test_Range_Else_OnEmpty;
    procedure Test_Range_Continue_And_Break;
    procedure Test_Break_OutsideRange_IsError;
    procedure Test_Continue_OutsideRange_IsError;
  end;

implementation

procedure TTestRange.Test_Range_DynArray;
var
  T: TTemplate;
  A: array of Integer;
begin
  T := New('t');
  SetLength(A, 3);
  A[0] := 1; A[1] := 2; A[2] := 3;

  T.Parse('{{range .}}{{.}}{{end}}');
  AssertEquals('123', T.ExecuteToString(TValue.From(TypeInfo(A),@A)));
end;

procedure TTestRange.Test_Range_String;
var
  T: TTemplate;
  ab: string;
begin
  T := New('t');
  T.Parse('{{range .}}{{.}}{{end}}');
  ab := 'ab';
  AssertEquals(ab, T.ExecuteToString(TValue.From(TypeInfo(ab),@ab)));
end;

procedure TTestRange.Test_Range_TStrings;
var
  T: TTemplate;
  S: TStringList;
begin
  T := New('t');
  S := TStringList.Create;
  try
    S.Add('x');
    S.Add('y');
    T.Parse('{{range .}}{{.}}{{end}}');
    AssertEquals('xy', T.ExecuteToString(TValue.From(TypeInfo(S),@S)));
  finally
    S.Free;
  end;
end;

procedure TTestRange.Test_Range_TFPList_Pointers;
var
  T: TTemplate;
  L: TFPList;
  P1, P2: Pointer;
  a,a1: integer;
begin
  T := New('t');
  L := TFPList.Create;
  try
    a := 1;
    a1 := 2;
    P1 := @a;
    P2 := @a1;
    L.Add(P1);
    L.Add(P2);

    // элементы TFPList оборачиваются в TValue типа Pointer и печатаются как число/адрес строкой
    T.Parse('{{range .}}{{.}};{{end}}');
    AssertTrue(Length(T.ExecuteToString(TValue.From(TypeInfo(L),@L))) > 0);
  finally
    L.Free;
  end;
end;

type
  TFoo = class
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

procedure TTestRange.Test_Range_TFPHashObjectList_Objects;

var
  T: TTemplate;
  H: TFPHashObjectList;
  A, B: TFoo;
  OutS: string;
begin
  T := New('t');
  H := TFPHashObjectList.Create(True);
  try
    A := TFoo.Create; A.Name := 'a';
    B := TFoo.Create; B.Name := 'b';
    H.Add('k1', A);
    H.Add('k2', B);

    T.Parse('{{range .}}{{.Name}}{{end}}');
    OutS := T.ExecuteToString(TValue.From(TypeInfo(H),@H));

    // порядок в hash-list может быть стабильным/нестабильным; проверяем содержимое
    AssertTrue(Pos('a', OutS) > 0);
    AssertTrue(Pos('b', OutS) > 0);
  finally
    H.Free;
  end;
end;

procedure TTestRange.Test_Range_Else_OnEmpty;
var
  T: TTemplate;
  A: array of Integer;
begin
  T := New('t');
  SetLength(A, 0);
  T.Parse('{{range .}}X{{else}}EMPTY{{end}}');
  AssertEquals('EMPTY', T.ExecuteToString(TValue.From(TypeInfo(A),@A)));
end;

procedure TTestRange.Test_Range_Continue_And_Break;
var
  T: TTemplate;
  A: array of Integer;
begin
  T := New('t');
  SetLength(A, 5);
  A[0] := 1; A[1] := 2; A[2] := 3; A[3] := 4; A[4] := 5;

  // continue на 2, break на 4 => печатаем 1,3 и останавливаемся
  T.Parse('{{range .}}' +
          '{{if eq . 2}}{{continue}}{{end}}' +
          '{{if eq . 4}}{{break}}{{end}}' +
          '{{.}}' +
          '{{end}}');
  AssertEquals('13', T.ExecuteToString(TValue.From(TypeInfo(A),@A)));
end;

procedure TTestRange.Test_Break_OutsideRange_IsError;
var
  T: TTemplate;
begin
  T := New('t');
  try
    T.Parse('{{break}}');
    Fail('Expected parse error');
  except
    on E: Exception do
      AssertTrue(Pos('outside', LowerCase(E.Message)) > 0);
  end;
end;

procedure TTestRange.Test_Continue_OutsideRange_IsError;
var
  T: TTemplate;
begin
  T := New('t');
  try
    T.Parse('{{continue}}');
    Fail('Expected parse error');
  except
    on E: Exception do
      AssertTrue(Pos('outside', LowerCase(E.Message)) > 0);
  end;
end;

initialization
  RegisterTest(TTestRange);
end.
