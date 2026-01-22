unit test_template_exec_rtti;

{$mode delphi}{$H+}{$M+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, Rtti,
  TemplateRuntime;

type
  TPub = class
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  TTestRTTI = class(TTestCase)
  published
    procedure Test_Class_PublishedProperty;
  end;

implementation

procedure TTestRTTI.Test_Class_PublishedProperty;
var
  T: TTemplate;
  O: TPub;
begin
  T := New('t');
  O := TPub.Create;
  try
    O.Name := 'n';
    T.Parse('{{.Name}}');
    AssertEquals('n', T.ExecuteToString(TValue.From(TypeInfo(O),@O)));
  finally
    O.Free;
  end;
end;

initialization
  RegisterTest(TTestRTTI);
end.
