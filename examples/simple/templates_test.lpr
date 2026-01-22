program templates_test;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

uses
  SysUtils, Classes, Variants, RTTI,
  TemplateRuntime;

type
  TMyData = class
  private
    FName: string;
    FValue: Integer;
  published
    property Name: string read FName write FName;
    property Value: Integer read FValue write FValue;
  end;

var
  Tmpl: TTemplate;
  Data: TMyData;
  Output: TStringList;
  MyFuncArr: TFuncDefArray;

begin
  Tmpl := New('test');

  SetLength(MyFuncArr, 2);
  MyFuncArr[0].Name := 'add';
  MyFuncArr[0].Func := function(const Args: array of TValue): TValue
    begin
      if Length(Args) < 2 then
        raise Exception.Create('add: requires 2 arguments');
      Result := Args[0].AsInteger + Args[1].AsInteger;
    end;

  MyFuncArr[1].Name := 'upper';
  MyFuncArr[1].Func := function(const Args: array of TValue): TValue
    begin
      if Length(Args) < 1 then
        raise Exception.Create('upper: requires 1 argument');
      Result := UpperCase(Args[0].AsString);
    end;
  Tmpl.Funcs(MyFuncArr);

  Tmpl.Option(['missingkey=default']);
  Tmpl.Delims('{{', '}}');
  Tmpl.Parse('Hello {{.Name | upper}}! Value is {{.Value}}. ' +
             '{{if gt .Value 5}}Big!{{else}}Small.{{end}}');

  Data := TMyData.Create;
  try
    Data.Name := 'world';
    Data.Value := 10;

    Output := TStringList.Create;
    try
      Tmpl.Execute(Output, Data);

      WriteLn('--- Result ---');
      WriteLn(Output.Text);
    finally
      Output.Free;
    end;
  finally
    Data.Free;
  end;

  ReadLn;
end.
