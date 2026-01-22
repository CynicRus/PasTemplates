unit test_template_exec_templates;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, Rtti,
  TemplateRuntime;

type
  TObj = class
  private
    FName: string;
  published
    property Name: string read FName write FName;
  end;

  TUser = class
  private
    FName: string;
    FEmail: string;
  published
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

  { TTestTemplates }

  TTestTemplates = class(TTestCase)
  published
    procedure Test_Template_Call_WithPipeDot;
    procedure Test_Template_NotDefined_IsError;
    procedure Test_Block_WithFallback;
    procedure Test_Block_OverrideFromAnotherTemplate;
    procedure Test_HTML_Escaping_Function;
    procedure Test_ParseFile_LoadsTemplate;
    procedure Test_ParseFiles_MultipleTemplates;
  end;

implementation

procedure TTestTemplates.Test_Template_Call_WithPipeDot;
var
  T: TTemplate;
  O: TObj;
begin
  T := New('main');
  T.Parse('{{define "sub"}}[{{.Name}}]{{end}}Main={{template "sub" .}}');

  O := TObj.Create;
  try
    O.Name := 'X';
    AssertEquals('Main=[X]', T.ExecuteToString(TValue.From(TypeInfo(O),@O)));
  finally
    O.Free;
  end;
end;

procedure TTestTemplates.Test_Template_NotDefined_IsError;
var
  T: TTemplate;
begin
  T := New('main');
  T.Parse('{{template "nope" .}}');
  try
    T.ExecuteToString(TValue.Empty);
    Fail('Expected exec error');
  except
    on E: Exception do
      AssertTrue(Pos('not defined', LowerCase(E.Message)) > 0);
  end;
end;

procedure TTestTemplates.Test_Block_WithFallback;
var
  T: TTemplate;
  O: TObj;
begin
  T := New('main');
  T.Parse('{{block "title"}}Default Title{{end}} | {{block "content"}}Default Content{{end}}');

   O := TObj.Create;
  try
    O.Name:='ignored';
  AssertEquals('Default Title | Default Content', T.ExecuteToString(TValue.From(TypeInfo(O),@O)));

  finally
    O.Free;
  end;
end;


procedure TTestTemplates.Test_Block_OverrideFromAnotherTemplate;
var
  T: TTemplate;
  O: TObj;
begin
  // Layout
  T := New('layout');
  T.Parse(
    '<title>{{block "title"}}Site{{end}}</title>' +
    '<body>{{block "body"}}Body{{end}}</body>'
  );

  // Page
  T.New('page').
    Parse(
      '{{template "layout" .}}' +
      '{{define "title"}}User Page{{end}}' +
      '{{define "body"}}Hello {{.Name}}!{{end}}'
    );
  O := TObj.Create;
  try
  O.Name := 'Username';
  AssertEquals('<title>User Page</title><body>Hello Username!</body>',
    T.Lookup('page').ExecuteToString(TValue.From(TypeInfo(O),@O)));

  finally
    O.Free;
  end;
end;

procedure TTestTemplates.Test_HTML_Escaping_Function;
var
  T: TTemplate;
  O: TObj;
begin
  T := New('main');
  T.ParseHTML('{{html .Name}}');
  O := TObj.Create;
  try
  O.Name := '<script>alert("XSS")</script> & "quotes"';
  AssertEquals('&lt;script&gt;alert(&quot;XSS&quot;)&lt;/script&gt; &amp; &quot;quotes&quot;',
    T.ExecuteToString(TValue.From(TypeInfo(O),@O)));

  finally
    O.Free;
  end;
end;

procedure TTestTemplates.Test_ParseFile_LoadsTemplate;
var
  T: TTemplate;
  TempFile: string;
  O: TObj;
begin
  TempFile := GetTempDir + PathDelim + 'test_tpl_1.html';
  O := TObj.Create;
  try
    with TStringList.Create do
    try
      Add('<h1>Hello {{.Name}}!</h1>');
      SaveToFile(TempFile);
    finally
      Free;
    end;

    T := New('main');
    T.ParseFile(TempFile);

    O.Name := 'Username';
    AssertEquals('<h1>Hello Username!</h1>', T.ExecuteToString(TValue.From(TypeInfo(O),@O)));
  finally
    O.Free;
    if FileExists(TempFile) then DeleteFile(TempFile);
  end;
end;

procedure TTestTemplates.Test_ParseFiles_MultipleTemplates;
var
  T: TTemplate;
  TempFile1, TempFile2: string;
  O: TObj;
begin
  TempFile1 := GetTempDir + PathDelim + 'layout.html';
  TempFile2 := GetTempDir + PathDelim + 'page.html';
  O := TObj.Create;
  try
    // layout.html
    with TStringList.Create do
    try
      Add('<title>{{block "title"}}Default{{end}}</title>');
      Add('<body>{{template "content" .}}</body>');
      SaveToFile(TempFile1);
    finally
      Free;
    end;

    // page.html
    with TStringList.Create do
    try
      Add('{{template "layout" .}}');
      Add('{{define "title"}}User{{end}}');
      Add('{{define "content"}}Hi {{.Name}}!{{end}}');
      SaveToFile(TempFile2);
    finally
      Free;
    end;

    T := New('layout');
    T.ParseFiles([TempFile1, TempFile2]);

    O.Name := 'Username';
    AssertEquals('<title>User</title><body>Hi Username!</body>',
      T.Lookup('page').ExecuteToString(TValue.From(TypeInfo(O),@O)));
  finally
    O.Free;
    if FileExists(TempFile1) then DeleteFile(TempFile1);
    if FileExists(TempFile2) then DeleteFile(TempFile2);
  end;
end;

initialization
  RegisterTest(TTestTemplates);
end.
