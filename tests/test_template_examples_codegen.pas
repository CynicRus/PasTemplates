unit test_template_examples_codegen;

{$mode objfpc}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

uses
  SysUtils, Classes, fpcunit, testregistry, Rtti,
  TemplateRuntime;

type
  TPageModel = class
  private
    FTitle: string;
    FPrimaryColor: string;
    FItems: TStringList;
  published
    property Title: string read FTitle write FTitle;
    property PrimaryColor: string read FPrimaryColor write FPrimaryColor;
    property Items: TStringList read FItems write FItems;
  end;

  TTestExamplesCodegen = class(TTestCase)
  published
    procedure Test_Codegen_HTML_JS_CSS_Templates;
  end;

implementation

procedure TTestExamplesCodegen.Test_Codegen_HTML_JS_CSS_Templates;
var
  T: TTemplate;
  M: TPageModel;
  Fns: TFuncDefArray;
  OutHtml, OutCss, OutJs: string;
begin
  T := New('bundle');

  // function upper just for demo
  SetLength(Fns, 1);
  Fns[0].Name := 'upper';
  Fns[0].Func := function(const Args: array of TValue): TValue
    begin
      Result := UpperCase(Args[0].AsString);
    end;
  T.Funcs(Fns);

  T.Parse(
    '{{define "html"}}' +
      '<!doctype html>' +
      '<html><head><title>{{.Title}}</title>' +
      '<link rel="stylesheet" href="app.css"></head>' +
      '<body>' +
        '<h1>{{.Title | upper}}</h1>' +
        '<ul>{{range .Items}}<li>{{.}}</li>{{end}}</ul>' +
        '<script src="app.js"></script>' +
      '</body></html>' +
    '{{end}}' +

    '{{define "css"}}' +
      ':root{--primary: {{.PrimaryColor}};}' +
      'h1{color: var(--primary);}' +
    '{{end}}' +

    '{{define "js"}}' +
      '(function(){' +
        'const title="{{.Title}}";' +
        'console.log("title:", title);' +
      '})();' +
    '{{end}}'
  );

  M := TPageModel.Create;
  try
    M.Items := TStringList.Create;
    M.Title := 'Demo';
    M.PrimaryColor := '#3366ff';
    M.Items.Add('one');
    M.Items.Add('two');

    OutHtml := T.Lookup('html').ExecuteToString(TValue.From(TypeInfo(M),@M));
    OutCss  := T.Lookup('css').ExecuteToString(TValue.From(TypeInfo(M),@M));
    OutJs   := T.Lookup('js').ExecuteToString(TValue.From(TypeInfo(M),@M));

    AssertTrue(Pos('<h1>DEMO</h1>', OutHtml) > 0);
    AssertTrue(Pos('--primary: #3366ff', OutCss) > 0);
    AssertTrue(Pos('const title="Demo"', OutJs) > 0);
  finally
    if M.Items <> nil then M.Items.Free;
    M.Free;
  end;
end;

initialization
  RegisterTest(TTestExamplesCodegen);
end.
