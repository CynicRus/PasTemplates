program mail_router_gen;

{$mode objfpc}{$H+}{$M+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

uses
  SysUtils, Classes, Variants, RTTI,
  TemplateRuntime;

type
  TEmailKind = (ekWelcome, ekReset);

  TEmailUser = class
  private
    FName: string;
    FEmail: string;
  published
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

  TEmailModel = class
  private
    FAppName: string;
    FBaseURL: string;
    FUser: TEmailUser;
    FResetToken: string;
    FFeatures: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AppName: string read FAppName write FAppName;
    property BaseURL: string read FBaseURL write FBaseURL;
    property User: TEmailUser read FUser write FUser;
    property ResetToken: string read FResetToken write FResetToken;
    property Features: TStringList read FFeatures write FFeatures;
  end;

function JoinURL(const A, B: string): string;
begin
  if (A <> '') and A.EndsWith('/') then
    Result := A + B.TrimLeft(['/'])
  else
    Result := A + '/' + B.TrimLeft(['/']);
end;

procedure AddEmailFuncs(T: TTemplate);
var
  F: TFuncDefArray;
begin
  SetLength(F, 2);

  F[0].Name := 'url';
  F[0].Func := function(const Args: array of TValue): TValue
  begin
    if Length(Args) <> 2 then
      raise Exception.Create('url: expects 2 args: base, path');
    Result := JoinURL(Args[0].AsString, Args[1].AsString);
  end;

  F[1].Name := 'upper';
  F[1].Func := function(const Args: array of TValue): TValue
  begin
    if Length(Args) < 1 then
      raise Exception.Create('upper: expects 1 arg');
    Result := UpperCase(Args[0].AsString);
  end;

  T.Funcs(F);
end;

const
  EmailTemplates =
    '{{define "welcome_html"}}' +
      '<html><body style="font-family:Arial">' +
      '<h2>{{.AppName | html}}</h2>' +
      '<p>Hello, <b>{{.User.Name | html}}</b>!</p>' +
      '<p>Welcome to {{.AppName | html}}.</p>' +
      '{{if gt (len .Features) 0}}' +
        '<ul>{{range .Features}}<li>{{. | html}}</li>{{end}}</ul>' +
      '{{else}}' +
        '<p><i>No features listed yet.</i></p>' +
      '{{end}}' +
      '<hr><small>If you didn''t initiate this action, just ignore it.</small>' +
      '</body></html>' +
    '{{end}}' +

    '{{define "welcome_text"}}' +
      '{{.AppName}}' + LineEnding +
      '---' + LineEnding +
      'Hello, {{.User.Name}}!' + LineEnding +
      'Welcome to {{.AppName}}.' + LineEnding +
      '{{if gt (len .Features) 0}}' +
        'Features:' + LineEnding +
        '{{range .Features}}- {{.}}' + LineEnding + '{{end}}' +
      '{{else}}' +
        'Features: none' + LineEnding +
      '{{end}}' +
      '---' + LineEnding +
      'If you didn''t initiate this action, just ignore it.' +
    '{{end}}' +

    '{{define "reset_html"}}' +
      '<html><body style="font-family:Arial">' +
      '<h2>{{.AppName | html}}</h2>' +
      '<p>Hello, {{.User.Name | html}}.</p>' +
      '{{if .ResetToken}}' +
        '<p>Password reset link:</p>' +
        '<p><a href="{{url .BaseURL (print "reset?token=" .ResetToken) | html}}">Reset password</a></p>' +
      '{{else}}' +
        '<p><b>Error:</b> reset token is missing.</p>' +
      '{{end}}' +
      '<hr><small>If you didn''t initiate this action, just ignore it.</small>' +
      '</body></html>' +
    '{{end}}' +

    '{{define "reset_text"}}' +
      '{{.AppName}}' + LineEnding +
      '---' + LineEnding +
      'Hello, {{.User.Name}}.' + LineEnding +
      '{{if .ResetToken}}' +
        'Password reset: {{url .BaseURL (print "reset?token=" .ResetToken)}}' + LineEnding +
      '{{else}}' +
        'ERROR: reset token is missing' + LineEnding +
      '{{end}}' +
      '---' + LineEnding +
      'If you didn''t initiate this action, just ignore it.' +
    '{{end}}';

function RenderEmail(const T: TTemplate; Kind: TEmailKind; const Model: TValue;
  out HtmlBody, TextBody: string): boolean;
var
  HtmlName, TextName: string;
begin
  case Kind of
    ekWelcome:
      begin
        HtmlName := 'welcome_html';
        TextName := 'welcome_text';
      end;
    ekReset:
      begin
        HtmlName := 'reset_html';
        TextName := 'reset_text';
      end;
  end;

  HtmlBody := T.Lookup(HtmlName).ExecuteToString(Model);
  TextBody := T.Lookup(TextName).ExecuteToString(Model);
  Result := True;
end;

{ TEmailModel }

constructor TEmailModel.Create;
begin
  inherited Create;
  FUser := nil;
  FFeatures := TStringList.Create;
end;

destructor TEmailModel.Destroy;
begin
  FFeatures.Free;
  FUser.Free;
  inherited Destroy;
end;

var
  T: TTemplate;
  M: TEmailModel;
  HtmlBody, TextBody: string;
begin
  T := New('emails');
  AddEmailFuncs(T);

  // режим: missingkey=zero.
  // Полезно когда модель может быть частично заполнена и вручную обрабатывается if-ами.
  T.Option(['missingkey=zero']);
  T.Delims('{{', '}}');
  T.ParseHTML(EmailTemplates);

  M := TEmailModel.Create;
  try
    M.AppName := 'MyApp';
    M.BaseURL := 'https://example.com';
    M.User := TEmailUser.Create;
    M.User.Name := 'Ivan';
    M.User.Email := 'ivan@example.com';

    // welcome без features (покажет "none")
    M.Features.Clear;
    RenderEmail(T, ekWelcome, M, HtmlBody, TextBody);
    WriteLn('==== WELCOME (no features) HTML ====');
    WriteLn(HtmlBody);
    WriteLn('==== WELCOME (no features) TEXT ====');
    WriteLn(TextBody);
    WriteLn;

    // reset с токеном
    M.ResetToken := 'abc123';
    RenderEmail(T, ekReset, M, HtmlBody, TextBody);
    WriteLn('==== RESET (with token) HTML ====');
    WriteLn(HtmlBody);
    WriteLn('==== RESET (with token) TEXT ====');
    WriteLn(TextBody);
    WriteLn;

    // reset без токена (покажет ветку ERROR, без падения)
    M.ResetToken := '';
    RenderEmail(T, ekReset, M, HtmlBody, TextBody);
    WriteLn('==== RESET (missing token) HTML ====');
    WriteLn(HtmlBody);
    WriteLn('==== RESET (missing token) TEXT ====');
    WriteLn(TextBody);
  finally
    M.Free;
  end;
end.
