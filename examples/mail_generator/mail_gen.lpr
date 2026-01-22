program mail_gen;

{$mode objfpc}{$H+}{$M+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

uses
  SysUtils,
  Classes,
  Variants,
  RTTI,
  TemplateRuntime;

const
  EmailTemplates =
    '{{define "welcome_html"}}' +
      '<html><body style="font-family:Arial">' +
      '<h2>{{.AppName | html}}</h2>' +
      '<p>Hello, <b>{{.User.Name | html}}</b>!</p>' +
      '<p>Welcome to {{.AppName | html}}.</p>' +
      '<ul>{{range .Features}}<li>{{. | html}}</li>{{end}}</ul>' +
      '<hr><small>If you didn''''t initiate this action, just ignore it.</small>' +
      '</body></html>' +
    '{{end}}' +

    '{{define "welcome_text"}}' +
      '{{.AppName}}' + LineEnding +
      '---' + LineEnding +
      'Hello, {{.User.Name}}!' + LineEnding +
      'Welcome to {{.AppName}}.' + LineEnding +
      'Features:' + LineEnding +
      '{{range .Features}}- {{.}}' + LineEnding + '{{end}}' +
      '---' + LineEnding +
      'If you didn''t initiate this action, just ignore it.' +
    '{{end}}' +

    '{{define "reset_html"}}' +
      '<html><body style="font-family:Arial">' +
      '<h2>{{.AppName | html}}</h2>' +
      '<p>Hello, {{.User.Name | html}}.</p>' +
      '<p>Password reset link:</p>' +
      '<p><a href="{{url .BaseURL (print "reset?token=" .ResetToken) | html}}">Сбросить пароль</a></p>' +
      '<hr><small>If you didn''t initiate this action, just ignore it.</small>' +
      '</body></html>' +
    '{{end}}' +

    '{{define "reset_text"}}' +
      '{{.AppName}}' + LineEnding +
      '---' + LineEnding +
      'Hello, {{.User.Name}}.' + LineEnding +
      'Password reset: {{url .BaseURL (print "reset?token=" .ResetToken)}}' + LineEnding +
      '---' + LineEnding +
      'If you didn''''t initiate this action, just ignore it.' +
    '{{end}}';

type
  TEmailUser = class
  private
    FName: string;
    FEmail: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

  { TEmailModel }

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
      if Length(Args) <> 2 then  raise Exception.Create(
        'url: expects 2 args: base, path');
      Result := JoinURL(Args[0].AsString, Args[1].AsString);
    end;

    F[1].Name := 'upper';
    F[1].Func := function(const Args: array of TValue): TValue
    begin
      if Length(Args) < 1 then raise Exception.Create('upper: expects 1 arg');
      Result := UpperCase(Args[0].AsString);
    end;

    T.Funcs(F);
  end;

var
  T: TTemplate;
  M: TEmailModel;
  HtmlBody, TextBody: string;

{ TEmailUser }

constructor TEmailUser.Create;
begin
  inherited Create;
  FName := '';
  FEmail := '';
end;

destructor TEmailUser.Destroy;
begin
  inherited Destroy;
end;

{ TEmailModel }

constructor TEmailModel.Create;
begin
  inherited Create;
  FAppName := '';
  FBaseURL := '';
  FUser := nil;
  FResetToken := '';
  FFeatures := TStringList.Create;
end;

destructor TEmailModel.Destroy;
begin
  FFeatures.Free;
  inherited Destroy;
end;

begin
  T := New('emails');
  AddEmailFuncs(T);
  T.Option(['missingkey=error']); // для писем лучше падать, чем отправить битое
  T.Delims('{{','}}');

  // HTML-ветка
  T.ParseHTML(EmailTemplates);

  M := TEmailModel.Create;
  M.User := TEmailUser.Create;
  M.Features := TStringList.Create;
  try
    M.AppName := 'MyApp';
    M.BaseURL := 'https://example.com';
    M.User.Name := 'Ivan';
    M.User.Email := 'ivan@example.com';
    M.ResetToken := 'abc123';
    M.Features.Add('Profile');
    M.Features.Add('Notifications');

    HtmlBody := T.Lookup('welcome_html').ExecuteToString(M);
    TextBody := T.Lookup('welcome_text').ExecuteToString(M);

    WriteLn('--- HTML ---');
    WriteLn(HtmlBody);
    WriteLn('--- TEXT ---');
    WriteLn(TextBody);
  finally
    M.User.Free;
    M.Free;
  end;
end.

