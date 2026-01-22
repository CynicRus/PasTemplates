program mail_digest_gen;

{$mode objfpc}{$H+}{$M+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

uses
  SysUtils, Classes, Variants, RTTI,contnrs,
  TemplateRuntime;

type
  TDigestItem = class
  private
    FTitle: string;
    FURL: string;
  published
    property Title: string read FTitle write FTitle;
    property URL: string read FURL write FURL;
  end;

  TEmailUser = class
  private
    FName: string;
    FEmail: string;
  published
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

  TDigestModel = class
  private
    FAppName: string;
    FBaseURL: string;
    FUser: TEmailUser;
    FPeriodStart: TDateTime;
    FPeriodEnd: TDateTime;
    FHighlights: TStringList;
    FItems: TObjectList; // TDigestItem
  public
    constructor Create;
    destructor Destroy; override;
  published
    property AppName: string read FAppName write FAppName;
    property BaseURL: string read FBaseURL write FBaseURL;
    property User: TEmailUser read FUser write FUser;
    property PeriodStart: TDateTime read FPeriodStart write FPeriodStart;
    property PeriodEnd: TDateTime read FPeriodEnd write FPeriodEnd;
    property Highlights: TStringList read FHighlights write FHighlights;
    property Items: TObjectList read FItems write FItems;
  end;

function JoinURL(const A, B: string): string;
begin
  if (A <> '') and A.EndsWith('/') then
    Result := A + B.TrimLeft(['/'])
  else
    Result := A + '/' + B.TrimLeft(['/']);
end;

procedure AddDigestFuncs(T: TTemplate);
var
  F: TFuncDefArray;
begin
  SetLength(F, 3);

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

  F[2].Name := 'formatDate';
  F[2].Func := function(const Args: array of TValue): TValue
  var
    D: TDateTime;
  begin
    if Length(Args) <> 1 then
      raise Exception.Create('formatDate: expects 1 arg (TDateTime)');
    D := Args[0].AsExtended; // TValue carries TDateTime as float
    Result := FormatDateTime('yyyy-mm-dd', D);
  end;

  T.Funcs(F);
end;

const
  DigestTemplates =
    // общий layout для html
    '{{define "layout_html"}}' +
      '<html><body style="font-family:Arial">' +
      '<div style="max-width:720px;margin:0 auto">' +
      '<h2>{{.AppName | html}}</h2>' +
      '{{block "content_html" .}}{{end}}' +
      '<hr><small>Sent to {{.User.Email | html}}</small>' +
      '</div>' +
      '</body></html>' +
    '{{end}}' +

    // общий layout для text
    '{{define "layout_text"}}' +
      '{{.AppName}}' + LineEnding +
      '================================' + LineEnding +
      '{{block "content_text" .}}{{end}}' + LineEnding +
      '--------------------------------' + LineEnding +
      'Sent to {{.User.Email}}' +
    '{{end}}' +

    // контент digest (html)
    '{{define "digest_html"}}' +
      '{{template "layout_html" .}}' +
    '{{end}}' +

    '{{define "content_html"}}' +
      '<p>Hello, <b>{{.User.Name | html}}</b>!</p>' +
      '<p>Your weekly digest for <b>{{formatDate .PeriodStart}}</b> - <b>{{formatDate .PeriodEnd}}</b>:</p>' +

      '{{if gt (len .Highlights) 0}}' +
        '<h3>Highlights</h3>' +
        '<ul>{{range .Highlights}}<li>{{. | html}}</li>{{end}}</ul>' +
      '{{else}}' +
        '<p><i>No highlights this week.</i></p>' +
      '{{end}}' +

      '<h3>Links</h3>' +
      '{{if gt (len .Items) 0}}' +
        '<ol>' +
          '{{range .Items}}' +
            '<li><a href="{{.URL | html}}">{{.Title | html}}</a></li>' +
          '{{end}}' +
        '</ol>' +
      '{{else}}' +
        '<p><i>No links.</i></p>' +
      '{{end}}' +

      '<p>' +
        '<a href="{{url .BaseURL "settings/notifications" | html}}">Manage notifications</a>' +
      '</p>' +
    '{{end}}' +

    // контент digest (text)
    '{{define "digest_text"}}' +
      '{{template "layout_text" .}}' +
    '{{end}}' +

    '{{define "content_text"}}' +
      'Hello, {{.User.Name}}!' + LineEnding +
      'Digest period: {{formatDate .PeriodStart}} - {{formatDate .PeriodEnd}}' + LineEnding + LineEnding +

      '{{if gt (len .Highlights) 0}}' +
        'Highlights:' + LineEnding +
        '{{range .Highlights}}- {{.}}' + LineEnding + '{{end}}' + LineEnding +
      '{{else}}' +
        'Highlights: none' + LineEnding + LineEnding +
      '{{end}}' +

      '{{if gt (len .Items) 0}}' +
        'Links:' + LineEnding +
        '{{range .Items}}* {{.Title}} -> {{.URL}}' + LineEnding + '{{end}}' + LineEnding +
      '{{else}}' +
        'Links: none' + LineEnding + LineEnding +
      '{{end}}' +

      'Settings: {{url .BaseURL "settings/notifications"}}' +
    '{{end}}';

{ TDigestModel }

constructor TDigestModel.Create;
begin
  inherited Create;
  FUser := nil;
  FHighlights := TStringList.Create;
  FItems := TObjectList.Create(True {owns objects});
end;

destructor TDigestModel.Destroy;
begin
  FItems.Free;
  FHighlights.Free;
  FUser.Free;
  inherited Destroy;
end;

var
  T: TTemplate;
  M: TDigestModel;
  Item: TDigestItem;
  HtmlBody, TextBody: string;
begin
  T := New('digest');
  AddDigestFuncs(T);

  // для писем удобно падать на missingkey=error, чтобы не отправлять мусор
  T.Option(['missingkey=error']);
  T.Delims('{{', '}}');

  // HTML-режим даст функцию html (escape)
  T.ParseHTML(DigestTemplates);

  M := TDigestModel.Create;
  try
    M.AppName := 'MyApp';
    M.BaseURL := 'https://example.com';
    M.User := TEmailUser.Create;
    M.User.Name := 'Anna';
    M.User.Email := 'anna@example.com';

    M.PeriodStart := EncodeDate(2026, 1, 1);
    M.PeriodEnd := EncodeDate(2026, 1, 7);

    M.Highlights.Add('You gained +12 new followers');
    M.Highlights.Add('2FA is now available in settings');

    Item := TDigestItem.Create;
    Item.Title := 'Account overview';
    Item.URL := JoinURL(M.BaseURL, 'account');
    M.Items.Add(Item);

    Item := TDigestItem.Create;
    Item.Title := 'Security checklist';
    Item.URL := JoinURL(M.BaseURL, 'help/security');
    M.Items.Add(Item);

    HtmlBody := T.Lookup('digest_html').ExecuteToString(M);
    TextBody := T.Lookup('digest_text').ExecuteToString(M);

    WriteLn('--- DIGEST HTML ---');
    WriteLn(HtmlBody);
    WriteLn;
    WriteLn('--- DIGEST TEXT ---');
    WriteLn(TextBody);
  finally
    M.Free;
  end;
end.
