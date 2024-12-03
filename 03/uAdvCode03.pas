unit uAdvCode03;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  , uregexpr
  ;

type

  { TFrmAdvCode }

  TFrmAdvCode = class(TForm)
    btn_Start: TButton;
    ed_Answer1: TEdit;
    ed_Answer2: TEdit;
    ed_Filename: TFileNameEdit;
    lbl_Answer2: TLabel;
    lbl_Filename: TLabel;
    lbl_Answer1: TLabel;

    procedure btn_StartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure part1(lines: TStringList);
    procedure part2(lines: TStringList);

  private

  public

  end;

var
  FrmAdvCode: TFrmAdvCode;

implementation

{$R *.lfm}

{ TFrmAdvCode }

procedure TFrmAdvCode.btn_StartClick(Sender: TObject);
var
  lines: TStringList;
begin
  with TStringList.Create() do
  begin
    try
      Add(ed_Filename.Text);
      SaveToFile('settings.txt');
    finally
      Free();
    end;
  end;
  lines := TStringList.Create();
  try
    lines.LoadFromFile(ed_Filename.Text);
    part1(lines);
    part2(lines);
  finally
    FreeAndNil(lines);
  end;
end;

procedure TFrmAdvCode.FormCreate(Sender: TObject);
begin
  if FileExists('settings.txt') then
  begin
    with TStringList.Create() do
    begin
      try
        LoadFromFile('settings.txt');
        if(Count > 0) then
        begin
          ed_Filename.Text := Strings[0];
        end;
      finally
        Free();
      end;
    end;
  end;
end;

procedure TFrmAdvCode.part1(lines: TStringList);
var
  re: TRegExpr;
  a,b: Integer;
  sum: Integer = 0;
begin
  re := TRegExpr.Create('mul\((\d+),(\d+)\)');
  re.Exec(lines[0]);
  a := StrToInt(re.Match[1]);
  b := StrToInt(re.Match[2]);
  Inc(sum,a*b);
  while re.ExecNext() do
  begin
    a := StrToInt(re.Match[1]);
    b := StrToInt(re.Match[2]);
    Inc(sum,a*b);
  end;
  FreeAndNil(re);

  ed_Answer1.Text := IntToStr(sum);
end;

procedure TFrmAdvCode.part2(lines: TStringList);
var
  re: TRegExpr;
  a,b: Integer;
  filteredString: String = '';
  sum: Integer = 0;
begin
  // Greedy regex:
  re := TRegExpr.Create('don''t\(\).*?do\(\)');
  filteredString := re.Replace(lines[0], '');
  FreeAndNil(re);

  // Below is same as part 1...
  re := TRegExpr.Create('mul\((\d+),(\d+)\)');
  re.Exec(filteredString);
  a := StrToInt(re.Match[1]);
  b := StrToInt(re.Match[2]);
  Inc(sum,a*b);
  while re.ExecNext() do
  begin
    a := StrToInt(re.Match[1]);
    b := StrToInt(re.Match[2]);
    Inc(sum,a*b);
  end;
  FreeAndNil(re);

  ed_Answer2.Text := IntToStr(sum);
end;

end.

