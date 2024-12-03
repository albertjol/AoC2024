unit uAdvCode02;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  , Generics.Collections
  ;

type

  TIntlist = TList<Integer>;

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
  line, numStr: String;
  countUnsafe: Integer = 0;
  sign: Integer;
  prev: Integer;
  num: Integer;
  diff: Integer;
begin
  for line in lines do
  begin
    sign := 0;
    prev := -1;
    for numStr in line.Split(' ') do
    begin
      num := StrToInt(numStr);
      if (prev >= 0) then
      begin
        diff := num - prev;

        if (sign = 0) then
        begin
          sign := diff;
        end;

        if ((Abs(diff) = 0) or (Abs(diff) > 3) or (sign * diff < 0)) then
        begin
          Inc(countUnsafe);
          break;
        end;
      end;
      prev := num;
    end;
  end;
  ed_Answer1.Text := IntToStr(lines.Count - countUnsafe);
end;

procedure TFrmAdvCode.part2(lines: TStringList);
  function LineIsUnsafe(line: TList<Integer>): Boolean;
  var
    sign: Integer;
    prev: Integer;
    num: Integer;
    diff: Integer;
  begin
    Result := False;
    sign := 0;
    prev := -1;
    for num in line do
    begin
      if (prev >= 0) then
      begin
        diff := num - prev;

        if (sign = 0) then
        begin
          sign := diff;
        end;

        if ((Abs(diff) = 0) or (Abs(diff) > 3) or (sign * diff < 0)) then
        begin
          Result := True;
          break;
        end;
      end;
      prev := num;
    end;
  end;

var
  line: String;
  split: TStringArray;
  countSafe: Integer = 0;
  dl: TIntlist;
  dampenedLines: TObjectList<TIntlist>;

  procedure MakeDampenedLines(splittedLine: TStringArray);
  var
    i, j: Integer;
    newLine: TIntlist;
  begin
    // The original line is not relevant anymore.
    dampenedLines.Clear();
    for i := 0 to Length(splittedLine) - 1 do
    begin
      newLine := TIntlist.Create();
      dampenedLines.Add(newLine);
      for j := 0 to Length(splittedLine) - 1 do
      begin
        if i <> j then
          // Skip one of the elements.
          newline.Add(StrToInt(splittedLine[j]));
      end;
    end;
  end;

begin
  dampenedLines := TObjectList<TIntlist>.Create();
  try
    for line in lines do
    begin
      split := line.Split(' ');
      MakeDampenedLines(split);
      for dl in dampenedLines do
      begin
        if (not LineIsUnsafe(dl)) then
        begin
          Inc(countSafe);
          break;
        end;
      end;
    end;
  finally
    FreeAndNil(dampenedLines);
  end;
  ed_Answer2.Text := IntToStr(countSafe);
end;

end.

