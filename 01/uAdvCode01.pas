unit uAdvCode01;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  , Generics.Collections
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
    procedure part12(lines: TStringList);
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
    part12(lines);
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

procedure TFrmAdvCode.part12(lines: TStringList);
var
  line: String;
  split : TStringArray;
  list1, list2: TList<Integer>;
  i, y: Integer;
  sum1: Integer = 0;
  sum2: Integer = 0;
  count: Integer;
begin
  list1 := TList<Integer>.Create();
  list2 := TList<Integer>.Create();
  try
    for line in lines do
    begin
      split := line.Split(' ');
      list1.Add(StrToInt(split[0]));
      // 2 empty strings in between as there are 2 spaces inbetween
      list2.Add(StrToInt(split[3]));
    end;
    list1.Sort();
    list2.Sort();

    for i := 0 to lines.Count - 1 do
    begin
      // Part 1:
      Inc(sum1, Abs(list1[i] - list2[i]));

      // Part 2:
      count := 0;
      for y in list2 do
      begin
        if y = list1[i] then
        begin
          Inc(count);
        end
        else if count > 0 then
        begin
          // So we counted those numbers in the sorted list, no need to continue
          break;
        end;
      end;
      Inc(sum2, list1[i] * count);
    end;
    ed_Answer1.Text := IntToStr(sum1);
    ed_Answer2.Text := IntToStr(sum2);

  finally
    FreeAndNil(list1);
    FreeAndNil(list2);
  end;
end;

end.

