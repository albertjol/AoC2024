unit uAdvCode07;

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
  sum: QWord = 0;
  res: QWord;
  tryResult: QWord;
  terms: TList<QWord>;
  nrCombi: integer;
  i,j: Integer;
  term: String;
  split: TStringArray;
  line: String;
begin
  terms := TList<QWord>.Create();
  try
  for line in lines do
  begin
    terms.Clear();
    split := line.Split(':');
    res := StrToInt64(split[0]);
    split := TrimLeft(split[1]).Split(' ');
    for term in split do
      terms.Add(StrToInt64(term));
    // Why is there no integer power in FPC?? It's all floats...
    // Well, then i just bitshift...
    nrCombi := 1 shl terms.Count; // 2^#combi's
    for i := 0 to nrCombi do
    begin
      // OK. Now: i tells me in binary whether it is add or mul...: 0 = add, 1 = mul
      tryResult := terms[0];
      for j := 1 to terms.Count - 1 do
      begin
        if (1 and (i shr j)) = 1 then
          tryResult := tryResult * terms[j]
        else
          tryResult := tryResult + terms[j];
      end;
      if tryResult = res then
      begin
        Inc(sum, res);
        break;
      end;
    end;

  end;
  finally
    FreeAndNil(terms);
  end;
  ed_Answer1.Text := IntToStr(sum);
end;

procedure TFrmAdvCode.part2(lines: TStringList);
var
  count: Integer = 0;
begin
  ed_Answer2.Text := IntToStr(count);
end;

end.

