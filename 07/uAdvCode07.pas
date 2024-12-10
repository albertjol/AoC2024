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
    lbl_duration1: TLabel;
    lbl_duration2: TLabel;

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

uses
  Math;

{$R *.lfm}

{ TFrmAdvCode }

procedure TFrmAdvCode.btn_StartClick(Sender: TObject);

  function FormatSeparated(number: QWord): String;
  var
    i, j: SizeInt;
  begin
    Result := IntToStr(number);
    j := 0;
    for i := Result.Length downto 1 do
    begin
      if (j > 0) and ((j mod 3) = 0) then
        Result.Insert(i, ' ');
      Inc(j);
    end;
  end;

var
  lines: TStringList;
  ms: QWord;
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
    ms := GetTickCount64();
    part1(lines);
    lbl_duration1.Caption := 'Duration of part 1 (ms): ' + FormatSeparated(GetTickCount64() - ms);
    Application.ProcessMessages();
    ms := GetTickCount64();
    part2(lines);
    lbl_duration2.Caption := 'Duration of part 2 (ms): ' + FormatSeparated(GetTickCount64() - ms);
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
  sum: QWord = 0;
  res: QWord;
  tryResult: QWord;
  terms: TList<QWord>;
  nrCombi: integer;
  i,j, choice: Integer;
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

    nrCombi := Floor(IntPower(3, terms.Count));
    for i := 0 to nrCombi do
    begin
      tryResult := terms[0];
      for j := 1 to terms.Count - 1 do
      begin
        choice := ((i div Floor(IntPower(3, j-1))) mod 3);
        case choice of
          0: tryResult := tryResult + terms[j];
          1: tryResult := tryResult * terms[j];
          2: tryResult := StrToInt64(IntToStr(tryResult) + IntToStr(terms[j]));
        end;
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
  ed_Answer2.Text := IntToStr(sum);
end;

end.

