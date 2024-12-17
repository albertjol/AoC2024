unit uAdvCode13;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  ;

type

  { TFrmAdvCode }

  TFrmAdvCode = class(TForm)
    btn_Start: TButton;
    ed_Answer1: TEdit;
    ed_Answer2: TEdit;
    ed_Filename: TFileNameEdit;
    lbl_duration1: TLabel;
    lbl_duration2: TLabel;
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

uses
  Math
  ;

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

  function IsInteger(val: Double): Boolean;
  const
    epsilon = 0.00001;
  var
    residu: Double;
  begin
    residu := val mod 1;
    Result := (residu < epsilon) or (residu > (1 - epsilon));
  end;

var
  count: Integer = 0;
  line: String;
  i, j, k, l, m, n: Integer;
  a, b: double;
  split: TStringArray;
begin
  i:=0;j:=0;k:=0;l:=0;m:=0;n:=0;

  {
    Button A: X+94, Y+34
    Button B: X+22, Y+67
    Prize: X=8400, Y=5400

    94a + 22b = 8400
    34a + 67b = 5400

    ma + ib = j
    na + kb = l

    -ib+j    -kb+l
    ----- =  -----
      m        n
    b = (l-j*n/m) / (-i*n/m+k)
    b=40
    Substitute in rule 1 -> a=80
    ma + ib = j
    ma = j - ib
    a = (j - ib) / m
  }
  for line in lines do
  begin
    // If the last block is valid, this will fail as it will not get processed.
    // because the empty line in the input  is not read by the LoadFromFile function
    // because there is no LF character in the last line... In my input, this
    // was fine for part 1 as the last block had no solution.
    // This took me one day finding it
    split := line.Split([':', ',', '+', '=', ' ']);
    if (split[0] = '') then
    begin
      // calculate
      if (m/i) = (n/k) then
        MessageDlg('Lines are parallel. solution is invalid', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0, mbOk);
      b := (l-j*n/m) / (-i*n/m+k);
      a := (j - (i * b)) / m;
      if (a >= 0) and (b >= 0) and IsInteger(a) and IsInteger(b) then
        count += Round(b + (3 * a));

    end
    else if split[1] = 'A' then
    begin
      m := StrToInt(split[4]);
      n := StrToInt(split[7]);
    end
    else if split[1] = 'B' then
    begin
      i := StrToInt(split[4]);
      k := StrToInt(split[7]);
    end
    else if split[0] = 'Prize' then
    begin
      j := StrToInt(split[3]);
      l := StrToInt(split[6]);
    end;

    {
        split[0] @$00000000016BFF18 = 'Button'
        split[1] @$00000000016C0018 = 'A'
        split[2] @nil = ''
        split[3] @$00000000016C0118 = 'X'
        split[4] @$00000000016C0218 = '94'
        split[5] @nil = ''
        split[6] @$00000000016C0318 = 'Y'
        split[7] @$00000000016C0418 = '34'

        split[0] @$00000000016C0418 = 'Prize'
        split[1] @nil = ''
        split[2] @$00000000016BFF18 = 'X'
        split[3] @$00000000016C0518 = '8400'
        split[4] @nil = ''
        split[5] @$00000000016C0118 = 'Y'
        split[6] @$00000000016C0018 = '5400'
    }

  end;
  ed_Answer1.Text := IntToStr(count);
end;

procedure TFrmAdvCode.part2(lines: TStringList);
// Is equal to part1 except for bigger ints and a smaller epsilon value
// for checking wheter we have an int value
  function IsInteger(val: Double): Boolean;
  const
    epsilon = 0.001;
  var
    residu: Double;
  begin
    residu := val mod 1;
    Result := (residu < epsilon) or (residu > (1 - epsilon));
  end;

var
  count: Int64 = 0;
  line: String;
  i, j, k, l, m, n: Int64;
  a, b: double;
  split: TStringArray;
begin
  i:=0;j:=0;k:=0;l:=0;m:=0;n:=0;

  {
    Button A: X+94, Y+34
    Button B: X+22, Y+67
    Prize: X=8400, Y=5400

    94a + 22b = 8400
    34a + 67b = 5400

    ma + ib = j
    na + kb = l

    -ib+j    -kb+l
    ----- =  -----
      m        n
    b = (l-j*n/m) / (-i*n/m+k)
    b=40
    Substitute in rule 1 -> a=80
    ma + ib = j
    ma = j - ib
    a = (j - ib) / m
  }

  for line in lines do
  begin
    // Skip empty lines
    if line = '' then continue;

    split := line.Split([':', ',', '+', '=', ' ']);
    if split[1] = 'A' then
    begin
      m := StrToInt(split[4]);
      n := StrToInt(split[7]);
    end
    else if split[1] = 'B' then
    begin
      i := StrToInt(split[4]);
      k := StrToInt(split[7]);
    end
    else if split[0] = 'Prize' then
    begin
      j := StrToInt(split[3]) + 10000000000000;
      l := StrToInt(split[6]) + 10000000000000;

      // now we can calculate
      if (m/i) = (n/k) then
        MessageDlg('Lines are parallel. solution is invalid', TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0, mbOk);

      b := (l-j*n/m) / (-i*n/m+k);
      a := (j - (i * b)) / m;
      if (a >= 0) and (b >= 0) and IsInteger(a) and IsInteger(b) then
        count += Round(b + (3 * a));
    end;

    {
        split[0] @$00000000016BFF18 = 'Button'
        split[1] @$00000000016C0018 = 'A'
        split[2] @nil = ''
        split[3] @$00000000016C0118 = 'X'
        split[4] @$00000000016C0218 = '94'
        split[5] @nil = ''
        split[6] @$00000000016C0318 = 'Y'
        split[7] @$00000000016C0418 = '34'

        split[0] @$00000000016C0418 = 'Prize'
        split[1] @nil = ''
        split[2] @$00000000016BFF18 = 'X'
        split[3] @$00000000016C0518 = '8400'
        split[4] @nil = ''
        split[5] @$00000000016C0118 = 'Y'
        split[6] @$00000000016C0018 = '5400'
    }

  end;
  ed_Answer2.Text := IntToStr(count);
end;

end.

