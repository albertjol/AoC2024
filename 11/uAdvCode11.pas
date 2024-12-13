unit uAdvCode11;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  , Generics.Collections
  ;

type

  { TFrmAdvCode }

  { TLinkedIntList }

  { TLinkedQWordList }

  TLinkedQWordList = class(TObject)
  public
    Value: QWord;
    Next: TLinkedQWordList;

    constructor Create(AValue: QWord);
    function InsertNProceed(AValue: QWord): TLinkedQWordList;
  end;

  TFrmAdvCode = class(TForm)
    btn_Start: TButton;
    ed_Answer1: TEdit;
    ed_Answer2: TEdit;
    ed_Filename: TFileNameEdit;
    lbl_i: TLabel;
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

{$R *.lfm}

{ TLinkedQWordList }

constructor TLinkedQWordList.Create(AValue: QWord);
begin
  Next := nil;
  Value := AValue;
end;

function TLinkedQWordList.InsertNProceed(AValue: QWord): TLinkedQWordList;
var
  tmp: TLinkedQWordList;
begin
  tmp := Next;
  Next := TLinkedQWordList.Create(AValue);
  Next.Next := tmp;
  Result := Next;
end;

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
  stones: TList<QWord>;

  procedure ParseStones();
  var
    s: String;
  begin
    for s in lines[0].Split(' ') do
      stones.Add(StrToQWord(s));
  end;

var
  i,j: Integer;
  stoneStr : String;
  half: SizeInt;
begin
  stones := TList<QWord>.Create();
  try
    ParseStones();
    for i := 1 to 25 do
    begin
      j := 0;
      while j < stones.Count do
      begin
        {*
    If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
    If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
    If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.
        *}
        if stones[j] = 0 then
          stones[j] := 1
        else
        begin
          stoneStr := IntToStr(stones[j]);
          if (stoneStr.Length mod 2) = 0 then
          begin
            half := stoneStr.Length div 2;
            stones.Insert(j+1, StrToQWord(stoneStr.Substring(half, half)));
            stones[j] := StrToQWord(stoneStr.Substring(0, half));
            Inc(j);
          end
          else
            stones[j] := stones[j] * 2024;
        end;
        Inc(j);
      end;
    end;
    ed_Answer1.Text := IntToStr(stones.Count);
  finally
    FreeAndNil(stones);
  end;

end;

procedure TFrmAdvCode.part2(lines: TStringList);
var
  stonemap, newStonemap: TDictionary<QWord, QWord>;

  procedure ParseStones();
  var
    i: Integer;
    split: TStringArray;
  begin
    split := lines[0].Split(' ');
    for i := 0 to Length(split) - 1 do
      stonemap.Add(StrToQWord(split[i]), 1);
  end;

  procedure AddOrIncrease(map: TDictionary<QWord, QWord>; key: QWord; value: QWord);
  begin
    if map.ContainsKey(key) then
      map[key] := map[key] + value
    else
      map.Add(key, value);
  end;

  procedure ReplaceStone(stone, newValue: QWord);
  begin
    AddOrIncrease(newStonemap, newValue, stonemap[stone]);
  end;

  procedure SplitStone(stone: QWord; stoneStr: String);
  var
    half: SizeInt;
    s1, s2: QWord;
  begin
    half := stoneStr.Length div 2;
    s1 := StrToQWord(stoneStr.Substring(0, half));
    s2 := StrToQWord(stoneStr.Substring(half, half));
    if s1 = s2 then
      AddOrIncrease(newStonemap, s1, stonemap[stone] * 2)
    else
    begin
      AddOrIncrease(newStonemap, s1, stonemap[stone]);
      AddOrIncrease(newStonemap, s2, stonemap[stone]);
    end;
  end;

var
  i,
  stone: QWord;
  stoneStr : String;
  count: QWord = 0;
begin
  stonemap := TDictionary<QWord, QWord>.Create();
  newStonemap := TDictionary<QWord, QWord>.Create();
  try
    ParseStones();

    for i := 1 to 75 do
    begin
      lbl_i.Caption := IntToStr(i);
      Application.ProcessMessages();

      newStonemap.Clear;

      for stone in stonemap.Keys do
      begin
        (*
    If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
    If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
    If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.
        *)
        if stone = 0 then
        begin
          ReplaceStone(stone, 1);
        end
        else
        begin
          stoneStr := IntToStr(stone);
          if (stoneStr.Length mod 2) = 0 then
          begin
            SplitStone(stone, stoneStr);
          end
          else
            ReplaceStone(stone, stone * 2024);
        end;
      end;

      stonemap.Clear;
      for stone in newStonemap.Keys do
        stonemap.Add(stone, newStonemap[stone]);

    end;
    for stone in stonemap.Values do
      Inc(count, stone);
    ed_Answer2.Text := IntToStr(count);
  finally
    FreeAndNil(stonemap);
    FreeAndNil(newStonemap);
  end;

end;

end.

