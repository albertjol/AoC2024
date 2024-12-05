unit uAdvCode05;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  , Generics.Collections
  , Generics.Defaults
  ;

type

  TIntSet = THashSet<Integer>;
  TIntList = TList<Integer>;

  { TMapComparer }

  TMapComparer = class(TInterfacedObject, IComparer<Integer>)
  public
    constructor Create(map: TDictionary<Integer, TIntSet>);
    function Compare(constref Left, Right: Integer): Integer; overload;
  private
    FMap: TDictionary<Integer, TIntSet>;
  end;

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
    procedure FormDestroy(Sender: TObject);
    procedure part1(lines: TStringList);
    procedure part2();

  private
    FWronglyOrdered: TObjectList<TIntList>;
    FSortMap: TDictionary<Integer, TIntSet>;

  public

  end;

var
  FrmAdvCode: TFrmAdvCode;

implementation

uses
  Math
  ;

{$R *.lfm}

{ TMapComparer }

constructor TMapComparer.Create(map: TDictionary<Integer, TIntSet>);
begin
  FMap := map;
end;

function TMapComparer.Compare(constref Left, Right: Integer): Integer;
var
  sl: TIntSet;
begin
  // This is very simplistic. Lets see if it works...
  // Nope, 7075 is too high...

  Result := EqualsValue;
  if FMap.TryGetValue(Left, sl) then
  begin
    if sl.Contains(Right) then
      Result := LessThanValue
    else if Right = Left then
      Result := EqualsValue
    else
    begin
      // Okay, try reverse:
      if FMap.TryGetValue(Right, sl) then
      begin
        if sl.Contains(Left) then
          Result := GreaterThanValue
        else
          Result := LessThanValue;
      end;
      // This reverse thing did the trick for me ;)
    end;
  end;

end;

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
    part2();
  finally
    FreeAndNil(lines);
  end;
end;

procedure TFrmAdvCode.FormCreate(Sender: TObject);
begin
  FWronglyOrdered := TObjectList<TIntList>.Create();
  FSortMap := TDictionary<Integer, TIntSet>.Create();

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

procedure TFrmAdvCode.FormDestroy(Sender: TObject);
var
  hs: TIntSet;
begin
  for hs in FSortMap.Values do
      hs.Free();
  FreeAndNil(FSortMap);
  FreeAndNil(FWronglyOrdered);
end;

procedure TFrmAdvCode.part1(lines: TStringList);
var
  prevs: TIntSet;
  split: TStringArray;
  str: String;
  i, a, b: Integer;
  sum: Integer = 0;
  hs: TIntSet;
  ok: Boolean;
  wrongOrderedCandidate: TIntList;
begin
  for hs in FSortMap.Values do
    hs.Free();
  FSortMap.Clear();
  try
    // building the orderingmap
    i := 0;
    while lines[i] <> '' do
    begin
      split := lines[i].Split('|');
      a := StrToInt(split[0]);
      b := StrToInt(split[1]);
      if(not FSortMap.ContainsKey(a)) then
        FSortMap.Add(a, TIntSet.Create());
      FSortMap[a].Add(b);
      Inc(i);
    end;

    Inc(i);
    prevs := TIntSet.Create();
    for i := i to lines.Count - 1 do
    begin
      prevs.Clear;
      ok := True;
      split := lines[i].Split(',');
      wrongOrderedCandidate := TIntList.Create();
      for str in split do
      begin
        a := StrToInt(str);
        if(FSortMap.TryGetValue(a, hs)) then
        begin
          for b in hs do
          begin
            if(prevs.Contains(b)) then
            begin
              // Oops, ordering is wrong
              ok := False;
            end;
          end;
        end;
        wrongOrderedCandidate.Add(a);
        prevs.Add(a);
      end;

      if ok then
      begin
        // Find'n sum the middle number
        Inc(sum, StrToInt(split[Floor(Length(split)/2)]));
        FreeAndNil(wrongOrderedCandidate);
      end
      else
        FWronglyOrdered.Add(wrongOrderedCandidate);
    end;

  finally       
    FreeAndNil(prevs);
  end;
  ed_Answer1.Text := IntToStr(sum);
end;

procedure TFrmAdvCode.part2();
var
  sum: Integer = 0;
  sequence: TIntList;
  mapComparer: IComparer<Integer>;
begin
  mapComparer := TMapComparer.Create(FSortMap);
  for sequence in FWronglyOrdered do
  begin
    sequence.Sort(mapComparer);
    Inc(sum, sequence[Floor(sequence.Count/2)]);
  end;
  ed_Answer2.Text := IntToStr(sum);
end;

end.

