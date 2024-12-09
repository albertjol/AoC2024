unit uAdvCode06;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  , Generics.Collections
  ;

type

  { TFrmAdvCode }

  TIntList = TList<Integer>;

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
    procedure FormDestroy(Sender: TObject);
    procedure part1(lines: TStringList);
    procedure part2(lines: TStringList);

  private
    FTrackpoints: TList<TPoint>;
  public

  end;

  eState = (MovingNorth, MovingEast, MovingSouth, MovingWest, Untouched);
  TStateList = TList<eState>;

var
  FrmAdvCode: TFrmAdvCode;

implementation

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

  FTrackpoints := TList<TPoint>.Create();
end;

procedure TFrmAdvCode.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTrackpoints);
end;

procedure TFrmAdvCode.part1(lines: TStringList);
var
  pt: TPoint;
  state : eState;
  gone: Boolean = False;
  count : Integer;
  track: TObjectList<TIntList>;

  function findGuard(): TPoint;
  var
    y, x: Integer;
    c: char;
  begin
    Result := Point(-1,-1);
    for y := 0 to lines.Count - 1 do
    begin
      for x := 1 to lines[y].Length do
      begin
        c := lines[y][x];
        if (c = '^') or (c = 'v') or (c = 'V') or (c = '<') or (c = '>') then
        begin
          Result.X := x;
          Result.Y := y;
          track[y][x] := 1;
          case c of
            '^'        : state := MovingNorth;
            '>'        : state := MovingEast;
            'V', 'v'   : state := MovingSouth;
            '<'        : state := MovingWest;
          end;
        end
      end;
    end;
  end;

  function isGone(point: TPoint): Boolean;
  begin
    Result := (
       (1 > point.X) or (point.X > lines[0].Length) or
       (0 > point.Y) or (point.Y >= lines.Count));
  end;

  function isNextObstacle(): Boolean;
  var
    nextPt : TPoint;
  begin
    nextPt := pt;
    case state of
      MovingNorth: Dec(nextPt.Y);
      MovingEast:  Inc(nextPt.X);
      MovingSouth: Inc(nextPt.Y);
      MovingWest:  Dec(nextPt.X);
    end;
    Result := ((not isGone(nextPt)) and (lines[nextPt.Y][nextPt.X] = '#'));
  end;

  procedure changeDirection();
  begin
    case state of
      MovingNorth: state := MovingEast;
      MovingEast:  state := MovingSouth;
      MovingSouth: state := MovingWest;
      MovingWest:  state := MovingNorth;
    end;
  end;

  function countXes(): Integer;
  var
    y, x: Integer;
  begin
    Result := 0;
    FTrackpoints.Clear();
    for y := 0 to track.Count - 1 do
    begin
      for x := 0 to track[y].Count - 1 do
      begin
        FTrackpoints.Add(Point(x,y));
        Inc(Result, track[y][x]);
      end;
    end;
  end;

  function createTrack: TObjectList<TIntList>;
  var
    x,y: Integer;
  begin
    Result := TObjectList<TIntList>.Create();
    for y := 0 to lines.Count - 1 do
    begin
      Result.Add(TIntList.Create);
      for x := 0 to lines[y].Length do
        Result[y].Add(0);
    end;
  end;

begin
  track := createTrack();
  try
    pt := findGuard();

    while not gone do
    begin
      if isNextObstacle() then
        changeDirection();
      case state of
        MovingNorth: Dec(pt.Y);
        MovingEast:  Inc(pt.X);
        MovingSouth: Inc(pt.Y);
        MovingWest:  Dec(pt.X);
      end;
      if isGone(pt) then
        gone := True
      else
      begin
        track[pt.Y][pt.X] := 1;
      end;
    end;

    count := countXes();

    ed_Answer1.Text := IntToStr(count);
  finally
    FreeAndNil(track);
  end;
end;

procedure TFrmAdvCode.part2(lines: TStringList);
var
  pt: TPoint;
  state : eState;
  gone: Boolean = False;
  count : Integer = 0;
  track: TObjectList<TStateList>;
  obstacle : TPoint;

  function findGuard(): TPoint;
  var
    y, x: Integer;
    c: char;
  begin
    Result := Point(-1,-1);
    for y := 0 to lines.Count - 1 do
    begin
      for x := 1 to lines[y].Length do
      begin
        c := lines[y][x];
        if (c = '^') or (c = 'v') or (c = 'V') or (c = '<') or (c = '>') then
        begin
          Result.X := x;
          Result.Y := y;
          case c of
            '^'        : state := MovingNorth;
            '>'        : state := MovingEast;
            'V', 'v'   : state := MovingSouth;
            '<'        : state := MovingWest;
          end;
          track[y][x] := state;
        end
      end;
    end;
  end;

  function isGone(point: TPoint): Boolean;
  begin
    Result := (
       (1 > point.X) or (point.X > lines[0].Length) or
       (0 > point.Y) or (point.Y >= lines.Count));
  end;

  function isNextObstacle(): Boolean;
  var
    nextPt : TPoint;
  begin
    nextPt := pt;
    case state of
      MovingNorth: Dec(nextPt.Y);
      MovingEast:  Inc(nextPt.X);
      MovingSouth: Inc(nextPt.Y);
      MovingWest:  Dec(nextPt.X);
    end;
    Result := ((not isGone(nextPt)) and ((lines[nextPt.Y][nextPt.X] = '#')) or (nextPt = obstacle));
  end;

  procedure changeDirection();
  begin
    case state of
      MovingNorth: state := MovingEast;
      MovingEast:  state := MovingSouth;
      MovingSouth: state := MovingWest;
      MovingWest:  state := MovingNorth;
    end;
  end;

  function createTrack: TObjectList<TStateList>;
  var
    x,y: Integer;
  begin
    Result := TObjectList<TStateList>.Create();
    for y := 0 to lines.Count - 1 do
    begin
      Result.Add(TStateList.Create);
      for x := 0 to lines[y].Length do
        Result[y].Add(Untouched);
    end;
  end;
begin
  // 1909. Was 12 seconds in release, 73 seconds in debug mode
  // Looping through trackpoints instead of every x/y does not change anything
  // It takes exactly as long... And this is funny as you expect that you have 3
  // times less points to check (5161 vs 17030)
  for obstacle in FTrackpoints do
  begin
    track := createTrack();
    try
      pt := findGuard();
      if(obstacle = pt) then
        // cannot be at starting position.
        continue;
      gone := False;
      while not gone do
      begin
        if isNextObstacle() then
        begin
          changeDirection();
          // If there is an obstacle right next to it, change again (so turn around)
          if isNextObstacle() then
             changeDirection();
        end;
        case state of
          MovingNorth: Dec(pt.Y);
          MovingEast:  Inc(pt.X);
          MovingSouth: Inc(pt.Y);
          MovingWest:  Dec(pt.X);
        end;
        if isGone(pt) then
          gone := True
        else
        begin
          if (track[pt.Y][pt.X] = state) then
          begin
            // We're in a loop, cool.
            Inc(count);
            break;
          end;

          track[pt.Y][pt.X] := state;
        end;
      end;
      // So we are gone. this doesnt work. Continue to the next one
    finally
      FreeAndNil(track);
    end;
  end;

  ed_Answer2.Text := IntToStr(count);
end;

end.

