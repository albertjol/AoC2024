unit uAdvCode14;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  , Generics.Collections
  ;

type

  { TFrmAdvCode }
  TGrid = array of array of TList<TPoint>;

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

  procedure PrintGrid(grid: TGrid);
  var
    x,y: Integer;
  begin
    for y := 0 to Length(grid) - 1 do
    begin
      for x := 0 to Length(grid[y]) - 1 do
        Write(grid[y,x].Count);
      WriteLn();
    end;


  end;

var
  h,w : Integer;
  x,y,vx,vy,xn,yn : Integer;
  r: TPoint;
  count: Integer = 0;
  mul: Integer = 1;
  line: String;
  grid, newGrid: TGrid;
  split: TStringArray;
begin
  if String(ed_Filename.Text).EndsWith('input.txt') then
  begin
    h := 103;
    w := 101;
  end
  else
  begin
    h := 7;
    w := 11;
  end;
  SetLength(grid, h, w);
  SetLength(newGrid, h, w);
  for y := 0 to Length(grid) - 1 do
    for x := 0 to Length(grid[y]) - 1 do
    begin
      grid[y,x] := TList<TPoint>.Create();
      newGrid[y,x] := TList<TPoint>.Create();
    end;
  try
    for line in lines do
    begin
      split := line.Split(['=',',',' ']);
      // 1, 2, 4, 5 -> x, y, vx, vy
      x := StrToInt(split[1]);
      y := StrToInt(split[2]);
      vx := StrToInt(split[4]);
      vy := StrToInt(split[5]);
      grid[y,x].Add(Point(vx,vy));
    end;

    for y := 0 to Length(grid) - 1 do
      for x := 0 to Length(grid[y]) - 1 do
        for r in grid[y,x] do
        begin
          xn := (x + 100*r.X) mod w;
          yn := (y + 100*r.Y) mod h;
          // workaround for mod returning negative values:
          if xn < 0 then xn += w;
          if yn < 0 then yn += h;
          newGrid[yn,xn].Add(r);
        end;
    //PrintGrid(grid);
    //WriteLn();
    //PrintGrid(newGrid);
    //WriteLn();

    //q1
    count := 0;
    for y := 0 to Length(newGrid) div 2 - 1 do
      for x := 0 to Length(newGrid[y]) div 2 - 1 do
        count += newGrid[y,x].Count;
    mul *= count;

    //q2
    count := 0;
    for y := Length(newGrid) div 2 + 1 to Length(newGrid) - 1 do
      for x := 0 to Length(newGrid[y]) div 2 - 1 do
        count += newGrid[y,x].Count;
    mul *= count;

    //q3
    count := 0;
    for y := Length(newGrid) div 2 + 1 to Length(newGrid) - 1 do
      for x := Length(newGrid[y]) div 2 + 1 to Length(newGrid[y]) - 1 do
        count += newGrid[y,x].Count;
    mul *= count;

    //q4
    count := 0;
    for y := 0 to Length(newGrid) div 2 - 1 do
      for x := Length(newGrid[y]) div 2 + 1 to Length(newGrid[y]) - 1 do
        count += newGrid[y,x].Count;
    mul *= count;

    ed_Answer1.Text := IntToStr(mul);

  finally
    for y := 0 to Length(grid) - 1 do
      for x := 0 to Length(grid[y]) - 1 do
      begin
        grid[y,x].Free;
        newGrid[y,x].Free;
      end;
  end;
end;

procedure TFrmAdvCode.part2(lines: TStringList);
var
  count: Integer = 0;
begin
  ed_Answer2.Text := IntToStr(count);
end;

end.

