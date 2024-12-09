unit uAdvCode08;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  , Generics.Collections
  ;

type

  TPointlist = TList<TPoint>;
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
  i, j: Integer;
  map: TObjectDictionary<Char, TPointlist>;
  antiNodes: THashSet<TPoint>;
  freq: Char;
  diff, pt: TPoint;
  cnt: SizeInt;

  procedure InitMap;
  var
    c: char;
  begin
    map := TObjectDictionary<Char, TPointlist>.Create();
    for c in 'qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890' do
      map.Add(c, TPointlist.Create());
  end;

  procedure FillMap;
  var
    i, j: Integer;
  begin
    for i := 0 to lines.Count - 1 do
    begin
      for j := 1 to lines[i].Length do
      begin
        if (lines[i][j] <> '.') then
        begin
          map[lines[i][j]].Add(Point(j,i));
        end;
      end;
    end;
  end;

  function IsInBounds(): Boolean;
  begin
    Result := (
      (0 < pt.X) and (pt.X <= lines[0].Length) and
      (0 <= pt.Y) and (pt.Y < lines.Count)
      );
  end;

begin
  antiNodes := THashSet<TPoint>.Create();
  InitMap();
  try
    FillMap();

    for freq in map.Keys do
    begin
      cnt := map[freq].Count;
      for i := 0 to cnt - 1 do
      begin
        for j := 0 to cnt - 1 do
        begin
          if i = j then continue;
          diff := map[freq][i] - map[freq][j];
          pt := map[freq][i] + diff;
          if IsInBounds() then
          begin
            if(not antiNodes.Contains(pt)) then
              antiNodes.Add(pt);
          end;
        end;
      end;
    end;
    ed_Answer1.Text := IntToStr(antiNodes.Count);
  finally
    FreeAndNil(map);
    FreeAndNil(antiNodes);
  end;
end;

procedure TFrmAdvCode.part2(lines: TStringList);
var
  i, j: Integer;
  map: TObjectDictionary<Char, TPointlist>;
  antiNodes: THashSet<TPoint>;
  freq: Char;
  diff, pt: TPoint;
  cnt: SizeInt;

  procedure InitMap;
  var
    c: char;
  begin
    map := TObjectDictionary<Char, TPointlist>.Create();
    for c in 'qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890' do
      map.Add(c, TPointlist.Create());
  end;

  procedure FillMap;
  var
    i, j: Integer;
  begin
    for i := 0 to lines.Count - 1 do
    begin
      for j := 1 to lines[i].Length do
      begin
        if (lines[i][j] <> '.') then
        begin
          map[lines[i][j]].Add(Point(j,i));
        end;
      end;
    end;
  end;

  function IsInBounds(): Boolean;
  begin
    Result := (
      (0 < pt.X) and (pt.X <= lines[0].Length) and
      (0 <= pt.Y) and (pt.Y < lines.Count)
      );
  end;

begin
  antiNodes := THashSet<TPoint>.Create();
  InitMap();
  try
    FillMap();

    for freq in map.Keys do
    begin
      cnt := map[freq].Count;
      for i := 0 to cnt - 1 do
      begin
        // Every antenna is also an antinode.
        antinodes.Add(map[freq][i]);
        for j := 0 to cnt - 1 do
        begin
          if i = j then continue;
          diff := map[freq][i] - map[freq][j];
          pt := map[freq][i];
          repeat
            pt := pt + diff;
            if IsInBounds() then
            begin
              if(not antiNodes.Contains(pt)) then
                antiNodes.Add(pt);
            end;
          until not IsInBounds();
        end;
      end;
    end;
    ed_Answer2.Text := IntToStr(antiNodes.Count);
  finally
    FreeAndNil(map);
    FreeAndNil(antiNodes);
  end;
end;

end.

