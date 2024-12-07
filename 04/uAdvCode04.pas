unit uAdvCode04;

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
  line: String;
  count: Integer = 0;
  i,j: Integer;
  u,v: Integer;
  newLine: String = '';
  totalLines: TStringList;
begin
  // Okay, this works but it is way to complex....
  SetLength(newLine, lines[0].Length);
  totalLines := TStringList.Create();
  try
      // Add original
    totalLines.AddStrings(lines);

    // Add vertical
    SetLength(newLine, lines.Count);
    for i := 1 to lines[0].Length do
    begin
      for j := 0 to lines.Count - 1 do
      begin
        newLine[j+1] := lines[j][i];
      end;
      totalLines.Add(newLine);
    end;

    // The diagonals...
    // This is rather interesting as the size varies...
    // Strings are 1-based

    // descending
    // Start at [Y_n-4][1], [Y_n-5][1] .. [0][1], [0][2] .. [0][X_n-3]
    // Part 1
    for i := lines.Count - 4 downto 0 do
    begin
      SetLength(newLine, lines.Count - i);
      for j := 1 to lines.Count - i do
      begin
        u := i-1+j;
        v := j;
        newLine[j] := lines[u][v];
      end;
      totalLines.Add(newLine);
    end;

    // Part 2
    for i := 2 to lines[0].Length - 3 do
    begin
      SetLength(newLine, lines[0].Length + 1 - i);
      for j := 1 to lines[0].Length - i + 1 do
      begin
        u := j-1;
        v := i+j-1;
        newLine[j] := lines[u][v];
      end;
      totalLines.Add(newLine);
    end;

    // ascending
    // Start at [3][1], [4][1] .. [Y_n-1][1], [Y_n-1][2] .. [Y_n-1][X_n-3]
    // Part 1
    for i := 3 to lines.Count - 1 do
    begin
      SetLength(newLine, i+1);
      for j := 1 to i + 1 do
      begin
        u := i+1-j;
        v := j;
        newLine[j] := lines[u][v];
      end;
      totalLines.Add(newLine);
    end;

    // Part 2
    // [Y_n-1][2] .. [Y_n-1][X_n-3]
    for i := 2 to lines[0].Length - 3 do
    begin
      SetLength(newLine, lines[0].Length - i + 1);
      for j := 1 to lines[0].Length - i + 1 do
      begin
        u := lines.Count-j;
        v := i+j-1;
        newLine[j] := lines[u][v];//[0][1];
      end;
      totalLines.Add(newLine);
    end;

    for line in totalLines do
    begin
      i := 0;
      repeat
        i := line.IndexOf('XMAS', i) + 1;
        if i > 0 then
          Inc(count);
      until i = 0;
      i := 0;
      repeat
        i := line.IndexOf('SAMX', i) + 1;
        if i > 0 then
          Inc(count);
      until i = 0;
    end;
    ed_Answer1.Text := IntToStr(count);
  finally
    FreeAndNil(totalLines);
  end;
end;

procedure TFrmAdvCode.part2(lines: TStringList);
var
  count: Integer = 0;
  i, j: Integer;

  function masAscending(): Boolean;
  begin
    case lines[i+1][j-1] of
      'M': Result := lines[i-1][j+1] = 'S';
      'S': Result := lines[i-1][j+1] = 'M';
    else
      Result := False;
    end;
  end;

  function masDescending(): Boolean;
  begin
    case lines[i-1][j-1] of
      'M': Result := lines[i+1][j+1] = 'S';
      'S': Result := lines[i+1][j+1] = 'M';
    else
      Result := False;
    end;
  end;

begin
  // OK. Just find a A in the center part of the grid.
  for i := 1 to lines.Count - 2 do
  begin
    for j := 2 to lines[i].Length - 1 do
    begin
      if lines[i][j] = 'A' then
      begin
        if masAscending() and masDescending() then
          Inc(count);
      end;
    end;
  end;
  ed_Answer2.Text := IntToStr(count);
end;

end.

