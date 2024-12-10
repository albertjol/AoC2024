unit uAdvCode09;

{$mode delphi}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls
  , Generics.Collections
  ;

type

  TFile = record
    Id, Size: Integer;
  end;

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

  function ParseDiskmap: TList<Integer>;
  var
    i,j: Integer;
  begin
    Result := TList<Integer>.Create();
    for i := 1 to lines[0].Length do
    begin
      if (i mod 2) = 0 then // empty
        for j := 1 to StrToInt(lines[0][i]) do
          Result.Add(-1)
      else
        for j := 1 to StrToInt(lines[0][i]) do
          Result.Add(i div 2);
    end;
  end;

  procedure DefragmentDisk(diskmap: TList<Integer>);
  var
    indexGap: Integer = 0;
    indexFile: Integer;
  begin
    indexFile := diskmap.Count - 1;
    while indexGap < indexFile do
    begin
      while diskmap[indexFile] >= 0 do
      begin
        while diskmap[indexGap] < 0 do
        begin
          diskmap[indexGap] := diskmap[indexFile];
          diskmap[indexFile] := -1;
        end;
        Inc(indexGap);
      end;
      Dec(indexFile);
    end;
  end;

  function CalcChecksum(diskmap: TList<Integer>): QWord;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to diskmap.Count do
    begin
      if diskmap[i] < 0 then
        break;
      Inc(Result, i * diskmap[i]);
    end;
  end;

  var
    diskmap: TList<Integer>;
    checkSum: QWord;

begin
  diskmap := ParseDiskmap();
  try
    DefragmentDisk(diskmap);
    checkSum := CalcChecksum(diskmap);
  finally
    FreeAndNil(diskmap);
  end;

  ed_Answer1.Text := IntToStr(checkSum);
end;

procedure TFrmAdvCode.part2(lines: TStringList);

  function ExpandDiskmap(diskmap: TList<TFile>): TList<Integer>;
  var
    i,j: Integer;
  begin
    Result := TList<Integer>.Create();
    for i := 0 to diskmap.Count - 1 do
    begin
      for j := 1 to diskmap[i].Size do
        Result.Add(diskmap[i].Id)
    end;
  end;

  function ParseDiskmapPrimitive: TList<TFile>;
  var
    i: Integer;
    newFile: TFile;
  begin
    Result := TList<TFile>.Create();
    for i := 1 to lines[0].Length do
    begin
      newFile.Size := StrToInt(lines[0][i]);
      if (i mod 2) = 0 then // empty
        newFile.Id := -1
      else
        newFile.Id := i div 2;
      Result.Add(newFile);
    end;
  end;

  procedure DefragmentDisk(diskmap: TList<TFile>);
    procedure IncSize(diskmap: TList<TFile>; index: Integer; count: Integer);
    var
      tmp: TFile;
    begin
      // WHY?????
      tmp := diskmap[index];
      tmp.Size := tmp.Size + count;
      diskmap[index] := tmp;
    end;

  var
    indexGap: Integer = 0;
    indexFile: Integer;
    emptyRecord: TFile;

  begin
    emptyRecord.Id := -1;
    emptyRecord.Size := 0;

    indexFile := diskmap.Count - 1;
    while indexFile >= 0 do
    begin
      indexGap := 1;
      while indexGap < indexFile do
      begin
        if diskmap[indexGap].Size >= diskmap[indexFile].Size then
        begin
          //it fits!

          IncSize(diskmap, indexGap, -diskmap[indexFile].Size);
          if indexFile = (diskmap.Count - 1) then
            IncSize(diskmap, indexFile - 1, diskmap[indexFile].Size)
          else
          begin
            IncSize(diskmap, indexFile - 1, diskmap[indexFile].Size + diskmap[indexFile+1].Size);
            diskmap.Delete(indexFile+1)
          end;
          // This is supposed to work like a stack
          diskmap.Insert(indexGap, diskmap[indexFile]);
          diskmap.Insert(indexGap, emptyRecord);
          diskmap.Delete(indexFile+2);

          // Twice increment..:
          Inc(indexFile, 2);
          break;
        end;
        // Twice increment:
        Inc(indexGap, 2);
      end;
    // Twice decrement:
    Dec(indexFile, 2);
    end;
  end;

  function CalcChecksum(diskmap: TList<Integer>): QWord;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to diskmap.Count - 1 do
    begin
      if diskmap[i] > 0 then
        Inc(Result, i * diskmap[i]);
    end;
  end;

  var

    diskmapPrim: TList<TFile>;
    diskmap: TList<Integer>;
    checkSum: QWord;

begin
  diskmapPrim := ParseDiskmapPrimitive();
  try
    DefragmentDisk(diskmapPrim);
    diskmap := ExpandDiskmap(diskmapPrim);
    try
      checkSum := CalcChecksum(diskmap);
    finally
      FreeAndNil(diskmap);
    end;
  finally
    FreeAndNil(diskmapPrim);
  end;

  ed_Answer2.Text := IntToStr(checkSum);
end;

end.

