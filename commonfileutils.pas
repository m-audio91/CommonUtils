unit CommonFileUtils;
{ Common routines to work with files.

  Copyright (C) 2017 Mohammadreza Bahrami m.audio91@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Dialogs;

function GenFileName(const SourceFile: String; const SomeStr: String = '';
  const NewExtension: String = ''; IncludePath: Boolean = true;
  const NewPath: String = ''): String;
procedure AdjustFileLineBreaks(const F: String);
function TryDirectoryIsWritable(const Dir: String): Boolean;
function OpenFiles(Multi: Boolean = False; const ATitle: String = '';
  const AFilters: String = 'All|*'; AFilterIndex: Integer = 0;
  AOptions: TOpenOptions = []; const APath: String = ''): TStringArray;

implementation

function GenFileName(const SourceFile: String; const SomeStr: String;
  const NewExtension: String; IncludePath: Boolean;
  const NewPath: String): String;
var
  SourcePath, SourceExt: String;
begin
  Result := SourceFile;
  if SourceFile = EmptyStr then Exit;
  SourcePath := ExtractFilePath(SourceFile);;
  SourceExt := ExtractFileExt(SourceFile);
  Result := ExtractFileNameOnly(SourceFile);
  if IncludePath then
  begin
    if NewPath <> EmptyStr then
      Result := IncludeTrailingPathDelimiter(NewPath) + Result
    else
      Result := SourcePath + Result;
  end;
  Result := Result + SomeStr;
  if NewExtension <> EmptyStr then
    Result := Result + NewExtension
  else
    Result := Result + SourceExt;
  if CompareText(Result, SourceFile) = 0 then
    raise Exception.Create('Identical names!');
end;

procedure AdjustFileLineBreaks(const F: String);
var
  sl: TStringList;
begin
  if not FileExists(F) then Exit;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(F);
    sl.TextLineBreakStyle := DefaultTextLineBreakStyle;
    sl.SaveToFile(F);
  finally
    if Assigned(sl) then
      sl.Free;
  end;
end;

function TryDirectoryIsWritable(const Dir: String): Boolean;
begin
  try
    Result := DirectoryIsWritable(Dir);
  Except
    Result := false;
  end;
end;

function OpenFiles(Multi: Boolean; const ATitle: String;
  const AFilters: String; AFilterIndex: Integer; AOptions: TOpenOptions;
  const APath: String): TStringArray;
var
  OD: TOpenDialog;
  i: Integer;
begin
  Result := nil;
  OD := TOpenDialog.Create(nil);
  try
    with OD do
    begin
      if ATitle <> EmptyStr then
        Title := ATitle;
      if AFilters <> EmptyStr then
        Filter := AFilters;
      FilterIndex := AFilterIndex;
      Options := Options + [ofFileMustExist,ofNoNetworkButton]
        + AOptions - [ofAllowMultiSelect];
      if Multi then
        Options := Options + [ofAllowMultiSelect];
      if (APath <> EmptyStr)
      and DirectoryExists(APath) then
        InitialDir := APath;
      if Execute then
      begin
        if Multi then
        begin
          SetLength(Result, Files.Count);
          for i := 0 to Files.Count-1 do
            Result[i] := Files[i];
        end
        else
        begin
          SetLength(Result, 1);
          Result[0] := FileName;
        end;
      end;
    end;
  finally
    if Assigned(OD) then OD.Free;
  end;
end;

end.

