unit CommonStrUtils;
{ Common routines to work with strings.

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
  Classes, SysUtils, Character;

function IsEmptyStr(const AStr: String): Boolean;
function Escape(const S: String; const ToEscape: String = ' ';
  const EscapeTo: String = '?'): String;
function DeEscape(const S: String; const ToDeEscape: String = '?';
  const DeEscapeTo: String = ' '): String;
function QuoteAndEscape(const S: String; const ToEscape: String = ' ';
  const EscapeTo: String = '?'; QuoteChar: Char = '"'): String;
function PrependEscapeChars(const S, Chars: String; EscChar: Char): String;
function NoChainedSpaces(const S: String): String;
function NowToString: String;
function CountAWord(const AWord, AStr: String): Integer;
function StrForceLength(const AStr: String; ALen: Integer; APadChar: Char;
  PadFromRight: Boolean = False): String;
function CharsAreDigit(const AStr: String; CharIndexes: array of Integer;
  ZeroBasedIndexes: Boolean = False): Boolean;
function CharsEqual(const AStr: String; AChar: Char; CharIndexes:
  array of Integer): Boolean;
function NextNonDigit(const AStr: String; AIndex: Integer): Integer;
function PrevNonDigit(const AStr: String; AIndex: Integer): Integer;
procedure RemoveDupsInArray(var AArray: TStringArray);
procedure NilIfEmptyArray(var AArray: TStringArray);
function FixedFloatDecimalsCount(const AStr: String; ACount: Integer): String;
function StringListToArray(var SL: TStringList): TStringArray;
procedure ArrayToStringList(var AArray: TStringArray; var SL: TStringList);
function FindInArray(var AArray: TStringArray; AText: String; AStartIndex:
  Integer = 0): Integer;
procedure DeleteAllOccurrences(const SubStr: String; var Source: String;
  const CensorMask: String = '');
procedure DeleteAllOccurrencesVL(const SubStrStart, SubStrEnd: String;
  var Source: String; IncludeEnd: Boolean = True; const CensorMask: String = '');
function ReplaceStrings(const S: String; Old, New: array of String): String;
function NthIndexOf(const ASubStr, AStr: String; N: Integer): Integer;

implementation

function IsEmptyStr(const AStr: String): Boolean;
begin
  Result := Astr.Trim([#32,#13,#10]) = EmptyStr;
end;

function Escape(const S: String; const ToEscape: String; const EscapeTo: String
  ): String;
begin
  Result := StringReplace(S, ToEscape, EscapeTo, [rfReplaceAll,rfIgnoreCase]);
end;

function DeEscape(const S: String; const ToDeEscape: String;
  const DeEscapeTo: String): String;
begin
  Result := StringReplace(S, ToDeEscape, DeEscapeTo, [rfReplaceAll,rfIgnoreCase]);
end;

function QuoteAndEscape(const S: String; const ToEscape: String;
  const EscapeTo: String; QuoteChar: Char): String;
begin
  Result := QuoteChar + Escape(S, ToEscape, EscapeTo) + QuoteChar;
end;

function PrependEscapeChars(const S, Chars: String; EscChar: Char): String;
var
  i: Integer;
begin
  Result := S;
  if Chars = EmptyStr then Exit;
  if EscChar in [#0..#31] then Exit;
  for i := S.Length-1 downto 0 do
    if Chars.IndexOf(S.Chars[i]) >= 0 then
      Result := Result.Insert(i, EscChar);
end;

function NoChainedSpaces(const S: String): String;
var
  i: Integer;
begin
  Result := S;
  repeat
    i := Result.IndexOf('  ');
    if i >= 0 then
      Result := Result.Remove(i, 1);
  until i < 0;
  Result := Result.Trim;
end;

function NowToString: String;
var
  dt: TDateTime;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DateSeparator := '-';
  fs.TimeSeparator := '-';
  dt := Now;
  Result := Escape(DateTimeToStr(dt, fs), ' ', '-');
end;

function CountAWord(const AWord, AStr: String): Integer;
var
  i,j: Integer;
begin
  Result := 0;
  j := -1;
  repeat
    i := AStr.IndexOf(AWord, j+1);
    if i >= 0 then
    begin
      j := i;
      Inc(Result);
    end;
  until i < 0;
end;

function StrForceLength(const AStr: String; ALen: Integer; APadChar: Char;
  PadFromRight: Boolean = False): String;
begin
  Result := AStr;
  if AStr.Length < ALen then
  begin
    Result := EmptyStr;
    case PadFromRight of
    True: Result := AStr.PadRight(ALen, APadChar);
    False: Result := AStr.PadLeft(ALen, APadChar);
    end;
  end
  else if AStr.Length > ALen then
    Result := AStr.Substring(0, ALen);
end;

function CharsAreDigit(const AStr: String; CharIndexes: array of Integer;
  ZeroBasedIndexes: Boolean): Boolean;
var
  i,j: Integer;
begin
  Result := False;
  j := 0;
  if Astr = EmptyStr then Exit;
  if Length(CharIndexes) = 0 then Exit;
  if ZeroBasedIndexes then
    Inc(j);
  for i in CharIndexes do
    if not IsNumber(AStr, i+j) then Exit;
  Result := True;
end;

function CharsEqual(const AStr: String; AChar: Char;
  CharIndexes: array of Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i in CharIndexes do
    if not (AStr.Chars[i-1] = AChar) then Exit;
  Result := True;
end;

function NextNonDigit(const AStr: String; AIndex: Integer): Integer;
begin
  Result := 0;
  if AIndex < 1 then Exit;
  if AIndex > AStr.Length-1 then Exit;
  repeat
    Inc(AIndex);
    if not CharsAreDigit(AStr, [AIndex]) then
    begin
      Result := AIndex;
      Break;
    end;
  until AIndex >= AStr.Length;
end;

function PrevNonDigit(const AStr: String; AIndex: Integer): Integer;
begin
  Result := 0;
  if AIndex <= 1 then Exit;
  if AIndex > AStr.Length then Exit;
  repeat
    Dec(AIndex);
    if not CharsAreDigit(AStr, [AIndex]) then
    begin
      Result := AIndex;
      Break;
    end;
  until AIndex <= 1;
end;

procedure RemoveDupsInArray(var AArray: TStringArray);
var
  TempArray: TStringArray;
  i,j,l: Integer;
begin
  if Length(AArray) < 2 then Exit;
  for i := High(AArray) downto 0 do
    for j := i-1 downto 0 do
      if AArray[i].Equals(AArray[j]) then
      begin
        AArray[i] := EmptyStr;
        Break;
      end;
  l := 0;
  for i := 0 to High(AArray) do
    if AArray[i] = EmptyStr then
      Inc(l);
  SetLength(TempArray, Length(AArray)-l);
  j := 0;
  for i := 0 to High(AArray) do
    if AArray[i] <> EmptyStr then
    begin
      TempArray[j] := AArray[i];
      Inc(j);
    end;
  AArray := nil;
  AArray := TempArray;
end;

procedure NilIfEmptyArray(var AArray: TStringArray);
var
  CanNil: Boolean;
  i: Integer;
begin
  CanNil := True;
  for i := 0 to High(AArray) do
    if AArray[i] <> EmptyStr then
    begin
      CanNil := False;
      Break;
    end;
  if CanNil then
    AArray := nil;
end;

function FixedFloatDecimalsCount(const AStr: String; ACount: Integer): String;
var
  i,f,s: String;
begin
  Result := AStr;
  if AStr.Length < 3 then Exit;
  if StrToFloatDef(AStr, 0) = 0 then Exit;
  if ACount < 1 then Exit;
  s := AStr.SubString(AStr.IndexOfAny(['.',',']), 1);
  if s = EmptyStr then Exit;
  i := AStr.Substring(0, AStr.IndexOf(s));
  f := AStr.Substring(AStr.IndexOf(s)+1);
  if f.Length = ACount then Exit;
  Result := i + s + StrForceLength(f,ACount,'0',True);
end;

function StringListToArray(var SL: TStringList): TStringArray;
var
  i: Integer;
begin
  Result := nil;
  if not Assigned(SL) then Exit;
  SetLength(Result, SL.Count);
  for i := 0 to SL.Count-1 do
    Result[i] := SL[i];
end;

procedure ArrayToStringList(var AArray: TStringArray; var SL: TStringList);
var
  i: Integer;
begin
  if not Assigned(SL) then Exit;
  SL.Clear;
  i := Length(AArray);
  if i < 1 then Exit;
  SL.Capacity := i;
  for i := 0 to High(AArray) do
    SL.Add(AArray[i]);
end;

function FindInArray(var AArray: TStringArray; AText: String; AStartIndex:
  Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  i := Length(AArray);
  if i < 1 then Exit;
  if AText = EmptyStr then Exit;
  if i <= AStartIndex then Exit;
  for i := AStartIndex to High(AArray) do
    if AArray[i].IndexOf(AText) > 0 then
    begin
      Result := i;
      Break;
    end;
end;

procedure DeleteAllOccurrences(const SubStr: String; var Source: String;
  const CensorMask: String);
begin
  Source := Source.Replace(SubStr, CensorMask);
end;

procedure DeleteAllOccurrencesVL(const SubStrStart, SubStrEnd: String;
  var Source: String; IncludeEnd: Boolean; const CensorMask: String);
var
  i,j,k: Integer;
begin
  j := 0;
  repeat
    i := Source.IndexOf(SubStrStart, j);
    if i < 0 then Break;
    j := i;
    k := Source.IndexOf(SubStrEnd, i);
    if k >= 0 then
    begin
      k := k - j;
      if IncludeEnd then
        k := k + SubStrEnd.Length;
      Source := Source.Remove(i, k);
      if not IsEmptyStr(CensorMask) then
        Source := Source.Insert(i, CensorMask);
    end;
  until i < 0;
end;

function ReplaceStrings(const S: String; Old, New: array of String): String;
var
  Flags: TReplaceFlags;
  i: Integer;
begin
  Result := S;
  Flags := [rfReplaceAll,rfIgnoreCase];
  if High(Old) <> High(New) then Exit;
  for i := Low(Old) to High(Old) do
    Result := Result.Replace(Old[i], New[i], Flags);
end;

function NthIndexOf(const ASubStr, AStr: String; N: Integer): Integer;
var
  i,j: Integer;
begin
  Result := -1;
  if IsEmptyStr(ASubStr) or IsEmptyStr(AStr) then Exit;
  if (N < 1) or (AStr.Length < N) then Exit;
  i := 0;
  j := 0;
  repeat
    i := AStr.IndexOf(ASubStr, i);
    if i >= 0 then
    begin
      Inc(j);
      if j = N then Exit(i);
      Inc(i);
    end;
  until i < 0;
end;

end.

