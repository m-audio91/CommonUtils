unit uTimeCode;
{ A TimeCode and TimeCode List parser.

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

{$mode objfpc}{$H+}{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, CommonStrUtils, CommonNumeralUtils;

type

  { TTimeCodePattern }

  TTimeCodePattern = (tptInvalid,tptDouble,tpt01,tpt02,tpt03,tpt04,tpt05,tpt06,
    tpt07,tpt08,tpt09,tpt10,tpt11,tpt12);
    //###.### or ###,### [tptDouble]
    //#:# [tpt01]      -  length = 3
    //#:## [tpt02]     \  length = 4
    //##:# [tpt03]     /
    //##:## [tpt04]    \
    //#:#:# [tpt05]    /  length = 5
    //##:#:# [tpt06]   \
    //#:##:# [tpt07]    | length = 6
    //#:#:## [tpt08]   /
    //#:##:## [tpt09]  \
    //##:#:## [tpt10]   | length = 7
    //##:##:# [tpt11]  /
    //##:##:## [tpt12] -  length = 8

  { TBasicTimeCode }

  TBasicTimeCode = record
    H,
    M,
    S: Word;
    MS: Single;
    procedure Reset;
  end;

  { TBasicTimeCodeArray }

   TBasicTimeCodeArray = array[0..3] of Integer;

  { TTimeCodeFormatSettings }

  TTimeCodeFormatSettings = record
    MillisecondPrecision: Word;
    MajorSep: Char;
    MinorSep: Char;
  end;

  { TTimeCode }

  TTimeCode = record
  private
    FInitialized: String;
    FValue: TBasicTimeCode;
    FFormatSettings: TTimeCodeFormatSettings;
    FDelay: Double;
    function MillisecondMultiplier: Integer;
    function GetValueString: String;
    procedure SetValueString(const AValue: String);
    function GetValueDouble: Double;
    procedure SetValueDouble(const AValue: Double);
    procedure InitCheck;
    function GetValueArray: TBasicTimeCodeArray;
    procedure SetValueArray(const AValue: TBasicTimeCodeArray);
    function GetValueWithDelay: TBasicTimeCode;
    function GetValueWithDelayString: String;
    function GetValueWithDelayDouble: Double;
    function GetValueWithDelayArray: TBasicTimeCodeArray;
  public
    procedure Initialize(MillisecondPrecision: Word; MajorSep, MinorSep: Char); overload;
    procedure Initialize(const AFormatSettings: TTimeCodeFormatSettings); overload;
    property Value: TBasicTimeCode read FValue write FValue;
    property ValueAsString: String read GetValueString write SetValueString;
    property ValueAsDouble: Double read GetValueDouble write SetValueDouble;
    property ValueAsArray: TBasicTimeCodeArray read GetValueArray write SetValueArray;
    property Delay: Double read FDelay write FDelay;
    property ValueWithDelay: TBasicTimeCode read GetValueWithDelay;
    property ValueWithDelayAsString: String read GetValueWithDelayString;
    property ValueWithDelayAsDouble: Double read GetValueWithDelayDouble;
    property ValueWithDelayAsArray: TBasicTimeCodeArray read GetValueWithDelayArray;
    property TimeCodeFormat: TTimeCodeFormatSettings read FFormatSettings write Initialize;
  end;

  { TConstantTimeCodes }

  TConstantTimeCodes = class
    class function Min: TTimeCode;
    class function Max: TTimeCode;
  end;

  { TTimeCodeList }

  TTimeCodeList = record
  private
    FList: array of TTimeCode;
    FTimeCode: TTimeCode;
    Procedure CheckIndex(AIndex: Integer); inline;
    function CheckIncremental: Boolean;
    function GetValueString: String;
    procedure SetValueString(AValue: String);
    function Get(Index: Integer): TTimeCode;
    procedure Put(Index: Integer; AValue: TTimeCode);
    function GetCount: Integer;
  public
    procedure Initialize(MillisecondPrecision: Word; MajorSep, MinorSep: Char);
    procedure LoadFromFile(AFile: String);
    property Incremental: Boolean read CheckIncremental;
    property Values[Index: Integer]: TTimeCode read Get write Put;
    property Value: String read GetValueString write SetValueString;
    property Count: Integer read GetCount;
  end;

const
  DefaultTimeSep = ':';
  DefaultMillisecSep = '.';
  AllowedTimeCodeSeps: set of Char = [':','.',',',';','`','/','\',''''];

implementation

function IdentTimeCodePattern(const TCStr: String; const AMajorSep,
  AMinorSep: Char): TTimeCodePattern;
var
  Frac: String;
  S: String;
begin
  Result := tptInvalid;
  S := TCStr;
  case S.CountChar(AMajorSep) of
  0:begin
    if StrToFloatDef(S, -1) <> -1 then Exit(tptDouble);
    end;
  1..2:
    begin
    if (S.CountChar(AMinorSep) > 1) then Exit;
    if (S.CountChar(AMinorSep) = 1)
    and Not (S.LastIndexOf(AMinorSep) > S.LastIndexOf(AMajorSep)) then Exit;
    if (S.CountChar(AMinorSep) = 1) then
    begin
      Frac := S.Substring(S.LastIndexOf(AMinorSep)+1);
      if (StrToIntDef(Frac, -1) = -1) then Exit;
      S := S.Substring(0, (S.Length - Frac.Length)-1);
    end;
    if (S.Length = 3) and CharsAreDigit(S, [1,3])
    and CharsEqual(S, AMajorSep, [2]) then Exit(tpt01);
    if (S.Length = 4) then
    begin
      if CharsAreDigit(S, [1,3,4])
      and CharsEqual(S, AMajorSep, [2]) then Exit(tpt02);
      if CharsAreDigit(S, [1,2,4])
      and CharsEqual(S, AMajorSep, [3]) then Exit(tpt03);
    end;
    if (S.Length = 5) then
    begin
      if CharsAreDigit(S, [1,2,4,5])
      and CharsEqual(S, AMajorSep, [3]) then Exit(tpt04);
      if CharsAreDigit(S, [1,3,5])
      and CharsEqual(S, AMajorSep, [2,4]) then Exit(tpt05);
    end;
    if (S.Length = 6) then
    begin
      if CharsAreDigit(S, [1,2,4,6])
      and CharsEqual(S, AMajorSep, [3,5]) then Exit(tpt06);
      if CharsAreDigit(S, [1,3,4,6])
      and CharsEqual(S, AMajorSep, [2,5]) then Exit(tpt07);
      if CharsAreDigit(S, [1,3,5,6])
      and CharsEqual(S, AMajorSep, [2,4]) then Exit(tpt08);
    end;
    if (S.Length = 7) then
    begin
      if CharsAreDigit(S, [1,3,4,6,7])
      and CharsEqual(S, AMajorSep, [2,5]) then Exit(tpt09);
      if CharsAreDigit(S, [1,2,4,6,7])
      and CharsEqual(S, AMajorSep, [3,5]) then Exit(tpt10);
      if CharsAreDigit(S, [1,2,4,5,7])
      and CharsEqual(S, AMajorSep, [3,6]) then Exit(tpt11);
    end;
    if (S.Length = 8) and CharsAreDigit(S, [1,2,4,5,7,8])
    and CharsEqual(S, AMajorSep, [3,6]) then Exit(tpt12);
    end;
  end;
end;

function TimeCodeToDouble(TC: TBasicTimeCode;
  const AMilisecondPrecision: Word): Double;
begin
  Result := (TC.H*3600) + (TC.M*60) + TC.S;
  if AMilisecondPrecision > 0 then
    Result := Result  + TC.MS;
end;

function DoubleToTimeCode(D: Double): TBasicTimeCode;
var
  i: Integer;
begin
  with Result do
  begin
    i := Trunc(D/3600);
    ForceInRange(i, 0, 59);
    H := i;
    i := Trunc(D/60) mod 60;
    ForceInRange(i, 0, 59);
    M := i;
    i := Trunc(D) mod 60;
    ForceInRange(i, 0, 59);
    S := i;
    MS := D.Fraction;
  end;
end;

function TimeCodeToString(TC: TBasicTimeCode; const AMajorSep,
  AMinorSep: Char; AMilisecondPrecision: Word): String;
var
  FST: TFormatSettings;
begin
  with TC do
  begin
    Result := StrForceLength(H.ToString, 2, '0');
    Result := Result + AMajorSep + StrForceLength(M.ToString, 2, '0');
    Result := Result + AMajorSep + StrForceLength(S.ToString, 2, '0');
    if AMilisecondPrecision > 0 then
    begin
      FST := Default(TFormatSettings);
      FST.DecimalSeparator := AMinorSep;
      Result := Result
        +MS.ToString(ffFixed, 1, AMilisecondPrecision, FST).Substring(1);
    end;
  end;
end;

function StringToTimeCode(const S: String;
  const AMajorSep, AMinorSep: Char): TBasicTimeCode;
var
  Pattern: TTimeCodePattern;
begin
  Result := Default(TBasicTimeCode);
  Result.Reset;
  Pattern := IdentTimeCodePattern(S, AMajorSep, AMinorSep);
  case Pattern of
  tptInvalid: Exit;
  tptDouble: Result := DoubleToTimeCode(S.ToDouble);
  tpt01: begin
    Result.M := S.Substring(0,1).ToInteger;
    Result.S := S.Substring(2,1).ToInteger;
    end;
  tpt02: begin
    Result.M := S.Substring(0,1).ToInteger;
    Result.S := S.Substring(2,2).ToInteger;
    end;
  tpt03: begin
    Result.M := S.Substring(0,2).ToInteger;
    Result.S := S.Substring(3,1).ToInteger;
    end;
  tpt04: begin
    Result.M := S.Substring(0,2).ToInteger;
    Result.S := S.Substring(3,2).ToInteger;
    end;
  tpt05: begin
    Result.H := S.Substring(0,1).ToInteger;
    Result.M := S.Substring(2,1).ToInteger;
    Result.S := S.Substring(4,1).ToInteger;
    end;
  tpt06: begin
    Result.H := S.Substring(0,2).ToInteger;
    Result.M := S.Substring(3,1).ToInteger;
    Result.S := S.Substring(5,1).ToInteger;
    end;
  tpt07: begin
    Result.H := S.Substring(0,1).ToInteger;
    Result.M := S.Substring(2,2).ToInteger;
    Result.S := S.Substring(5,1).ToInteger;
    end;
  tpt08: begin
    Result.H := S.Substring(0,1).ToInteger;
    Result.M := S.Substring(2,1).ToInteger;
    Result.S := S.Substring(4,2).ToInteger;
    end;
  tpt09: begin
    Result.H := S.Substring(0,1).ToInteger;
    Result.M := S.Substring(2,2).ToInteger;
    Result.S := S.Substring(5,2).ToInteger;
    end;
  tpt10: begin
    Result.H := S.Substring(0,2).ToInteger;
    Result.M := S.Substring(3,1).ToInteger;
    Result.S := S.Substring(5,2).ToInteger;
    end;
  tpt11: begin
    Result.H := S.Substring(0,2).ToInteger;
    Result.M := S.Substring(3,2).ToInteger;
    Result.S := S.Substring(6,1).ToInteger;
    end;
  tpt12: begin
    Result.H := S.Substring(0,2).ToInteger;
    Result.M := S.Substring(3,2).ToInteger;
    Result.S := S.Substring(6,2).ToInteger;
    end;
  end;
  if S.Contains(AMinorSep) then
    Result.MS := ('0.' + S.Substring(S.IndexOf(AMinorSep)+1)).ToSingle;
  with Result do
  begin
    if (H > 59) or (M > 59) or (S > 59) then
      Reset;
  end;
end;

function TimeCodeToArray(TC: TBasicTimeCode;
  AMilisecondMultiplier: Word): TBasicTimeCodeArray;
begin
  Result[0] := TC.H;
  Result[1] := TC.M;
  Result[2] := TC.S;
  if AMilisecondMultiplier > 1 then
    Result[3] := Round(TC.MS * AMilisecondMultiplier)
  else
    Result[3] := 0;
end;

function ArrayToTimeCode(const TCA: TBasicTimeCodeArray;
  AMilisecondMultiplier: Word): TBasicTimeCode;
begin
  Result := Default(TBasicTimeCode);
  Result.H := TCA[0];
  Result.M := TCA[1];
  Result.S := TCA[2];
  if AMilisecondMultiplier > 1 then
    Result.MS := TCA[3] / AMilisecondMultiplier
  else
    Result.MS := 0;
end;

{ TBasicTimeCode }

procedure TBasicTimeCode.Reset;
begin
  H := 0;
  M := 0;
  S := 0;
  MS := 0;
end;

{ TTimeCode }

procedure TTimeCode.Initialize(MillisecondPrecision: Word; MajorSep,
  MinorSep: Char);
begin
  if not (MajorSep in AllowedTimeCodeSeps) then
    raise Exception.Create('Invalid time separator!');
  if not (MinorSep in AllowedTimeCodeSeps) then
    raise Exception.Create('Invalid millisecond separator!');
  if MajorSep = MinorSep then
    raise Exception.Create('Similar values for time and millisecond separator!');
  FFormatSettings.MillisecondPrecision := MillisecondPrecision;
  FFormatSettings.MajorSep := MajorSep;
  FFormatSettings.MinorSep := MinorSep;
  FInitialized := 'Yes!';
end;

procedure TTimeCode.Initialize(const AFormatSettings: TTimeCodeFormatSettings);
begin
  Initialize(AFormatSettings.MillisecondPrecision, AFormatSettings.MajorSep,
    AFormatSettings.MinorSep);
end;

procedure TTimeCode.InitCheck;
begin
  if FInitialized <> 'Yes!' then
  begin
    FDelay := 0;
    Initialize(3, DefaultTimeSep, DefaultMillisecSep);
  end;
end;

function TTimeCode.MillisecondMultiplier: Integer;
begin
  Result := NMultiplier(FFormatSettings.MillisecondPrecision);
end;

function TTimeCode.GetValueString: String;
begin
  InitCheck;
  Result := TimeCodeToString(FValue, FFormatSettings.MajorSep,
    FFormatSettings.MinorSep, FFormatSettings.MillisecondPrecision);
end;

procedure TTimeCode.SetValueString(const AValue: String);
begin
  InitCheck;
  FValue := StringToTimeCode(AValue, FFormatSettings.MajorSep, FFormatSettings.MinorSep);
end;

function TTimeCode.GetValueDouble: Double;
begin
  InitCheck;
  Result := TimeCodeToDouble(FValue, FFormatSettings.MillisecondPrecision);
end;

procedure TTimeCode.SetValueDouble(const AValue: Double);
begin
  InitCheck;
  FValue := DoubleToTimeCode(AValue);
end;

function TTimeCode.GetValueArray: TBasicTimeCodeArray;
begin
  InitCheck;
  Result := TimeCodeToArray(FValue, MillisecondMultiplier);
end;

procedure TTimeCode.SetValueArray(const AValue: TBasicTimeCodeArray);
begin
  InitCheck;
  FValue := ArrayToTimeCode(AValue, MillisecondMultiplier);
end;

function TTimeCode.GetValueWithDelay: TBasicTimeCode;
begin
  Result.Reset;
  if ValueAsDouble + FDelay >= 0 then
    Result := DoubleToTimeCode(ValueAsDouble + FDelay);
end;

function TTimeCode.GetValueWithDelayString: String;
begin
  Result := TimeCodeToString(TConstantTimeCodes.Min.Value, FFormatSettings.MajorSep,
      FFormatSettings.MinorSep, FFormatSettings.MillisecondPrecision);
  if ValueAsDouble + FDelay >= 0 then
    Result := TimeCodeToString(ValueWithDelay, FFormatSettings.MajorSep,
      FFormatSettings.MinorSep, FFormatSettings.MillisecondPrecision);
end;

function TTimeCode.GetValueWithDelayDouble: Double;
begin
  Result := 0;
  if ValueAsDouble + FDelay >= 0 then
    Result := ValueAsDouble + FDelay;
end;

function TTimeCode.GetValueWithDelayArray: TBasicTimeCodeArray;
begin
  Result := TimeCodeToArray(TConstantTimeCodes.Min.Value, MillisecondMultiplier);
  if ValueAsDouble + FDelay >= 0 then
    Result := TimeCodeToArray(DoubleToTimeCode(ValueAsDouble + FDelay),
      MillisecondMultiplier);
end;

{ TConstantTimeCodes }

class function TConstantTimeCodes.Min: TTimeCode;
begin
  Result.Value.Reset;
end;

class function TConstantTimeCodes.Max: TTimeCode;
var
  TC: TBasicTimeCode;
begin
  TC.H := 59;
  TC.M := 59;
  TC.S := 59;
  TC.MS := 0.999;
  Result.Value := TC;
end;

{ TTimeCodeList }

procedure TTimeCodeList.Initialize(MillisecondPrecision: Word; MajorSep,
  MinorSep: Char);
begin
  FTimeCode.Initialize(MillisecondPrecision, MajorSep, MinorSep);
end;

procedure TTimeCodeList.CheckIndex(AIndex: Integer);
begin
  if (AIndex < Low(FList)) or (AIndex > High(FList)) then
    raise EListError.Create('Out of bounds index');
end;

function TTimeCodeList.CheckIncremental: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(FList)-1 do
    if FList[i].ValueAsDouble > FList[i+1].ValueAsDouble then Exit(False);
end;

function TTimeCodeList.Get(Index: Integer): TTimeCode;
begin
  CheckIndex(Index);
  Result := FList[Index];
end;

procedure TTimeCodeList.Put(Index: Integer; AValue: TTimeCode);
begin
  CheckIndex(Index);
  FList[Index] := AValue;
end;

function TTimeCodeList.GetCount: Integer;
begin
  if not CheckIncremental then Exit(0);
  Result := Length(FList);
end;

procedure TTimeCodeList.LoadFromFile(AFile: String);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFile);
    SetValueString(sl.Text.Trim);
  finally
    sl.Free;
  end;
end;

function TTimeCodeList.GetValueString: String;
var
  sl: TStringList;
  i: Integer;
begin
  Result := EmptyStr;
  sl := TStringList.Create;
  sl.Capacity := Length(FList);
  try
    for i := 0 to High(FList) do
      sl.Add(FList[i].ValueAsString);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure TTimeCodeList.SetValueString(AValue: String);
var
  sl: TStringList;
  i: Integer;
begin
  FList := nil;
  if AValue = EmptyStr then Exit;
  sl := TStringList.Create;
  try
    sl.Text := AValue;
    if sl.Count = 0 then Exit;
    SetLength(FList, sl.Count);
    for i := 0 to sl.Count-1 do
    begin
      FTimeCode.ValueAsString := sl[i];
      FList[i] := FTimeCode;
    end;
  finally
    sl.Free;
  end;
end;

end.

