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

const
  DefaultTimeSep = ':';
  DefaultMillisecSep = '.';
  DefaultMillisecondPrecision = 3;
  AllowedTimeCodeSeps: set of Char = [':','.',',',';','`','/','\',''''];

type

  { TTimeCodePattern }

  TTimeCodePattern = (tptInvalid,tptArray,tptDouble,tpt01,tpt02,tpt03,tpt04,
    tpt05,tpt06,tpt07,tpt08,tpt09,tpt10,tpt11,tpt12);

  //(MajorSep = MinorSep) [tptArray]
  //###.### [tptDouble]
  //#:#(.###) [tpt01]      -  length = 3
  //#:##(.###) [tpt02]     \  length = 4
  //##:#(.###) [tpt03]     /
  //##:##(.###) [tpt04]    \
  //#:#:#(.###) [tpt05]    /  length = 5
  //##:#:#(.###) [tpt06]   \
  //#:##:#(.###) [tpt07]    | length = 6
  //#:#:##(.###) [tpt08]   /
  //#:##:##(.###) [tpt09]  \
  //##:#:##(.###) [tpt10]   | length = 7
  //##:##:#(.###) [tpt11]  /
  //##:##:##(.###) [tpt12] -  length = 8

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
    SourceFPS: Double;    //  \
    HasFrame: Boolean;    //   for string parsing only
    IsFrame: Boolean;     //  /
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
    procedure Initialize(MillisecondPrecision: Word = DefaultMillisecondPrecision;
      MajorSep: Char = DefaultTimeSep;
      MinorSep: Char = DefaultMillisecSep;
      SourceFPS: Double = 25;
      HasFrame: Boolean = False;
      IsFrame: Boolean = False); overload;
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
    procedure Initialize(MillisecondPrecision: Word; MajorSep, MinorSep: Char;
      SourceFPS: Double; HasFrame, IsFrame: Boolean); overload;
    procedure Initialize(const AFormatSettings: TTimeCodeFormatSettings); overload;
    procedure LoadFromFile(AFile: String);
    property Incremental: Boolean read CheckIncremental;
    property Values[Index: Integer]: TTimeCode read Get write Put;
    property Value: String read GetValueString write SetValueString;
    property Count: Integer read GetCount;
  end;

const
  DefaultTimeCodeFormatSettings: TTimeCodeFormatSettings = (
    MillisecondPrecision: DefaultMillisecondPrecision;
    MajorSep: DefaultTimeSep;
    MinorSep: DefaultMillisecSep;
    SourceFPS: 25;
    HasFrame: False;
    IsFrame: False;);

implementation

function IdentTimeCodePattern(const TCStr: String; const AMajorSep,
  AMinorSep: Char): TTimeCodePattern;
var
  Frac: String;
  sa: TStringArray;
  S: String;
  i: Integer;
begin
  Result := tptInvalid;
  S := TCStr;

  if AMajorSep = AMinorSep then
  begin
    sa := S.Split(AMajorSep);
    i := Length(sa);
    if (i < 1) or (i > 4) then Exit;
    for i := 0 to i-1 do
      if StrToIntDef(sa[i], -1) = -1 then Exit;
    Exit(tptArray);
  end;

  case S.CountChar(AMajorSep) of
  0:begin
    if StrToFloatDef(S.Replace(AMinorSep, DefaultFormatSettings.DecimalSeparator)
      , -1) <> -1 then Exit(tptDouble);
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

function FramePartToMillisec(FF, FPS: Double): Double;
begin
  if FF < 0 then
    FF := 0;
  if FPS < 0.0001 then
    FPS := 0.0001;
  Result := FF;
  ForceInRange(Result,0,FPS);
  Result := FloatRound((1000/FPS)*Result, 3);
  ForceInRange(Result,0,999);
end;

function FramePosToSeconds(P: Integer; FPS: Double): Double;
begin
  if P < 0 then
    P := 0;
  if FPS < 0.0001 then
    FPS := 0.0001;
  Result := P/FPS;
end;

function TimeCodeToDouble(TC: TBasicTimeCode;
  const AMillisecondPrecision: Word): Double;
begin
  Result := (TC.H*3600) + (TC.M*60) + TC.S;
  if AMillisecondPrecision > 0 then
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

function TimeCodeToString(TC: TBasicTimeCode;
  const AFormat: TTimeCodeFormatSettings): String;
var
  FST: TFormatSettings;
begin
  with TC do
  begin
    Result := StrForceLength(H.ToString, 2, '0');
    Result := Result + AFormat.MajorSep + StrForceLength(M.ToString, 2, '0');
    Result := Result + AFormat.MajorSep + StrForceLength(S.ToString, 2, '0');
    if AFormat.MillisecondPrecision > 0 then
    begin
      FST := Default(TFormatSettings);
      FST.DecimalSeparator := AFormat.MinorSep;
      Result := Result
        +MS.ToString(ffFixed, 1, AFormat.MillisecondPrecision, FST).Substring(1);
    end;
  end;
end;

function StringToTimeCode(const S: String;
  const AFormat: TTimeCodeFormatSettings): TBasicTimeCode;
var
  Pattern: TTimeCodePattern;
  sa: TStringArray;
begin
  Result.Reset;
  if AFormat.IsFrame then
  begin
    Result := DoubleToTimeCode(FramePosToSeconds(Round(StrToFloatDef(
      S.Replace(AFormat.MinorSep,DefaultFormatSettings.DecimalSeparator), 0))
      ,AFormat.SourceFPS));
    Exit;
  end;
  Pattern := IdentTimeCodePattern(S, AFormat.MajorSep, AFormat.MinorSep);
  case Pattern of
  tptInvalid: Exit;
  tptArray: begin
    sa := S.Split(AFormat.MajorSep);
    case Length(sa) of
    1: Result := DoubleToTimeCode(sa[0].ToInteger);
    2: begin
       Result.M := sa[0].ToInteger;
       Result.S := sa[1].ToInteger;
      end;
    3: begin
      Result.H := sa[0].ToInteger;
      Result.M := sa[1].ToInteger;
      Result.S := sa[2].ToInteger;
      end;
    4: begin
      Result.H := sa[0].ToInteger;
      Result.M := sa[1].ToInteger;
      Result.S := sa[2].ToInteger;
      Result.MS := sa[3].ToInteger/NMultiplier(AFormat.MillisecondPrecision);
      end;
    end;
    end;
  tptDouble: Result := DoubleToTimeCode(S.Replace(AFormat.MinorSep,
    DefaultFormatSettings.DecimalSeparator).ToDouble);
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

  if (Pattern <> tptArray) and S.Contains(AFormat.MinorSep) then
    Result.MS := S.Substring(S.IndexOf(AFormat.MinorSep)+1).ToInteger
      /NMultiplier(AFormat.MillisecondPrecision);

  if AFormat.HasFrame and (Result.MS > 0) then
    Result.MS := FramePartToMillisec(Result.MS, AFormat.SourceFPS);

  with Result do
  begin
    if (H > 59) or (M > 59) or (S > 59) then
      Reset;
  end;
end;

function TimeCodeToArray(TC: TBasicTimeCode;
  AMillisecondMultiplier: Word): TBasicTimeCodeArray;
begin
  Result[0] := TC.H;
  Result[1] := TC.M;
  Result[2] := TC.S;
  if AMillisecondMultiplier > 1 then
    Result[3] := Round(TC.MS * AMillisecondMultiplier)
  else
    Result[3] := 0;
end;

function ArrayToTimeCode(const TCA: TBasicTimeCodeArray;
  AMillisecondMultiplier: Word): TBasicTimeCode;
begin
  Result.Reset;
  Result.H := TCA[0];
  Result.M := TCA[1];
  Result.S := TCA[2];
  if AMillisecondMultiplier > 1 then
    Result.MS := TCA[3] / AMillisecondMultiplier
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
  MinorSep: Char; SourceFPS: Double; HasFrame, IsFrame: Boolean);
begin
  if not (MajorSep in AllowedTimeCodeSeps) then
    raise Exception.Create('Invalid time separator!');
  if not (MinorSep in AllowedTimeCodeSeps) then
    raise Exception.Create('Invalid millisecond separator!');
  if SourceFPS <= 0 then
    raise Exception.Create('FPS should be more than zero!');
  FFormatSettings.MillisecondPrecision := MillisecondPrecision;
  FFormatSettings.MajorSep := MajorSep;
  FFormatSettings.MinorSep := MinorSep;
  FFormatSettings.SourceFPS := SourceFPS;
  FFormatSettings.HasFrame := HasFrame;
  FFormatSettings.IsFrame := IsFrame;
  FInitialized := 'Yes!';
end;

procedure TTimeCode.Initialize(const AFormatSettings: TTimeCodeFormatSettings);
begin
  Initialize(AFormatSettings.MillisecondPrecision, AFormatSettings.MajorSep,
    AFormatSettings.MinorSep, AFormatSettings.SourceFPS, AFormatSettings.HasFrame,
    AFormatSettings.IsFrame);
end;

procedure TTimeCode.InitCheck;
begin
  if FInitialized <> 'Yes!' then
  begin
    FDelay := 0;
    Initialize(DefaultTimeCodeFormatSettings);
  end;
end;

function TTimeCode.MillisecondMultiplier: Integer;
begin
  Result := NMultiplier(FFormatSettings.MillisecondPrecision);
end;

function TTimeCode.GetValueString: String;
begin
  InitCheck;
  Result := TimeCodeToString(FValue, FFormatSettings);
end;

procedure TTimeCode.SetValueString(const AValue: String);
begin
  InitCheck;
  if ((FFormatSettings.HasFrame) and (FFormatSettings.MillisecondPrecision < 3))
  or ((FFormatSettings.IsFrame) and (FFormatSettings.MillisecondPrecision < 3)) then
    FFormatSettings.MillisecondPrecision := 3;
  FValue := StringToTimeCode(AValue, FFormatSettings);
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
  Result := TimeCodeToString(TConstantTimeCodes.Min.Value, FFormatSettings);
  if ValueAsDouble + FDelay >= 0 then
    Result := TimeCodeToString(ValueWithDelay, FFormatSettings);
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
  MinorSep: Char; SourceFPS: Double; HasFrame, IsFrame: Boolean);
begin
  FTimeCode.Initialize(MillisecondPrecision, MajorSep, MinorSep, SourceFPS,
    HasFrame, IsFrame);
end;

procedure TTimeCodeList.Initialize(
  const AFormatSettings: TTimeCodeFormatSettings);
begin
  FTimeCode.Initialize(AFormatSettings);
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

