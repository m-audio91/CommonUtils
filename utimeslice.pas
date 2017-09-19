unit uTimeSlice;
{ A TimeSlice and TimeSlice List parser based on uTimeCode unit.

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
  Classes, SysUtils, uTimeCode;

type

  { TBasicTimeSlice }

  TBasicTimeSlice = record
    StartPos: TTimeCode;
    EndPos: TTimeCode;
  end;

  { TTimeSliceFormatSettings }

  TTimeSliceFormatSettings = record
    TimeCodeFormat: TTimeCodeFormatSettings;
    SliceSep: String;
  end;

  { TTimeSlice }

  TTimeSlice = record
  private 
    FInitialized: String;
    FValue: TBasicTimeSlice;
    FFormatSettings: TTimeSliceFormatSettings;
    procedure InitCheck;
    function CheckValidity: Boolean;
    function GetDuration: TTimeCode;
    function GetDelayedTimeSlice: TBasicTimeSlice;
    procedure SetDelay(AValue: Double);
    function GetValueString: String;
    procedure SetValueString(const AValue: String);
    function GetDelay: Double;
    function GetValueStringEx: String;
    procedure SetValueStringEx(AValue: String);
  public
    procedure Initialize(MillisecondPrecision: Word; MajorSep, MinorSep: Char;
      const SliceSep: String); overload;
    procedure Initialize(const AFormatSettings: TTimeSliceFormatSettings); overload;
    procedure Reset;
    property Valid: Boolean read CheckValidity;
    property Delay: Double read GetDelay write SetDelay;
    property Value: TBasicTimeSlice read FValue write FValue;
    property ValueAsString: String read GetValueString write SetValueString;
    property ValueAsStringEx: String read GetValueStringEx write SetValueStringEx;
    property ValueWithDelay: TBasicTimeSlice read GetDelayedTimeSlice;
    property Duration: TTimeCode read GetDuration;
    property TimeSliceFormat: TTimeSliceFormatSettings read FFormatSettings write Initialize;
  end;

  { TTimeSliceList }

  TTimeSliceList = record
  private
    FList: array of TTimeSlice;
    FSlice: TTimeSlice;
    Procedure CheckIndex(AIndex: Integer); inline;
    function CheckValidity: Boolean;
    function CheckIncremental: Boolean;
    procedure SetValueString(AValue: String);
    function GetInvertedValue: String;
    function GetValueString: String;
    function Get(Index: Integer): TTimeSlice;
    procedure Put(Index: Integer; AValue: TTimeSlice);
    function GetCount: Integer;
    function GetExtendedValueString: String;
    procedure SetExtendedValueString(AValue: String);
  public
    procedure Initialize(MillisecondPrecision: Word; MajorSep, MinorSep,
      SliceSep: Char);
    procedure LoadFromFile(AFile: String);
    procedure LoadFromFileEx(AFile: String);
    property Valid: Boolean read CheckValidity;
    property Incremental: Boolean read CheckIncremental;
    property Values[Index: Integer]: TTimeSlice read Get write Put;
    property Value: String read GetValueString write SetValueString;
    property InvertedValue: String read GetInvertedValue;
    property ExtendedValue: String read GetExtendedValueString write SetExtendedValueString;
    property Count: Integer read GetCount;
  end;

const
  DefaultTimeSliceSep = '-';

implementation

const
  SliceStartMarker = 'Start=';
  SliceEndMarker = 'End=';
  SliceDelayMarker = 'Delay=';

{ TTimeSlice }

procedure TTimeSlice.InitCheck;
begin
  if FInitialized <> 'Yes!' then
  begin
    FFormatSettings.TimeCodeFormat.MillisecondPrecision := DefaultMillisecondPrecision;
    FFormatSettings.TimeCodeFormat.MajorSep := DefaultTimeSep;
    FFormatSettings.TimeCodeFormat.MinorSep := DefaultMillisecSep;
    FFormatSettings.SliceSep := DefaultTimeSliceSep;
    FInitialized := 'Yes!';
  end;
end;

procedure TTimeSlice.Initialize(MillisecondPrecision: Word; MajorSep, MinorSep
  : Char; const SliceSep: String);
begin
  FValue.StartPos.Initialize(MillisecondPrecision, MajorSep, MinorSep);
  FValue.EndPos.Initialize(MillisecondPrecision, MajorSep, MinorSep);
  FFormatSettings.TimeCodeFormat.MillisecondPrecision := MillisecondPrecision;
  FFormatSettings.TimeCodeFormat.MajorSep := MajorSep;
  FFormatSettings.TimeCodeFormat.MinorSep := MinorSep;
  FFormatSettings.SliceSep := SliceSep;
  FInitialized := 'Yes!';
end;

procedure TTimeSlice.Initialize(const AFormatSettings: TTimeSliceFormatSettings
  );
begin
  Initialize(AFormatSettings.TimeCodeFormat.MillisecondPrecision,
    AFormatSettings.TimeCodeFormat.MajorSep,
    AFormatSettings.TimeCodeFormat.MinorSep,
    AFormatSettings.SliceSep);
end;

procedure TTimeSlice.Reset;
begin
  FValue.StartPos.Value.Reset;
  FValue.EndPos.Value.Reset;
  SetDelay(0);
end;

function TTimeSlice.CheckValidity: Boolean;
begin
  Result := FValue.StartPos.ValueAsDouble < FValue.EndPos.ValueAsDouble;
end;

procedure TTimeSlice.SetDelay(AValue: Double);
begin
  FValue.StartPos.Delay := AValue;
  FValue.EndPos.Delay := AValue;
end;

function TTimeSlice.GetValueString: String;
begin
  InitCheck;
  Result := FValue.StartPos.ValueAsString + FFormatSettings.SliceSep
    +FValue.EndPos.ValueAsString;
end;

procedure TTimeSlice.SetValueString(const AValue: String);
var
  i: Integer;
begin
  InitCheck;
  Reset;
  i := AValue.IndexOf(FFormatSettings.SliceSep);
  if (i < 1) or (i+FFormatSettings.SliceSep.Length > AValue.Length-2) then Exit;
  FValue.StartPos.ValueAsString := AValue.Substring(0, i);
  FValue.EndPos.ValueAsString := AValue.Substring(i+FFormatSettings.SliceSep.Length);
end;

function TTimeSlice.GetDelay: Double;
begin
  Result := FValue.StartPos.Delay;
end;

function TTimeSlice.GetValueStringEx: String;
begin
  InitCheck;
  Result := SliceStartMarker + FValue.StartPos.ValueAsString +' '+
  SliceEndMarker + FValue.EndPos.ValueAsString +' '+
  SliceDelayMarker + GetDelay.ToString;
end;

procedure TTimeSlice.SetValueStringEx(AValue: String);
var
  sa: TStringArray;
begin
  InitCheck;
  Reset;
  if (AValue.IndexOf(SliceStartMarker) < 0)
  or (AValue.IndexOf(SliceEndMarker) < 0)
  or (AValue.IndexOf(SliceDelayMarker) < 0) then
    Exit;
  sa := AValue.Trim.Split(' ');
  if Length(sa) <> 3 then Exit;
  FValue.StartPos.ValueAsString := sa[0].Substring(SliceStartMarker.Length);
  FValue.EndPos.ValueAsString := sa[1].Substring(SliceEndMarker.Length);
  SetDelay(sa[2].Substring(SliceDelayMarker.Length).ToDouble);
end;

function TTimeSlice.GetDuration: TTimeCode;
begin
  Result := Default(TTimeCode);
  Result.ValueAsDouble := FValue.EndPos.ValueAsDouble - FValue.StartPos.ValueAsDouble;
end;

function TTimeSlice.GetDelayedTimeSlice: TBasicTimeSlice;
begin
  Result := Default(TBasicTimeSlice);
  Result.StartPos.Value := FValue.StartPos.ValueWithDelay;
  Result.EndPos.Value := FValue.EndPos.ValueWithDelay;
end;

{ TTimeSliceList }

procedure TTimeSliceList.Initialize(MillisecondPrecision: Word; MajorSep,
  MinorSep, SliceSep: Char);
begin
  FSlice.Initialize(MillisecondPrecision, MajorSep, MinorSep, SliceSep);
end; 

procedure TTimeSliceList.CheckIndex(AIndex: Integer);
begin
  if (AIndex < Low(FList)) or (AIndex > High(FList)) then
    raise EListError.Create('Out of bounds index');
end;

function TTimeSliceList.CheckValidity: Boolean;
var
  i: Integer;
begin
  Result := False;
  if Length(FList) = 0 then Exit;
  for i := 0 to High(FList) do
    if not FList[i].Valid then Exit;
  Result := True;
end;

function TTimeSliceList.CheckIncremental: Boolean;
var
  i: Integer;
begin
  Result := False;
  if not CheckValidity then Exit;
  for i := 0 to High(FList)-1 do
    if FList[i].Value.EndPos.ValueAsDouble
      > FList[i+1].Value.StartPos.ValueAsDouble then Exit;
  Result := True;
end;

function TTimeSliceList.Get(Index: Integer): TTimeSlice;
begin
  CheckIndex(Index);
  Result := FList[Index];
end;

procedure TTimeSliceList.Put(Index: Integer; AValue: TTimeSlice);
begin
  CheckIndex(Index);
  FList[Index] := AValue;
end;

function TTimeSliceList.GetCount: Integer;
begin
  if not CheckIncremental then Exit(0);
  Result := Length(FList);
end;

procedure TTimeSliceList.LoadFromFile(AFile: String);
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

procedure TTimeSliceList.LoadFromFileEx(AFile: String);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFile);
    SetExtendedValueString(sl.Text.Trim);
  finally
    sl.Free;
  end;
end;

function TTimeSliceList.GetValueString: String;
var
  sl: TStringList;
  i: Integer;
begin
  Result := EmptyStr;
  if not CheckValidity then Exit;
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

procedure TTimeSliceList.SetValueString(AValue: String);
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
      FSLice.ValueAsString := sl[i];
      FList[i] := FSLice;
    end;
  finally
    sl.Free;
  end;
end;

function TTimeSliceList.GetInvertedValue: String;
//input: 00:00:01:00-00:00:02:00
//output: 00:00:00:00-00:00:01:00, 00:00:02:00-59:59:59:999
var
  sl: TStringList;
  i: Integer;
begin
  Result := EmptyStr;
  if not CheckIncremental then Exit;
  sl := TStringList.Create;
  sl.Capacity := Length(FList)*2;
  try
    if FList[0].Value.StartPos.ValueAsDouble > 0 then
    begin
      FSlice.Value.StartPos.ValueAsDouble := 0;
      FSlice.Value.EndPos.Value := FList[0].Value.StartPos.Value;
      sl.Add(FSlice.ValueAsString);
    end;
    for i := 0 to High(FList) do
    begin
      FSlice.Value.StartPos.Value := FList[i].Value.EndPos.Value;
      if i = High(FList) then
        FSlice.Value.EndPos.Value := TConstantTimeCodes.Max.Value
      else
        FSlice.Value.EndPos.Value := FList[i+1].Value.StartPos.Value;
      sl.Add(FSlice.ValueAsString);
    end;
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

function TTimeSliceList.GetExtendedValueString: String;
var
  sl: TStringList;
  i: Integer;
begin
  Result := EmptyStr;
  if not CheckValidity then Exit;
  sl := TStringList.Create;
  sl.Capacity := Length(FList);
  try
    for i := 0 to High(FList) do
      sl.Add(FList[i].ValueAsStringEx);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure TTimeSliceList.SetExtendedValueString(AValue: String);
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
      FSLice.ValueAsStringEx := sl[i];
      FList[i] := FSLice;
    end;
  finally
    sl.Free;
  end;
end;

end.

