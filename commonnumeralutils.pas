unit CommonNumeralUtils;
{ Common routines to work with numbers.

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
  Classes, SysUtils;

function ForceInRange(var AValue: Longint; AMin, AMax: Longint): Boolean; overload;
function ForceInRange(var Value: Double; Min, Max: Double): Boolean; overload;
function ConvertInRange(AValue, AOldRangeMin, AOldRangeMax, ANewRangeMin,
  ANewRangeMax: Integer): Integer;
function FloatRound(AValue: Double; ADecimalPrecision: Integer): Double;
function NMultiplier(AValue: Integer): Integer;

implementation

function ForceInRangeBase(const AValue, AMin, AMax: Double): Double; overload;
begin
  Result := AValue;
  if AValue > AMax then
    Result := AMax
  else if AValue < AMin then
    Result := AMin
end;

function ForceInRange(var AValue: Longint; AMin, AMax: Longint): Boolean; overload;
var
  d: Double;
begin
  Result := False;
  d := ForceInRangeBase(AValue, AMin, AMax);
  if d <> AValue then
  begin
    AValue := Trunc(d);
    Result := True;
  end;
end;

function ForceInRange(var Value: Double; Min, Max: Double): Boolean; overload;
var
  d: Double;
begin
  Result := False;
  d := ForceInRangeBase(Value, Min, Max);
  if d <> Value then
  begin
    Value := d;
    Result := True;
  end;
end;

function ConvertInRange(AValue, AOldRangeMin, AOldRangeMax, ANewRangeMin,
  ANewRangeMax: Integer): Integer;
var
  OldRange, NewRange: Integer;
begin
  Result := ANewRangeMin;
  if AOldRangeMin >= AOldRangeMax then Exit;
  if ANewRangeMin >= ANewRangeMax then Exit;
  if AValue <= AOldRangeMin then Exit(ANewRangeMin);
  if AValue >= AOldRangeMax then Exit(ANewRangeMax);
  OldRange := AOldRangeMax-AOldRangeMin;
  NewRange := ANewRangeMax-ANewRangeMin;
  Result := Round((((AValue-AOldRangeMin)*NewRange)/OldRange)+ANewRangeMin);
  ForceInRange(Result, ANewRangeMin, ANewRangeMax);
end;

function FloatRound(AValue: Double; ADecimalPrecision: Integer): Double;
var
  m: Integer;
begin
  Result := AValue;
  if ADecimalPrecision < 1 then
    Result := Round(AValue)
  else
  begin
    m := NMultiplier(ADecimalPrecision);
    Result := Round(AValue*m)/m;
  end;
end;

function NMultiplier(AValue: Integer): Integer;
var
  i: Integer;
begin
  Result := 1;
  if AValue < 1 then Exit;
  for i := 1 to AValue do
    Result := Result * 10;
end;

end.

