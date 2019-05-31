unit CommonDrawUtils;
{  some graphics and drawing methods

  Copyright (C) 2019 Mohammadreza Bahrami m.audio91@gmail.com

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
  Classes, SysUtils, Graphics, Controls, CommonNumeralUtils, Math;

procedure HalfEllipse(ACanvas: TCanvas; AColor: TColor;
  AWidth, AHeight: Integer; AAnchorKind: TAnchorKind = akTop);
procedure HorzScaleTicks(ACanvas: TCanvas; AColor: TColor;
  AWidth, AHeight: Integer; ATickWidth: Integer = 1; ASteps: Integer = 6;
  AHorizontalFlipped: Boolean = False; AVerticalFlipped: Boolean = False);
function Luminance(AColor: TColor): Single; //returns a value from 0 to 255
function LuminanceLevel(AColor: TColor): Integer; //returns a percentage value
function LuminanceLevel(ABitmap: TBitmap): Integer; overload;


implementation

procedure HalfEllipse(ACanvas: TCanvas; AColor: TColor;
  AWidth, AHeight: Integer; AAnchorKind: TAnchorKind = akTop);
var
  RoundPart: TRect;
begin
  if( AWidth < 4) or (AHeight < 4) then Exit;
  RoundPart:=Default(TRect);
  case AAnchorKind of
  akTop:
    begin
      RoundPart.Left:=0;
      RoundPart.Top:=-(AHeight div 2)-1;
    end;
  akLeft:
    begin
      RoundPart.Left:=-(AWidth div 2)-1;
      RoundPart.Top:=0;
    end;
  akRight:
    begin
      RoundPart.Left:=(AWidth div 2)+1;
      RoundPart.Top:=0;
    end;
  akBottom:
    begin
      RoundPart.Left:=0;
      RoundPart.Top:=(AHeight div 2)+1;
    end;
  end;
  RoundPart.Width:=AWidth;
  RoundPart.Height:=AHeight;

  ACanvas.Brush.Color:=AColor;
  ACanvas.Pen.Color:=AColor;
  ACanvas.Ellipse(RoundPart);
end;

procedure HorzScaleTicks(ACanvas: TCanvas; AColor: TColor; AWidth,
  AHeight: Integer; ATickWidth: Integer; ASteps: Integer;
  AHorizontalFlipped: Boolean; AVerticalFlipped: Boolean);
var
  RectPart: TRect;
  Scale: Double;
  i: Integer;
begin
  ACanvas.Brush.Color:=AColor;
  ACanvas.Pen.Color:=AColor;
  RectPart:=Default(TRect);
  if ATickWidth<1 then
     ATickWidth:=1;
  if ASteps<=ATickWidth then
    ASteps:=ATickWidth+1;
  Scale:=AHeight/AWidth;

  i:=0;
  with ACanvas do
  begin
    if AHorizontalFlipped and not AVerticalFlipped then
    begin
      while i < AWidth-1 do
      begin
        RectPart.Left:=i;
        RectPart.Top:=Round(Scale*i);
        RectPart.Width:=ATickWidth;
        RectPart.Height:=AHeight-RectPart.Top;
        Rectangle(RectPart);
        Inc(i, ASteps);
      end;
    end
    else if not AHorizontalFlipped and AVerticalFlipped then
    begin
      while i < AWidth-1 do
      begin
        RectPart.Left:=i;
        RectPart.Top:=0;
        RectPart.Width:=ATickWidth;
        RectPart.Height:=Round(Scale*i);
        Rectangle(RectPart);
        Inc(i, ASteps);
      end;
    end
    else if AHorizontalFlipped and AVerticalFlipped then
    begin
      while i < AWidth-1 do
      begin
        RectPart.Left:=i;
        RectPart.Top:=0;
        RectPart.Width:=ATickWidth;
        RectPart.Height:=AHeight-Round(Scale*i);
        Rectangle(RectPart);
        Inc(i, ASteps);
      end;
    end
    else
    begin
      while i < AWidth-1 do
      begin
        RectPart.Left:=i;
        RectPart.Top:=AHeight-Round(Scale*i);
        RectPart.Width:=ATickWidth;
        RectPart.Height:=AHeight-RectPart.Top;
        Rectangle(RectPart);
        Inc(i, ASteps);
      end;
    end;
  end;
end;

function Luminance(AColor: TColor): Single;
var
  r,g,b: Word;
begin
  r:=Red(AColor);
  g:=Green(AColor);
  b:=Blue(AColor);
  Result:=
    Sqrt((r*r*0.241)+(g*g*0.691)+(b*b*0.068));
end;

function LuminanceLevel(AColor: TColor): Integer;
begin
  Result:=
    ConvertInRange(Round(Luminance(AColor)),0,255,0,100);
end;

function LuminanceLevel(ABitmap: TBitmap): Integer;
var
  llevels: array of Single;
  c: TColor;
  i,j,k: Integer;
begin
  k:=-1;
  llevels:=nil;
  SetLength(llevels,ABitmap.Width*ABitmap.Height);
  for i:=0 to ABitmap.Height-1 do
  begin
    for j:=0 to ABitmap.Width-1 do
    begin
      c:=ABitmap.Canvas.Pixels[i,j];
      Inc(k);
      llevels[k]:=Luminance(c);
    end;
  end;
  Result:=ConvertInRange(Round(Mean(llevels)),0,255,0,100);
end;

end.

