unit CommonGUIUtils;
{ Common routines to work with GUI.

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
  Classes, SysUtils, Forms, LCLType, Graphics, Controls, ExtCtrls,
  IntfGraphics, LazCanvas, extinterpolation, FPImage,
  Menus, CommonDrawUtils;

procedure CheckDisplayInScreen(aForm: TForm);
procedure ShowError(const AMsg, ATitle: String);
function ShowWarnYN(const AMsg, ATitle: String): boolean;
procedure CopyImageList(AImgList: TImageList; ASourceImageList: TImageList;
  ADPI, ASourceDPI: Word; AMaskColor: TColor);
procedure SetMenuValues(AMenu: TMainMenu; Values: array of String;
      AClickEvent: TNotifyEvent); overload;
procedure SetMenuValues(AMenu: TPopupMenu; Values: array of String;
      AClickEvent: TNotifyEvent); overload;
function HasDarkBackgroundColor(AControl: TWinControl): Boolean;
function DefaultMacOSMenu(AForm: TForm): TMainMenu;

const
  MenuSep='-';

implementation

procedure CheckDisplayInScreen(aForm: TForm);
begin
  if not Assigned(aForm) then Exit;
  with aForm do
  begin
    if Top<0 then
      Top := 0;
    if Left<0 then
      Left := 0;
    if Left+Width > Screen.Width then
      Left := Left - ((Left+Width)-Screen.Width);
    if Height>Screen.Height then
      Height := Screen.Height;
    if Top+Height>Screen.Height then
    begin
      if Height<Screen.Height then
        Top := Screen.Height-Height
      else
      begin
        Top := 0;
        Height := Screen.Height;
      end;
    end;
  end;
end;

procedure ShowError(const AMsg, ATitle: String);
begin
  with Application do
    MessageBox(PChar(AMsg), PChar(ATitle), MB_ICONERROR);
end;

function ShowWarnYN(const AMsg, ATitle: String): boolean;
begin
  with Application do
    Result := MessageBox(PChar(AMsg), PChar(ATitle), MB_YESNO + MB_ICONWARNING) = IDYES;
end;

procedure CopyImageList(AImgList: TImageList; ASourceImageList: TImageList;
  ADPI, ASourceDPI: Word; AMaskColor: TColor);

  procedure ClearBitmap(ABitmap: TBitmap; AW, AH: Integer);
  begin
    ABitmap.Width := 0;
    ABitmap.Height := 0;
    ABitmap.Width := AW;
    ABitmap.Height := AH;
  end;

var
  IntfImg, ResizedIntfImg: TLazIntfImage;
  Canv: TLazCanvas;
  Bmp, ResizedBmp, TempBmp: TBitmap;
  w,h,i: Integer;
begin
  if not Assigned(AImgList) then Exit;
  if not Assigned(ASourceImageList) then Exit;
  if ASourceImageList.Count < 1 then Exit;
  if ADPI = ASourceDPI then
  begin
    AImgList.Clear;
    AImgList.Width := ASourceImageList.Width;
    AImgList.Height := ASourceImageList.Height;
    AImgList.AddImages(ASourceImageList);
    Exit;
  end;

  w := Round((ADPI / ASourceDPI) * ASourceImageList.Width);
  h := Round((ADPI / ASourceDPI) * ASourceImageList.Height);
  AImgList.Clear;
  AImgList.Width := w;
  AImgList.Height := h;

  Bmp := TBitmap.Create;
  TempBmp := TBitmap.Create;
  TempBmp.Canvas.Brush.Color := AMaskColor;
  ResizedBmp := TBitmap.Create;
  IntfImg := TLazIntfImage.Create(0, 0);
  ResizedIntfImg := TLazIntfImage.Create(0, 0);
  Canv := TLazCanvas.Create(ResizedIntfImg);
  Canv.Interpolation := TLanczosInterpolation.Create;

  Bmp.PixelFormat := {$IFDEF WINDOWS}pf32Bit{$ELSE}pf24Bit{$ENDIF};
  ResizedBmp.PixelFormat := {$IFDEF WINDOWS}pf32Bit{$ELSE}pf24Bit{$ENDIF};
  TempBmp.PixelFormat := {$IFDEF WINDOWS}pf32Bit{$ELSE}pf24Bit{$ENDIF};

  for i := 0 to ASourceImageList.Count-1 do
  begin
    ResizedBmp.Transparent := False;
    ClearBitmap(ResizedBmp, w, h);
    ClearBitmap(Bmp, ASourceImageList.Width, ASourceImageList.Height);
    ASourceImageList.GetBitmap(i, Bmp);
    ResizedIntfImg.LoadFromBitmap(ResizedBmp.Handle, 0);
    {$IFDEF WINDOWS}
    IntfImg.LoadFromBitmap(Bmp.Handle, 0);
    Canv.StretchDraw(0, 0, w, h, IntfImg);
    ResizedBmp.LoadFromIntfImage(ResizedIntfImg);
    {$ELSE}
    ClearBitmap(TempBmp, ASourceImageList.Width, ASourceImageList.Height);
    TempBmp.Canvas.FillRect(Rect(0,0,TempBmp.Width,TempBmp.Height));
    TempBmp.Canvas.Draw(0,0,Bmp);

    IntfImg.LoadFromBitmap(TempBmp.Handle, 0);
    Canv.StretchDraw(0, 0, w, h, IntfImg);
    ResizedBmp.LoadFromIntfImage(ResizedIntfImg);

    ResizedBmp.TransparentColor := AMaskColor;
    ResizedBmp.Transparent := True;
    {$ENDIF}
    AImgList.Add(ResizedBmp, nil);
  end;
  Bmp.Free;
  TempBmp.Free;
  ResizedBmp.Free;
  Canv.Interpolation.Free;
  Canv.Free;
  IntfImg.Free;
  ResizedIntfImg.Free;
end;

procedure SetMenuValues(AMenu: TMainMenu; Values: array of String;
  AClickEvent: TNotifyEvent);
var
  i: Integer;
begin
  i := Length(Values);
  AMenu.Items.Clear;
  for i := Low(Values) to High(Values) do
    AMenu.Items.Add(NewItem(Values[i], 0, False, True, AClickEvent, 0,
      AMenu.Name+'MI'+i.ToString));
end;

procedure SetMenuValues(AMenu: TPopupMenu; Values: array of String;
  AClickEvent: TNotifyEvent);
var
  i: Integer;
begin
  i := Length(Values);
  AMenu.Items.Clear;
  for i := Low(Values) to High(Values) do
    AMenu.Items.Add(NewItem(Values[i], 0, False, True, AClickEvent, 0,
      AMenu.Name+'MI'+i.ToString));
end;

function HasDarkBackgroundColor(AControl: TWinControl): Boolean;
begin
  Result := False;
  if LuminanceLevel(AControl.GetRGBColorResolvingParent)<50 then
    Result := True;
end;

function DefaultMacOSMenu(AForm: TForm): TMainMenu;
{$ifdef darwin}
var
  AppMenu: TMenuItem;
{$endif}
begin
  {$ifdef darwin}
    Result := TMainMenu.Create(AForm);
    Result.Parent := AForm;
    AppMenu:=TMenuItem.Create(AForm);
    AppMenu.Caption:=#$EF#$A3#$BF;
    Result.Items.Insert(0, AppMenu);
  {$endif}
end;

end.

