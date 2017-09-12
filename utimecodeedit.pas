unit uTimeCodeEdit;
{ A dialog form to work with uTimeCode unit.

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
  Classes, SysUtils, GraphType, Graphics, Forms, Controls, StdCtrls, ExtCtrls,
  Spin, uTimeCode;

type
{ TTimeCodeEdit }

  TTimeCodeEdit = class(TForm)
  private
    FValue: TTimeCode;
    FInputs: TPanel;
    FHour,
    FMinute,
    FSecond,
    FMilisecond: TSpinEdit;
    Sep1,
    Sep2,
    Sep3: TLabel;
    FActions: TPanel;
    FOk: TButton;
    FCancel: TButton;
    procedure OnShowing(Sender: TObject);
    procedure OnClosing(Sender: TObject; var CanClose: Boolean);
    function GetValue: String;
    procedure SetValue(const AValue: String);
  public
    property Value: String read GetValue write SetValue;
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

resourcestring
  rsFreeEditor = 'ویرایشگر زمانبندی';
  rsTFEOK = 'تایید';
  rsTFECancel = 'صرف نظر';

implementation

{ TFreeEditor }

procedure TTimeCodeEdit.OnShowing(Sender: TObject);
var
  w,h: Integer;
begin
  //FInputs: TPanel;
  FInputs := TPanel.Create(Self);
  with FInputs do
  begin
    Parent := Self;
    Caption := EmptyStr;
    BevelOuter := bvNone;
    AutoSize := True;
    BorderSpacing.Top := 8;
    ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    ChildSizing.ControlsPerLine := 7;
    ChildSizing.HorizontalSpacing := 2;
    Color := clForm;
  end;

  //FHour
  FHour := TSpinEdit.Create(Self);
  with FHour do
  begin
    Parent := FInputs;
    MinValue := 0;
    MaxValue := 59;
    Alignment := taCenter;
    Increment := 1;
    Constraints.MinWidth := Trunc(Width*1.5);
    Value := FValue.Value.H;
  end;

  //Sep1
  Sep1 := TLabel.Create(Self);
  with Sep1 do
  begin
    Parent := FInputs;
    Caption := ':';
  end;

  //FMinute
  FMinute := TSpinEdit.Create(Self);
  with FMinute do
  begin
    Parent := FInputs;
    MinValue := 0;
    MaxValue := 59;
    Alignment := taCenter;
    Increment := 1;
    Constraints.MinWidth := Trunc(Width*1.5);
    Value := FValue.Value.M;
  end;

  //Sep2
  Sep2 := TLabel.Create(Self);
  with Sep2 do
  begin
    Parent := FInputs;
    Caption := ':';
  end;
  
  //FSecond
  FSecond := TSpinEdit.Create(Self);
  with FSecond do
  begin
    Parent := FInputs;
    MinValue := 0;
    MaxValue := 59;
    Alignment := taCenter;
    Increment := 1;
    Constraints.MinWidth := Trunc(Width*1.5);
    Value := FValue.Value.S;
  end;

  //Sep3
  Sep3 := TLabel.Create(Self);
  with Sep3 do
  begin
    Parent := FInputs;
    Caption := '.';
  end;

  //FMilisecond
  FMilisecond := TSpinEdit.Create(Self);
  with FMilisecond do
  begin
    Parent := FInputs;
    MinValue := 0;
    MaxValue := 999;
    Alignment := taCenter;
    Increment := 1;
    Constraints.MinWidth := Width*2;
    Value := FValue.ValueAsArray[3];
  end;

  //FActions
  FActions := TPanel.Create(Self);
  with FActions do
  begin
    Parent := Self;
    Caption := EmptyStr;
    BevelOuter := bvNone;
    AutoSize := True;
    BorderSpacing.Top := 8;
    Color := clForm;
  end;

  //FOk
  FOk := TButton.Create(Self);
  with FOk do
  begin
    Parent := FActions;
    Align := alRight;
    Caption := rsTFEOK;
    AutoSize := True;
    Default := True;
    ModalResult := mrOk;
  end;

  //FCancel
  FCancel := TButton.Create(Self);
  with FCancel do
  begin
    Parent := FActions;
    Align := alRight;
    Left := 0;
    Caption := rsTFECancel;
    AutoSize := True;
    Cancel := True;
    ModalResult := mrCancel;
  end;
  FOk.Left := FCancel.Left+FCancel.Width;

  AutoSize := True;
  w := Width;
  h := Height;
  AutoSize := False;
  Width := w;
  Constraints.MinWidth := w;
  Height := h;
  Constraints.MinHeight := h;
end;

procedure TTimeCodeEdit.OnClosing(Sender: TObject; var CanClose: Boolean);
var
  tc: TBasicTimeCodeArray;
begin
  tc[0] := FHour.Value;
  tc[1] := FMinute.Value;
  tc[2] := FSecond.Value;
  tc[3] := FMilisecond.Value;
  FValue.ValueAsArray := tc;
  CanClose := True;
end;

function TTimeCodeEdit.GetValue: String;
begin
  Result := FValue.ValueAsString;
end;

procedure TTimeCodeEdit.SetValue(const AValue: String);
begin
  FValue.ValueAsString := AValue;
end;

constructor TTimeCodeEdit.CreateNew(AOwner: TComponent; Num: Integer);
begin
  //Self
  inherited CreateNew(AOwner, Num);
  Position := poScreenCenter;
  Caption := rsFreeEditor;
  Constraints.MinWidth := 300;
  ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
  ChildSizing.ShrinkHorizontal := crsHomogenousChildResize;
  ChildSizing.ControlsPerLine := 1;
  ChildSizing.LeftRightSpacing := 8;
  ChildSizing.TopBottomSpacing := 8;
  OnShow := @OnShowing;
  OnCloseQuery := @OnClosing;

  FValue := Default(TTimeCode);
end;

end.

