﻿unit uTimeSliceEdit;
{ A dialog form to work with uTimeCode and uTimeSlice units. without delay
  support.

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
  Classes, SysUtils, GraphType, Graphics, Forms, Controls, StdCtrls,
  ExtCtrls, Spin, uTimeCode, uTimeSlice;

type
{ TTimeSliceEdit }

  TTimeSliceEdit = class(TForm)
  private
    FValue: TTimeSlice;
    FInputs1: TPanel;
    FHour1,
    FMinute1,
    FSecond1,
    FMilisecond1: TSpinEdit;
    Sep1,
    Sep2,
    Sep3,
    Title1: TLabel;
    FInputs2: TPanel;
    FHour2,
    FMinute2,
    FSecond2,
    FMilisecond2: TSpinEdit;
    Sep4,
    Sep5,
    Sep6,
    Title2: TLabel;
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
  rsTimeSliceEditor = 'ویرایشگر زمانبندی';
  rsTSEOK = 'تایید';
  rsTSECancel = 'صرف نظر';
  rsTSEStart = 'شروع';
  rsTSEEnd = 'پایان';

implementation

{ TFreeEditor }

procedure TTimeSliceEdit.OnShowing(Sender: TObject);
var
  h: Integer;
begin
  //FInputs1: TPanel;
  FInputs1 := TPanel.Create(Self);
  with FInputs1 do
  begin
    Parent := Self;
    Caption := EmptyStr;
    BevelOuter := bvNone;
    AutoSize := True;
    BorderSpacing.Top := 8;
    ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    ChildSizing.ControlsPerLine := 8;
    ChildSizing.HorizontalSpacing := 2;
    Color := clForm;
  end;

  //FHour1
  FHour1 := TSpinEdit.Create(Self);
  with FHour1 do
  begin
    Parent := FInputs1;
    MinValue := 0;
    MaxValue := 59;
    Alignment := taCenter;
    Increment := 1;
    Value := FValue.Value.StartPos.Value.H;
  end;

  //Sep1
  Sep1 := TLabel.Create(Self);
  with Sep1 do
  begin
    Parent := FInputs1;
    Caption := ':';
    Layout := tlBottom;
  end;

  //FMinute1
  FMinute1 := TSpinEdit.Create(Self);
  with FMinute1 do
  begin
    Parent := FInputs1;
    MinValue := 0;
    MaxValue := 59;
    Alignment := taCenter;
    Increment := 1;
    Value := FValue.Value.StartPos.Value.M;
  end;

  //Sep2
  Sep2 := TLabel.Create(Self);
  with Sep2 do
  begin
    Parent := FInputs1;
    Caption := ':'; 
    Layout := tlBottom;
  end;
  
  //FSecond1
  FSecond1 := TSpinEdit.Create(Self);
  with FSecond1 do
  begin
    Parent := FInputs1;
    MinValue := 0;
    MaxValue := 59;
    Alignment := taCenter;
    Increment := 1;
    Value := FValue.Value.StartPos.Value.S;
  end;

  //Sep3
  Sep3 := TLabel.Create(Self);
  with Sep3 do
  begin
    Parent := FInputs1;
    Caption := '.';
    Layout := tlBottom;
  end;

  //FMilisecond1
  FMilisecond1 := TSpinEdit.Create(Self);
  with FMilisecond1 do
  begin
    Parent := FInputs1;
    MinValue := 0;
    MaxValue := 999;
    Alignment := taCenter;
    Increment := 1;
    Value := FValue.Value.StartPos.ValueAsArray[3];
  end;

  //Title1
  Title1 := TLabel.Create(Self);
  with Title1 do
  begin
    Parent := FInputs1;
    Caption := rsTSEStart;
    Layout := tlBottom;
  end;

  //FInputs2: TPanel;
  FInputs2 := TPanel.Create(Self);
  with FInputs2 do
  begin
    Parent := Self;
    Caption := EmptyStr;
    BevelOuter := bvNone;
    AutoSize := True;
    BorderSpacing.Top := 8;
    ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    ChildSizing.ControlsPerLine := 8;
    ChildSizing.HorizontalSpacing := 2;
    Color := clForm;
  end;

  //FHour2
  FHour2 := TSpinEdit.Create(Self);
  with FHour2 do
  begin
    Parent := FInputs2;
    MinValue := 0;
    MaxValue := 59;
    Alignment := taCenter;
    Increment := 1;
    Value := FValue.Value.EndPos.Value.H;
  end;

  //Sep4
  Sep4 := TLabel.Create(Self);
  with Sep4 do
  begin
    Parent := FInputs2;
    Caption := ':';
    Layout := tlBottom;
  end;

  //FMinute2
  FMinute2 := TSpinEdit.Create(Self);
  with FMinute2 do
  begin
    Parent := FInputs2;
    MinValue := 0;
    MaxValue := 59;
    Alignment := taCenter;
    Increment := 1;
    Value := FValue.Value.EndPos.Value.M;
  end;

  //Sep5
  Sep5 := TLabel.Create(Self);
  with Sep5 do
  begin
    Parent := FInputs2;
    Caption := ':';
    Layout := tlBottom;
  end;
  
  //FSecond2
  FSecond2 := TSpinEdit.Create(Self);
  with FSecond2 do
  begin
    Parent := FInputs2;
    MinValue := 0;
    MaxValue := 59;
    Alignment := taCenter;
    Increment := 1;
    Value := FValue.Value.EndPos.Value.S;
  end;

  //Sep6
  Sep6 := TLabel.Create(Self);
  with Sep6 do
  begin
    Parent := FInputs2;
    Caption := '.';
    Layout := tlBottom;
  end;

  //FMilisecond2
  FMilisecond2 := TSpinEdit.Create(Self);
  with FMilisecond2 do
  begin
    Parent := FInputs2;
    MinValue := 0;
    MaxValue := 999;
    Alignment := taCenter;
    Increment := 1;
    Value := FValue.Value.EndPos.ValueAsArray[3];
  end;

  //Title2
  Title2 := TLabel.Create(Self);
  with Title2 do
  begin
    Parent := FInputs2;
    Caption := rsTSEEnd;
    Layout := tlBottom;
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
    Caption := rsTSEOK;
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
    Caption := rsTSECancel;
    AutoSize := True;
    Cancel := True;
    ModalResult := mrCancel;
  end;
  FOk.Left := FCancel.Left+FCancel.Width;

  AutoSize := True;
  h := Height;
  AutoSize := False;
  Height := h;
  Constraints.MinHeight := h;
end;

procedure TTimeSliceEdit.OnClosing(Sender: TObject; var CanClose: Boolean);
var
  tc: TBasicTimeCodeArray;
begin
  tc[0] := FHour1.Value;
  tc[1] := FMinute1.Value;
  tc[2] := FSecond1.Value;
  tc[3] := FMilisecond1.Value;
  FValue.Value.StartPos.ValueAsArray := tc;

  tc[0] := FHour2.Value;
  tc[1] := FMinute2.Value;
  tc[2] := FSecond2.Value;
  tc[3] := FMilisecond2.Value;
  FValue.Value.EndPos.ValueAsArray := tc;

  if not FValue.Valid then
    ModalResult := mrCancel;

  CanClose := True;
end;

function TTimeSliceEdit.GetValue: String;
begin
  Result := FValue.ValueAsString;
end;

procedure TTimeSliceEdit.SetValue(const AValue: String);
begin
  FValue.ValueAsString := AValue;
end;

constructor TTimeSliceEdit.CreateNew(AOwner: TComponent; Num: Integer);
begin
  //Self
  inherited CreateNew(AOwner, Num);
  Position := poScreenCenter;
  Caption := rsTimeSliceEditor;
  Constraints.MinWidth := 300;
  ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
  ChildSizing.ShrinkHorizontal := crsHomogenousChildResize;
  ChildSizing.ControlsPerLine := 1;
  ChildSizing.LeftRightSpacing := 8;
  ChildSizing.TopBottomSpacing := 8;
  OnShow := @OnShowing;
  OnCloseQuery := @OnClosing;

  FValue := Default(TTimeSlice);
end;

end.
