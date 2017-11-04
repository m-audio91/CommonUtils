unit uTimeSliceEditEx;
{ A dialog form to work with uTimeCode and uTimeSlice units.

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
  ExtCtrls, Spin, Clipbrd, Menus, uModalEditor, uTimeCode, uTimeSlice;

type
{ TTimeSliceEditEx }

  TTimeSliceEditEx = class(TModalEditor)
  private
    FValue: TTimeSlice;
    FInputs1: TPanel;
    FPaste1: TButton;
    FHour1,
    FMinute1,
    FSecond1,
    FMilisecond1: TSpinEdit;
    Sep1,
    Sep2,
    Sep3,
    Title1: TLabel;
    FInputs2: TPanel;
    FPaste2: TButton;
    FHour2,
    FMinute2,
    FSecond2,
    FMilisecond2: TSpinEdit;
    Sep4,
    Sep5,
    Sep6,
    Title2: TLabel;
    FInputs3: TPanel;
    FDelay: TFloatSpinEdit;
    Title3: TLabel;
    FMillisecPopup: TPopupMenu;
    FMillisecConvertMI: TMenuItem;
    FPasteFormat: TTimeCodeFormatSettings;
    procedure LoadControls(Sender: TObject);
    procedure OnClosing(Sender: TObject; var CanClose: Boolean);
    procedure OnPasteClick(Sender: TObject);
    function GetValue: String;
    procedure SetValue(const AValue: String);
    procedure OnMillisecConvertMIClick(Sender: TObject);
  public
    property Value: String read GetValue write SetValue;
    property PasteFormat: TTimeCodeFormatSettings read FPasteFormat write FPasteFormat;
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

resourcestring
  rsTimeSliceEditorEx = 'ویرایشگر زمانبندی';
  rsTSEXStart = 'شروع';
  rsTSEXEnd = 'پایان';
  rsTSEXDelay = '+تاخیر -تعجیل';
  rsTSEXHour = 'ساعت';
  rsTSEXMinute = 'دقیقه';
  rsTSEXSecond = 'ثانیه';
  rsTSEXMillisecond = 'هزارم ثانیه';
  rsTSEPaste = 'چسباندن';
  rsConvertFFtoMS = 'تبدیل به هزارم ثانیه';

implementation

{ TFreeEditor }

procedure TTimeSliceEditEx.LoadControls(Sender: TObject);
begin
  //FMillisecConvertMI
  FMillisecConvertMI := NewItem(rsConvertFFtoMS,0,False,True,
    @OnMillisecConvertMIClick,0,'');

  //FMillisecPopup
  FMillisecPopup := NewPopupMenu(Self,'',paLeft,True,[FMillisecConvertMI]);

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
    ChildSizing.ControlsPerLine := 9;
    ChildSizing.HorizontalSpacing := 2;
    Color := clForm;
    ShowHint := True;
  end;

  //FPaste1
  FPaste1 := TButton.Create(Self);
  with FPaste1 do
  begin
    Parent := FInputs1;
    AutoSize := True;
    Caption := rsTSEPaste;
    Name := 'FPaste1';
    OnClick := @OnPasteClick;
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
    Hint := rsTSEXHour;
    Constraints.MinWidth := Trunc(Width*1.5);
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
    Hint := rsTSEXMinute;
    Constraints.MinWidth := Trunc(Width*1.5);
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
    Hint := rsTSEXSecond;
    Constraints.MinWidth := Trunc(Width*1.5);
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
    Hint := rsTSEXMillisecond;
    Constraints.MinWidth := Width*2;
    Value := FValue.Value.StartPos.ValueAsArray[3];
    if FPasteFormat.HasFrame then
      PopupMenu := FMillisecPopup;
  end;

  //Title1
  Title1 := TLabel.Create(Self);
  with Title1 do
  begin
    Parent := FInputs1;
    Caption := rsTSEXStart;
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
    ChildSizing.ControlsPerLine := 9;
    ChildSizing.HorizontalSpacing := 2;
    Color := clForm;
    ShowHint := True;
  end;

  //FPaste2
  FPaste2 := TButton.Create(Self);
  with FPaste2 do
  begin
    Parent := FInputs2;
    AutoSize := True;
    Caption := rsTSEPaste;
    Name := 'FPaste2';
    OnClick := @OnPasteClick;
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
    Hint := rsTSEXHour;
    Constraints.MinWidth := Trunc(Width*1.5);
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
    Hint := rsTSEXMinute;
    Constraints.MinWidth := Trunc(Width*1.5);
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
    Hint := rsTSEXSecond;
    Constraints.MinWidth := Trunc(Width*1.5);
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
    Hint := rsTSEXMillisecond;
    Constraints.MinWidth := Width*2;
    Value := FValue.Value.EndPos.ValueAsArray[3];
    if FPasteFormat.HasFrame then
      PopupMenu := FMillisecPopup;
  end;

  //Title2
  Title2 := TLabel.Create(Self);
  with Title2 do
  begin
    Parent := FInputs2;
    Caption := rsTSEXEnd;
    Layout := tlBottom;
  end;

  //FInputs3: TPanel;
  FInputs3 := TPanel.Create(Self);
  with FInputs3 do
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
    ShowHint := True;
  end;

  //FDelay
  FDelay := TFloatSpinEdit.Create(Self);
  with FDelay do
  begin
    Parent := FInputs3;
    MinValue := -3600;
    MaxValue := 3599;
    DecimalPlaces := 3;
    Alignment := taCenter;
    Increment := 0.010;
    Hint := rsTSEXSecond;
    Constraints.MinWidth := Width*2;
    Value := FValue.Delay;
  end;

  //Title3
  Title3 := TLabel.Create(Self);
  with Title3 do
  begin
    Parent := FInputs3;
    Caption := rsTSEXDelay;
    Layout := tlBottom;
  end;
end;

procedure TTimeSliceEditEx.OnClosing(Sender: TObject; var CanClose: Boolean);
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

  FValue.Delay := FDelay.Value;

  if not FValue.Valid then
    ModalResult := mrCancel;

  CanClose := True;
end;

procedure TTimeSliceEditEx.OnPasteClick(Sender: TObject);
var
  tc: TTimeCode;
begin
  tc.TimeCodeFormat := FPasteFormat;
  tc.ValueAsString := ClipBoard.AsText;
  case (Sender as TComponent).Name of
  'FPaste1': begin
    FHour1.Value := tc.ValueAsArray[0];
    FMinute1.Value := tc.ValueAsArray[1];
    FSecond1.Value := tc.ValueAsArray[2];
    FMilisecond1.Value := tc.ValueAsArray[3];
    end;
  'FPaste2': begin
    FHour2.Value := tc.ValueAsArray[0];
    FMinute2.Value := tc.ValueAsArray[1];
    FSecond2.Value := tc.ValueAsArray[2];
    FMilisecond2.Value := tc.ValueAsArray[3];
    end;
  end;
end;

function TTimeSliceEditEx.GetValue: String;
begin
  Result := FValue.ValueAsStringEx;
end;

procedure TTimeSliceEditEx.SetValue(const AValue: String);
begin
  FValue.ValueAsStringEx := AValue;
end;

procedure TTimeSliceEditEx.OnMillisecConvertMIClick(Sender: TObject);
var
  tc: TTimeCode;
  ta: TBasicTimeCodeArray;
  ms: ^TSpinEdit;
begin
  tc.TimeCodeFormat := FPasteFormat;
  ta := TConstantTimeCodes.MinAsArray;
  ms := nil;
  if (FMillisecPopup.PopupComponent as TControl).Parent = FInputs1 then
    ms := @FMilisecond1
  else if (FMillisecPopup.PopupComponent as TControl).Parent = FInputs2 then
    ms := @FMilisecond2;

  if Assigned(ms) then
  begin
    ta[3] := ms^.Value;
    if (ta[3] > FPasteFormat.SourceFPS+(FPasteFormat.SourceFPS/2)) then Exit;
    tc.ValueAsArray := ta;
    tc.ValueAsString := tc.ValueAsString;
    ms^.Value := tc.ValueAsArray[3];
  end;
end;

constructor TTimeSliceEditEx.CreateNew(AOwner: TComponent; Num: Integer);
begin
  //Self
  inherited CreateNew(AOwner, Num);
  Caption := rsTimeSliceEditorEx;
  OnShow := @LoadControls;
  OnCloseQuery := @OnClosing;

  FValue.Reset;
  FPasteFormat := DefaultTimeCodeFormatSettings;
end;

end.

