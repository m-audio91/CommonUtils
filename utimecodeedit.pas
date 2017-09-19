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
  Spin, Buttons, Clipbrd, uModalEditor, uTimeCode;

type
{ TTimeCodeEdit }

  TTimeCodeEdit = class(TModalEditor)
  private
    FValue: TTimeCode;
    FInputs: TPanel;
    FPaste: TSpeedButton;
    FHour,
    FMinute,
    FSecond,
    FMilisecond: TSpinEdit;
    Sep1,
    Sep2,
    Sep3: TLabel;
    FPasteFormat: TTimeCodeFormatSettings;
    procedure LoadControls(Sender: TObject);
    procedure OnClosing(Sender: TObject; var CanClose: Boolean);
    procedure OnPasteClick(Sender: TObject);
    function GetValue: String;
    procedure SetValue(const AValue: String);
  public
    property Value: String read GetValue write SetValue;
    property PasteFormat: TTimeCodeFormatSettings read FPasteFormat write FPasteFormat;
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

resourcestring
  rsTimeCodeEditor = 'ویرایشگر زمانبندی';
  rsTCEHour = 'ساعت';
  rsTCEMinute = 'دقیقه';
  rsTCESecond = 'ثانیه';
  rsTCEMillisecond = 'هزارم ثانیه';
  rsTCEPaste = 'چسباندن';

implementation

{ TFreeEditor }

procedure TTimeCodeEdit.LoadControls(Sender: TObject);
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
    ChildSizing.ControlsPerLine := 8;
    ChildSizing.HorizontalSpacing := 2;
    Color := clForm;
    ShowHint := True;
  end;

  //FPaste
  FPaste := TSpeedButton.Create(Self);
  with FPaste do
  begin
    Parent := FInputs;
    AutoSize := True;
    Caption := rsTCEPaste;
    OnClick := @OnPasteClick;
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
    Hint := rsTCEHour;
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
    Hint := rsTCEMinute;
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
    Hint := rsTCESecond;
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
    Hint := rsTCEMillisecond;
    Constraints.MinWidth := Width*2;
    Value := FValue.ValueAsArray[3];
  end;
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

procedure TTimeCodeEdit.OnPasteClick(Sender: TObject);
var
  tc: TTimeCode;
begin
  tc.TimeCodeFormat := FPasteFormat;
  tc.ValueAsString := ClipBoard.AsText;
  FHour.Value := tc.ValueAsArray[0];
  FMinute.Value := tc.ValueAsArray[1];
  FSecond.Value := tc.ValueAsArray[2];
  FMilisecond.Value := tc.ValueAsArray[3];
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
  Caption := rsTimeCodeEditor;
  OnShow := @LoadControls;
  OnCloseQuery := @OnClosing;

  FValue := Default(TTimeCode);
  FPasteFormat := DefaultTimeCodeFormatSettings;
end;

end.

