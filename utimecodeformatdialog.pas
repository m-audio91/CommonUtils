unit uTimeCodeFormatDialog;
{ A dialog form to define format for further TimeCode inputs. such as paste from
  clipboard, read from file etc.

  Copyright (C) 2018 Mohammadreza Bahrami m.audio91@gmail.com

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
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, Graphics, Controls, Spin,
  uTimeCode, uModalEditor, CommonGUIUtils;

type

  { TTimeCodeFormatDialog }

  TTimeCodeFormatDialog = class(TModalEditor)
  protected
    FValue: TTimeCode;
    procedure OnFormatChange(Sender: TObject); virtual;
  private
    FInputs: TPanel;
    FMajorSep: TComboBox;
    FMinorSep: TComboBox;
    FMilisecondPrecision: TSpinEdit;
    FSample: TLabel;
    FFileSample: TLabel;
    procedure LoadControls(Sender: TObject);
    procedure ShowFileSample;
  public
    property Value: TTimeCode read FValue;
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

resourcestring
  rsTimeCodeFormatDialog = 'انتخابگر ساختار کدزمان ها';
  rsTimeSep = 'جدا کننده ساعت، دقیقه و ثانیه';
  rsMillisecSep = 'جدا کننده ثانیه و هزارم ثانیه';
  rsMillisecPrecision = 'دقت هزارم ثانیه';

implementation

{ TTimeCodeFormatDialog }

procedure TTimeCodeFormatDialog.LoadControls(Sender: TObject);
var
  c: Char;
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
    ChildSizing.ControlsPerLine := 3;
    ChildSizing.HorizontalSpacing := 10;
    Color := clForm;
    ShowHint := True;
  end;

  //FMajorSep
  FMajorSep := TComboBox.Create(Self);
  with FMajorSep do
  begin
    Parent := FInputs;
    for c in AllowedTimeCodeSeps do
      Items.Add(c);
    ItemIndex := Items.IndexOf(DefaultTimeSep);
    Style := csDropDownList;
    Hint := rsTimeSep;
    OnChange := @OnFormatChange;
  end;

  //FMinorSep
  FMinorSep := TComboBox.Create(Self);
  with FMinorSep do
  begin
    Parent := FInputs;
    for c in AllowedTimeCodeSeps do
      Items.Add(c);
    ItemIndex := Items.IndexOf(DefaultMillisecSep);
    Style := csDropDownList;
    Hint := rsMillisecSep;
    OnChange := @OnFormatChange;
  end;

  //FMilisecondPrecision
  FMilisecondPrecision := TSpinEdit.Create(Self);
  with FMilisecondPrecision do
  begin
    Parent := FInputs;
    MinValue := 0;
    MaxValue := 9;
    Alignment := taCenter;
    Increment := 1;
    Hint := rsMillisecPrecision;
    Constraints.MinWidth := Trunc(Width*1.5);
    Value := DefaultMillisecondPrecision;
    OnChange := @OnFormatChange;
  end;

  //FSample
  FSample := TLabel.Create(Self);
  with FSample do
  begin
    Parent := Self;
    Alignment := taCenter;
    Caption := FValue.ValueAsString;
    ParentFont := False;
    Font.Height := Self.Canvas.GetTextHeight(Caption)*3;
  end;

  //FFileSample
  FFileSample := TLabel.Create(Self);
  with FFileSample do
  begin
    Parent := Self;
  end;
  ShowFileSample;
end;

procedure TTimeCodeFormatDialog.OnFormatChange(Sender: TObject);
begin
  try
    FValue.Initialize(FMilisecondPrecision.Value,
      FMajorSep.Items[FMajorSep.ItemIndex].Chars[0],
      FMinorSep.Items[FMinorSep.ItemIndex].Chars[0]);
  except
    on E: Exception do
    begin
      ShowError(E.Message, 'Error');
      FMajorSep.ItemIndex := FMajorSep.Items.IndexOf(DefaultTimeSep);
      FMinorSep.ItemIndex := FMinorSep.Items.IndexOf(DefaultMillisecSep);
      FMilisecondPrecision.Value := DefaultMillisecondPrecision;
      FValue.Initialize(FMilisecondPrecision.Value,
        FMajorSep.Items[FMajorSep.ItemIndex].Chars[0],
        FMinorSep.Items[FMinorSep.ItemIndex].Chars[0]);
    end;
  end;
  FSample.Caption := FValue.ValueAsString;
  ShowFileSample;
end;

procedure TTimeCodeFormatDialog.ShowFileSample;
var
  s: String;
begin
  s := FValue.ValueAsString;
  s := s+'-'+s + LineEnding;
  s := s+s+s;
  FFileSample.Caption := s;
end;

constructor TTimeCodeFormatDialog.CreateNew(AOwner: TComponent; Num: Integer);
begin
  //Self
  inherited CreateNew(AOwner, Num);
  Caption := rsTimeCodeFormatDialog;
  OnShow := @LoadControls;

  FValue := TConstantTimeCodes.Min;
end;

end.

