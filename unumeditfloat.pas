unit uNumEditFloat;
{ A simple dialog form to edit a float number.

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
  Classes, SysUtils, uModalEditor, StdCtrls, Spin;

type

  { TNumEditFloat }

  TNumEditFloat = class (TModalEditor)
  private
    FHeaderText: String;
    FValue: Double;
    FDecimalPlaces: Word;
    FHeaderTextL: TLabel;
    FValueEdit: TFloatSpinEdit;
    procedure LoadControls(Sender: TObject);
    procedure OnClosing(Sender: TObject; var CanClose: Boolean);
  public
    property DecimalPlaces: Word read FDecimalPlaces write FDecimalPlaces;
    property Value: Double read FValue write FValue;
    property HeaderText: String read FHeaderText write FHeaderText;
    constructor CreateNew(AOwner: TComponent; Num: Integer); override;
  end;

implementation

{ TNumEditFloat }

procedure TNumEditFloat.LoadControls(Sender: TObject);
begin
  //FHeaderTextL
  FHeaderTextL := TLabel.Create(Self);
  with FHeaderTextL do
  begin
    Parent := Self;
    Caption := FHeaderText;
    Alignment := taCenter;
    BorderSpacing.Bottom := 10;
  end;

  //FValueEdit
  FValueEdit := TFloatSpinEdit.Create(Self);
  with FValueEdit do
  begin
    Parent := Self;
    MinValue := MinValue.MinValue;
    MaxValue := MaxValue.MaxValue;
    DecimalPlaces := FDecimalPlaces;
    Alignment := taCenter;
    Increment := 1;
    if Canvas.TextWidth(FHeaderText) < 50 then
      Constraints.MinWidth := Trunc(Width*1.5);
    Value := FValue;
  end;
end;

procedure TNumEditFloat.OnClosing(Sender: TObject; var CanClose: Boolean);
begin
  FValue := FValueEdit.Value;
  CanClose := True;
end;

constructor TNumEditFloat.CreateNew(AOwner: TComponent; Num: Integer);
begin
  //Self
  inherited CreateNew(AOwner, Num);
  OnShow := @LoadControls;
  OnCloseQuery := @OnClosing;

  FHeaderText := EmptyStr;
  FValue := 0;
  FDecimalPlaces := 2;
end;

end.

