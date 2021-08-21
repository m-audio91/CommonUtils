unit uNumEditFloat;
{ A simple dialog form to edit a float number.

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
  Classes, SysUtils, uModalEditor, StdCtrls, Spin;

type

  { TNumEditFloat }

  TNumEditFloat = class (TModalEditor)
  private
    FHeaderTextL: TLabel;
    FValueEdit: TFloatSpinEdit;
    function GetDecimalPlaces: Integer;
    function GetHeaderText: String;
    function GetIncrement: Double;
    function GetMaxValue: Double;
    function GetMinValue: Double;
    function GetValue: Double;
    procedure LoadControls(Sender: TObject);
    procedure SetDecimalPlaces(AValue: Integer);
    procedure SetHeaderText(AValue: String);
    procedure SetIncrement(AValue: Double);
    procedure SetMaxValue(AValue: Double);
    procedure SetMinValue(AValue: Double);
    procedure SetValue(AValue: Double);
  public
    property Value: Double read GetValue write SetValue;
    property DecimalPlaces: Integer read GetDecimalPlaces write SetDecimalPlaces;
    property MinValue: Double read GetMinValue write SetMinValue;
    property MaxValue: Double read GetMaxValue write SetMaxValue;
    property Increment: Double read GetIncrement write SetIncrement;
    property HeaderText: String read GetHeaderText write SetHeaderText;
    constructor CreateNew(AOwner: TComponent; Num: Integer); override;
  end;

implementation

{ TNumEditFloat }

procedure TNumEditFloat.LoadControls(Sender: TObject);
begin
  //FHeaderTextL
  with FHeaderTextL do
  begin
    Parent := Self;
    Alignment := taCenter;
    BorderSpacing.Bottom := 10;
  end;

  //FValueEdit
  with FValueEdit do
  begin
    Parent := Self;
    Alignment := taCenter;
    Constraints.MinWidth := 250;
  end;
end;

function TNumEditFloat.GetHeaderText: String;
begin
  Result := FHeaderTextL.Caption;
end;

procedure TNumEditFloat.SetHeaderText(AValue: String);
begin
  if FHeaderTextL.Caption<>AValue then
    FHeaderTextL.Caption := AValue;
end;

function TNumEditFloat.GetValue: Double;
begin
  Result := FValueEdit.Value;
end;

procedure TNumEditFloat.SetValue(AValue: Double);
begin
  if FValueEdit.Value<>AValue then
    FValueEdit.Value := AValue;
end;

function TNumEditFloat.GetDecimalPlaces: Integer;
begin
  Result := FValueEdit.DecimalPlaces;
end;

procedure TNumEditFloat.SetDecimalPlaces(AValue: Integer);
begin
  if FValueEdit.DecimalPlaces<>AValue then
    FValueEdit.DecimalPlaces := AValue;
end;

function TNumEditFloat.GetMinValue: Double;
begin
  Result := FValueEdit.MinValue;
end;

procedure TNumEditFloat.SetMinValue(AValue: Double);
begin
  if FValueEdit.MinValue<>AValue then
    FValueEdit.MinValue := AValue;
end;

function TNumEditFloat.GetMaxValue: Double;
begin
  Result := FValueEdit.MaxValue;
end;

procedure TNumEditFloat.SetMaxValue(AValue: Double);
begin
  if FValueEdit.MaxValue<>AValue then
    FValueEdit.MaxValue := AValue;
end;

function TNumEditFloat.GetIncrement: Double;
begin
  Result := FValueEdit.Increment;
end;

procedure TNumEditFloat.SetIncrement(AValue: Double);
begin
  if FValueEdit.Increment<>AValue then
    FValueEdit.Increment := AValue;
end;

constructor TNumEditFloat.CreateNew(AOwner: TComponent; Num: Integer);
begin
  //Self
  inherited CreateNew(AOwner, Num);
  OnShow := @LoadControls;
  FHeaderTextL := TLabel.Create(Self);
  FHeaderTextL.Caption := EmptyStr;

  FValueEdit := TFloatSpinEdit.Create(Self); 
  FValueEdit.MinValue := MinValue.MinValue;
  FValueEdit.MaxValue := MaxValue.MaxValue;
  FValueEdit.Increment := 1;
end;

end.

