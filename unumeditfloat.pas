unit uNumEditFloat;

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

