unit uFreeEditor;
{ A dialog form to work with anything you like. it's ChildSizing is set. just
  create your controls after you've called CreateNew and set
  parent and owner of your newly created controls to MyFreeEditor instance.
  the layout is top to bottom so the creation order matters.
  also see uTimeCodeEdit for an example of a descendant class.

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
  Classes, SysUtils, GraphType, Graphics, Forms, Controls, StdCtrls, ExtCtrls;

type
  { TFreeEditor }

  TFreeEditor = class(TForm)
  protected
    procedure DoShow; override;
  private
    FActions: TPanel;
    FOk: TButton;
    FCancel: TButton;
    procedure LoadDefaultControls;
    procedure UpdateFormSize;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

resourcestring
  rsTFEOK = 'تایید';
  rsTFECancel = 'صرف نظر';

implementation

{ TFreeEditor }

procedure TFreeEditor.DoShow;
begin
  inherited DoShow;
  LoadDefaultControls;
  UpdateFormSize;
end;

procedure TFreeEditor.LoadDefaultControls;
begin
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
end;

procedure TFreeEditor.UpdateFormSize;
var
  w,h: Integer;
begin
  AutoSize := True;
  w := Width;
  h := Height;
  AutoSize := False;
  Width := w;
  Constraints.MinWidth := w;
  Height := h;
  Constraints.MinHeight := h;
end;

constructor TFreeEditor.Create(AOwner: TComponent);
begin
  CreateNew(AOwner, 0);
end;

constructor TFreeEditor.CreateNew(AOwner: TComponent; Num: Integer);
begin
  //Self
  inherited CreateNew(AOwner, Num);
  DefaultMonitor := dmActiveForm;
  if AOwner is TForm then
    Position := poOwnerFormCenter
  else
      Position := poScreenCenter;
  Caption := Application.Title;
  ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
  ChildSizing.ShrinkHorizontal := crsHomogenousChildResize;
  ChildSizing.ControlsPerLine := 1;
  ChildSizing.LeftRightSpacing := 8;
  ChildSizing.TopBottomSpacing := 8;
end;

end.

