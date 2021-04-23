unit uSimpleHelp;
{ A simple help form with collapsible headings + images suppor
  to write per form, short but effective help topics without going
  into CHM complications and huge documentations that most users ignore.

  Copyright (C) 2021 Mohammadreza Bahrami m.audio91@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 3 of the License, or (at your
  option) any later version with the following exception:

  As a special exception to the GNU Lesser General Public License
  version 3 ("LGPL3"), the copyright holders of this Library give you
  permission to convey to a third party a Combined Work that links statically
  or dynamically to this Library without providing any Minimal Corresponding
  Source or Minimal Application Code as set out in 4d or providing
  the installation information set out in section 4e, provided that you comply
  with the other provisions of LGPL3 and provided that you meet, for
  the Application the terms and conditions of the license(s) which apply to
  the Application.
  Except as stated in this special exception, the provisions of LGPL3 will
  continue to comply in full to this Library. If you modify this Library,
  you may apply this exception to your version of this Library, but you are not
  obliged to do so. If you do not wish to do so, delete this exception statement
  from your version. This exception does not (and cannot) modify any license
  terms which apply to the Application, with which you must still comply.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, uUrlLabel, LCLType, CommonGUIUtils, CommonStrUtils,
  uMinimalList;

type

  { TChildsClass }

  TChildsClass = specialize TMinimalList<TControl>;

  { TCollapsibleHeading }

  TCollapsibleHeading = class(TCustomUrlLabel)
  public
    Index: Integer;
    Childs: TChildsClass;
    destructor Destroy; override;
  end;

  { TSimpleHelp }

  TSimpleHelp = class(TForm)
  protected
    procedure DoShow; override;
  private
    FAllowWrap: Boolean;
    FHeader: TPanel;
    FTitle: TLabel;
    FContents: TPanel;
    FContentsContainer: TScrollBox;
    FHeadings: TChildsClass;
    FHeadingColor: TColor;
    FContentSpacing: Byte;
    FContentSpacingApplied: Boolean;
    procedure LoadControls;
    procedure DoAlignControls;
    procedure OnHeadingClick(Sender: TObject);
    procedure SetAllowWrap(AValue: Boolean);
    procedure SetBiDiModeContents(AValue: TBiDiMode);
    procedure SetContentSpacing(AValue: Byte);
    procedure ApplyContentSpacing;
    procedure SetTitle(const aTitle: String);
    procedure SetHeaderColor(const aColor: TColor);
    procedure SetTitleColor(const aColor: TColor);
    procedure SetHeadingColor(const aColor: TColor);
    function NewHeading(const aTitle: String): TCollapsibleHeading;
    function FindHeading(const aTitle: String): TCollapsibleHeading;
  public
    property Title: String write SetTitle;
    property HeaderColor: TColor write SetHeaderColor; 
    property TitleColor: TColor write SetTitleColor;
    property HeadingColor: TColor read FHeadingColor write SetHeadingColor;
    property BiDiModeContents: TBiDiMode write SetBiDiModeContents;
    property ContentSpacing: Byte read FContentSpacing write SetContentSpacing;
    property AllowWrap: Boolean read FAllowWrap write SetAllowWrap;
    procedure Clear;
    procedure AddSection(const aTitle: String);
    procedure Add(const aValue: String; Bold: Boolean = False);
    procedure AddCollapsible(const aTitle, aDescription: String); virtual; overload;
    procedure AddCollapsible(const aTitle: String; aBitmap: TBitmap); virtual; abstract; overload;
    procedure AddImage(aBitmap: TBitmap); virtual; abstract;
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;
  end;

resourcestring
  rsHelp = 'راهنما';

implementation

{ TCollapsibleHeading }

destructor TCollapsibleHeading.Destroy;
begin
  if Assigned(Childs) then
    Childs.Free;
  inherited Destroy;
end;


{ TSimpleHelp }

procedure TSimpleHelp.Clear;
var
  i: Integer;
begin
  FHeadings.Clear;
  for i:=FContents.ComponentCount-1 downto 0 do
    FContents.Components[i].Free;
  FContentSpacingApplied := False;
end;

procedure TSimpleHelp.DoShow;
begin
  inherited DoShow;
  CheckDisplayInScreen(Self);
  ApplyContentSpacing;
  DoAlignControls;
end;

procedure TSimpleHelp.LoadControls;
begin
  FHeader := TPanel.Create(Self);
  with FHeader do
  begin
    Color := clHighlight;
    Caption := '';
  end;
  FTitle := TLabel.Create(Self);
  with FTitle do
  begin
    Caption := rsHelp;
    if HasDarkBackgroundColor(FHeader) then
      Font.Color := $00FFFFFF
    else
      Font.Color := $00000000;
    Font.Style:=Font.Style+[fsBold];
  end;
  FContentsContainer := TScrollBox.Create(Self);
  FContents := TPanel.Create(Self);
  FHeadings := TChildsClass.Create(Self);
end;

procedure TSimpleHelp.DoAlignControls;
var
  i: Integer;
begin
  with FHeader do
  begin
    Parent := Self;
    AutoSize := True;
    Align := alTop;
    BevelOuter := bvNone;
  end;
  with FTitle do
  begin
    Parent := FHeader;
    Align := alTop;
    Alignment := taCenter;
  end;
  with FContentsContainer do
  begin
    Parent := Self;
    Align := alClient;
    BorderStyle := bsNone;
    if FAllowWrap then
      HorzScrollBar.Visible := False
    else
    begin
      with HorzScrollBar do
      begin
        Increment := 4;
        Smooth := True;
        Tracking := True;
        Position := 0;
      end;
    end;
    with VertScrollBar do
    begin
      Increment := 4;
      Smooth := True;
      Tracking := True;
      Position := 0;
    end;
  end;
  with FContents do
  begin 
    Parent := FContentsContainer;
    Align := alTop;
    AutoSize := True;
    BevelOuter := bvNone;
    BorderSpacing.Left := 4;
    BorderSpacing.Right := 4;
    ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
    ChildSizing.ShrinkHorizontal := crsHomogenousChildResize;
    ChildSizing.ControlsPerLine := 1;
    ChildSizing.LeftRightSpacing := 4;
  end;
end;

procedure TSimpleHelp.SetTitle(const aTitle: String);
begin
  if aTitle<>FTitle.Caption then
    FTitle.Caption := aTitle;
end;

procedure TSimpleHelp.SetHeaderColor(const aColor: TColor);
begin
  if aColor<>FHeader.Color then
    FHeader.Color := aColor;
end;

procedure TSimpleHelp.SetTitleColor(const aColor: TColor);
begin
  if aColor<>FTitle.Font.Color then
    FTitle.Font.Color := aColor;
end;

procedure TSimpleHelp.SetHeadingColor(const aColor: TColor);
begin
  if aColor<>FHeadingColor then
    FHeadingColor := aColor;
end;

procedure TSimpleHelp.AddSection(const aTitle: String);
var     
  b: TDividerBevel;
  s: TLabel;
begin
  b := TDividerBevel.Create(FContents);
  with b do
  begin
    Parent := FContents;
  end;
  if IsEmptyStr(aTitle) then Exit;
  s := TLabel.Create(FContents);
  with s do
  begin
    Parent := FContents;
    Caption := aTitle;
    Font.Style:=Font.Style+[fsBold];
    Alignment := taCenter;
    BorderSpacing.Bottom := 4;
  end;
end;

procedure TSimpleHelp.Add(const aValue: String; Bold: Boolean = False);
var
  d: TLabel;
begin
  if IsEmptyStr(aValue) then Exit;
  d := TLabel.Create(FContents);
  with d do
  begin
    Parent := FContents;
    Caption := AddLineEndings(aValue,ContentSpacing,True);
    if FAllowWrap then
      WordWrap := True;
    if Bold then
      Font.Style:=Font.Style+[fsBold];
  end;
end;

function TSimpleHelp.NewHeading(const aTitle: String): TCollapsibleHeading;
begin
  Result := TCollapsibleHeading.Create(FContents);
  with Result do
  begin
    Parent := FContents;
    Index := FContents.GetControlIndex(Result);
    Caption := aTitle;
    Font.Color := FHeadingColor;
    Font.Style:=Font.Style+[fsBold];
    ParentBiDiMode := True;
    Childs := TChildsClass.Create(Result);
    OnClick := @OnHeadingClick;
  end;
  FHeadings.Add(Result);
end;

function TSimpleHelp.FindHeading(const aTitle: String): TCollapsibleHeading;
var
  i: integer;
begin
  Result := nil;
  for i:=0 to FHeadings.Count-1 do
    if String((FHeadings.Items[i] as TCollapsibleHeading).Caption).Equals(aTitle) then
      Exit((FHeadings.Items[i] as TCollapsibleHeading));
end;

procedure TSimpleHelp.AddCollapsible(const aTitle,aDescription: String);
var
  h: TCollapsibleHeading;
  d: TLabel;
begin
  if IsEmptyStr(aTitle) or IsEmptyStr(aDescription) then Exit;
  h := FindHeading(aTitle);
  if not Assigned(h) then
    h := NewHeading(aTitle);
  d := TLabel.Create(FContents);
  with d do
  begin
    FContents.InsertControl(d,h.Index+h.Childs.Count+1);
    Caption := aDescription;
    if FAllowWrap then
      WordWrap := True;
    Visible := False;
  end;
  h.Childs.Add(d);
end;  

procedure TSimpleHelp.OnHeadingClick(Sender: TObject);
var
  h: TCollapsibleHeading;
  d: TControl;
  i: Integer;
begin
  h := (Sender as TCollapsibleHeading);
  for i:=0 to h.Childs.Count-1 do
  begin
    d := h.Childs.Items[i];
    with d do
    begin
      if Visible then
        Visible := False
      else
        Visible := True;
    end;
  end;
end;

procedure TSimpleHelp.SetAllowWrap(AValue: Boolean);
begin
  if FAllowWrap<>AValue then
    FAllowWrap := AValue;
end;

procedure TSimpleHelp.SetBiDiModeContents(AValue: TBiDiMode);
begin
  if FContents.BiDiMode<>AValue then
    FContents.BiDiMode := AValue;
end;

procedure TSimpleHelp.SetContentSpacing(AValue: Byte);
begin
  if FContentSpacing<>AValue then
    FContentSpacing := AValue;
end;

procedure TSimpleHelp.ApplyContentSpacing;
var
  h: TCollapsibleHeading;
  l: TLabel;
  i: Integer;
begin
  if FContentSpacingApplied then Exit;
  for i:=0 to FHeadings.Count-1 do
  begin
    h := FHeadings.Items[i] as TCollapsibleHeading;
    if (h.Childs.Count>0) then
    begin
      try
        l := h.Childs.Items[h.Childs.Count-1] as TLabel;
        l.Caption:= AddLineEndings(l.Caption,ContentSpacing,True);
      except
      end;
    end;
  end;
  FContentSpacingApplied := True;
end;

constructor TSimpleHelp.Create(AOwner: TComponent);
begin
  CreateNew(AOwner, 0);
end;

constructor TSimpleHelp.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);  
  DefaultMonitor := dmActiveForm;
  if AOwner is TForm then
  begin
    Position := poOwnerFormCenter;
    Caption := (AOwner as TForm).Caption+' - '+rsHelp;
    Width := (AOwner as TForm).Width;
    Height := (AOwner as TForm).Height;
  end
  else
  begin
    Position := poScreenCenter;
    Caption := rsHelp;
  end;
  FHeadingColor := clHighlight;
  FContentSpacing := 1;
  FContentSpacingApplied := False;
  FAllowWrap := True;
  LoadControls;
end;

destructor TSimpleHelp.Destroy;
begin
  if Assigned(FHeadings) then
    FHeadings.Free;
  inherited Destroy;
end;

  end.
