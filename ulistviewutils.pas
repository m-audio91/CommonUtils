unit uListViewUtils;
{ Utility to work with standard ListView. DragDrop sorting items, delete, save,
  load, etc all supporting multi-select.

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
  Classes, SysUtils, LCLType, Controls, ComCtrls;

type
  TListItemDynArray = array of TListItem;

  { TListViewUtils }

  TListViewUtils = class
  private
    class var FDragStartPoint: TPoint;
    function MovingConditionsMet(LV: TListView): Boolean;
    function GetSelectedItems(LV: TListView): TListItemDynArray;
    function IndexOfFirstSelected(LV: TListView): Integer;
    function IndexOfLastSelected(LV: TListView): Integer;
    procedure SelectItems(Items: TListItemDynArray);
    procedure MoveItems(LV: TListView; DestIndex: Integer);
  public
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MoveItemsUp(LV: TListView);
    procedure MoveItemsDown(LV: TListView);
    procedure MoveItemsTop(LV: TListView);
    procedure MoveItemsBottom(LV: TListView);
    procedure ToggleCheckItems(LV: TListView);
    procedure ToggleSelectAll(LV: TListView);
    procedure DeleteItems(LV: TListView);
    function GetItems(LV: TListView; const ASep: Char): String; overload;
    procedure SetItems(LV: TListView; const ASettings: String;
      const ASep: Char); overload;
    function GetItems(LV: TListView): TStringArray; overload;
    procedure SetItems(LV: TListView; const ASettings: TStringArray); overload;
  end;

var
  ListViewUtils: TListViewUtils;

implementation

function TListViewUtils.MovingConditionsMet(LV: TListView): Boolean;
begin
  Result := True;
  if (LV.Items.Count < 1) or (LV.SelCount < 1) or (LV.SelCount = LV.Items.Count) then
    Result := False;
end;

function TListViewUtils.GetSelectedItems(LV: TListView): TListItemDynArray;
var
  i,j: Integer;
begin
  SetLength(Result, LV.SelCount);
  j := 0;
  for i := 0 to LV.Items.Count-1 do
    if LV.Items[i].Selected then
    begin
      Result[j] := LV.Items[i];
      Inc(j);
    end;
end;

function TListViewUtils.IndexOfFirstSelected(LV: TListView): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to LV.Items.Count-1 do
    if LV.Items[i].Selected then
      Exit(i);
end;

function TListViewUtils.IndexOfLastSelected(LV: TListView): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := LV.Items.Count-1 downto 0 do
    if LV.Items[i].Selected then
      Exit(i);
end;

procedure TListViewUtils.SelectItems(Items: TListItemDynArray);
var
  i: Integer;
begin
  for i := High(Items) downto 0 do
    Items[i].Selected := True;
end;

procedure TListViewUtils.MoveItems(LV: TListView; DestIndex: Integer);
const
  PH = '.PlaceHolder.';
var
  TheItems: TListItemDynArray;
  i: Integer;
begin
  with LV do
  begin
    TheItems := GetSelectedItems(LV);
    ClearSelection;
    for i := 0 to High(TheItems) do
    begin
      Items.Add;
      Items[Items.Count-1].Caption := PH;
      Items.Exchange(TheItems[i].Index, Items.Count-1);
      TheItems[i] := Items[Items.Count-1];
    end;
    for i := 0 to High(TheItems) do
      Items.Move(TheItems[i].Index, DestIndex+i);
    for i := 0 to High(TheItems) do
      Items.Delete(Items.FindCaption(0, PH, False, True, False).Index);
    SelectItems(TheItems);
  end;
end;

procedure TListViewUtils.DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  LV: TListView;
  Dest: TListItem;
begin
  if (Sender <> Source) then Exit;
  LV := (Sender as TListView);
  with LV do
  begin
    if not MovingConditionsMet(LV) then Exit;
    Dest := GetItemAt(X, Y);
    if not Assigned(Dest) then
    begin
      if (Y > FDragStartPoint.y) then
        MoveItemsBottom(LV)
      else
        MoveItemsTop(LV);
    end
    else
    begin
      BeginUpdate;
      try
        MoveItems(LV, Dest.Index);
        SetFocus;
      finally
        EndUpdate;
      end;
    end;
  end;
end;

procedure TListViewUtils.DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Sender = Source;
end;

procedure TListViewUtils.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragStartPoint := Point(X,Y);
  {$ifdef LINUX}
    if not Assigned((Sender as TListView).GetItemAt(X, Y)) then
      (Sender as TListView).ClearSelection;
  {$endif};
end;

procedure TListViewUtils.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if not (ssLeft in Shift) then
    (Sender as TListView).EndDrag(False);
end;

procedure TListViewUtils.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    DeleteItems(Sender as TListView);
end;

procedure TListViewUtils.MoveItemsUp(LV: TListView);
var
  i: Integer;
begin
  with LV do
  begin
    if not MovingConditionsMet(LV) then Exit;
    i := IndexOfFirstSelected(LV);
    Dec(i);
    if i < 0 then
      i := 0;
    BeginUpdate;
    try
      MoveItems(LV, i);
    finally
      EndUpdate;
    end;
    SetFocus;
  end;
end;

procedure TListViewUtils.MoveItemsDown(LV: TListView);
var
  i: Integer;
begin
  with LV do
  begin
    if not MovingConditionsMet(LV) then Exit;
    i := IndexOfLastSelected(LV);
    Inc(i, 2);
    if i > Items.Count then
      i := Items.Count;
    BeginUpdate;
    try
      MoveItems(LV, i);
    finally
      EndUpdate;
    end;
    SetFocus;
  end;
end;

procedure TListViewUtils.MoveItemsTop(LV: TListView);
begin
  with LV do
  begin
    if not MovingConditionsMet(LV) then Exit;
    BeginUpdate;
    try
      MoveItems(LV, 0);
    finally
      EndUpdate;
    end;
    SetFocus;
  end;
end;

procedure TListViewUtils.MoveItemsBottom(LV: TListView);
begin
  with LV do
  begin
    if not MovingConditionsMet(LV) then Exit;
    BeginUpdate;
    try
      MoveItems(LV, Items.Count);
    finally
      EndUpdate;
    end;
    SetFocus;
  end;
end;

procedure TListViewUtils.ToggleCheckItems(LV: TListView);
var
  TheItems: TListItemDynArray;
  OnCheck: TLVCheckedItemEvent;
  i: Integer;
  Check: Boolean;
begin
  with LV do
  begin
    if (Items.Count < 1) or (SelCount < 1) or Not CheckBoxes then Exit;
    OnCheck := OnItemChecked;
    OnItemChecked := nil;
    TheItems := GetSelectedItems(LV);
    Check := not TheItems[0].Checked;
    BeginUpdate;
    try
      for i := 0 to High(TheItems) do
        TheItems[i].Checked := Check;
    finally
      OnItemChecked := OnCheck;
      EndUpdate;
      {$IFDEF LINUX}
      Refresh;
      {$ENDIF}
    end;
    SetFocus;
  end;
end;

procedure TListViewUtils.ToggleSelectAll(LV: TListView);
begin
  with LV do
  begin
    if Items.Count < 1 then Exit;
    if SelCount > 0 then
      ClearSelection
    else
      SelectAll;
    SetFocus;
  end;
end;

procedure TListViewUtils.DeleteItems(LV: TListView);
var
  TheItems: TListItemDynArray;
  i: Integer;
begin
  with LV do
  begin
    if SelCount < 1 then Exit;
    BeginUpdate;
    try
      if SelCount = Items.Count then
        Items.Clear
      else
      begin
        TheItems := GetSelectedItems(LV);
        for i := 0 to High(TheItems) do
          Items.Delete(TheItems[i].Index);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

function TListViewUtils.GetItems(LV: TListView; const ASep: Char): String;
var
  sl : TStringList;
  i,j: Integer;
begin
  Result := EmptyStr;
  if ASep in [#0..#32] then Exit;
  with LV do
  begin
    sl := TStringList.Create;
    try
      for i := 0 to Items.Count-1 do
      begin
        if i > 0 then
          sl.Add(ASep + Items[i].Checked.ToString)
        else
          sl.Add(Items[i].Checked.ToString);
        sl.Add(Integer(Items[i].ImageIndex).ToString);
        sl.Add(Items[i].Caption);
        for j := 0 to Items[i].SubItems.Count-1 do
          sl.Add(Items[i].SubItems[j]);
      end;
      Result := sl.CommaText;
    finally
      sl.Free;
    end;
  end;
end;

procedure TListViewUtils.SetItems(LV: TListView; const ASettings: String;
  const ASep: Char);
var
  OnCheck: TLVCheckedItemEvent;
  sl: TStringList;
  sa: TStringArray;
  i,j: Integer;
begin
  if ASettings = EmptyStr then Exit;
  if ASep in [#0..#32] then Exit;
  with LV do
  begin
    OnCheck := OnItemChecked;
    OnItemChecked := nil;
    BeginUpdate;
    Clear;
    sl := TStringList.Create;
    try
      sa := ASettings.Split([ASep]);
      for i := 0 to High(sa) do
      begin
        sl.CommaText := sa[i];
        Items.Add;
        Items[i].Checked := Sl.Strings[0].ToBoolean;
        Items[i].ImageIndex := Sl.Strings[1].ToInteger;
        Items[i].Caption := Sl.Strings[2];
        for j := 3 to Sl.Count-1 do
          Items[i].SubItems.Add(Sl[j]);
      end;
    finally
      EndUpdate;
      OnItemChecked := OnCheck;
      sl.Free;
    end;
  end;
end;

function TListViewUtils.GetItems(LV: TListView): TStringArray;
var
  sl : TStringList;
  i,j: Integer;
begin
  sl := TStringList.Create;
  try
    with LV do
    begin
      SetLength(Result, Items.Count);
      for i := 0 to Items.Count-1 do
      begin
        sl.Clear;
        sl.Add(Items[i].Checked.ToString);
        sl.Add(Integer(Items[i].ImageIndex).ToString);
        sl.Add(Items[i].Caption);
        for j := 0 to Items[i].SubItems.Count-1 do
          sl.Add(Items[i].SubItems[j]);
        Result[i] := sl.CommaText;
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TListViewUtils.SetItems(LV: TListView;
  const ASettings: TStringArray);
var
  OnCheck: TLVCheckedItemEvent;
  sl: TStringList;
  i,j: Integer;
begin
  if Length(ASettings) = 0 then Exit;
  with LV do
  begin
    OnCheck := OnItemChecked;
    OnItemChecked := nil;
    BeginUpdate;
    Clear;
    sl := TStringList.Create;
    try
      for i := 0 to High(ASettings) do
      begin
        sl.CommaText := ASettings[i];
        Items.Add;
        Items[i].Checked := Sl.Strings[0].ToBoolean;
        Items[i].ImageIndex := Sl.Strings[1].ToInteger;
        Items[i].Caption := Sl.Strings[2];
        for j := 3 to Sl.Count-1 do
          Items[i].SubItems.Add(Sl[j]);
      end;
    finally
      EndUpdate;
      OnItemChecked := OnCheck;
      sl.Free;
    end;
  end;
end;

end.

