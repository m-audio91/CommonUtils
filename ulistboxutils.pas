unit uListBoxUtils;
{ Utility to work with standard (Check)ListBox. DragDrop sorting items, delete,
  save, load, etc all supporting multi-select.

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
  Classes, SysUtils, LCLType, Controls, ComCtrls, StdCtrls, CheckLst;

type
  TIntegerDynArray = array of Integer;

  { TListBoxUtils }

  TListBoxUtils = class
  private
    class var FDragStartPoint: TPoint;
    function MovingConditionsMet(LB: TCustomListBox): Boolean;
    function GetSelectedItems(LB: TCustomListBox): TIntegerDynArray;
    procedure SelectItems(LB: TCustomListBox; Indexes: TIntegerDynArray);
    function IndexOfFirstSelected(LB: TCustomListBox): Integer;
    function IndexOfLastSelected(LB: TCustomListBox): Integer;
    procedure MoveItems(LB: TCustomListBox; DestIndex: Integer);
  public
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MoveItemsUp(LB: TCustomListBox);
    procedure MoveItemsDown(LB: TCustomListBox);
    procedure MoveItemsTop(LB: TCustomListBox);
    procedure MoveItemsBottom(LB: TCustomListBox);
    procedure ToggleCheckItems(LB: TCustomListBox);
    procedure ToggleSelectAll(LB: TCustomListBox);
    procedure DeleteItems(LB: TCustomListBox);
    function GetItems(LB: TCustomListBox; const ASep: Char = #0): String;
    procedure SetItems(LB: TCustomListBox; const ASettings: String;
      const ASep: Char = #0);
  end;

var
  ListBoxUtils: TListBoxUtils;

implementation

function TListBoxUtils.MovingConditionsMet(LB: TCustomListBox): Boolean;
begin
  Result := True;
  if (LB.Count < 1) or (LB.SelCount < 1) or (LB.SelCount = LB.Count) then
    Result := False;
end;

function TListBoxUtils.GetSelectedItems(LB: TCustomListBox): TIntegerDynArray;
var
  i, j: Integer;
begin
  SetLength(Result, LB.SelCount);
  j := 0;
  for i := 0 to LB.Count-1 do
    if LB.Selected[i] then
    begin
      Result[j] := i;
      Inc(j);
    end;
end;

procedure TListBoxUtils.SelectItems(LB: TCustomListBox; Indexes: TIntegerDynArray);
var
  i: Integer;
begin
  for i in Indexes do
    LB.Selected[i] := True;
end;

function TListBoxUtils.IndexOfFirstSelected(LB: TCustomListBox): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to LB.Count-1 do
    if LB.Selected[i] then
      Exit(i);
end;

function TListBoxUtils.IndexOfLastSelected(LB: TCustomListBox): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := LB.Count-1 downto 0 do
    if LB.Selected[i] then
      Exit(i);
end;

procedure TListBoxUtils.MoveItems(LB: TCustomListBox; DestIndex: Integer);
const
  PH = '.PlaceHolder.';
var
  TheItems: TIntegerDynArray;
  Checks: TIntegerDynArray;
  OnCheck: TNotifyEvent;
  i, j, Cnt: Integer;
begin
  with LB do
  begin
    TheItems := GetSelectedItems(LB);
    if (LB is TCheckListBox) then
      with (LB as TCheckListBox) do
      begin
        OnCheck := OnClickCheck;
        OnClickCheck := nil;
        SetLength(Checks, Length(TheItems));
        for i := 0 to High(TheItems) do
          Checks[i] := Ord(State[TheItems[i]]);
      end;
    Cnt := Count;
    ClearSelection;
    for i in TheItems do
    begin
      j := Items.Add(PH);
      Items.Exchange(j, i);
    end;
    j := 0;
    for i := 0 to High(TheItems) do
    begin
      TheItems[i] := DestIndex+j;
      Items.Move(Cnt+j, DestIndex+j);
      Inc(j);
    end;
    if (LB is TCheckListBox) then
      with (LB as TCheckListBox) do
      begin
        for i := 0 to High(TheItems) do
          State[TheItems[i]] := TCheckBoxState(Checks[i]);
       OnClickCheck := OnCheck;
      end;
    SelectItems(LB, TheItems);
    for i := 0 to High(TheItems) do
    begin
      j := Items.IndexOf(PH);
      Items.Delete(j);
    end;
    SetFocus;
  end;
end;

procedure TListBoxUtils.DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  LB: TCustomListBox;
  i: Integer;
  IsDownward: Boolean;
begin
  if (Sender <> Source) then Exit;
  IsDownward := False;
  LB := (Sender as TCustomListBox);
  with LB do
  begin
    if not MovingConditionsMet(LB) then Exit;
    if Y > FDragStartPoint.y then
      IsDownward := True;
    i := GetIndexAtXY(X, Y);
    if i < 0 then
    begin
      if IsDownward then
        MoveItemsBottom(LB)
      else
        MoveItemsTop(LB);
    end
    else
    begin
      Items.BeginUpdate;
      try
        MoveItems(LB, i);
      finally
        Items.EndUpdate;
      end;
    end;
  end;
end;

procedure TListBoxUtils.DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Sender = Source;
end;

procedure TListBoxUtils.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragStartPoint := Point(X, Y);
end;

procedure TListBoxUtils.KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    DeleteItems(Sender as TCustomListBox);
end;

procedure TListBoxUtils.MoveItemsUp(LB: TCustomListBox);
var
  i: Integer;
begin
  with LB do
  begin
    if not MovingConditionsMet(LB) then Exit;
    i := IndexOfFirstSelected(LB);
    Dec(i);
    if i < 0 then
      i := 0;
    Items.BeginUpdate;
    try
      MoveItems(LB, i);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TListBoxUtils.MoveItemsDown(LB: TCustomListBox);
var
  i: Integer;
begin
  with LB do
  begin
    if not MovingConditionsMet(LB) then Exit;
    i := IndexOfLastSelected(LB);
    Inc(i, 2);
    if i > Count then
      i := Count;
    Items.BeginUpdate;
    try
      MoveItems(LB, i);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TListBoxUtils.MoveItemsTop(LB: TCustomListBox);
begin
  with LB do
  begin
    if not MovingConditionsMet(LB) then Exit;
    Items.BeginUpdate;
    try
      MoveItems(LB, 0);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TListBoxUtils.MoveItemsBottom(LB: TCustomListBox);
begin
  with LB do
  begin
    if not MovingConditionsMet(LB) then Exit;
    Items.BeginUpdate;
    try
      MoveItems(LB, Count);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TListBoxUtils.ToggleCheckItems(LB: TCustomListBox);
var
  TheItems: TIntegerDynArray;
  OnCheck: TNotifyEvent;
  Check: Boolean;
  i: Integer;
begin
  if not (LB is TCheckListBox) then Exit;
  with (LB as TCheckListBox) do
  begin
    if (Count < 1) or (SelCount < 1) then Exit;
    OnCheck := OnClickCheck;
    OnClickCheck := nil;
    TheItems := GetSelectedItems(LB);
    Check := not Checked[TheItems[0]];
    Items.BeginUpdate;
    try
      for i in TheItems do
        Checked[i] := Check;
    finally
      OnClickCheck := OnCheck;
      Items.EndUpdate;
    end;
  end;
end;

procedure TListBoxUtils.ToggleSelectAll(LB: TCustomListBox);
begin
  with LB do
  begin
    if Count < 1 then Exit;
    if SelCount > 0 then
      ClearSelection
    else
      SelectAll;
  end;
end;

procedure TListBoxUtils.DeleteItems(LB: TCustomListBox);
var
  TheItems: TIntegerDynArray;
  i: Integer;
begin
  with LB do
  begin
    if SelCount < 1 then Exit;
    Items.BeginUpdate;
    try
      if SelCount = Count then
        Clear
      else
      begin
        TheItems := GetSelectedItems(LB);
        for i := High(TheItems) downto 0 do
          Items.Delete(TheItems[i]);
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

function TListBoxUtils.GetItems(LB: TCustomListBox;
  const ASep: Char = #0): String;
var
  sl: TStringList;
  i: Integer;
begin
  if (LB is TCheckListBox) then
  begin
    if ASep in [#0..#32] then Exit;
    with (LB as TCheckListBox) do
    begin
      sl := TStringList.Create;
      try
        sl.AddStrings(Items);
        for i := 0 to Count-1 do
          sl[i] := Integer(Ord(State[i])).ToString + ASep + sl[i];
        Result := sl.CommaText;
      finally
        if Assigned(sl) then sl.Free;
      end;
    end;
    Exit;
  end;
  Result := LB.Items.CommaText;
end;

procedure TListBoxUtils.SetItems(LB: TCustomListBox; const ASettings: String;
  const ASep: Char);
var
  OnCheck: TNotifyEvent;
  i: Integer;
begin
  if ASettings = EmptyStr then Exit;
  if (LB is TCheckListBox) then
  begin
    if ASep in [#0..#32] then Exit;
    with (LB as TCheckListBox) do
    begin
      OnCheck := OnClickCheck;
      OnClickCheck := nil;
      Items.BeginUpdate;
      try
        Clear;
        Items.CommaText := ASettings;
        for i := 0 to Count-1 do
        begin
          State[i] := TCheckBoxState((Items[i].Split([ASep])[0]).ToInteger);
          Items[i] := Items[i].Split([ASep])[1];
        end;
      finally
        Items.EndUpdate;
        OnClickCheck := OnCheck;
      end;
    end;
    Exit;
  end;
  LB.Items.CommaText := ASettings;
end;

end.

