unit uMinimalList;
{ A minimal generic list with virtual methods. mainly for small list of records

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
  Classes, SysUtils;

type

  { TMinimalList }

  generic TMinimalList<T> = class
  private
    type TItems = specialize TArray<T>;
    type PtrT = ^T;
  strict private 
    FOwner: TObject;
    FList: TItems;
    FCount: Integer;
  private
    procedure CheckIndex(AIndex: Integer); inline;
    procedure CheckCapacity(AAmount: Integer = 50);
    procedure SetItem(AIndex: Integer; AItem: T); virtual;
    function GetItem(AIndex: Integer): T;
    procedure SetPItem(AIndex: Integer; AItem: PtrT); virtual;
    function GetPItem(AIndex: Integer): PtrT;
    function GetValue: TItems; virtual;
    procedure SetValue(AValue: TItems); virtual;
    function GetCapacity: Integer;
    procedure SetCapacity(AValue: Integer);
  public
    //this is not that kind of Owner. by defining a valid Owner we can access
    //methods and properties of the Owner class where this type is defined as
    //a local type.
    property Owner: TObject read FOwner;
    function Count: Integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    procedure Clear; virtual;
    function New: Integer; virtual;
    function Add(AItem: T): Integer; virtual;
    procedure AddItems(AItems: TItems); virtual;
    procedure Remove(AIndex: Integer); virtual;
    procedure Exchange(AFirstIndex, ASecondIndex: Integer); virtual;
    property Items[AIndex: Integer]: T read GetItem write SetItem; default;
    property PItems[AIndex: Integer]: PtrT read GetPItem write SetPItem;
    property Value: TItems read GetValue write SetValue;
    constructor Create(AOwner: TObject = nil); virtual;
    destructor Destroy; override;
  end;

  EMinimalListError = class(Exception);

implementation


{ TMinimalList }

procedure TMinimalList.CheckIndex(AIndex: Integer);
begin
  if (AIndex < Low(FList)) or (AIndex > Count-1) then
    raise EMinimalListError.Create('Out of bounds index: '+AIndex.ToString);
end;

procedure TMinimalList.CheckCapacity(AAmount: Integer);
begin
  if (GetCapacity-Count) < AAmount then
    SetCapacity(GetCapacity+AAmount);
end;

procedure TMinimalList.SetItem(AIndex: Integer; AItem: T);
begin
  CheckIndex(AIndex);
  FList[AIndex] := AItem;
end;

function TMinimalList.GetItem(AIndex: Integer): T;
begin
  CheckIndex(AIndex);
  Result := FList[AIndex];
end;

procedure TMinimalList.SetPItem(AIndex: Integer; AItem: PtrT);
begin
  CheckIndex(AIndex);
  FList[AIndex] := AItem^;
end;

function TMinimalList.GetPItem(AIndex: Integer): PtrT;
begin
  CheckIndex(AIndex);
  Result := @FList[AIndex];
end;

function TMinimalList.GetValue: TItems;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count-1 do
    Result[i] := FList[i];
end;

procedure TMinimalList.SetValue(AValue: TItems);
var
  i: Integer;
begin
  Clear;
  CheckCapacity(Length(AValue));
  for i := 0 to High(AValue) do
    Add(AValue[i]);
end;

function TMinimalList.GetCapacity: Integer;
begin
  Result := Length(FList);
end;

procedure TMinimalList.SetCapacity(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  SetLength(FList, AValue);
end;

function TMinimalList.Count: Integer;
begin
  Result := FCount;
end;

procedure TMinimalList.Clear;
begin
  FList := nil;
  FCount := 0;
end;

function TMinimalList.New: Integer;
begin
  CheckCapacity;
  Inc(FCount);
  Result := FCount-1;
end;

function TMinimalList.Add(AItem: T): Integer;
begin
  Result := New;
  SetItem(Result, AItem);
end;

procedure TMinimalList.AddItems(AItems: TItems);
var
  i: Integer;
begin
  for i := 0 to High(AItems) do
    Add(AItems[i]);
end;

procedure TMinimalList.Remove(AIndex: Integer);
var
  i: Integer;
begin
  CheckIndex(AIndex);
  for i := AIndex+1 to Count-1 do
    FList[i-1] := FList[i];
  Dec(FCount);
end;

procedure TMinimalList.Exchange(AFirstIndex, ASecondIndex: Integer);
var
  Temp: T;
begin
  CheckIndex(AFirstIndex);
  CheckIndex(ASecondIndex);
  Temp := Items[AFirstIndex];
  PItems[AFirstIndex]^ := Items[ASecondIndex];
  PItems[ASecondIndex]^ := Temp;
end;

constructor TMinimalList.Create(AOwner: TObject);
begin
  Clear;
  if Assigned(AOwner) then
    FOwner := AOwner;
end;

destructor TMinimalList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

end.

