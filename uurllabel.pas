unit uUrlLabel;
{ A simple TLabel which opens it's Caption in systems default browser on click

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
  Classes, SysUtils, Controls, StdCtrls, Graphics, LclIntf;

type

  { TUrlLabel }

  TUrlLabel = class(TLabel)
  public
    procedure Click; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    constructor Create(TheAOwner: TComponent); override;
  end;

implementation

{ TUrlLabel }

procedure TUrlLabel.Click;
begin
  inherited Click;
  if Caption <> EmptyStr then
    OpenURL(Caption);
end;

procedure TUrlLabel.MouseEnter;
begin
  Inherited MouseEnter;
  Font.Style := Font.Style+[fsUnderline];
  Cursor := crHandPoint;
end;

procedure TUrlLabel.MouseLeave;
begin
  inherited MouseLeave;
  Font.Style := Font.Style-[fsUnderline];
  Cursor := crDefault;
end;

constructor TUrlLabel.Create(TheAOwner: TComponent);
begin
  inherited Create(TheAOwner);
  ParentBidiMode := False;
  BiDiMode := bdLeftToRight;
end;

end.

