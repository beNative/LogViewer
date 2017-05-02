{
  Copyright (C) 2013-2017 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit LogViewer.Watches.Data;

{ Copyright (C) 2006 Luiz Américo Pereira Câmara

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

interface

{ Implements support for watches to monitor values over time. }

uses
  System.Classes, System.SysUtils,

  Spring.Collections;

type
  TUpdateWatchEvent = procedure (
    const AName  : string;
    const AValue : string
  ) of object;

  TNewWatchEvent = procedure (
    const AName : string;
    AID         : Integer
  ) of object;

  TWatchValue = class
  private
    FID        : Integer;
    FValue     : string;
    FTimeStamp : TDateTime;

  public
    property ID: Integer
      read FID write FID;

    property Value: string
      read FValue write FValue;

    property TimeStamp: TDateTime
      read FTimeStamp write FTimeStamp;
  end;

  TWatch = class
  private
    FFirstID   : Integer;
    FCurrentID : Integer;
    FName      : string;
    FList      : IList<TWatchValue>;

    function GetCount: Integer;
    function GetValue:string;
    function GetValues(AID: Integer): string;
    function GetTimeStamp: TDateTime;
    function GetList: IList<TWatchValue>;

  public
    constructor Create(
      const AName : string;
      AID         : Integer
    );

    procedure AddValue(
      const AValue : string;
      AID       : Integer;
      ATimeStamp   : TDateTime
    );
    function Find(AID: Integer): Boolean;

    property List: IList<TWatchValue>
      read GetList;

    property Name: string
      read FName;

    property Value: string
      read GetValue;

    property TimeStamp: TDateTime
      read GetTimeStamp;

    property Values[AID: Integer]: string
      read GetValues; default;

    property Count: Integer
      read GetCount;
  end;

  { TWatchList }

  TWatchList = class
  private
    FList          : IList<TWatch>;
    FOnNewWatch    : TNewWatchEvent;
    FOnUpdateWatch : TUpdateWatchEvent;

    function GetCount: Integer;
    function GetItems(AValue: Integer): TWatch;
    function GetList: IList<TWatch>;

  public
    procedure AfterConstruction; override;

    function IndexOf(const AName: string): Integer;
    procedure Add(
      const AName          : string;
      AID                  : Integer; // ID of the logmessage
      ATimeStamp           : TDateTime;
      ASkipOnNewWatchEvent : Boolean = False // used for counter support
    );
    procedure Clear;
    procedure Update(AID: Integer);

    property List: IList<TWatch>
      read GetList;

    property Items[AValue: Integer]: TWatch
      read GetItems; default;

    property Count: Integer
      read GetCount;

    property OnUpdateWatch: TUpdateWatchEvent
      read FOnUpdateWatch write FOnUpdateWatch;

    property OnNewWatch: TNewWatchEvent
      read FOnNewWatch write FOnNewWatch;
  end;

implementation

{$REGION 'TWatch'}
{$REGION 'construction and destruction'}
constructor TWatch.Create(const AName: string; AID: Integer);
begin
  FList := TCollections.CreateObjectList<TWatchValue>;
  FName := AName;
  FFirstID := AID;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWatch.GetValue: string;
begin
  Result := FList[FCurrentID].Value;
end;

function TWatch.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWatch.GetList: IList<TWatchValue>;
begin
  Result := FList;
end;

function TWatch.GetTimeStamp: TDateTime;
begin
  Result := FList[FCurrentID].TimeStamp;
end;

function TWatch.GetValues(AID: Integer): string;
begin
  Result := FList[AID].Value;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TWatch.AddValue(const AValue: string; AID: Integer; ATimeStamp:
  TDateTime);
var
  Item : TWatchValue;
begin
  Item := TWatchValue.Create;
  Item.ID        := AID;
  Item.Value     := AValue;
  Item.TimeStamp := ATimeStamp;
  FList.Add(Item);
end;

function TWatch.Find(AID: Integer): Boolean;
var
  I : Integer;
begin
  Result := False;
  if AID < FFirstID then
    Exit;
  for I := FList.Count - 1 downto 0 do
  begin
    if AID >= FList[I].ID then
    begin
      Result := True;
      FCurrentID := I;
      Exit;
    end;
  end;
end;
{$ENDREGION}
{$ENDREGION}

{$REGION 'TWatchList'}
{$REGION 'construction and destruction'}
procedure TWatchList.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TCollections.CreateObjectList<TWatch>;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWatchList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWatchList.GetItems(AValue: Integer): TWatch;
begin
  Result := FList[AValue];
end;

function TWatchList.GetList: IList<TWatch>;
begin
  Result := FList;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TWatchList.IndexOf(const AName: string): Integer;
var
  W : TWatch;
begin
  if FList.TryGetSingle(W,
    function(const AWatchVariable: TWatch): Boolean
    begin
      Result := AWatchVariable.Name = AName;
    end
  ) then
    Result := FList.IndexOf(W)
  else
    Result := -1;
end;

procedure TWatchList.Add(const AName: string; AID: Integer; ATimeStamp
  : TDateTime; ASkipOnNewWatchEvent: Boolean);
var
  PosEqual : Integer;
  I        : Integer;
  S        : string;
begin
  PosEqual := Pos('=', AName);
  S := Copy(AName, 1, PosEqual - 1);
  I := IndexOf(S);
  if I = -1 then
  begin
    I := FList.Add(TWatch.Create(S, AID));
    if not ASkipOnNewWatchEvent then
      FOnNewWatch(S, I);
  end;
  S := Copy(AName, PosEqual + 1, Length(AName) - PosEqual);
  FList[I].AddValue(S, AID, ATimeStamp);
end;

procedure TWatchList.Clear;
begin
  FList.Clear;
end;

procedure TWatchList.Update(AID: Integer);
var
  W : TWatch;
begin
  if Assigned(FOnUpdateWatch) then
  begin
    for W in FList do
    begin
      if W.Find(AID) then
        FOnUpdateWatch(W.Name, W.Value);
    end;
  end;
end;
{$ENDREGION}
{$ENDREGION}

end.

