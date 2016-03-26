{
  Copyright (C) 2013-2016 Tim Sinaeve tim.sinaeve@gmail.com

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

uses
  System.Classes, System.SysUtils,

  Spring.Collections;

type
  TWatchUpdateEvent = procedure (
    const AVariable : string;
    const AValue    : string
  ) of object;

  TNewWatchVariableEvent = procedure (
    const AVariable : string;
    AIndex          : Integer
  ) of object;

  TVariableValue = record
    Index : Integer;
    Value : string;
  end;

  { TWatchVariable }

  TWatchVariable = class
  private
    FFirstIndex   : Integer;
    FCurrentIndex : Integer;
    FName         : string;
    FList         : IList<TVariableValue>;

    function GetCount: Integer;
    function GetCurrentValue:string;
    function GetValues(AIndex: Integer): string;

  public
    constructor Create(const AName: string; AIndex: Integer);

    procedure AddValue(const AValue: string; AIndex: Integer);
    function Find (AIndex: Integer): Boolean;

    property Name: string
      read FName;

    property CurrentValue: string
      read GetCurrentValue;

    property Values[AIndex: Integer]: string
      read GetValues; default;

    property Count: Integer
      read GetCount;
  end;

  { TWatchList }

  TWatchList = class
  private
    FList          : IList<TWatchVariable>;
    FOnNewVariable : TNewWatchVariableEvent;
    FOnUpdate      : TWatchUpdateEvent;

    function GetCount: Integer;
    function GetItems(AValue: Integer): TWatchVariable;

  public
    procedure AfterConstruction; override;

    function IndexOf(const AName: string): Integer;
    procedure Add(
      const ANameValue   : string;
      AIndex             : Integer;
      ASkipOnNewVariable : Boolean
    );
    procedure Clear;
    procedure Update(AIndex: Integer);

    property OnUpdate: TWatchUpdateEvent
      read FOnUpdate write FOnUpdate;

    property OnNewVariable: TNewWatchVariableEvent
      read FOnNewVariable write FOnNewVariable;

    property Items[AValue: Integer]: TWatchVariable
      read GetItems; default;

    property Count: Integer
      read GetCount;
  end;

implementation

{$REGION 'TWatchVariable'}
{$REGION 'construction and destruction'}
constructor TWatchVariable.Create(const AName: string; AIndex: Integer);
begin
  FList := TCollections.CreateList<TVariableValue>;
  FName := AName;
  FFirstIndex := AIndex;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWatchVariable.GetCurrentValue: string;
begin
  Result := FList[FCurrentIndex].Value;
end;

function TWatchVariable.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWatchVariable.GetValues(AIndex: Integer): string;
begin
  Result := FList[AIndex].Value;
end;
{$ENDREGION}

{$REGION 'public methods'}
procedure TWatchVariable.AddValue(const AValue: string; AIndex: Integer);
var
  Item : TVariableValue;
begin
  Item.Index := AIndex;
  Item.Value := AValue;
  FList.Add(Item);
end;

function TWatchVariable.Find(AIndex: Integer): Boolean;
var
  I : Integer;
begin
  Result := False;
  if AIndex < FFirstIndex then
    Exit;
  for I := FList.Count - 1 downto 0 do
  begin
    if AIndex >= FList[I].Index then
    begin
      Result := True;
      FCurrentIndex := I;
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
  FList := TCollections.CreateObjectList<TWatchVariable>;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWatchList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWatchList.GetItems(AValue: Integer): TWatchVariable;
begin
  Result := FList[AValue];
end;
{$ENDREGION}

{$REGION 'public methods'}
function TWatchList.IndexOf(const AName: string): Integer;
var
  WV : TWatchVariable;
begin
  if FList.TryGetSingle(WV,
    function(const AWatchVariable: TWatchVariable): Boolean
    begin
      Result := AWatchVariable.Name = AName;
    end
  ) then
    Result := FList.IndexOf(WV)
  else
    Result := -1;
end;

procedure TWatchList.Add(const ANameValue: string; AIndex: Integer;
  ASkipOnNewVariable: Boolean);
var
  PosEqual : Integer;
  I        : Integer;
  S        : string;
begin
  PosEqual := Pos('=', ANameValue);
  S := Copy(ANameValue, 1, PosEqual - 1);
  I := IndexOf(S);
  if I = -1 then
  begin
    I := FList.Add(TWatchVariable.Create(S,AIndex));
    if not ASkipOnNewVariable then
      FOnNewVariable(S, I);
  end;
  S := Copy(ANameValue, PosEqual + 1, Length(ANameValue) - PosEqual);
  FList[I].AddValue(S, AIndex);
end;

procedure TWatchList.Clear;
begin
  FList.Clear;
end;

procedure TWatchList.Update(AIndex: Integer);
var
  WV : TWatchVariable;
begin
  if Assigned(FOnUpdate) then
  begin
    for WV in FList do
    begin
      if WV.Find(AIndex) then
        FOnUpdate(WV.Name, WV.CurrentValue);
    end;
  end;
end;
{$ENDREGION}
{$ENDREGION}

end.

