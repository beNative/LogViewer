{
  Copyright (C) 2013-2018 Tim Sinaeve tim.sinaeve@gmail.com

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

interface

{ Implements support for watches. A watch is an object used to keep track of
  a variable's value over time. Each watch holds a list of its historical
  logged values. }

{ This implementation is inspired by the watch implementation made by
  Luiz Américo Pereira Câmara (FPC-Lazarus). }

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
    AId         : Int64
  ) of object;

  TWatchValue = class
  private
    FId        : Int64;
    FValue     : string;
    FTimeStamp : TDateTime;

  public
    property Id: Int64
      read FId write FId;

    property Value: string
      read FValue write FValue;

    property TimeStamp: TDateTime
      read FTimeStamp write FTimeStamp;
  end;

  TWatch = class
  private
    FFirstId          : Int64;
    FCurrentId        : Int64;
    FName             : string;
    FValueType        : string;
    FList             : IList<TWatchValue>;
    FOnlyTrackChanges : Boolean;

    {$REGION 'property access methods'}
    function GetCount: Integer;
    function GetValue:string;
    function GetValues(AId: Int64): string;
    function GetTimeStamp: TDateTime;
    function GetList: IList<TWatchValue>;
    function GetCurrentWatchValue: TWatchValue;
    function GetOnlyTrackChanges: Boolean;
    procedure SetOnlyTrackChanges(const Value: Boolean);
    function GetValueType: string;
    {$ENDREGION}

  public
    constructor Create(
      const AName       : string;
      const AValueType  : string;
      AFirstId          : Int64;
      AOnlyTrackChanges : Boolean = False
    );
    procedure BeforeDestruction; override;

    function AddValue(
      const AValue : string;
      AId          : Int64;
      ATimeStamp   : TDateTime
    ): Boolean;
    function Find(AId: Int64): Boolean;

    { Watch history list. }
    property List: IList<TWatchValue>
      read GetList;

    { Unique watch name. Typically this is the name of the variable you wish to
      monitor. }
    property Name: string
      read FName;

    property ValueType: string
      read GetValueType;

    { Last received watch value. }
    property Value: string
      read GetValue;

    { Timestamp of last received watch value. }
    property TimeStamp: TDateTime
      read GetTimeStamp;

    property Values[AId: Int64]: string
      read GetValues; default;

    { History list count. }
    property Count: Integer
      read GetCount;

    property OnlyTrackChanges: Boolean
      read GetOnlyTrackChanges write SetOnlyTrackChanges;

    property CurrentWatchValue: TWatchValue
      read GetCurrentWatchValue;
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
      const AValueType     : string;
      const AValue         : string;
      AId                  : Int64; // Unique Id of the logmessage
      ATimeStamp           : TDateTime;
      AOnlyTrackChanges    : Boolean = False;
      ASkipOnNewWatchEvent : Boolean = False // used for counter support
    );
    procedure Clear;
    procedure Update(AId: Integer);

    property List: IList<TWatch>
      read GetList;

    property Items[AValue: Integer]: TWatch
      read GetItems; default;

    property Count: Integer
      read GetCount;

    // not used yet
    property OnUpdateWatch: TUpdateWatchEvent
      read FOnUpdateWatch write FOnUpdateWatch;

    // not used yet
    property OnNewWatch: TNewWatchEvent
      read FOnNewWatch write FOnNewWatch;
  end;

implementation

{$REGION 'TWatch'}
{$REGION 'construction and destruction'}
procedure TWatch.BeforeDestruction;
begin
  FList.Clear;
  inherited BeforeDestruction;

end;

constructor TWatch.Create(const AName: string; const AValueType: string;
  AFirstId: Int64; AOnlyTrackChanges: Boolean);
begin
  FList := TCollections.CreateObjectList<TWatchValue>;
  FName             := AName;
  FValueType        := AValueType;
  FFirstId          := AFirstId;
  FOnlyTrackChanges := AOnlyTrackChanges;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWatch.GetValue: string;
begin
  Result := FList[FCurrentId].Value;
end;

function TWatch.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWatch.GetCurrentWatchValue: TWatchValue;
begin
  if FList.Count > 0 then
    Result := FList[FCurrentId]
  else
    Result := nil;
end;

function TWatch.GetList: IList<TWatchValue>;
begin
  Result := FList;
end;

function TWatch.GetTimeStamp: TDateTime;
begin
  Result := FList[FCurrentId].TimeStamp;
end;

function TWatch.GetValues(AId: Int64): string;
begin
  Result := FList[AId].Value;
end;

function TWatch.GetValueType: string;
begin
  Result := FValueType;
end;

function TWatch.GetOnlyTrackChanges: Boolean;
begin
  Result := FOnlyTrackChanges;
end;

procedure TWatch.SetOnlyTrackChanges(const Value: Boolean);
begin
  FOnlyTrackChanges := Value;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TWatch.AddValue(const AValue: string; AId: Int64; ATimeStamp:
  TDateTime): Boolean;
var
  Item : TWatchValue;
  B    : Boolean;
begin
  B := (not OnlyTrackChanges) or (OnlyTrackChanges and
    (not Assigned(CurrentWatchValue) or
      (Assigned(CurrentWatchValue) and (AValue <> CurrentWatchValue.Value)
      )
    )
  );
  if B then
  begin
    Item := TWatchValue.Create;
    Item.Id        := AId;
    Item.Value     := AValue;
    Item.TimeStamp := ATimeStamp;
    FList.Add(Item);
    Result := True;
  end
  else
    Result := False;
end;

function TWatch.Find(AId: Int64): Boolean;
var
  I : Integer;
begin
  Result := False;
  if AId < FFirstId then
    Exit;
  for I := FList.Count - 1 downto 0 do
  begin
    if AId >= FList[I].Id then
    begin
      Result := True;
      FCurrentId := I;
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

procedure TWatchList.Add(const AName: string; const AValueType: string;
  const AValue: string; AId: Int64; ATimeStamp: TDateTime;
  AOnlyTrackChanges: Boolean; ASkipOnNewWatchEvent: Boolean);
var
  I : Integer;
begin
  I := IndexOf(AName);
  if I = -1 then
  begin
    I := FList.Add(TWatch.Create(AName, AValueType, AId, AOnlyTrackChanges));
    if not ASkipOnNewWatchEvent then
      FOnNewWatch(AName, I);
  end;
  FList[I].AddValue(AValue, AId, ATimeStamp);
end;

procedure TWatchList.Clear;
begin
  FList.Clear;
end;

procedure TWatchList.Update(AId: Integer);
var
  W : TWatch;
begin
  if Assigned(FOnUpdateWatch) then
  begin
    for W in FList do
    begin
      if W.Find(AId) then
        FOnUpdateWatch(W.Name, W.Value);
    end;
  end;
end;
{$ENDREGION}
{$ENDREGION}

end.

