{
  Copyright (C) 2013-2020 Tim Sinaeve tim.sinaeve@gmail.com

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
  logged values. A watch is identified by the combination of its name and
  value type (both are case sensitive). }

{ This implementation is inspired by the watch implementation made by
  Luiz Américo Pereira Câmara (FPC-Lazarus) in the Multilog project. }

uses
  System.Classes, System.SysUtils,

  Spring.Collections,

  DDuce.Logger.Interfaces;

type
  TUpdateWatchEvent = procedure(
    const AName  : string;
    const AValue : string
  ) of object;

  TNewWatchEvent = procedure(
    const AName  : string;
    const AIndex : Integer // index in watchlist
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
    FFirstId          : Int64; // first message id
    FName             : string;
    FValueType        : string;
    FCurrentIndex     : Integer;
    FList             : IList<TWatchValue>;
    FOnlyTrackChanges : Boolean;
    FMessageType      : TLogMessageType; // lmtCounter or lmtWatch

    {$REGION 'property access methods'}
    function GetCount: Integer;
    function GetValue:string;
    function GetValues(AIndex: Integer): string;
    function GetTimeStamp: TDateTime;
    function GetList: IList<TWatchValue>;
    function GetCurrentWatchValue: TWatchValue;
    function GetOnlyTrackChanges: Boolean;
    procedure SetOnlyTrackChanges(const Value: Boolean);
    function GetValueType: string;
    function GetMessageType: TLogMessageType;
    {$ENDREGION}

  public
    constructor Create(
      const AName       : string;
      const AValueType  : string;
      AMessageType      : TLogMessageType;
      AFirstId          : Int64;
      AOnlyTrackChanges : Boolean = False
    );
    destructor Destroy; override;

    function AddValue(
      const AValue : string;
      AIndex       : Int64;
      ATimeStamp   : TDateTime
    ): Boolean;
    function Locate(const AId: Int64): Boolean;

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

    property MessageType: TLogMessageType
      read GetMessageType;

    { Timestamp of last received watch value. }
    property TimeStamp: TDateTime
      read GetTimeStamp;

    property Values[AIndex: Integer]: string
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

  protected
    procedure DoUpdateWatch(const AName: string; const AValue: string);
    procedure DoNewWatch(const AName: string; const AIndex: Integer);

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    function IndexOf(const AName, AValueType: string): Integer;
    procedure Add(
      const AName          : string;
      const AValueType     : string;
      const AValue         : string;
      AMessageType         : TLogMessageType;
      AId                  : Int64; // Unique Id of the logmessage
      ATimeStamp           : TDateTime;
      AOnlyTrackChanges    : Boolean = False;
      ASkipOnNewWatchEvent : Boolean = False // used for counter support
    );
    procedure Clear;
    procedure Update(const AIndex: Integer);

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

uses
  DDuce.Logger;

{$REGION 'TWatch'}
{$REGION 'construction and destruction'}
constructor TWatch.Create(const AName: string; const AValueType: string;
  AMessageType: TLogMessageType; AFirstId: Int64; AOnlyTrackChanges: Boolean);
begin
  FList := TCollections.CreateObjectList<TWatchValue>;
  FName             := AName;
  FValueType        := AValueType;
  FMessageType      := AMessageType;
  FFirstId          := AFirstId;
  FOnlyTrackChanges := AOnlyTrackChanges;
end;

destructor TWatch.Destroy;
begin
  FList.Clear;
  FList := nil;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TWatch.GetValue: string;
begin
  if FList.Count > 0 then
    Result := FList[FCurrentIndex].Value
  else
    Result := '';
end;

function TWatch.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWatch.GetCurrentWatchValue: TWatchValue;
begin
  if FList.Count > 0 then
    Result := FList[FCurrentIndex]
  else
    Result := nil;
end;

function TWatch.GetList: IList<TWatchValue>;
begin
  Result := FList;
end;

function TWatch.GetMessageType: TLogMessageType;
begin
  Result := FMessageType;
end;

function TWatch.GetTimeStamp: TDateTime;
begin
  Result := FList[FCurrentIndex].TimeStamp;
end;

function TWatch.GetValues(AIndex: Integer): string;
begin
  Result := FList[AIndex].Value;
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
function TWatch.AddValue(const AValue: string; AIndex: Int64; ATimeStamp:
  TDateTime): Boolean;
var
  LItem : TWatchValue;
  B     : Boolean;
begin
  B := (not OnlyTrackChanges) or (OnlyTrackChanges and
    (not Assigned(CurrentWatchValue) or
      (Assigned(CurrentWatchValue) and (AValue <> CurrentWatchValue.Value)
      )
    )
  );
  if B then
  begin
    LItem := TWatchValue.Create;
    LItem.Id        := AIndex;
    LItem.Value     := AValue;
    LItem.TimeStamp := ATimeStamp;
    FList.Add(LItem);
    Result := True;
  end
  else
    Result := False;
end;

{ Locate current watchvalue for a given message Id.
  TODO: hide watch when value does not exist for the corresponding Id! }

function TWatch.Locate(const AId: Int64): Boolean;
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
  Logger.Track(Self, 'AfterConstruction');
  inherited AfterConstruction;
  FList := TCollections.CreateObjectList<TWatch>;
end;

destructor TWatchList.Destroy;
begin
  Logger.Track(Self, 'Destroy');
  FList.Clear;
  FList := nil;
  inherited Destroy;
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

{$REGION 'protected methods'}
procedure TWatchList.DoNewWatch(const AName: string; const AIndex: Integer);
begin
  if Assigned(FOnNewWatch) then
    FOnNewWatch(AName, AIndex);
end;

procedure TWatchList.DoUpdateWatch(const AName, AValue: string);
begin
  if Assigned(FOnUpdateWatch) then
    FOnUpdateWatch(AName, AValue);
end;
{$ENDREGION}

{$REGION 'public methods'}
function TWatchList.IndexOf(const AName, AValueType: string): Integer;
var
  W : TWatch;
begin
  if FList.TryGetSingle(W,
    function(const AWatchVariable: TWatch): Boolean
    begin
      Result := (AWatchVariable.Name = AName)
        and (AWatchVariable.ValueType = AValueType);
    end
  ) then
    Result := FList.IndexOf(W)
  else
    Result := -1;
end;

procedure TWatchList.Add(const AName: string; const AValueType: string;
  const AValue: string; AMessageType: TLogMessageType; AId: Int64;
  ATimeStamp: TDateTime; AOnlyTrackChanges: Boolean;
  ASkipOnNewWatchEvent: Boolean);
var
  I : Integer;
begin
  I := IndexOf(AName, AValueType);
  if I = -1 then
  begin
    I := FList.Add(
      TWatch.Create(AName, AValueType, AMessageType, AId, AOnlyTrackChanges)
    );
    if not ASkipOnNewWatchEvent then
      DoNewWatch(AName, I);
  end;
  FList[I].AddValue(AValue, AId, ATimeStamp);
end;

procedure TWatchList.Clear;
begin
  FList.Clear;
end;

procedure TWatchList.Update(const AIndex: Integer);
var
  W : TWatch;
begin
  for W in FList do
  begin
    if W.Locate(AIndex) then
    begin
      DoUpdateWatch(W.Name, W.Value);
    end
  end;
end;
{$ENDREGION}
{$ENDREGION}

end.

