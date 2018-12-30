{
  Copyright (C) 2013-2019 Tim Sinaeve tim.sinaeve@gmail.com

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

unit LogViewer.MessageFilter.Data;

{ Holds fields and criteria to filter messages on. }

interface

uses
  System.Classes,

  Spring, Spring.Collections,

  VirtualTrees,

  DDuce.Logger.Interfaces;

type
  TFilterData = class
  private
    FMessageTypes : TLogMessageTypes;
    FCaption      : string;
    FImageIndex   : Integer;

    {$REGION 'property access methods'}
    function GetMessageTypes: TLogMessageTypes;
    procedure SetMessageTypes(const Value: TLogMessageTypes);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
    {$ENDREGION}
  public
    procedure AfterConstruction; override;
    constructor Create(
      const ACaption : string;
      AMessageTypes  : TLogMessageTypes;
      AImageIndex    : Integer = -1
    );

    property MessageTypes: TLogMessageTypes
      read GetMessageTypes write SetMessageTypes;

    property Caption: string
      read GetCaption write SetCaption;

    property ImageIndex: Integer
      read GetImageIndex write SetImageIndex;
  end;

implementation

{$REGION 'property access methods'}
procedure TFilterData.AfterConstruction;
begin
  inherited AfterConstruction;
end;

constructor TFilterData.Create(const ACaption: string;
  AMessageTypes: TLogMessageTypes; AImageIndex: Integer);
begin
  FMessageTypes := AMessageTypes;
  FCaption      := ACaption;
  FImageIndex   := AImageIndex;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TFilterData.GetCaption: string;
begin
  Result := FCaption;
end;

function TFilterData.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

procedure TFilterData.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;

procedure TFilterData.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

function TFilterData.GetMessageTypes: TLogMessageTypes;
begin
  Result := FMessageTypes;
end;

procedure TFilterData.SetMessageTypes(const Value: TLogMessageTypes);
begin
  FMessageTypes := Value;
end;
{$ENDREGION}
end.
