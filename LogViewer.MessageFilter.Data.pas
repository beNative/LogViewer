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
  Vcl.Graphics,

  Spring,

  VirtualTrees,

  DDuce.Logger.Interfaces;

type
  TFilterData = class
  private
    FMessageTypes  : TLogMessageTypes;
    FMessageLevels : TLogMessageLevels;
    FCaption       : string;
    FImageIndex    : Integer;
    FColor         : TColor;

    {$REGION 'property access methods'}
    function GetMessageTypes: TLogMessageTypes;
    procedure SetMessageTypes(const Value: TLogMessageTypes);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetImageIndex: Integer;
    procedure SetImageIndex(const Value: Integer);
    function GetMessageLevels: TLogMessageLevels;
    procedure SetMessageLevels(const Value: TLogMessageLevels);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    {$ENDREGION}

  public
    constructor Create(
      const ACaption : string;
      AMessageTypes : TLogMessageTypes = [];
      AImageIndex   : Integer = -1
    ); overload;

    constructor Create(
      const ACaption : string;
      AMessageLevels : TLogMessageLevels = [];
      AColor         : TColor = clNone
    ); overload;

    property MessageTypes: TLogMessageTypes
      read GetMessageTypes write SetMessageTypes;

    property MessageLevels: TLogMessageLevels
      read GetMessageLevels write SetMessageLevels;

    property Caption: string
      read GetCaption write SetCaption;

    property ImageIndex: Integer
      read GetImageIndex write SetImageIndex;

    property Color: TColor
      read GetColor write SetColor;

  end;

implementation

{$REGION 'construction and destruction'}
constructor TFilterData.Create(const ACaption: string;
  AMessageTypes: TLogMessageTypes; AImageIndex: Integer);
begin
  FMessageTypes := AMessageTypes;
  FCaption      := ACaption;
  FImageIndex   := AImageIndex;
end;

constructor TFilterData.Create(const ACaption: string;
  AMessageLevels: TLogMessageLevels; AColor: TColor);
begin
  FMessageLevels := AMessageLevels;
  FCaption       := ACaption;
  FColor         := AColor;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TFilterData.GetCaption: string;
begin
  Result := FCaption;
end;

procedure TFilterData.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

function TFilterData.GetColor: TColor;
begin
  Result := FColor;
end;

procedure TFilterData.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

function TFilterData.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

procedure TFilterData.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;

function TFilterData.GetMessageLevels: TLogMessageLevels;
begin
  Result := FMessageLevels;
end;

procedure TFilterData.SetMessageLevels(const Value: TLogMessageLevels);
begin
  FMessageLevels := Value;
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
