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

unit LogViewer.MessageList.LogNode;

{ Datastructure holding the relevant data to display for each node. }

interface

uses
  System.Classes,

  VirtualTrees,

  DDuce.Logger.Interfaces, DDuce.Components.VirtualTrees.Node;

{$REGION 'documentation'}
{
  See this topic for more information on creating a datastructure for tree
  nodes:

  https://stackoverflow.com/questions/5365365/tree-like-datastructure-for-use-with-virtualtreeview
}
{$ENDREGION}

type
  TLogNode = class(TPersistent)
  private
    FId          : Int64;
    FText        : string; // warning/info/error
    FVTNode      : PVirtualNode;
    FValueName   : string; // variables
    FValueType   : string;
    FValue       : string;
    FMessageType : TLogMessageType;
    FLogLevel    : Byte;
    FMessageData : TStream; // binary data stream for bitmaps, etc.
    FTimeStamp   : TDateTime;
    FHighlighter : string; // highlighter to use in text editor

  protected
    {$REGION 'property access methods'}
    function GetHighlighter: string;
    procedure SetHighlighter(const Value: string);
    function GetLogLevel: Byte;
    procedure SetLogLevel(const Value: Byte);
    function GetMessageData: TStream;
    procedure SetMessageData(const Value: TStream);
    function GetTimeStamp: TDateTime;
    procedure SetTimeStamp(const Value: TDateTime);
    function GetId: Int64;
    procedure SetId(const Value: Int64);
    function GetMessageType: TLogMessageType;
    procedure SetMessageType(const Value: TLogMessageType);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetVTNode: PVirtualNode;
    procedure SetVTNode(const Value: PVirtualNode);
    function GetValue: string;
    procedure SetValue(const Value: string);
    function GetValueName: string;
    procedure SetValueName(const Value: string);
    function GetValueType: string;
    procedure SetValueType(const Value: string);
    {$ENDREGION}

  public
    procedure BeforeDestruction; override;

    property Id: Int64
      read GetId write SetId;

    property Highlighter: string
      read GetHighlighter write SetHighlighter;

    property LogLevel: Byte
      read GetLogLevel write SetLogLevel;

    property Text: string
      read GetText write SetText;

    property ValueName: string
      read GetValueName write SetValueName;

    property Value: string
      read GetValue write SetValue;

    property ValueType: string
      read GetValueType write SetValueType;

    property MessageType : TLogMessageType
      read GetMessageType write SetMessageType;

    property MessageData: TStream
      read GetMessageData write SetMessageData;

    property TimeStamp: TDateTime
      read GetTimeStamp write SetTimeStamp;

    property VTNode: PVirtualNode
      read GetVTNode write SetVTNode;
  end;

implementation

uses
  System.SysUtils;

{$REGION 'construction and destruction'}
procedure TLogNode.BeforeDestruction;
begin
  if Assigned(FMessageData) then
    FreeAndNil(FMessageData);
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogNode.GetHighlighter: string;
begin
  Result := FHighlighter;
end;

procedure TLogNode.SetHighlighter(const Value: string);
begin
  FHighlighter := Value;
end;

function TLogNode.GetId: Int64;
begin
  Result := FId;
end;

procedure TLogNode.SetId(const Value: Int64);
begin
  FId := Value;
end;

function TLogNode.GetLogLevel: Byte;
begin
  Result := FLogLevel;
end;

procedure TLogNode.SetLogLevel(const Value: Byte);
begin
  FLogLevel := Value;
end;

function TLogNode.GetMessageData: TStream;
begin
  Result := FMessageData;
end;

procedure TLogNode.SetMessageData(const Value: TStream);
begin
  FMessageData := Value;
end;

function TLogNode.GetMessageType: TLogMessageType;
begin
  Result := FMessageType;
end;

procedure TLogNode.SetMessageType(const Value: TLogMessageType);
begin
  FMessageType := Value;
end;

function TLogNode.GetText: string;
begin
  Result := FText;
end;

procedure TLogNode.SetText(const Value: string);
begin
  FText := Value;
end;

function TLogNode.GetTimeStamp: TDateTime;
begin
  Result := FTimeStamp;
end;

procedure TLogNode.SetTimeStamp(const Value: TDateTime);
begin
  FTimeStamp := Value;
end;

function TLogNode.GetValue: string;
begin
  Result := FValue;
end;

procedure TLogNode.SetValue(const Value: string);
begin
  FValue := Value;
end;

function TLogNode.GetValueName: string;
begin
  Result := FValueName;
end;

procedure TLogNode.SetValueName(const Value: string);
begin
  FValueName := Value;
end;

function TLogNode.GetValueType: string;
begin
  Result := FValueType;
end;

procedure TLogNode.SetValueType(const Value: string);
begin
  FValueType := Value;
end;

function TLogNode.GetVTNode: PVirtualNode;
begin
  Result := FVTNode;
end;

procedure TLogNode.SetVTNode(const Value: PVirtualNode);
begin
  FVTNode := Value;
end;
{$ENDREGION}

end.
