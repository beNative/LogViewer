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

unit LogViewer.CallStack.Data;

{ Class holding information shown in the callstack list. }

interface

uses
  LogViewer.MessageList.LogNode;

type
  TCallStackData = class
  private
    FTitle    : string;
    FLevel    : Integer;
    FDuration : Integer;
    FNode1    : TLogNode;
    FNode2    : TLogNode;

  public
    constructor Create(
      ANode1       : TLogNode;
      ANode2       : TLogNode;
      const ATitle : string = '';
      ALevel       : Integer = 0;
      ADuration    : Integer = 0
    );

    property Level: Integer
      read FLevel write FLevel;

    property Title: string
      read FTitle write FTitle;

    property Duration: Integer
      read FDuration write FDuration;

    property Node1: TLogNode
      read FNode1 write FNode1;

    property Node2: TLogNode
      read FNode2 write FNode2;
  end;

implementation

{$REGION 'construction and destruction'}
constructor TCallStackData.Create(ANode1: TLogNode; ANode2: TLogNode;
  const ATitle: string; ALevel, ADuration: Integer);
begin
  inherited Create;
  FNode1    := ANode1;
  FNode2    := ANode2;
  FTitle    := ATitle;
  FLevel    := ALevel;
  FDuration := ADuration;
end;
{$ENDREGION}

end.
