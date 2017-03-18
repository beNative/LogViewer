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

unit LogViewer.Messages.Data;

// will be dismissed.

interface

uses
  System.Classes,

  Spring.Collections,

  DDuce.Logger.Interfaces;

type
  TLogMessageData = class
  private
    FMessageType : TLogMessageType;
    FTimeStamp   : TDateTime;
    FText        : string;
    FData        : TStream;
    FLevel       : Integer;
    FIndex       : Integer;
    FParent      : TLogMessageData;
    FChildren    : IList<TLogMessageData>;

  protected
    function GetChildren: IList<TLogMessageData>;

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property Index: Integer
      read FIndex write FIndex;

    property Parent: TLogMessageData
      read FParent write FParent;

    property Level: Integer
      read FLevel write FLevel;

    property Data: TStream
      read FData;

    property MessageType : TLogMessageType
      read FMessageType write FMessageType;

    property TimeStamp: TDateTime
      read FTimeStamp write FTimeStamp;

    property Text: string
      read FText write FText;

    property Children: IList<TLogMessageData>
      read GetChildren;

  end;

implementation

{$REGION 'construction and destruction'}
procedure TLogMessageData.AfterConstruction;
begin
  inherited AfterConstruction;
  FData := TMemoryStream.Create;
  FChildren := TCollections.CreateObjectList<TLogMessageData>(False);
end;

procedure TLogMessageData.BeforeDestruction;
begin
  FData.Free;
  FParent := nil;
  inherited BeforeDestruction;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TLogMessageData.GetChildren: IList<TLogMessageData>;
begin
  Result := FChildren;
end;
{$ENDREGION}

end.
