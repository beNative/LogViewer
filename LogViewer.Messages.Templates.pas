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

unit LogViewer.Messages.Templates;

interface

uses
  System.Rtti,

  Spring.Collections,

  DSharp.Windows.TreeViewPresenter, DSharp.Windows.ColumnDefinitions,
  DSharp.Windows.ControlTemplates,
  DSharp.Core.DataTemplates, DSharp.Windows.ColumnDefinitions.ControlTemplate,

  LogViewer.Messages.Data;

type
  TLogTemplate = class(TColumnDefinitionsControlTemplate)
  private
    FMessages : IList<TLogMessageData>;

  public
    constructor Create(
      AColumnDefinitions : IColumnDefinitions;
      AMessages          : IList<TLogMessageData>
    );
    function GetItemCount(const Item: TObject): Integer; override;
    function GetItems(const Item: TObject): IObjectList; override;
  end;

  TLogMessageTemplate = class(TColumnDefinitionsControlTemplate)
  private
    FMessages : IList<TLogMessageData>;

  public
    constructor Create(
      AColumnDefinitions : IColumnDefinitions;
      AMessages          : IList<TLogMessageData>
    );
    function GetItemCount(const Item: TObject): Integer; override;
    function GetItems(const Item: TObject): IObjectList; override;
    function GetItem(const Item: TObject; const Index: Integer): TObject;
      override;

  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,

  Spring.Collections.Enumerable,

  DDuce.Logger.Interfaces, DDuce.Reflect;

{$REGION 'non-interfaced routines'}
procedure Debug(const AString: string);
begin
  OutputDebugString(PWideChar(AString));
end;
{$ENDREGION}

{$REGION 'TLogTemplate'}
constructor TLogTemplate.Create(AColumnDefinitions: IColumnDefinitions;
  AMessages: IList<TLogMessageData>);
begin
  inherited Create(AColumnDefinitions);
  FMessages := AMessages;
  RegisterDataTemplate(TLogMessageTemplate.Create(AColumnDefinitions, AMessages));
end;

{ TLogTemplate }

function TLogTemplate.GetItemCount(const Item: TObject): Integer;
begin
  Debug('TLogTemplate.GetItemCount'#13#10 + Reflect.Properties(Item).ToString);

//  if Item is IList then
//    Result := (Item as IList).Count
//  else
  Result := GetItems(Item).Count;
end;

function TLogTemplate.GetItems(const Item: TObject): IObjectList;
begin
  Result := FMessages as IObjectList;
end;
{$ENDREGION}

{$REGION 'TLogMessageTemplate'}
{$REGION 'construction and destruction'}
constructor TLogMessageTemplate.Create(AColumnDefinitions: IColumnDefinitions;
  AMessages: IList<TLogMessageData>);
begin
  inherited Create(AColumnDefinitions);
  FMessages := AMessages;
end;
{$ENDREGION}

function TLogMessageTemplate.GetItem(const Item: TObject;
  const Index: Integer): TObject;
var
  LMD : TLogMessageData;
begin
  if Item is TLogMessageData then
  begin
    Result := GetItems(Item)[Index];
  end
  else
    Result := inherited GetItem(Item, Index);

  Debug(Reflect.Properties(Result).ToString);
end;

function TLogMessageTemplate.GetItemCount(const Item: TObject): Integer;
var
  LMD : TLogMessageData;
begin
  if Item is TLogMessageData then
  begin
    LMD := Item as TLogMessageData;
    Result := LMD.Children.Count;
    if Result > 0 then
      Debug(Format('GetItemCount = %d', [Result]));
  end
  else
    Result := inherited GetItemCount(Item);
end;

function TLogMessageTemplate.GetItems(const Item: TObject): IObjectList;
var
  LMD : TLogMessageData;
  O   : IList<TLogMessageData>;
  I   : TLogMessageData;
begin
//  if Item is TLogMessageData then
//  begin
//    LMD := Item as TLogMessageData;
//    if (LMD.MessageType = lmtEnterMethod) {and (LMD.Level > 0)} then
//    begin
//      Result := LMD.Children as IObjectList
//
//    end
//    else
//      Result := inherited GetItems(Item);
//  end
//  else
  begin
    Result := inherited GetItems(Item);
  end;
end;
{$ENDREGION}

end.
