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

unit LogViewer.Factories.Toolbars;

interface

uses
  System.Classes, System.SysUtils,
  Vcl.Controls, Vcl.Menus, Vcl.ActnList, Vcl.ComCtrls, Vcl.Toolwin,

  LogViewer.Interfaces;

const
  DEFAULT_EDGE_BORDERS = [ebLeft, ebTop, ebRight, ebBottom];
  DEFAULT_EDGE_INNER   = esNone;
  DEFAULT_EDGE_OUTER   = esNone;
  DEFAULT_TRANSPARANT  = True;

type
  TLogViewerToolbarsFactory = class(TInterfacedObject, ILogViewerToolbarsFactory)
  strict private
    FActions : ILogViewerActions;
    FMenus   : ILogViewerMenus;

    FEdgeBorders : TEdgeBorders;
    FEdgeInner   : TEdgeStyle;
    FEdgeOuter   : TEdgeStyle;
    FTransparant : Boolean;

    procedure ApplyDefaultProperties(
      AToolbar : TToolbar
    );

    function CreateToolButton(
       AParent    : TToolBar;
       AAction    : TBasicAction = nil;
       AStyle     : TToolButtonStyle = tbsButton;
       APopupMenu : TPopupMenu = nil
    ): TToolButton; overload;

    function CreateToolButton(
      AParent           : TToolBar;
      const AActionName : string;
      AStyle            : TToolButtonStyle = tbsButton;
      APopupMenu        : TPopupMenu = nil
    ): TToolButton; overload;

    class procedure OnDropdownMenuButtonClick(Sender: TObject);

  public
    procedure AfterConstruction; override;

    constructor Create(
      AActions : ILogViewerActions;
      AMenus   : ILogViewerMenus
    );

    function CreateMainToolbar(
        AOwner  : TComponent;
        AParent : TWinControl
    ): TToolbar;

    property EdgeBorders: TEdgeBorders
      read FEdgeBorders write FEdgeBorders default DEFAULT_EDGE_BORDERS;

    property EdgeInner: TEdgeStyle
      read FEdgeInner write FEdgeInner default DEFAULT_EDGE_INNER;

    property EdgeOuter: TEdgeStyle
      read FEdgeOuter write FEdgeOuter default DEFAULT_EDGE_OUTER;

    property Transparant: Boolean
      read FTransparant write FTransparant default DEFAULT_TRANSPARANT;
  end;

implementation

uses
  Spring;

{$REGION 'construction and destruction'}
procedure TLogViewerToolbarsFactory.AfterConstruction;
begin
  inherited AfterConstruction;
  FEdgeBorders := DEFAULT_EDGE_BORDERS;
  FEdgeInner   := DEFAULT_EDGE_INNER;
  FEdgeOuter   := DEFAULT_EDGE_OUTER;
  FTransparant := DEFAULT_TRANSPARANT;
end;

constructor TLogViewerToolbarsFactory.Create(AActions: ILogViewerActions;
  AMenus: ILogViewerMenus);
begin
  inherited Create;
  Guard.CheckNotNull(AActions, 'AActions');
  Guard.CheckNotNull(AMenus, 'AMenus');
  FActions := AActions;
  FMenus   := AMenus;
end;
{$ENDREGION}

{$REGION 'private methods'}
procedure TLogViewerToolbarsFactory.ApplyDefaultProperties(AToolbar: TToolbar);
begin
  Guard.CheckNotNull(AToolbar, 'AToolbar');
  AToolbar.EdgeBorders := EdgeBorders;
  AToolbar.EdgeInner   := EdgeInner;
  AToolbar.EdgeOuter   := EdgeOuter;
  AToolbar.Transparent := Transparant;
  AToolbar.ParentColor := True;
  AToolbar.AutoSize    := True;
  AToolbar.ShowHint    := True;
end;

function TLogViewerToolbarsFactory.CreateToolButton(AParent: TToolBar;
  const AActionName: string; AStyle: TToolButtonStyle; APopupMenu: TPopupMenu)
  : TToolButton;
begin
  if AActionName = '' then
    Result := CreateToolButton(AParent)
  else
    Result :=
      CreateToolButton(AParent, FActions[AActionName], AStyle, APopupMenu);
end;

class procedure TLogViewerToolbarsFactory.OnDropdownMenuButtonClick(Sender: TObject);
begin
  (Sender as TToolButton).CheckMenuDropdown;
end;

function TLogViewerToolbarsFactory.CreateToolButton(AParent: TToolBar;
  AAction: TBasicAction; AStyle: TToolButtonStyle; APopupMenu: TPopupMenu)
  : TToolButton;
var
  TB : TToolButton;
  N  : Integer;
begin
  TB := TToolButton.Create(AParent);
  N := AParent.ButtonCount - 1;
  if N > -1 then
    TB.Left := AParent.Buttons[N].Left + AParent.Buttons[N].Width
  else
    TB.Left := 0;
  TB.Parent := AParent;
  if not Assigned(AAction) then
  begin
    TB.Style := tbsSeparator;
    TB.Width := 4;
  end
  else
  begin
    TB.Style := AStyle;
    if Assigned(APopupMenu) then
    begin
      TB.Style        := tbsDropDown;
      TB.DropdownMenu := APopupMenu;
      TB.OnClick      := OnDropdownMenuButtonClick;
    end;
    TB.Action := AAction;
  end;
  AParent.Realign;
  Result := TB;
end;
{$ENDREGION}

{$REGION 'public methods'}
function TLogViewerToolbarsFactory.CreateMainToolbar(AOwner: TComponent;
  AParent: TWinControl): TToolbar;
var
  TB : TToolbar;
begin
  Guard.CheckNotNull(AOwner, 'AOwner');
  Guard.CheckNotNull(AParent, 'AParent');
  TB := TToolBar.Create(AOwner);
  ApplyDefaultProperties(TB);
  TB.Parent           := AParent;
  TB.Images           := FActions.ActionList.Images;
  TB.ButtonWidth      := 10;
  TB.AllowTextButtons := True;
  CreateToolButton(TB, 'actToggleAlwaysOnTop');
  CreateToolButton(TB, 'actToggleFullScreen');
  CreateToolButton(TB, 'actStart');
  CreateToolButton(TB, 'actStop');
  CreateToolButton(TB, 'actClearMessages');
  CreateToolButton(TB, 'actAbout');
  CreateToolButton(TB);
  CreateToolButton(
    TB,
    'actMessageTypesMenu',
    tbsDropDown,
    FMenus.MessageTypesPopupMenu
  );
//  CreateToolButton(TB, 'actInfo', tbsDropDown);
//  CreateToolButton(TB, 'actWarning', tbsTextButton);
//  CreateToolButton(TB, 'actError', tbsTextButton);
//  CreateToolButton(TB, 'actConditional', tbsTextButton);
//  CreateToolButton(TB, 'actEcxeption', tbsTextButton);
//  CreateToolButton(TB);
//  CreateToolButton(TB, 'actCheckPoint', tbsTextButton);
//  //CreateToolButton(TB, 'actCallStack', tbsTextButton);
//  CreateToolButton(TB);
//  CreateToolButton(TB, 'actValue', tbsTextButton);
//  CreateToolButton(TB, 'actStrings', tbsTextButton);
//  CreateToolButton(TB, 'actComponent', tbsTextButton);
//  CreateToolButton(TB, 'actObject', tbsTextButton);
//  CreateToolButton(TB, 'actPersistent', tbsTextButton);
//  CreateToolButton(TB, 'actInterface', tbsTextButton);
//  CreateToolButton(TB, 'actBitmap', tbsTextButton);
//  //CreateToolButton(TB, 'actMemory', tbsTextButton);
//  //CreateToolButton(TB, 'actHeapInfo', tbsTextButton);
//  CreateToolButton(TB, 'actCustomData', tbsTextButton);
////  CreateToolButton(TB);
//  CreateToolButton(TB, 'actMethodTraces', tbsTextButton);
//  CreateToolButton(TB);
//  CreateToolButton(TB, 'actFilterMessages');
//  CreateToolButton(TB);
//  CreateToolButton(TB, 'actSetFocusToFilter');
//  CreateToolButton(TB);
  CreateToolButton(TB, 'actCollapseAll', tbsTextButton);
  CreateToolButton(TB, 'actExpandAll', tbsTextButton);
  CreateToolButton(TB);
  CreateToolButton(TB, 'actAutoScrollMessages', tbsTextButton);
  Result := TB;
end;
{$ENDREGION}

end.
