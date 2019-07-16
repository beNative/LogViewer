unit LogViewer.DisplayValues.Settings.ValueManager;

interface

{ The ValueManager class allows us to customize the property
  inspector used in the settings dialog. }

uses
  System.Classes, System.Rtti, System.Types,
  Vcl.Graphics,

  zObjInspTypes, zValueManager;

type
  TDisplayValuesValueManager = class(TzCustomValueManager)
  public
    procedure SetValue(
      const PItem : PPropItem;
      var Value   : TValue
    ); override;

    function HasButton(const PItem: PPropItem): Boolean; override;
    function HasList(const PItem: PPropItem): Boolean; override;
    function HasDialog(const PItem: PPropItem): Boolean; override;

    function GetDialog(const PItem: PPropItem): TComponentClass; override;
    function DialogResultValue(
      const PItem : PPropItem;
      Dialog      : TComponent
    ): TValue; override;

    procedure GetListItems(
      const PItem : PPropItem;
      Items       : TStrings
    ); override;
  end;

implementation

uses
  System.StrUtils,
  Vcl.Dialogs,

  Spring.Container,

  DDuce.Settings.TextFormat, DDuce.Logger;

function TDisplayValuesValueManager.DialogResultValue(
  const PItem: PPropItem; Dialog: TComponent): TValue;
var
  S  : string;
  TF : TTextFormatSettings;
begin
  S := TFontDialog(Dialog).Font.Name;
  Result := GetValue(PItem, S);
  TF := (PItem.Component) as TTextFormatSettings;
  TF.FontColor := TFontDialog(Dialog).Font.Color;
  TF.FontSize  := TFontDialog(Dialog).Font.Size;
  TF.FontStyle := TFontDialog(Dialog).Font.Style;
end;

function TDisplayValuesValueManager.GetDialog(
  const PItem: PPropItem): TComponentClass;
begin
  if MatchStr(PItem.Name, ['FontName']) then
    Result := TFontDialog
  else
    Result := inherited GetDialog(PItem)
end;

procedure TDisplayValuesValueManager.GetListItems(const PItem: PPropItem;
  Items: TStrings);
begin
  inherited GetListItems(PItem, Items);
end;

function TDisplayValuesValueManager.HasButton(
  const PItem: PPropItem): Boolean;
begin
  Result := inherited HasButton(PItem);
end;

function TDisplayValuesValueManager.HasDialog(
  const PItem: PPropItem): Boolean;
begin
  if MatchStr(PItem.Name, ['FontName']) then
  begin
    Result := True;
  end
  else
    Result := inherited HasDialog(PItem);
end;

function TDisplayValuesValueManager.HasList(
  const PItem: PPropItem): Boolean;
begin
  Result := inherited HasList(PItem);
end;

procedure TDisplayValuesValueManager.SetValue(const PItem: PPropItem;
  var Value: TValue);
begin
  inherited SetValue(PItem, Value);
end;

end.


