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

unit LogViewer.Settings.Dialog.Data;

interface

uses
  Vcl.ComCtrls;

type
  TConfigData = class
  private
    FText     : string;
    FTabSheet : TTabSheet;

  protected
    {$REGION 'property access methods'}
    function GetText: string;
    procedure SetText(const Value: string);
    {$ENDREGION}

  public
    constructor Create(
      const AText : string = '';
      ATabSheet   : TTabSheet = nil
    );

    property Text: string
      read GetText write SetText;

    property TabSheet: TTabSheet
      read FTabSheet write FTabSheet;
  end;

implementation

{$REGION 'construction and destruction'}
constructor TConfigData.Create(const AText: string; ATabSheet: TTabSheet);
begin
  inherited Create;
  FText     := AText;
  FTabSheet := ATabSheet;
end;
{$ENDREGION}

{$REGION 'property access methods'}
function TConfigData.GetText: string;
begin
  Result := FText;
end;

procedure TConfigData.SetText(const Value: string);
begin
  FText := Value;
end;
{$ENDREGION}

end.
