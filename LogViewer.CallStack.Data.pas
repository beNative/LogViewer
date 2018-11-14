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

unit LogViewer.CallStack.Data;

{ Class holding information shown in the callstack list. }

interface

type
  TCallStackData = class
  private
    FTitle    : string;
    FLevel    : Integer;
    FDuration : Integer;

  public
    property Level: Integer
      read FLevel write FLevel;

    property Title: string
      read FTitle write FTitle;

    property Duration: Integer
      read FDuration write FDuration;
  end;

implementation

end.
