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

unit LogViewer.Subscribers.MIDI;

interface

uses
  System.Classes,

  Spring,

  LogViewer.Interfaces, LogViewer.Subscribers.Base,

  MidiIn;

{  Subscribe to a MIDI device }

type
  TMIDISubscriber = class(TSubscriber, ISubscriber, IMIDI)
  private
    FMidiInput : TMidiInput;
    FBuffer    : TMemoryStream;

    procedure FMidiInputMidiInput(Sender: TObject);

  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

  end;

implementation

uses
  System.SysUtils, System.AnsiStrings,

  Vcl.Dialogs,

  MidiType, Midicons,

  DDuce.Logger.Interfaces, DDuce.Logger;

type
	TEventNames  = array[1..8] of string[24];
	TSysMsgNames = array[1..16] of string[24];

const
	EventNames: TEventNames = (
		'Note Off',
		'Note On',
		'Key Aftertouch',
		'Control Change',
		'Program Change',
		'Channel Aftertouch',
		'Pitch Bend',
		'System Message'
  );
	SysMsgNames: TSysMsgNames = (
		'System Exclusive',
		'MTC Quarter Frame',
		'Song Position Pointer',
		'Song Select',
		'Undefined',
		'Undefined',
		'Tune Request',
		'System Exclusive End',
		'Timing Clock',
		'Undefined',
		'Start',
		'Continue',
		'Stop',
		'Undefined',
		'Active Sensing',
		'System Reset'
  );

	format3 = '%4.4x%4.4x   %2.2x       %2.2x    %2.2x     %s';
	format2 = '%4.4x%4.4x   %2.2x       %2.2x           %s';
	format1 = '%4.4x%4.4x   %2.2x                    %s';

function BinaryToHexList(ABin: PChar; ABinSize: Word): string;
var
	I : Word;
	C : Char;
begin
	Result := '';
	for I := 0 to ABinSize-1 do
	begin
		C := ABin^;
		Result := Result + Format('%2.2x ', [Integer(C)]);
		Inc(ABin);
	end;
end;

{ Converts MIDI event to text description. Straight out of Microsoft MIDIMON
  example. }

function MonitorMessageText(AEvent: TMyMidiEvent): string;
var
	LStatus    : Byte;
	LEventDesc : string;
	LTimeLow   : Word;
	LTimeHigh  : Word;
  //N          : Integer;
begin
	LStatus    := AEvent.MidiMessage and $F0;
	LTimeHigh  := Word(AEvent.Time div 65536);
	LTimeLow   := Word(AEvent.Time mod 65536);
	LEventDesc := 'Unrecognized MIDI Event';

	case LStatus of
    MIDI_NOTEOFF, MIDI_NOTEON, MIDI_KEYAFTERTOUCH, MIDI_CONTROLCHANGE,
    MIDI_PITCHBEND: { 3-byte events }
		begin
			{ Note on with velocity of 0 is a Note Off }
{			if (bStatus = MIDI_NOTEON) And (ThisEvent.Data2 = 0) then
				bStatus := MIDI_NOTEOFF; }
			LEventDesc := Format(format3,
				[LTimeHigh, LTimeLow,
				AEvent.MidiMessage,
				AEvent.Data1,
				AEvent.Data2,
				EventNames[((AEvent.MidiMessage-$80) div 16) + 1]]
      );
		end;
	  MIDI_PROGRAMCHANGE,	MIDI_CHANAFTERTOUCH: { 2-byte events }
		begin
			LEventDesc := Format(format2,[LTimeHigh, LTimeLow,
				AEvent.MidiMessage,
				AEvent.Data1,
				EventNames[((AEvent.MidiMessage-$80) div 16) + 1]]
      );
		end;
	  MIDI_BEGINSYSEX: { System events $F0-$FF }
		begin
			case AEvent.MidiMessage of
        MIDI_BEGINSYSEX:
        begin
          //N := AEvent.SysexLength;
          Logger.SendObject(AEvent);
          //EventDesc := 'Sysex : ' + BinaryToHexList(PWideChar(ThisEvent.Sysex), ThisEvent.SysexLength);
          LEventDesc := PWideChar(AEvent.Sysex);
          //N.ToString + ')';
          //  + BinaryToHexList(PWideChar(ThisEvent.Sysex), N);
        end;
//         EventDesc := Format('Sysex (%s): ', [ThisEvent.SysexLength.ToString])
//         + BinaryToHexList(PWideChar(ThisEvent.Sysex), ThisEvent.SysexLength);
				//EventDesc := Format('Sysex (%d): ', [ThisEvent.SysexLength]);
         //+
        // EventDesc := BinaryToHexList(PWideChar(ThisEvent.Sysex), ThisEvent.SysexLength);
			  MIDI_MTCQUARTERFRAME, MIDI_SONGSELECT: { 2-byte system events }
        begin
          LEventDesc := Format(format1, [
            LTimeHigh,
            LTimeLow,
            AEvent.MidiMessage,
            AEvent.Data1,
            SysMsgNames[(AEvent.MidiMessage and $F) + 1]
          ]);
        end;
			  MIDI_SONGPOSPTR: { 3-byte system events }
        begin
          LEventDesc := Format(format3,[LTimeHigh, LTimeLow,
            AEvent.MidiMessage,
            AEvent.Data1,
            AEvent.Data2,
            SysMsgNames[(AEvent.MidiMessage and $F) + 1]]);

        end
			  else { 1-byte system events }
        begin
          LEventDesc := Format(format1,[LTimeHigh, LTimeLow,
            AEvent.MidiMessage,
            SysMsgNames[(AEvent.MidiMessage and $F) + 1]]);
        end;
			end; // case AEvent.MidiMessage of
		end; // case MIDI_BEGINSYSEX
	end; // case LStatus of
	Result := LEventDesc;
end;

{$REGION 'construction and destruction'}
procedure TMIDISubscriber.AfterConstruction;
begin
  inherited AfterConstruction;
  FMidiInput := TMidiInput.Create(nil);
  FBuffer    := TMemoryStream.Create;
  FMidiInput.DeviceID    := SourceId;
  FMidiInput.OnMidiInput := FMidiInputMidiInput;
  FMidiInput.Open;
  FMidiInput.Start;
end;

destructor TMIDISubscriber.Destroy;
begin
  FMidiInput.Free;
  FBuffer.Free;
  inherited Destroy;
end;
{$ENDREGION}

{$REGION 'event handlers'}
procedure TMIDISubscriber.FMidiInputMidiInput(Sender: TObject);
const
  LZero : Integer = 0;
var
  LMsgType   : Integer;
  LTimeStamp : TDateTime;
  LTextSize  : Integer;
  LString    : UTF8String;
  LMidiEvent : TMyMidiEvent;
begin
  FBuffer.Clear;
  LMsgType := Integer(lmtText);

  while FMidiInput.MessageCount > 0 do
  begin
    LMidiEvent := FMidiInput.GetMidiEvent;
    try
      try
        LString    := UTF8String(MonitorMessageText(LMidiEvent));
        LTextSize  := Length(LString);

        LTimeStamp := Now;
        FBuffer.Seek(0, soFromBeginning);
        FBuffer.WriteBuffer(LMsgType, SizeOf(Integer));
        FBuffer.WriteBuffer(LTimeStamp, SizeOf(TDateTime));
        FBuffer.WriteBuffer(LTextSize, SizeOf(Integer));
        if LTextSize > 0 then
          FBuffer.WriteBuffer(LString[1], LTextSize);
        FBuffer.WriteBuffer(LZero, SizeOf(Integer));
        Receiver.DoReceiveMessage(FBuffer);
      except
        on E:Exception do
        begin
          //Logger.SendException('Hier', E);
          Logger.SendObject(E);
        end;

      end;
    finally
      { Event was dynamically created by GetMidiEvent so must
            free it here }
      FreeAndNil(LMidiEvent);
    end;
  end;
end;
{$ENDREGION}

end.




