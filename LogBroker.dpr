program LogBroker;



{$R *.dres}

uses
  Vcl.Forms,
  LogBroker.MainForm in 'LogBroker.MainForm.pas' {frmMain},
  LogBroker.Settings in 'LogBroker.Settings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
