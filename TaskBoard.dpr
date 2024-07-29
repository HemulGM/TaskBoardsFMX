program TaskBoard;

uses
  System.StartUpCopy,
  FMX.Forms,
  TaskBoard.Main in 'TaskBoard.Main.pas' {FormMain},
  TaskBoard.View.Column in 'TaskBoard.View.Column.pas' {FrameColumn: TFrame},
  TaskBoard.View.Card in 'TaskBoard.View.Card.pas' {FrameCard: TFrame},
  DarkModeApi.Consts in 'WindowDarkMode\DarkModeApi.Consts.pas',
  DarkModeApi.FMX in 'WindowDarkMode\DarkModeApi.FMX.pas',
  DarkModeApi in 'WindowDarkMode\DarkModeApi.pas',
  DarkModeApi.Types in 'WindowDarkMode\DarkModeApi.Types.pas',
  HGM.FMX.StackAnimate in 'StackAnimate\HGM.FMX.StackAnimate.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
