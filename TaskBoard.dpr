program TaskBoard;

uses
  System.StartUpCopy,
  FMX.Forms,
  TaskBoard.Main in 'TaskBoard.Main.pas' {FormMain},
  TaskBoard.View.Column in 'TaskBoard.View.Column.pas' {FrameColumn: TFrame},
  TaskBoard.View.Card in 'TaskBoard.View.Card.pas' {FrameCard: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
