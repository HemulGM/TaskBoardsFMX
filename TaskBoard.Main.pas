unit TaskBoard.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.Layouts, TaskBoard.View.Column, TaskBoard.View.Card, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Objects;

type
  TFormMain = class(TForm)
    StyleBook: TStyleBook;
    LayoutItems: TLayout;
    ScrollBoxItems: TScrollBox;
    FrameColumn1: TFrameColumn;
    FrameCard1: TFrameCard;
    FrameCard2: TFrameCard;
    FrameCard3: TFrameCard;
    FrameCard4: TFrameCard;
    FrameCard5: TFrameCard;
    RectangleBG: TRectangle;
    FrameColumn2: TFrameColumn;
    FrameColumn3: TFrameColumn;
    FrameCard7: TFrameCard;
    FrameCard8: TFrameCard;
    FrameCard11: TFrameCard;
    FrameCard12: TFrameCard;
    FrameCard13: TFrameCard;
    FrameColumn4: TFrameColumn;
    FrameCard14: TFrameCard;
    FrameCard15: TFrameCard;
    FrameCard16: TFrameCard;
    LayoutHeader: TLayout;
    RectangleHeaderBG: TRectangle;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  DarkModeApi.FMX;

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  SetWindowColorModeAsSystem;
  {$ENDIF}
  ScrollBoxItems.AniCalculations.Animation := True;
end;

end.

