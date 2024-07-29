unit TaskBoard.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.Layouts, TaskBoard.View.Column, TaskBoard.View.Card, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Objects,
  HGM.FMX.StackAnimate;

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
    StackAnimate: TStackAnimate;
    procedure HookColumn(Frame: TFrameColumn);
    procedure HookMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure HookMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure HookMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure HookFrameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure HookFrameMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure HookFrameMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
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

  StackAnimate := TStackAnimate.Create(ScrollBoxItems.Content);
  StackAnimate.AnimationDuration := 0.1;
  ScrollBoxItems.AddObject(StackAnimate);
  StackAnimate.Orientation := TOrientation.Horizontal;
  StackAnimate.UpdateList;
  //StackAnimate.OnChangeOrder := FOnChangeOrder;
  //StackAnimate.OnEndOrder := FOnEndOrder;
  HookColumn(FrameColumn1);
  HookColumn(FrameColumn2);
  HookColumn(FrameColumn3);
  HookColumn(FrameColumn4);
end;

procedure TFormMain.HookMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Control: TControl absolute Sender;
begin
  if Button = TMouseButton.mbLeft then
    StackAnimate.StartMoving(Control.ParentControl);
end;

procedure TFormMain.HookMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  Control: TControl absolute Sender;
begin
  if ssLeft in Shift then
    StackAnimate.MoveControl(Control.ParentControl);
end;

procedure TFormMain.HookMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Control: TControl absolute Sender;
begin
  if Button = TMouseButton.mbLeft then
    StackAnimate.StopMoving(Control.ParentControl);
end;

procedure TFormMain.HookFrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Control: TControl absolute Sender;
begin
  if Button = TMouseButton.mbLeft then
    StackAnimate.StartMoving(Control);
end;

procedure TFormMain.HookFrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  Control: TControl absolute Sender;
begin
  if ssLeft in Shift then
    StackAnimate.MoveControl(Control);
end;

procedure TFormMain.HookFrameMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Control: TControl absolute Sender;
begin
  if Button = TMouseButton.mbLeft then
    StackAnimate.StopMoving(Control);
end;

procedure TFormMain.HookColumn(Frame: TFrameColumn);
begin
  Frame.LayoutContent.OnMouseDown := HookMouseDown;
  Frame.LayoutContent.OnMouseMove := HookMouseMove;
  Frame.LayoutContent.OnMouseUp := HookMouseUp;
  Frame.OnMouseDown := HookFrameMouseDown;
  Frame.OnMouseMove := HookFrameMouseMove;
  Frame.OnMouseUp := HookFrameMouseUp;
end;

end.

