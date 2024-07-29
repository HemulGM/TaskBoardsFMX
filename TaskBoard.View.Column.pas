unit TaskBoard.View.Column;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation, FMX.Objects, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TFrameColumn = class(TFrame)
    VertScrollBoxItems: TVertScrollBox;
    LayoutContent: TLayout;
    LayoutHead: TLayout;
    RectangleBG: TRectangle;
    LabelTitle: TLabel;
    LayoutDropCard: TLayout;
    Rectangle1: TRectangle;
    LayoutFooter: TLayout;
    ButtonAdd: TButton;
    TimerScroll: TTimer;
    procedure VertScrollBoxItemsDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure VertScrollBoxItemsDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure VertScrollBoxItemsCalcContentBounds(Sender: TObject; var ContentBounds: TRectF);
    procedure VertScrollBoxItemsDragEnd(Sender: TObject);
    procedure VertScrollBoxItemsDragLeave(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure TimerScrollTimer(Sender: TObject);
  private
    procedure ShowDropCard(Position: TPointF; Height: Single);
    procedure HideDropCard;
    procedure CheckScroll;
    procedure StopScroll;
  protected
    procedure BeginAutoDrag; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  System.Math, TaskBoard.View.Card;

{$R *.fmx}

constructor TFrameColumn.Create(AOwner: TComponent);
begin
  inherited;
  VertScrollBoxItems.AniCalculations.Animation := True;
end;

procedure TFrameColumn.FrameResize(Sender: TObject);
begin
  var H: Single := 0;
  for var Control in VertScrollBoxItems.Content.Controls do
    if Control.IsVisible then
      H := Max(H, Control.Position.Y + Control.Height + Control.Margins.Bottom);
  H := Max(5, H) + LayoutHead.Height + LayoutFooter.Height + LayoutContent.Padding.Bottom + LayoutContent.Padding.Top;
  H := Round(Max(Min(Height, H), 100));
  if LayoutContent.Height <> H then
    LayoutContent.Height := H;
end;

procedure TFrameColumn.HideDropCard;
begin
  StopScroll;
  LayoutDropCard.Visible := False;
  LayoutDropCard.Align := TAlignLayout.None;
end;

procedure TFrameColumn.BeginAutoDrag;
const
  DraggingOpacity = 1;
var
  B, S: TBitmap;
  R: TRectF;
begin
  if Root <> nil then
  begin
    S := MakeScreenshot;
    S.Rotate(5);
    try
      B := nil;
      try
        if (S.Width > 512) or (S.Height > 512) then
        begin
          R := TRectF.Create(0, 0, S.Width, S.Height);
          R.Fit(TRectF.Create(0, 0, 512, 512));
          B := TBitmap.Create(Round(R.Width), Round(R.Height));
          B.Clear(0);
          if B.Canvas.BeginScene then
          try
            B.Canvas.DrawBitmap(S, TRectF.Create(0, 0, S.Width, S.Height), TRectF.Create(0, 0, B.Width, B.Height),
              DraggingOpacity, False);
          finally
            B.Canvas.EndScene;
          end;
        end
        else
        begin
          B := TBitmap.Create(S.Width, S.Height);
          B.Clear(0);
          if B.Canvas.BeginScene then
          try
            B.Canvas.DrawBitmap(S, TRectF.Create(0, 0, B.Width, B.Height), TRectF.Create(0, 0, B.Width, B.Height),
              DraggingOpacity, False);
          finally
            B.Canvas.EndScene;
          end;
        end;
        Root.BeginInternalDrag(Self, B);
      finally
        B.Free;
      end;
    finally
      S.Free;
    end;
  end;
end;

procedure TFrameColumn.ShowDropCard(Position: TPointF; Height: Single);
begin
  LayoutDropCard.Position.Y := Position.Y - Height / 2 + VertScrollBoxItems.ViewportPosition.Y;
  LayoutDropCard.Height := Height;
  LayoutDropCard.Align := TAlignLayout.Top;
  LayoutDropCard.Visible := True;
  CheckScroll;
end;

procedure TFrameColumn.VertScrollBoxItemsCalcContentBounds(Sender: TObject; var ContentBounds: TRectF);
begin
  FrameResize(nil);
end;

procedure TFrameColumn.VertScrollBoxItemsDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  if Data.Source is TFrameCard then
  begin
    TFrameCard(Data.Source).Parent := VertScrollBoxItems;
    TFrameCard(Data.Source).Visible := True;
    TFrameCard(Data.Source).Position.Y := LayoutDropCard.Position.Y;
    TFrameCard(Data.Source).ChangedParent;
  end;
  HideDropCard;
end;

procedure TFrameColumn.VertScrollBoxItemsDragEnd(Sender: TObject);
begin
  HideDropCard;
end;

procedure TFrameColumn.VertScrollBoxItemsDragLeave(Sender: TObject);
begin
  HideDropCard;
end;

procedure TFrameColumn.StopScroll;
begin
  TimerScroll.Enabled := False;
end;

procedure TFrameColumn.TimerScrollTimer(Sender: TObject);
begin
  var MPos := VertScrollBoxItems.ScreenToLocal(Screen.MousePos);
  if MPos.Y > VertScrollBoxItems.Height - 40 then
  begin
    VertScrollBoxItems.ViewportPosition := VertScrollBoxItems.ViewportPosition + TPointF.Create(0, 5);
  end
  else if MPos.Y < 40 then
  begin
    VertScrollBoxItems.ViewportPosition := VertScrollBoxItems.ViewportPosition + TPointF.Create(0, -5);
  end
  else
  begin
    MPos := ParentControl.ScreenToLocal(Screen.MousePos);
    if MPos.X > ParentControl.Height - 40 then
    begin
      if ParentControl is TScrollBox then
        TScrollBox(ParentControl).AniCalculations.ViewportPosition := TScrollBox(ParentControl).AniCalculations.ViewportPosition + TPointF.Create(5, 0);
    end
    else if MPos.X < 40 then
    begin
      if ParentControl is TScrollBox then
        TScrollBox(ParentControl).AniCalculations.ViewportPosition := TScrollBox(ParentControl).AniCalculations.ViewportPosition + TPointF.Create(-5, 0);
    end
    else
      StopScroll;
  end;
end;

procedure TFrameColumn.CheckScroll;
begin
  TimerScroll.Enabled := True;
end;

procedure TFrameColumn.VertScrollBoxItemsDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if Data.Source is TFrameCard then
  begin
    Operation := TDragOperation.Move;
    var Card := TFrameCard(Data.Source);
    ShowDropCard(VertScrollBoxItems.AbsoluteToLocal(Application.MainForm.ScreenToClient(Screen.MousePos)), Card.Height);
  end;
end;

end.

