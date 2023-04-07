unit TaskBoard.View.Card;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Objects, FMX.Effects, FMX.Layouts, FMX.Ani;

type
  TFrameCard = class(TFrame)
    RectangleContent: TRectangle;
    MemoText: TMemo;
    LayoutAction: TLayout;
    Button1: TButton;
    LayoutEdit: TLayout;
    ColorAnimationOver: TColorAnimation;
    FloatAnimationActions: TFloatAnimation;
    Rectangle1: TRectangle;
    procedure FrameDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure FrameDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure FrameDragEnd(Sender: TObject);
    procedure FrameDragLeave(Sender: TObject);
    procedure FrameDragEnter(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure FrameResize(Sender: TObject);
    procedure MemoTextViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
  private
    FLastSize: Single;
  protected
    procedure BeginAutoDrag; override;
    procedure SetParent(const Value: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ChangedParent;
  end;

implementation

uses
  System.Math;

{$R *.fmx}

procedure TFrameCard.ChangedParent;
begin
  MemoText.PrepareForPaint;
  FrameResize(nil);
end;

constructor TFrameCard.Create(AOwner: TComponent);
begin
  inherited;
  FLastSize := 0;
end;

procedure TFrameCard.BeginAutoDrag;
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

procedure TFrameCard.FrameDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  if (not Assigned(ParentControl)) or (not Assigned(ParentControl.ParentControl)) then
    Exit;
  ParentControl.ParentControl.OnDragDrop(Sender, Data, Point);
end;

procedure TFrameCard.FrameDragEnd(Sender: TObject);
begin
  Visible := True;
  ParentControl.ParentControl.OnDragEnd(Sender);
end;

procedure TFrameCard.FrameDragEnter(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  if Data.Source = Self then
    Visible := False;
end;

procedure TFrameCard.FrameDragLeave(Sender: TObject);
begin
  if (not Assigned(ParentControl)) or (not Assigned(ParentControl.ParentControl)) then
    Exit;
  ParentControl.ParentControl.OnDragLeave(Sender);
end;

procedure TFrameCard.FrameDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if (not Assigned(ParentControl)) or (not Assigned(ParentControl.ParentControl)) then
    Exit;
  ParentControl.ParentControl.OnDragOver(Sender, Data, Point, Operation);
end;

procedure TFrameCard.FrameResize(Sender: TObject);
begin
  MemoText.Height := Round(MemoText.ContentBounds.Height + 2);
  var H: Single := 0;
  for var Control in RectangleContent.Controls do
    if Control.IsVisible and (Control.Align in [TAlignLayout.Top, TAlignLayout.MostTop]) then
      H := Max(H, Control.Position.Y + Control.Height + Control.Margins.Bottom);
  H := Round(H + RectangleContent.Padding.Top + RectangleContent.Padding.Bottom);
  if Height <> H then
    Height := H;
end;

procedure TFrameCard.MemoTextViewportPositionChange(Sender: TObject; const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: Boolean);
begin
  TThread.ForceQueue(nil,
    procedure
    begin
      FrameResize(nil);
    end);
end;

procedure TFrameCard.SetParent(const Value: TFmxObject);
begin
  inherited;
  FrameResize(nil);
end;

end.

