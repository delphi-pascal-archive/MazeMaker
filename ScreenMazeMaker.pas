// ============================================================================
// Maze generation algorithm
// Copyright © 1999 Gary Williams
// graywolf@pcpros.net
//
// Not for commercial use without prior consent from original author
// ============================================================================
// Modified and enhanced
// efg, 27 February 1999
// www.efg2.com/lab
// ============================================================================

unit ScreenMazeMaker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ExtCtrls, Spin, ExtDlgs;

type
  TFormMazeMaker = class(TForm)
    ColorDialogFloodFill: TColorDialog;
    PanelControls: TPanel;
    LabelCellsColumns: TLabel;
    LabelCellsRows: TLabel;
    LabelCells: TLabel;
    LabelRandSeed: TLabel;
    LabelPixelsColumlns: TLabel;
    LabelPixelsRows: TLabel;
    ShapeColor: TShape;
    ButtonNewMaze: TButton;
    EditRandSeed: TEdit;
    CheckBoxShowPath: TCheckBox;
    SpinEditXCells: TSpinEdit;
    SpinEditYCells: TSpinEdit;
    SpinEditXPixels: TSpinEdit;
    SpinEditYPixels: TSpinEdit;
    ButtonPrint: TButton;
    ButtonSave: TButton;
    RadioGroupPathStyle: TRadioGroup;
    RadioGroupPathColor: TRadioGroup;
    ScrollBoxMaze: TScrollBox;
    ImageMaze: TImage;
    SavePictureDialogMaze: TSavePictureDialog;
    LabelLength: TLabel;
    LabelLabefgLab1: TLabel;
    LabelAlgorithmCredit: TLabel;
    LabelLabefgLab2: TLabel;

    procedure ButtonNewMazeClick(Sender: TObject);
    procedure CheckBoxShowPathClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NumberKeyPress(Sender: TObject; var Key: Char);
    procedure ValueChange(Sender: TObject);
    procedure ButtonUpdateClick(Sender: TObject);
    procedure ButtonPrintClick(Sender: TObject);
    procedure ShapeColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImageMazeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
  private
    InitialFormWidth :  INTEGER;
    InitialFormHeight:  INTEGER;

    PROCEDURE WMGetMinMaxInfo (VAR Msg:  TWMGetMinMaxInfo);
      MESSAGE wm_GetMinMaxInfo;
  public
    PROCEDURE UpdateMaze;
  end;


var
  FormMazeMaker: TFormMazeMaker;

implementation
{$R *.DFM}

  uses
    MazeLibrary,      // DrawMaze
    ScreenPrintMaze;


// ============================================================================

  PROCEDURE TFormMazeMaker.WMGetMinMaxInfo(VAR Msg:  TWMGetMinMaxInfo);
  BEGIN
    WITH Msg.MinMaxInfo^ DO
    BEGIN
      // Set Minimum Size
      ptMinTrackSize.x := InitialFormWidth;
      ptMinTrackSize.y := InitialFormHeight
    END
  END {WMGetMinMaxInfo};


// ============================================================================

PROCEDURE TFormMazeMaker.UpdateMaze;
  CONST
    Margin = 0; // no margin for now on screen
  VAR
    Bitmap    :  TBitmap;
    PathColor :  TPathColor;
    PathLength:  INTEGER;
    PathStyle :  TPathStyle;
    Seed      :  INTEGER;
    xWidth    :  INTEGER;
    xCells    :  INTEGER;
    yCells    :  INTEGER;
    yHeight   :  INTEGER;
BEGIN
  Screen.Cursor := crHourGlass;

  TRY
    Bitmap := TBitmap.Create;
    TRY
      // Use TRY..EXCEPT to trap blank SpinBoxes
      TRY
        xWidth := SpinEditXPixels.Value + 1  + 2*Margin
      EXCEPT
        ON EConvertError DO  xWidth := 201   + 2*Margin
      END;

      TRY
        yHeight := SpinEditYPixels.Value + 1 + 2*Margin
      EXCEPT
        ON EConvertError DO  yHeight := 201  + 2*Margin
      END;

      Bitmap.Width  := xWidth;
      Bitmap.Height := yHeight;
      // Don't set PixelFormat -- will be pfDevice

      TRY
        xCells := SpinEditXCells.Value
      EXCEPT
        ON EConvertError DO  xCells := 20  // arbitrary small number
      END;

      TRY
        yCells := SpinEditYCells.Value
      EXCEPT
        ON EConvertError DO  yCells := 15  // arbitrary small number
      END;

      TRY
        Seed := StrToInt(EditRandSeed.Text);
      EXCEPT
        ON EConvertError DO Seed := 19937  // a nice prime number
      END;

      IF   RadioGroupPathStyle.ItemIndex = 0
      THEN PathStyle := psLine
      ELSE PathStyle := psBlock;

      IF   RadioGroupPathColor.ItemIndex = 0
      THEN PathColor := pcRainbow
      ELSE PathColor := pcSolid;

      PathLength := DrawMaze(Bitmap.Canvas,
                             Bitmap.Width, Bitmap.Height,
                             xCells,       yCells,
                             Margin, Margin,
                             Seed,
                             CheckBoxShowPath.Checked,
                             PathStyle,
                             PathColor,
                             ShapeColor.Brush.Color);
      ImageMaze.Picture.Graphic := Bitmap;

      LabelLength.Caption := 'Length = ' + IntToStr(PathLength)

    FINALLY
      Bitmap.Free
    END;

  FINALLY
    Screen.Cursor := crDefault
  END
END {UpdateMaze};


procedure TFormMazeMaker.ButtonNewMazeClick(Sender: TObject);
VAR
    Seed:  INTEGER;
begin
  Randomize;
  Seed := RandSeed;
  EditRandSeed.Text := IntToStr(Seed);
  UpdateMaze
end;


procedure TFormMazeMaker.CheckBoxShowPathClick(Sender: TObject);
begin
  UpdateMaze
end;


procedure TFormMazeMaker.FormCreate(Sender: TObject);
begin
  // Use with WMGetMinMaxInfo to limit minimum size of form
  InitialFormWidth  := Width;
  InitialFormHeight := Height;

  ButtonNewMazeClick(Sender)
end;


// Make sure Random Seed is valid number
procedure TFormMazeMaker.NumberKeyPress(Sender: TObject;
  var Key: Char);
  CONST
    Backspace = #$08;
begin
 // List of valid keys in name fields -- same as patient names
  IF   NOT (Key IN [Backspace, '0'..'9'])
  THEN Key := #$00
end;


procedure TFormMazeMaker.ValueChange(Sender: TObject);
begin
  UpdateMaze
end;


procedure TFormMazeMaker.ButtonUpdateClick(Sender: TObject);
begin
  IF   EditRandSeed.Text = ''
  THEN EditRandSeed.Text := '0';

  UpdateMaze
end;


procedure TFormMazeMaker.ButtonPrintClick(Sender: TObject);
begin
  FormPrintMaze.ShowModal;

  IF   FormPrintMaze.ModalResult = mrOK
  THEN ShowMessage('Maze printed.')
end;


procedure TFormMazeMaker.ShapeColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  IF   ColorDialogFloodFill.Execute
  THEN BEGIN
    ShapeColor.Brush.Color := ColorDialogFloodFill.Color;
    UpdateMaze
  END
end;


// Floodfill as much as possible at given point
procedure TFormMazeMaker.ImageMazeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  VAR
    Color:  TColor;
begin
  Color := ImageMaze.Canvas.Pixels[X,Y];
  ImageMaze.Canvas.Brush.Color := ShapeColor.Brush.Color;
  ImageMaze.Canvas.FloodFill(X,Y, Color, fsSurface)
end;


procedure TFormMazeMaker.FormResize(Sender: TObject);
begin
  // Make size jump in steps of 50
  SpinEditXPixels.Value := 50 * ((ScrollBoxMaze.Width -1) DIV 50);
  SpinEditYPixels.Value := 50 * ((ScrollBoxMaze.Height-1) DIV 50)
end;


procedure TFormMazeMaker.ButtonSaveClick(Sender: TObject);
begin
  IF   SavePictureDialogMaze.Execute
  THEN BEGIN
    ImageMaze.Picture.Bitmap.SaveToFile(SavePictureDialogMaze.Filename);
    ShowMessage('File Saved.')
  END
end;


end.
