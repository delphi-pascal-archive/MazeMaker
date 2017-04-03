// ============================================================================
// Maze generation algorithm
// Copyright © 1999 Gary Williams
// graywolf@pcpros.net
//
// Not for commercial use without prior consent from original author
//
// ============================================================================
// Note:  Clever use of assembler push/pop statements to create a path stack.
//
// Restructured by efg to
// - use 0-origin array instead of 1-origin array, mostly to
// - use Delphi 4 dynamic arrays;
// - use enumerated type 0..3 instead of BYTE for Cells Array
// - create separate function to draw on a given Canvas (Display, Memory, Printer)
// - separate algorithm from user interface (into this unit)
// - avoid exit/break statements (forced single entry, single exit)
// - eliminated some unnecessary drawing
// - implemented path styles:  block, line
// - print maze including parameters to create maze again
//
// efg, 27 February 1999
// www.efg2.com/lab
//
// ============================================================================


UNIT MazeLibrary;

INTERFACE

  USES
    Graphics;     //  TCanvas

  TYPE
    TPathStyle = (psLine, psBlock);
    TPathColor = (pcRainbow, pcSolid);

  FUNCTION DrawMaze(CONST Canvas:  TCanvas;
                    CONST xPixelCount, yPixelCount:  INTEGER;
                    CONST xCellCount,  yCellCount :  INTEGER;
                    CONST xOffset,     yOffset    :  INTEGER;
                    CONST MazeSeed:  Integer;
                    CONST ShowPath:  BOOLEAN;
                    CONST PathStyle:  TPathStyle = psBlock;
                    CONST PathColor:  TPathColor = pcRainbow;
                    CONST SolidColor: TColor = clWhite):  INTEGER;  // Ignore if white

IMPLEMENTATION

  USES
    Windows,        // MulDiv
    Classes,        // Rect
    Dialogs,        // ShowMessage
    SpectraLibrary, // WavelengthToColor
    Math;           // Math


  //  Assumes Canvas Dimensions:
  //  Width  := (xPixelCount DIV xCellCount) * xCellCount + 1
  //  Height := (yPixelCount DIV yCellCount) * yCellCount + 1
  //
  //  Each cell is represented by a 2-bit quantity in a two-dimensional Cells array.
  //  Only two bits are used -- bit 0 is set if the cell has an East exit, and
  //  bit 1 is set if it has a South exit.  To test if a North or West exit
  //  exists, look at the appropriate adjacent cell.

  // Returns path length [cell count] of solution
  FUNCTION DrawMaze(CONST Canvas:  TCanvas;
                    CONST xPixelCount, yPixelCount:  INTEGER;
                    CONST xCellCount,  yCellCount :  INTEGER;
                    CONST xOffset,     yOffset    :  INTEGER;
                    CONST MazeSeed:  Integer;
                    CONST ShowPath:  BOOLEAN;
                    CONST PathStyle:  TPathStyle;
                    CONST PathColor:  TPathColor;
                    CONST SolidColor:  TColor):  INTEGER;
    const
      EAST  = 1;
      SOUTH = 2;

    var
      Cells     :  array of array of 0..3;   // 2-bits:  $00, $01, $10, $11
      Count     :  Integer;
      MazeHeight:  Integer;
      MazeWidth :  Integer;
      NX        :  Integer;
      NY        :  Integer;
      PathLength:  Integer;
      PathPos   :  Integer;
      PosX      :  Integer;
      PosY      :  Integer;
      StackCount:  Integer;
      xCellSize :  Integer;
      yCellSize :  Integer;


    function IsValidCell(const X: Integer; const Y: Integer): Boolean;
    begin
      Result := ((X >= 0) and (X < xCellCount) and
                 (Y >= 0) and (Y < yCellCount))
    end;


    function Connected(const X2: Integer; const Y2: Integer): Boolean;
    begin
      //   Is this cell inside the maze?
      if   IsValidCell(X2, Y2)
      then begin
        // Does this cell have an East or South exit?
        Result := (Cells[X2, Y2] <> 0);

        // If not, does it have a North exit?
        if   (not Result) and IsValidCell(X2, Y2 - 1)
        then Result := ((Cells[X2, Y2 - 1] and SOUTH) > 0);

        // If not, does it have a West exit?
        if   (not Result) and IsValidCell(X2 - 1, Y2)
        then Result := ((Cells[X2 - 1, Y2] and EAST) > 0)
      end
           // Return TRUE so we don't try to open an exit to there.
      else Result := TRUE;
    end;


    procedure Connect(const X2: Integer; const Y2: Integer);
    begin
      if   PosX = X2
      then Cells[PosX, Min(PosY, Y2)] := Cells[PosX, Min(PosY, Y2)] or SOUTH
      else
        if  PosY = Y2
        then Cells[Min(PosX, X2), PosY] := Cells[Min(PosX, X2), PosY] or EAST
        else ShowMessage('Attempting to connect cells that are not ' +
                         'orthogonally adjacent.');
    end;


  begin
    xCellSize  :=  xPixelCount DIV xCellCount;
    yCellSize  :=  yPixelCount DIV yCellCount;

    MazeWidth  := xCellCount * xCellSize + 1;
    MazeHeight := yCellCount * yCellSize + 1;

    SetLength(Cells, xCellCount+1, yCellCount+1);  // Dynamically allocate

    for PosX := 0 to xCellCount-1 do
      for PosY := 0 to yCellCount-1 do
        Cells[PosX, PosY] := 0;

    StackCount := 0;
    PathLength := 0;

    RandSeed := MazeSeed;    // set random number seed

    PosX := 0;
    PosY := 0;

    NX := 0;
    NY := 0;

    PathPos := -1;

    repeat
      Count := 0;

      // Each neighboring cell has a 1 / Count chance of being selected when
      // it is discovered.

      if   not Connected(PosX + 1, PosY)
      then begin
        // Since this is the only cell found so far, it will always have a 1/1
        // chance of becoming the currently favored destination, so no need for
        // Random() call.
        Inc(Count);
        NX := PosX + 1;
        NY := PosY
      end;

      if   not Connected(PosX - 1, PosY)
      then begin
        Inc(Count);
        if   Random(Count) = 0
        then begin
          NX := PosX - 1;
          NY := PosY
        end;
      end;

      if   not Connected(PosX, PosY + 1)
      then begin
        Inc(Count);
        if   Random(Count) = 0
        then begin
          NX := PosX;
          NY := PosY + 1
        end
      end;

      if   not Connected(PosX, PosY - 1)
      then begin
        Inc(Count);
        if (Random(Count) = 0)
        then begin
          NX := PosX;
          NY := PosY - 1
        end
      end;

      if   Count > 0
      then begin
        Connect(NX, NY);

        // Remember where we are so we can back up later.
        Inc(StackCount);
        asm
          Push PosX;
          Push PosY;
        end;

        PosX := NX;
        PosY := NY;
      end
      else begin
        if  StackCount > 0
        then begin
          // If the current cell is the bottom right cell, then we've reached
          // the end.  At this point the stack will contain the X and Y values
          // of all the cells on the solution path.
          if  (PosX = xCellCount-1) and (PosY = yCellCount-1)
          then begin
            PathLength := StackCount;
            PathPos    := PathLength;
          end;

          // If we're backing up the solution path, color the cell appropriately.
          IF   StackCount <= PathPos
          THEN BEGIN
            IF   ShowPath
            THEN BEGIN
              WITH Canvas DO
              BEGIN

                CASE PathStyle OF

                  psLine:
                    BEGIN
                      // Not worth optimizing this
                      IF   StackCount = PathLength
                      THEN BEGIN
                        Pen.Width := Max(1, MulDiv( Min(xCellSize,yCellSize), 2, 10));
                        MoveTo(xOffset + PosX * xCellSize + xCellSize DIV 2,
                               yOffset + PosY * yCellSize + yCellSize DIV 2);
                      END
                      ELSE BEGIN
                        IF   PathColor = pcRainbow
                        THEN Pen.Color := WavelengthToColor(WavelengthMinimum +
                             PathPos / PathLength * (WavelengthMaximum - WavelengthMinimum))
                        ELSE Pen.Color := SolidColor;
                        LineTo(xOffset + PosX * xCellSize + xCellSize DIV 2,
                               yOffset + PosY * yCellSize + yCellSize DIV 2);
                      END
                    END;

                  psBlock:
                    BEGIN
                      IF   PathColor = pcRainbow
                      THEN Brush.Color := WavelengthToColor(WavelengthMinimum +
                             PathPos / PathLength * (WavelengthMaximum - WavelengthMinimum))
                      ELSE Brush.Color := SolidColor;

                      FillRect(Rect(xOffset +  PosX     * xCellSize,
                                    yOffset +  PosY     * yCellSize,
                                    xOffset + (PosX + 1)* xCellSize,
                                    yOffset + (PosY + 1)* yCellSize))
                    END

                END
              END
            END;
            // Ensure that we don't accidentally colorize a divergent path.
            Dec(PathPos);
          end;

          asm
            Pop PosY;
            Pop PosX;
          end;

          Dec(StackCount);
        end
      end
    until (StackCount = 0);  // Maze is complete

    // Show Maze Lines

    WITH Canvas DO
    BEGIN
      Pen.Width := 1;
      Pen.Color := clBlack;

      IF  ShowPath
      THEN BEGIN
        // The first cell of the maze must be colored manually.

        CASE PathStyle OF

          psLine:
            BEGIN
              Pen.Width := Max(1, MulDiv( Min(xCellSize,yCellSize), 2, 10));
              IF   PathColor = pcRainbow
              THEN Pen.Color := WavelengthToColor(WavelengthMinimum +
                                PathPos / PathLength * (WavelengthMaximum - WavelengthMinimum))
              ELSE Pen.Color := SolidColor;
              LineTo(xOffset + xCellSize DIV 2,
                     yOffset + yCellSize DIV 2)
            END;

          psBlock:
            BEGIN
              IF   PathColor = pcRainbow
              THEN Brush.Color := WavelengthToColor(WavelengthMinimum)
              ELSE Brush.Color := SolidColor;
              FillRect(Rect(xOffset+0, yOffset+0, xOffset+xCellSize, yOffset+yCellSize))
            END

        END
      END
      ELSE BEGIN
        // If path is not shown, show points for beginning and end of path
        // Show ellipse for beginning of path
        Brush.Color := clRed;
        Ellipse( xOffset + MulDiv(xCellSize, 2,10),
                 yOffset + MulDiv(yCellSize, 2,10),
                 xOffset + MulDiv(xCellSize, 8,10),
                 yOffset + MulDiv(yCellSize, 8,10) );

        // Show ellipse for end of path
        Ellipse( xOffset + MulDiv(xCellSize, 2,10) + (xCellCount - 1) * xCellSize,
                 yOffset + MulDiv(yCellSize, 2,10) + (yCellCount - 1) * yCellSize,
                 xOffset + MulDiv(xCellSize, 8,10) + (xCellCount - 1) * xCellSize,
                 yOffset + MulDiv(yCellSize, 8,10) + (yCellCount - 1) * yCellSize)
      END;

      Pen.Width := 1;
      Pen.Color := clBlack;
      Brush.Color := clBlack;
      FrameRect(Rect(xOffset+0,
                     yOffset+0,
                     xOffset + MazeWidth,
                     yOffset + MazeHeight));

      for PosX := 0 to xCellCount-1 do
      begin
        for PosY := 0 to yCellCount-1 do
        begin

          // Vertical "East" Lines
          if  (Cells[PosX, PosY] and EAST) = 0
          then begin
            MoveTo(xOffset + (PosX + 1) * xCellSize, yOffset +  PosY      * yCellSize);
            LineTo(xOffset + (PosX + 1) * xCellSize, yOffset + (PosY + 1) * yCellSize+1)
          end;

          // Horizontal "South" lines
          if   (Cells[PosX, PosY] and SOUTH) = 0
          then begin
            MoveTo(xOffset +  PosX      * xCellSize  , yOffset + (PosY + 1) * yCellSize);
            LineTo(xOffset + (PosX + 1) * xCellSize+1, yOffset + (PosY + 1) * yCellSize)
          end

        end
      end
    end;

    RESULT := 1 + PathLength   // 0-origin

  end {DrawMaze};


END.
