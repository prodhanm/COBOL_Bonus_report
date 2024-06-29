        IDENTIFICATION DIVISION.
        PROGRAM-ID. BONUS.
        AUTHOR. REF.
        INSTALLATION. REF.
        DATE-WRITTEN. 2024-06-29.
        DATE-COMPILED. 2024-06-29.

        ENVIRONMENT DIVISION.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT INPUT-BONUS ASSIGN TO INBONUS.
            SELECT STATE-TAX   ASSIGN TO INSTTAX.
            SELECT OUT-REPORT  ASSIGN TO OUTREPT.

        DATA DIVISION.
        FILE SECTION.
        * INSTTAX
        FD  STATE-TAX
            RECORDING MODE IS F
            LABEL RECORDS ARE STANDARD
            RECORD CONTAINS 80 CHARACTERS
            BLOCK CONTAINS 0 RECORDS
            DATA RECORD IS INPUT-STATE-RECORD.
        01  INPUT-STATE-RECORD.
            05  STATE-CODE                  PIC X(2).
            05  FILLER                      PIC X.
            05  STATE-TAX-NUMBER            PIC V999999.
            05  FILLER                      PIC X(71).

        * INPUT-BONUS
        FD  INPUT-BONUS
            RECORDING MODE IS F
            LABEL RECORDS ARE STANDARD
            RECORD CONTAINS 76 CHARACTERS
            BLOCK CONTAINS 0 RECORDS
            DATA RECORD IS INPUT-RECORD.
        01  INPUT-RECORD.
            05  IN-STATE-CODE               PIC X(20).
            05  IN-LAST-NAME                PIC X(20).
            05  IN-FIRST-NAME               PIC X(15).
            05  IN-MID-INIT                 PIC X.
            05  IN-BONUS-AMT                PIC S9(7) V99 COMP-3.
            05  IN-FED-EXEMPT-IND           PIC X.
            05  IN-STATE-EXEMPT-IND         PIC X.
            05  IN-FILLER                   PIC X(31).

        * OUT-REPORT
        FD  OUT-REPORT
            RECORDING MODE IS F
            LABEL RECORDS ARE STANDARD
            RECORD CONTAINS 133 CHARACTERS
            BLOCK CONTAINS 0 RECORDS
            DATA RECORD IS OUT-REPT-REC.
        01  OUT-REPT-REC.
            05  OR-LAST-NAME                PIC X(20).
            05  FILLER                      PIC X(2).
            05  OR-FIRST-NAME               PIC X(15).
            05  FILLER                      PIC X(2).
            05  OR-MID-INIT                 PIC X.
            05  FILLER                      PIC X(2).
            05  OR-STATE-CODE               PIC X(2).
            05  FILLER                      PIC X(2).
            05  OR-BONUS-AMT                PIC $$$,$$9.99.
            05  FILLER                      PIC X(4).
            05  OR-FED-TAX                  PIC $$$,$$9.99.
            05  FILLER                      PIC X(1).
            05  OR-STATE-TAX                PIC $$$,$$9.99.
            05  FILLER                      PIC X(4).
            05  OR-NET                      PIC $$$,$$9.99.
            05  FILLER                      PIC X(2).
            05  OR-MESSAGE                  PIC X(36).

        WORKING-STORAGE SECTION.

        01  TAX-TABLE-SWITCH               PIC X VALUE 'N'.
            88  END-OF-TAX-TABLE                 VALUE 'Y'.

        01  STATE-TAX-SWITCH               PIC X VALUE 'N'.
            88  END-OF-STATE                     VALUE 'Y'.

        01  TAX-TABLE.
            05  TAXES OCCURS 44 TIMES.
                10  STATE              PIC X(2).
                10  FILLER             PIC X.
                10  TAX                PIC V999999.
                10  FILLER             PIC X(71).

        01  WS-HOLD-ST-CODE            PIC X(2).

        01  FOOTER-ONE.
            05 FILLER                 PIC X(22) VALUE SPACES.
            05 FILLER                 PIC X(15) VALUE 'TOTAL FOR STATE'.
            05 FILLER                 PIC X  VALUE ":".
            05 FILLER                 PIC X  VALUE SPACES.
            05 PR-ST-STATE-CODE       PIC X(2) VALUE " ".
            05 FILLER                 PIC X(2) VALUE SPACES.
            05 PR-ST-GROSS            PIC ZZZ,ZZZ,ZZ9.99-.
            05 FILLER                 PIC X(4) VALUE SPACES.
            05 PR-ST-FED-TAX          PIC ZZZ,ZZZ,ZZ9.99-.
            05 FILLER                 PIC X(4) VALUE SPACES.
            05 PR-ST-STATE-TAX        PIC ZZZ,ZZZ,ZZ9.99-.
            05 FILLER                 PIC X(4) VALUE SPACES.
            05 PR-ST-NET              PIC ZZZ,ZZZ,ZZ9.99-.

        01  FOOTER-TWO.
            05 FILLER                 PIC X(17) VALUE "TOTAL FOR ALL :".
            05 FILLER                 PIC X(30) VALUE SPACES.
            05 PR-GR-GROSS            PIC ZZZ,ZZZ,ZZ9.99-.
            05 FILLER                 PIC X(4) VALUE SPACES.
            05 PR-GR-FED-TAX          PIC ZZZ,ZZZ,ZZ9.99-.
            05 FILLER                 PIC X(4) VALUE SPACES.
            05 PR-GR-STATE-TAX        PIC ZZZ,ZZZ,ZZ9.99-.
            05 FILLER                 PIC X(4) VALUE SPACES.
            05 PR-GR-NET              PIC ZZZ,ZZZ,ZZ9.99-.  

        01  WORKING-VARIABLES.
            05  WS-GROSS              PIC 9(9)V99.
            05  WS-FED-TAX            PIC 9(9)V99.
            05  WS-NET                PIC 9(9)V99.
            05  WS-PERCENT            PIC V99  VALUE .28.
            05  WS-MESSAGE            PIC X(20) VALUE SPACES.
            05  WS-ST-GROSS           PIC 9(9)V99.
            05  WS-ST-FED-TAX         PIC 9(9)V99.
            05  WS-ST-STATE-TAX       PIC 9(9)V99.
            05  WS-ST-NET             PIC 9(9)V99.
            05  WS-GR-GROSS           PIC 9(9)V99.
            05  WS-GR-FED-TAX         PIC 9(9)V99.
            05  WS-GR-STATE-TAX       PIC 9(9)V99.
            05  WS-GR-NET             PIC 9(9)V99.
            05  BONUS                 PIC X(8)  VALUE 'BONUS'.
            05  TABLE-SUB             PIC 9(2)  VALUE 1.

        01  HEADING-LINE-1.
            05  FILLER            PIC X(20) VALUE 'REPORT ID: BONUS'.
            05  FILLER            PIC X(20) VALUE  SPACE.
            05  FILLER            PIC X(24) VALUE "MEL'S AMAZING SHOES".
            05  FILLER            PIC X(12) VALUE SPACE.
            05  FILLER            PIC X(5)  VALUE SPACE.
            05  FILLER            PIC X(20) VALUE 'PAGE: '.
            05  PAGE-COUNT-1      PIC X(2)  VALUE ZERO.

        01  DATE-LINE.
            05  FILLER            PIC X(45) VALUE SPACE.
            05  FILLER            PIC X(20) 'MONTHLY BONUS REPORT'.
            05  FILLER            PIC X(6)  VALUE SPACE.
            05  HEAD-DATE         PIC X(34).

        01  TIME-LINE.
            05  FILLER            PIC X(81) VALUE SPACE.
            05  HEAD-TIME         PIC X(20).

        01  HEADING-LINE-2.
            05  FILLER           PIC X(20) VALUE 'NAME'.
            05  FILLER           PIC X(42)  VALUE SPACE.
            05  FILLER           PIC X(5) VALUE 'STATE'.
            05  FILLER           PIC X(7) VALUE SPACE.
            05  FILLER           PIC X(5) VALUE 'GROSS'.
            05  FILLER           PIC X(9) VALUE SPACE.
            05  FILLER           PIC X(5) VALUE 'FED TAX'.
            05  FILLER           PIC X(2) VALUE SPACE.
            05  FILLER           PIC X(10) VALUE 'STATE TAX'.
            05  FILLER           PIC X(3) VALUE 'NET'.
            05  FILLER           PIC X(9) VALUE SPACE.
            05  FILLER           PIC X(8) VALUE 'MESSAGE'.

        01  HEADING-LINE-3.
            05  FILLER           PIC X(132) VALUE ALL '_'.
            05  FILLER           PIC X(42) VALUE SPACE.

        01  W01-ACCUMULATORS.
            05  W01-REC-IN       PIC S9(04) COMP  VALUE ZERO.
            05  W01-REC-OUT      PIC S9(04) COMP  VALUE ZERO.
            05  LINE-COUNT       PIC S9(2)        VALUE ZERO.
            05  PAGE-COUNT       PIC S9(2)        VALUE ZERO.

        01  W02-SWITCHES.
            05  W02-IN-EOF-SW    PIC X VALUE 'N'.
                88  W02-IN-EOF-TRUE    VALUE 'Y'.

        PROCEDURE DIVISION.
        P0100-MAINLINE.

            PERFORM P0200-INITIALIZE        THRU P0299-EXIT

            PERFORM P1300-POP-TABLE         THRU P1399-EXIT
                UNTIL END-OF-TAX-TABLE
                      OR
                      END-OF-STATE

            PERFORM P0600-GOOD-REC          THRU P0699-EXIT
                UNTIL W02-IN-EOF-SW = 'Y'
            PERFORM P0400-WRAP-UP           THRU P0499-EXIT
            
            GOBACK

            .
        P0199-EXIT.
            EXIT.

        P0200-INITIALIZE.

            OPEN INPUT INPUT-BONUS
                       STATE-TAX
                OUTPUT OUT-REPORT
        * PRIMING READ FOR TABLE
            PERFORM P1200-READ-TABLE        THRU P1299-EXIT.
        * DATE AND TIME PROGRAM
            CALL 'DATETIME' USING HEAD-DATE HEAD-TIME
        * PRIMING READ
            PERFORM P0900-READ-INPUT       THRU P0999-EXIT.
        * FIRST TIME FOR HEADERS
            PERFORM  P0500-HEADING.

            MOVE  IN-STATE-CODE            TO WS-HOLD-ST-CODE

            IF END-OF-INPUT-FILE
                DISPLAY 'BONUS - NO INPUT TO PROCESS'
            END-IF

            .
        P0299-EXIT.
            EXIT.
        * PERFORMING MATH FOR STATE TAX CALCULATIONS
        P0300-MATH-PROC.
            IF IN-STATE-CODE  NOT EQUAL  WS-HOLD-ST-CODE
            PERFORM P1000-FOOTER-ONE THRU P1099-EXIT
            END-IF.

            MOVE IN-BONUS-AMT TO WS-GROSS
                 IF IN-FED-EXEMPT-IND = 'N'
            
            MULTIPLY WS-GROSS BY WS-PERCENT GIVING WS-FED-TAX
            MOVE     WS-FED-TAX TO OR-FED-TAX

            MOVE SPACES TO OR-MESSAGE

            SUBTRACT WS-FED-TAX FROM WS-GROSS GIVING WS-NET
            MOVE WS-NET TO OR-NET

                 ELSE
                 IF IN-FED-EXEMPT-IND = 'Y'
            MOVE ZEROES TO WS-FED-TAX
            MOVE "                    " TO OR-MESSAGE.

            ADD WS-GROSS TO WS-ST-GROSS, WS-GR-GROSS
            ADD WS-FED-TAX TO WS-ST-FED-TAX, WS-GR-FED-TAX
            ADD WS-NET TO WS-ST-NET, WS-GR-NET
            .
        P0399-EXIT.
            EXIT.

        P0400-WRAP-UP.

            WRITE OUT-REPT-REC FROM FOOTER-ONE
            PERFORM P1100-FOOTER-ONE THRU P1199-EXIT

            MOVE "*** END OF REPORT ***" TO OUT-REPT-REC
            WRITE OUT-REPT-REC
            CLOSE INPUT-BONUS
                  OUT-REPORT
                  STATE-TAX

            DISPLAY 'BONUS - RECORD COUNTS'
            DISPLAY 'INPUT RECORDS READ: ' W01-REC-IN
            DISPLAY 'OUTPUT RECORDS OUT: ' W01-REC-OUT

            IF W01-REC-IN = W01-REC-OUT
                MOVE +0 TO RETURN-CODE
            ELSE
                DISPLAY 'BONUS - RECORD COUNTS OUT OF BALANCE'
                .
        P0499-EXIT.
            EXIT.

        P0600-GOOD-REC.

            PERFORM P0300-MATH-PROC     THRU P0399-EXIT
            MOVE IN-LAST-NAME           TO OR-LAST-NAME
            MOVE IN-FIRST-NAME          TO OR-FIRST-NAME
            MOVE IN-MID-INIT            TO OR-MID-INIT
            MOVE WS-HOLD-ST-CODE        TO OR-STATE-CODE
            MOVE IN-BONUS-AMT           TO OR-BONUS-AMT
            PERFORM P0800-WRITE-GOOD   THRU P0899-EXIT
            ADD 1 TO LINE-COUNT
            IF LINE-COUNT = 45
                PERFORM P0500-HEADING  THRU P0599-EXIT
            END-IF.

            PERFORM P0900-READ-INPUT    THRU P0999-EXIT
            .
        P0699-EXIT.
            EXIT.

        P0800-WRITE-GOOD.

            IF LINE-COUNT = 4
            WRITE OUT-REPT-REC AFTER ADVANCING 2 LINES
            ADD 2 TO LINE-COUNT
            ELSE
            WRITE OUT-REPT-REC AFTER ADVANCING 1 LINE
            ADD +1 TO W01-REC-OUT
            END-IF
            .
        P0899-EXIT.
            EXIT.

        P0900-READ-INPUT.
            
                READ INPUT-BONUS
                    AT END
                        MOVE 'Y' TO W02-IN-EOF-SW
                    NOT AT END
                        ADD +1 TO W01-REC-IN
                END-READ


                .
        P0999-EXIT.

            EXIT.

        * HEADING FOR EACH PAGE
        P0500-HEADING.
            ADD 1 TO PAGE-COUNT.
            MOVE PAGE-COUNT TO PAGE-COUNT-1
            MOVE 0 TO LINE-COUNT

            MOVE HEADING-LINE-1 TO OUT-REPT-REC.
            WRITE OUT-REPT-REC FROM HEADING-LINE-1 
                AFTER ADVANCING PAGE.
            
            MOVE DATE-LINE TO OUT-REPT-REC.
            WRITE OUT-REPT-REC FROM DATE-LINE.

            MOVE TIME-LINE TO OUT-REPT-REC.
            WRITE OUT-REPT-REC FROM TIME-LINE.

            MOVE HEADING-LINE-2 TO OUT-REPT-REC.
            WRITE OUT-REPT-REC FROM HEADING-LINE-2
                  AFTER ADVANCING 3 LINES.

            MOVE HEADING-LINE-3 TO OUT-REPT-REC.
            WRITE OUT-REPT-REC FROM HEADING-LINE-3
                  AFTER ADVANCING 0 LINE.
            ADD 4 TO LINE-COUNT
            MOVE SPACES TO OUT-REPT-REC

            .
        P0599-EXIT.
            EXIT.

        * PERFORM FOOTERS
        P1000-FOOTER-ONE.
            MOVE WS-HOLD-ST-CODE TO PR-ST-STATE-CODE
            MOVE WS-ST-GROSS TO PR-ST-GROSS
            MOVE WS-ST-FED-TAX TO PR-ST-FED-TAX
            MOVE WS-ST-NET TO PR-ST-NET

            WRITE OUT-REPT-REC FROM FOOTER-ONE
            AFTER ADVANCING 1 LINE
            MOVE SPACES TO OUT-REPT-REC

            MOVE IN-STATE-CODE TO WS-HOLD-ST-CODE
            MOVE ZEROES TO WS-ST-GROSS 
            MOVE ZEROES TO WS-ST-FED-TAX
            MOVE ZEROES TO WS-ST-NET
            .
        P1099-EXIT.
            EXIT.

        P1100-FOOTER-TWO.
                MOVE WS-GR-GROSS TO PR-GR-GROSS
                MOVE WS-GR-FED-TAX TO PR-GR-FED-TAX
                MOVE WS-GR-NET TO PR-GR-NET

                WRITE OUT-REPT-REC FROM FOOTER-TWO
                AFTER ADVANCING 1 LINE





                .
        P1199-EXIT.
            EXIT.

        P1200-READ-TABLE.
            READ STATE-TAX
                AT END
                    MOVE 'Y' TO END-OF-TAX-TABLE
            END-READ
            .
        P1299-EXIT.
            EXIT.

        P1300-POP-TABLE.
            MOVE INPUT-STATE-RECORD TO TAXES(TABLE-SUB)
            ADD 1 TO TABLE-SUB

            IF TABLE-SUB IS GREATEER THAN 44
                THEN MOVE 'Y' TO TAX-TABLE-SWITCH
            END-IF

            PERFORM P1200-READ-TABLE THRU P1299-EXIT
            .
        P1399-EXIT.
            EXIT.

        * END OF PROGRAM
        