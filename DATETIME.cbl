        IDENTIFICATION DIVISION.
        PROGRAM-ID. DATETIME.
        AUTHOR. REF.
        DATE-WRITTEN. 2024-06-29
        DATE-COMPILED. 2024-06-29

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        * No files are neccessary to configure because,
        * there is no use for that, as this is a date-time
        * program.

        DATA DIVISION.
        FILE SECTION.

        WORKING-STORAGE SECTION.
        01 WS-WORK-FIELDS.
            05 WS-FILLER1                   PIC X(37) VALUE 
            'DATE-TIME WORKING STORAGE BEGINS HERE'.
            05 WS-TIME-HOUR-C3              PIC 9(2)  COMP-3 VALUE ZERO.
            05 WS-DATE-TIME                 PIC X(16).
            05 WS-TIME-HOUR                 PIC Z9.
            05 WS-TIME                      PIC X(6)  VALUE  ':MM AM'.
            05 WS-TIMELINE                  PIC X(14) VALUE  
                                                 'TIME: HH:MM AM'.
            05 WS-DATE-LITERAL              PIC X(6) VALUE  'DATE:  '.
            05 WS-DAY-OF-WEEK-9             PIC 9(1) VALUE ZERO.
            05 WS-DAY-OF-WEEK-X             PIC X(10) VALUE SPACES.
            05 WS-MONTH-X                   PIC X(10) VALUE SPACES.
            05 WS-DD-X.
                10 WS-DD1                   PIC X(10) VALUE SPACE.
                10 WS-DD2                   PIC X(10) VALUE SPACE.
                10 WS-DD3                   PIC X(10) VALUE SPACE.
            05 WS-YYYYMMDD
                10 WS-YYYY                 PIC X(4) VALUE SPACES.
                10 WS-MM                   PIC X(2) VALUE SPACES.
                10 WS-DD                   PIC X(2) VALUE SPACES.
            05 WS-PLACE-MARK               PIC 9(3) COMP-3 VALUE ZERO.
            05 WS-TEST-BYTE                PIC X(1) VALUE SPACES.
                88 WS-TEST-BYTE-BLANK          VALUE SPACES.
        01  WS-OUT-DATE-LAYOUT             PIC X(35) VALUE SPACES.

        LINKAGE SECTION.

        01  LS-DATELINE.                PIC X(35) VALUE JUSTIFIED RIGHT.
        01  LS-TIMELINE.                PIC X(14).

        PROCEDURE DIVISION USING
                            LS-DAELINE LS-TIMELINE.

        0000-MAINLINE.
            PERFORM 1000-INITIALIZE        THRU 1000-EXIT
            PERFORM 2000-FORMAT-DATELINE   THRU 2000-EXIT
            PERFORM 2500-BUILD-TIME-STRING THRU 2500-EXIT
            PERFORM 3000-TERMINATE         THRU 3000-EXIT

            GOBACK

            .
        0000-EXIT.
            EXIT.

        1000-INITIALIZATION.

            MOVE FUNCTION CURRENT-DATE (1:16)   TO WS-DATE-TIME
            MOVE WS-DATE-TIME (1:8)             TO WS-YYYYMMDD
            MOVE WS-DATE-TIME (9:2)             TO WS-TIME-HOUR-C3
            ACCEPT WS-DAY-OF-WEEK-9 FROM DAY-OF-WEEK

            .
        1000-EXIT.
            EXIT.                

        2000-FORMAT-DATELINE.

            EVALUATE WS-DAY-OF-WEEK-9
                WHEN    1      MOVE 'MONDAY'       TO WS-DAY-OF-WEEK-X
                WHEN    2      MOVE 'TUESDAY'      TO WS-DAY-OF-WEEK-X
                WHEN    3      MOVE 'WEDNESDAY'    TO WS-DAY-OF-WEEK-X
                WHEN    4      MOVE 'THURSDAY'     TO WS-DAY-OF-WEEK-X
                WHEN    5      MOVE 'FRIDAY'       TO WS-DAY-OF-WEEK-X
                WHEN    6      MOVE 'SATURDAY'     TO WS-DAY-OF-WEEK-X
                WHEN    7      MOVE 'SUNDAY'       TO WS-DAY-OF-WEEK-X
                WHEN OTHER     MOVE 'INVALIDX'     TO WS-DAY-OF-WEEK-X
            END-EVALUATE                 

            EVALUATE WS-MM
                WHEN    '01'   MOVE 'JANUARY'      TO WS-MONTH-X
                WHEN    '02'   MOVE 'FEBRUARY'     TO WS-MONTH-X
                WHEN    '03'   MOVE 'MARCH'        TO WS-MONTH-X
                WHEN    '04'   MOVE 'APRIL'        TO WS-MONTH-X
                WHEN    '05'   MOVE 'MAY'          TO WS-MONTH-X
                WHEN    '06'   MOVE 'JUNE'         TO WS-MONTH-X
                WHEN    '07'   MOVE 'JULY'         TO WS-MONTH-X
                WHEN    '08'   MOVE 'AUGUST'       TO WS-MONTH-X
                WHEN    '09'   MOVE 'SEPTEMBER'    TO WS-MONTH-X
                WHEN    '10'   MOVE 'OCTOBER'      TO WS-MONTH-X
                WHEN    '11'   MOVE 'NOVEMBER'     TO WS-MONTH-X
                WHEN    '12'   MOVE 'DECEMBER'     TO WS-MONTH-X
                WHEN OTHER     MOVE 'INVALIDX'     TO WS-MONTH-X
            END-EVALUATE

            IF WS-DD < '10'
                MOVE WS-DD (2:1) TO WS-DD1
                MOVE 'X'         TO WS-DD2
            ELSE
                MOVE WS-DD       TO WS-DD-X
                MOVE 'X'         TO WS-DD3
            END-IF

            STRING WS-DATE-LITERAL
                   WS-DAY-OF-WEEK-X  ', '
                   WS-MONTH-X        ' '
                   WS-DD-X           ', '
                   WS-YYYY
                DELIMITED BY 'X'
                    INTO WS-OUT-DATE-LAYOUT
            
            IF WS-OUT-DATE-LAYOUT (35:1) = SPACES
                MOVE +35 TO WS-PLACE-MARK

                PERFORM UNTIL NOT WS-TEST-BYTE-BLANK
                    SUBTRACY 1 FROM WS-PLACE-MARK
                    MOVE WS-OUT-DATE-LAYOUT (WS-PLACE-MARK:1)
                        TO WS-TEST-BYTE
                END-PERFORM
            END-IF

            MOVE WS-OUT-DATE-LAYOUT (1:WS-PLACE-MARK) TO LS-DATELINE

            .
        2000-EXIT.
            EXIT.

        2500-BUILD-TIME-STRING.

            EVALUATE WS-TIME-HOUR-C3
                WHEN 0
                    MOVE  12    TO WS-TIME-HOUR
                    MOVE '12'   TO WS-TIMELINE (7:2)
                WHEN 1 THRU 11
                    MOVE WS-TIME-HOUR-C3 TO WS-TIME-HOUR
                WHEN 12
                    MOVE WS-TIME-HOUR-C3 TO WS-TIME-HOUR
                    MOVE 'PM'            TO WS-TIMELINE (13:2)
                WHEN 13 THRU 23
                    SUBTRACT 12         FROM WS-TIME-HOUR-C3
                    MOVE WS-TIME-HOUR-C3 TO WS-TIME-HOUR
                    MOVE 'PM'            TO WS-TIMELINE (13:2)
                WHEN OTHER
                    DISPLAY  'PROBLEM BUILDING TIME STRING'
            END-EVALUATE

            MOVE WS-DATE-TIME (11:2)     TO WS-TIMELINE (10:2)
            MOVE WS-TIME-HOUR            TO WS-TIMELINE (7:2)
            MOVE WS-TIMELINE             TO LS-TIMELINE

            .
        2500-EXIT.
            EXIT.
        
        3000-TERMINATE.
            MOVE  +0 TO RETURN-CODE
            .
        3000-EXIT.
            EXIT.

        * End of program DATETIME.cbl