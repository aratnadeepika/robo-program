      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROBOPGM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IPFILE ASSIGN TO "/Users/venkatpk/deepa/input.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS.

           SELECT RPFILE ASSIGN TO "/Users/venkatpk/deepa/report.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS1.    

       DATA DIVISION.
       FILE SECTION.
       
       FD IPFILE.
       01 CMD PIC X(17).

       FD RPFILE.
       01 RPT PIC X(75).

       WORKING-STORAGE SECTION.
       01 ROBO1 PIC X.
       01 FS PIC XX.
       01 FS1 PIC XX.
       01 IP-EOF PIC X VALUE 'N'.
       01 PLACE-ISSUED PIC X VALUE 'N'.
       01 ROW1 PIC 99.
          88 ROW-VALID VALUES ARE 05 THRU 15.
       01 COL1 PIC 99.
          88 COL-VALID VALUES ARE 20 THRU 40.
       01 FACE1 PIC X(5).
       01 ROW-TMP PIC 99.
       01 COL-TMP PIC 99.
       01 FACE-TMP PIC X(5).
       01 RPT-REC.
           05 CMD-STR PIC X(9) VALUE "COMMAND: ".
           05 CMD-R PIC X(20).
           05 FILLER PIC X.
           05 TXT1 PIC X(10) VALUE "BEFORE: ".
           05 BEF-R PIC 99.
           05 FILLER PIC X.
           05 BEF-C PIC 99.
           05 FILLER PIC X.
           05 BEF-F PIC X(5).
           05 FILLER PIC X(5).
           05 TXT2 PIC X(7) VALUE "AFTER: ".
           05 AFT-R PIC 99.
           05 FILLER PIC X.
           05 AFT-C PIC 99.
           05 FILLER PIC X.
           05 AFT-F PIC X(5).
       01 CMD-P.
           05 CMD1 PIC X(5).
              88 VALID-CMD VALUE "PLACE", "MOVE", "LEFT", "RIGHT".
           05 FILLER PIC X.
           05 R PIC 99.
           05 FILLER PIC X.
           05 C PIC 99.
           05 FILLER PIC X.
           05 FACE PIC X(5).
              88 VALID-FACE VALUE "EAST", "WEST", "NORTH", "SOUTH".
       SCREEN SECTION.
       01 TABLE1.
         05 LINE 5 COLUMN 20 VALUE IS '* * * * * * * * * * *'.
         05 LINE 6 COLUMN 20 VALUE IS '*                   *'.
         05 LINE 7 COLUMN 20 VALUE IS '*                   *'.
         05 LINE 8 COLUMN 20 VALUE IS '*                   *'.
         05 LINE 9 COLUMN 20 VALUE IS '*                   *'.
         05 LINE 10 COLUMN 20 VALUE IS '*                   *'.
         05 LINE 11 COLUMN 20 VALUE IS '*                   *'.
         05 LINE 12 COLUMN 20 VALUE IS '*                   *'.
         05 LINE 13 COLUMN 20 VALUE IS '*                   *'.
         05 LINE 14 COLUMN 20 VALUE IS '*                   *'.
         05 LINE 15 COLUMN 20 VALUE IS '* * * * * * * * * * *'.
      
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY TABLE1.
           INITIALIZE CMD CMD-P.
           INITIALIZE R C ROW1 COL1.
           INITIALIZE RPT.
           PERFORM OPEN-PARA THRU OPEN-PARA-X.
           PERFORM READ-PARA UNTIL PLACE-ISSUED = 'Y'.
           PERFORM ROBO-MOVE-PARA UNTIL IP-EOF = 'Y'.
           PERFORM CLOSE-PARA.
           STOP RUN.

       OPEN-PARA.
             OPEN INPUT IPFILE.
             OPEN OUTPUT RPFILE.
       OPEN-PARA-X. EXIT.

       READ-PARA.
           READ IPFILE INTO CMD AT END MOVE "Y" TO IP-EOF.
           MOVE CMD TO CMD-P CMD-R.
           IF IP-EOF IS = 'Y' THEN
                MOVE "NO COMMANDS IN INPUT FILE" TO RPT
                WRITE RPT
                GO TO CLOSE-PARA.

           IF NOT VALID-CMD THEN
                  MOVE SPACES TO RPT
                  STRING "COMMAND: " DELIMITED BY SIZE
                         CMD DELIMITED BY SIZE
                         "  INVALID COMMAND" INTO RPT
                  WRITE RPT    
           ELSE IF CMD1 IS NOT = 'PLACE' THEN
                   MOVE SPACES TO RPT
                   STRING "COMMAND: " DELIMITED BY SIZE
                          CMD DELIMITED BY SIZE
                          "  NEED TO ENTER PLACE COMMAND FIRST" INTO RPT
                   WRITE RPT
                ELSE IF (R IS NOT NUMERIC) OR (C IS NOT NUMERIC) THEN
                         MOVE SPACES TO RPT
                         STRING "COMMAND: " DELIMITED BY SIZE
                                 CMD DELIMITED BY SIZE
                          "  INVALID PLACE COMMAND COORDINATES" INTO RPT
                         WRITE RPT
                     ELSE IF NOT VALID-FACE THEN
                             MOVE SPACES TO RPT
                             STRING "COMMAND: " DELIMITED BY SIZE
                                    CMD DELIMITED BY SIZE
                           "  INVALID PLACE COMMAND FACE VALUE" INTO RPT
                           WRITE RPT
                          ELSE
                             MOVE SPACES TO RPT
                             PERFORM PLACE-PARA THRU PLACE-PARA-X.

       READ-PARA-X. EXIT.

       PLACE-PARA. 
           IF (R IS NOT NUMERIC) OR (C IS NOT NUMERIC) THEN
                   MOVE SPACES TO RPT
                   STRING "COMMAND: " DELIMITED BY SIZE
                           CMD DELIMITED BY SIZE
                          "  INVALID PLACE COMMAND COORDINATES" INTO RPT
                    WRITE RPT
           ELSE IF NOT VALID-FACE THEN
                   MOVE SPACES TO RPT
                   STRING "COMMAND: " DELIMITED BY SIZE
                           CMD DELIMITED BY SIZE
                          "  INVALID PLACE COMMAND FACE VALUE" INTO RPT
                   WRITE RPT
                ELSE
                    MOVE ROW1 TO ROW-TMP
                    MOVE COL1 TO COL-TMP
                    MOVE FACE1 TO FACE-TMP
                    ADD 5 TO R GIVING ROW1
                    ADD 20 TO C GIVING COL1
                    MOVE FACE TO FACE1
                    IF ROW-VALID AND COL-VALID THEN
                       MOVE "Y" TO PLACE-ISSUED
                       EVALUATE FACE
                          WHEN "EAST" MOVE "E" TO ROBO1
                          WHEN "WEST" MOVE "W" TO ROBO1
                          WHEN "NORTH" MOVE "N" TO ROBO1
                          WHEN "SOUTH" MOVE "S" TO ROBO1
                       END-EVALUATE
                       PERFORM DISPLAY-PARA
                    ELSE
                       MOVE SPACES TO RPT
                       MOVE ROW-TMP TO ROW1
                       MOVE COL-TMP TO COL1
                       MOVE FACE-TMP TO FACE1
                       STRING "COMMAND: " DELIMITED BY SIZE
                              CMD DELIMITED BY SIZE
                           "  OUT OF THE TABLE - IGNORED" INTO RPT
                       WRITE RPT.

       PLACE-PARA-X. EXIT.
                     
       DISPLAY-PARA.
                SUBTRACT 5 FROM ROW1 GIVING AFT-R.
                SUBTRACT 20 FROM COL1 GIVING AFT-C.
                MOVE FACE1 TO AFT-F.
                MOVE RPT-REC TO RPT.
                WRITE RPT.
                DISPLAY ROBO1 AT LINE ROW1 COLUMN COL1.

       DISPLAY-PARA-X. EXIT.   

       MOVE-PARA.
            MOVE ROW1 TO ROW-TMP.
            MOVE COL1 TO COL-TMP.
            IF FACE1 = "EAST"
               ADD 1 TO COL1
            ELSE IF FACE1 = "WEST"
                    SUBTRACT 1 FROM COL1
            ELSE IF FACE1 ="NORTH"
                    SUBTRACT 1 FROM ROW1
            ELSE IF FACE1 = "SOUTH"
                    ADD 1 TO ROW1
            END-IF.
            IF ROW-VALID AND COL-VALID THEN
                PERFORM DISPLAY-PARA
            ELSE
                MOVE ROW-TMP TO ROW1
                MOVE COL-TMP TO COL1
                MOVE SPACES TO RPT
                STRING "COMMAND: " DELIMITED BY SIZE
                       CMD DELIMITED BY SIZE
                       "  OUT OF THE TABLE - IGNORED" INTO RPT
                WRITE RPT
            END-IF.

       MOVE-PARA-X. EXIT.

       LEFT-PARA.
            IF FACE1 = "EAST"
                MOVE "NORTH" TO FACE1
                MOVE "N" TO ROBO1
            ELSE IF FACE1 = "WEST"
                    MOVE "SOUTH" TO FACE1
                    MOVE "S" TO ROBO1
            ELSE IF FACE1 ="NORTH"
                    MOVE "WEST" TO FACE1
                    MOVE "W" TO ROBO1
            ELSE IF FACE1 = "SOUTH"
                    MOVE "EAST" TO FACE1
                    MOVE "E" TO ROBO1
            END-IF.
            PERFORM DISPLAY-PARA.

       LEFT-PARA-X. EXIT.

       RIGHT-PARA.
            IF FACE1 = "EAST"
               MOVE "SOUTH" TO FACE1
               MOVE "S" TO ROBO1
            ELSE IF FACE1 = "WEST"
                    MOVE "NORTH" TO FACE1
                    MOVE "N" TO ROBO1
            ELSE IF FACE1 ="NORTH"
                    MOVE "EAST" TO FACE1
                    MOVE "E" TO ROBO1
            ELSE IF FACE1 = "SOUTH"
                    MOVE "WEST" TO FACE1
                    MOVE "W" TO ROBO1
            END-IF.
            PERFORM DISPLAY-PARA.

       RIGHT-PARA-X. EXIT.

       ROBO-MOVE-PARA.
            INITIALIZE CMD CMD-P.
            INITIALIZE RPT.
            READ IPFILE INTO CMD AT END MOVE "Y" TO IP-EOF.
            IF IP-EOF IS = 'Y'
                GO TO CLOSE-PARA.
            MOVE CMD TO CMD-P.
           MOVE CMD TO CMD-R.
           SUBTRACT 5 FROM ROW1 GIVING BEF-R.
           SUBTRACT 20 FROM COL1 GIVING BEF-C.
           MOVE FACE1 TO BEF-F.
            EVALUATE CMD1
                WHEN "PLACE" PERFORM PLACE-PARA THRU PLACE-PARA-X
                WHEN "MOVE"  PERFORM MOVE-PARA THRU MOVE-PARA-X
                WHEN "LEFT"  PERFORM LEFT-PARA THRU LEFT-PARA-X
                WHEN "RIGHT" PERFORM RIGHT-PARA THRU RIGHT-PARA-X
                WHEN OTHER   
                       STRING "COMMAND: " DELIMITED BY SIZE
                              CMD DELIMITED BY SIZE
                              "  INVALID COMMAND" INTO RPT
                       WRITE RPT     
            END-EVALUATE.
       
       ROBO-MOVE-PARA-X. EXIT.

       CLOSE-PARA.
           CLOSE IPFILE.
           CLOSE RPFILE.
