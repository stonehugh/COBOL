      **************************************************************************
      * Program Name:               CNTRLBRK                                   *
      * Workshop                    18.2                                       *
      * Developer:                  SAYLES                                     *
      * Created:                    Unknown                                    *
      * Modified:                   2020-08-22 hstone                          *
      * Modified:                   2020-08-24 hstone                          *
      * Developer Contact:                                                     *
      *                                                                        *
      **************************************************************************
      **************************************************************************
      * Modifications
      *  2020-08-22 HStone
      *    Add a column to the far-right side of the report named: SALARY-ACCUM
      *     In this column display the accumulated salary while each president
      *     was in office.
      *         Multiply the SALARY * number of years in office
      *     Added SAL-ACCUM-0.
      *     Added SALARY-ACCUM-RECORD-INFO.
      *     Added 401-COMPUTE-SALARY-ACCUM.
      *     Modified Some Fillers PIC X Sizes to allow format to fit page.
      *
      *  2020-08-24 HStone
      *    Also add a set of accumulators at the end of the report:
      *    Total of all presidents' Salaries (All presidents from all states)
      *    President that has highest Salary
      *    President that has the lowest Salary
      *    The average salary for all presidents
      *    Added 402-TEST-HIGH-LOW-SAL
      *    Added 999-DEBUG-OUTPUT-EXIT
      *    Added ACCUM-RECORD-TOTAL-SAL-O
      *    Added ACCUM-RECORD-HIGH-SAL
      *    Added ACCUM-RECORD-LOW-SAL
      *    Added ACCUM-RECORD-AVERAGE-SAL-O
      *    Modified 500-CONTROL-BREAK.
      **************************************************************************
      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CNTRLBRK.
       AUTHOR.        SAYLES.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTSORT.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01 PRINT-REC.
          05 FILLER                     PIC X(03)      VALUE SPACE.
          05 USA-STATE-O                PIC X(18).
          05 FIRST-NAME-O               PIC X(15).
          05 LAST-NAME-O                PIC X(20).
          05 ELECTED-O                  PIC X(6).
          05 LAST-YEAR-O                PIC X(6).
          05 ACCT-LIMIT-O               PIC $$,$$$,$$9.99.
          05 FILLER                     PIC X(02)      VALUE SPACES.
          05 ACCT-BALANCE-O             PIC $$,$$$,$$9.99.
          05 FILLER                     PIC X(01)      VALUE SPACES.
      *Adding the field SALARY-ACCUM.
      *Output is less any decimal point to save space on the report.
          05 SAL-ACCUM-0                PIC $,$$$,$$$,$$$.
      *
       FD  ACCT-REC RECORDING MODE F.
       01 ACCT-FIELDS.
          05 ACCT-NO                    PIC X(8).
          05 ACCT-LIMIT                 PIC S9(7)V99 COMP-3.
          05 ACCT-BALANCE               PIC S9(7)V99 COMP-3.
          05 LAST-NAME                  PIC X(20).
          05 FIRST-NAME                 PIC X(15).
          05 CLIENT-ADDR.
             10 STREET-ADDR             PIC X(25).
             10 CITY-COUNTY             PIC X(20).
             10 USA-STATE               PIC X(15).
                                              *> Input Sort Key
          05 RESERVED                   PIC X(7).
          05 COMMENTS                   PIC X(50).

      ***************************************************************
      * STORAGE SECTIONS
      ***************************************************************
       WORKING-STORAGE SECTION.
       01 PROGRAM-INDICATOR-SWITCHES.
          05 WS-EOF-INPUT-SW            PIC X(1)       VALUE 'N'.
             88 EOF-INPUT                              VALUE 'Y'.

       01 WS-BREAK-CONTROLS.
          05 WS-CONTROL-KEY             PIC X(15). *> Hold/Control Key
      ***************************************************************
      *New Record Layouts and Variables
       01 SALARY-ACCUM-RECORD-INFO.
          05 START-CCYY                 PIC 9(4).
          05 END-CCYY                   PIC 9(4).
      * Max years as president, WWII caused this to be 12 not 8.
          05 DIFF-CCYY                  PIC 9(2).
          05 SAL-ACCUM                  PIC 9(10).

      * Accumulators then the output Records for the
      * 402-TEST-HIGH-LOW-SAL.
       01 TOTAL-SALARY-ACCUM-O-TMP      PIC 9(09)V99.
       01 TOTAL-SALARY-ACCUM-O          PIC 9(15)V99.
       01 TOTAL-PRESIDENT-COUNT         PIC 9(2)       VALUE 0.

       01 HIGH-LOW-SALARY-ACCUM-O.
          05 INPUT-SAL                  PIC 9(15)      VALUE 0.
          05 NEW-HIGH-SAL               PIC 9(15)      VALUE 0.
          05 NEW-LOW-SAL                PIC 9(15)      VALUE 0.
          05 MULTIPLE-HIGH-SAL-FLAG     PIC 9(2)       VALUE 0.
          05 MULTIPLE-LOW-SAL-FLAG      PIC 9(2)       VALUE 0.

      * Total of all Presidents annual Salaries, this does not take into
      * account what they earned in office, just a total of all of the
      * base salaries.
       01 ACCUM-RECORD-TOTAL-SAL-O.
          05 FILLER                     PIC X(1)       VALUE SPACES.
          05 LABEL-TOTAL-PRES-SAL-O     PIC X(40)
                                                       VALUE
                'Total of all President Annual Salary '.
          05 FILLER                     PIC X(1)       VALUE '='.
          05 TOTAL-SALARY-0             PIC $$$,$$$,$$$,$$9.99.

      * Store the records for output. Higest Salary of President.
       01 ACCUM-RECORD-HIGH-SAL.
          05 FILLER                     PIC X(1)       VALUE SPACES.
          05 LABEL-HIGH-PRES-SAL        PIC X(40)
                                                       VALUE
                'Highest Salary of President '.

          05 FILLER                     PIC X(1)       VALUE '='.
          05 HIGH-SALARY-0              PIC $$$,$$$,$$$,$$9.99.
      * Store the records for output. Lowest Salary of President.
       01 ACCUM-RECORD-LOW-SAL.
          05 FILLER                     PIC X(1)       VALUE SPACES.
          05 LABEL-LOW-PRES-SAL         PIC X(40)
                                                       VALUE
                'Lowest Salary of President '.

          05 FILLER                     PIC X(1)       VALUE '='.
          05 LOW-SALARY-0               PIC $$$,$$$,$$$,$$9.99.

      * Store the records for output. Average Salary of President.
      * Base salary only not the accumulative salary.
       01 ACCUM-RECORD-AVERAGE-SAL-O.
          05 FILLER                     PIC X(1)       VALUE SPACES.
          05 LABEL-AVERAGE-PRES-SAL-O   PIC X(40)
                                                       VALUE
                'Average Presidents Salary '.
          05 FILLER                     PIC X(1)       VALUE '='.
          05 AVG-SALARY-0               PIC $$$,$$$,$$$,$$9.99.

      *************************************************************
      ****** Report headings begin here ******
      *************************************************************
       01 WS-BLANK-LINE                 PIC X(133)     VALUE SPACES.

       01 WS-HEADER-1.
          05 FILLER                     PIC X(1)       VALUE SPACES.
          05 FILLER                     PIC X(12)      VALUE
                'Report: A124'.
          05 DATE-O                     PIC X(10)      VALUE SPACE.
          05 FILLER                     PIC X(13)      VALUE SPACES.
          05 FILLER                     PIC X(47)
                                                       VALUE
                'Presidents Broken Out By State of Birth'.
          05 RPT-DATE                   PIC XXXX/XX/XX.
          05 FILLER                     PIC X(10)      VALUE SPACES.
017800    05 FILLER                     PIC X(5)       VALUE 'PAGE '.
017900    05 RPT-PAGE-NO                PIC ZZ.
018000    05 FILLER                     PIC X(12)      VALUE SPACES.
018100
018200 01 WS-HEADER-2.
018300    05 FILLER                     PIC X(3)       VALUE SPACES.
018400    05 FILLER                     PIC X(18)      VALUE 'STATE'.
018500    05 FILLER                     PIC X(9)       VALUE 'PRESIDENT'
           .
018600    05 FILLER                     PIC X(24)      VALUE SPACES.
019100    05 FILLER                     PIC X(7)       VALUE 'ELECTED'.
019200    05 FILLER                     PIC X(1)       VALUE SPACES.
019300    05 FILLER                     PIC X(8)       VALUE 'THRU'.
019500    05 FILLER                     PIC X(14)      VALUE 'SALARY'.
          05 FILLER                     PIC X(2)       VALUE SPACES.
019700    05 FILLER                     PIC X(12)      VALUE
                'NET WORTH'.
019700    05 FILLER                     PIC X(12)      VALUE
                'SALARY-ACCUM'.

018200 01 WS-HEADER-3.
          05 FILLER                     PIC X(3)       VALUE SPACES.
018400    05 FILLER                     PIC X(17)      VALUE ALL '='.
          05 FILLER                     PIC X(01)      VALUE SPACE.
018600    05 FILLER                     PIC X(32)      VALUE ALL '='.
          05 FILLER                     PIC X(01)      VALUE SPACE.
019100    05 FILLER                     PIC X(7)       VALUE '======='.
019200    05 FILLER                     PIC X(1)       VALUE SPACES.
019300    05 FILLER                     PIC X(7)       VALUE '====='.
019400    05 FILLER                     PIC X(01)      VALUE SPACES.
019500    05 FILLER                     PIC X(12)      VALUE ALL '='.
          05 FILLER                     PIC X(1)       VALUE SPACES.
019700    05 FILLER                     PIC X(13)      VALUE
                '============='.
          05 FILLER                     PIC X(1)       VALUE SPACES.
          05 FILLER                     PIC X(13)      VALUE
                '============='.
      *************************************************************
      ****** Control Break Subtotal Line ******
      *************************************************************
018200 01 WS-TRLR-LINE-1.
018300    05 FILLER                     PIC X(03)      VALUE SPACES.
          05 FILLER                     PIC X(12)      VALUE
                'Sub Totals:'.
          05 STATE-TRLR-LINE            PIC X(15).
          05 FILLER                     PIC X(16)      VALUE SPACE.
          05 FILLER                     PIC X(21)
                                                       VALUE
                'Salary | Net Worth: '
                JUST
                RIGHT.
          05 SALARY-SUB-TOT-OUT         PIC $$$,$$$,$$$.99.
          05 FILLER                     PIC X(01)      VALUE SPACES.
          05 NET-WORTH-SUB-TOT-OUT      PIC $$$,$$$,$$$.99.
          05 FILLER                     PIC X(14)      VALUE SPACE.

       01 WS-COUNTERS-AND-ACCUMULATORS.
          05 WS-CONTROL-BREAK-TOTAL     PIC S9(7)V99 COMP-3.
          05 WS-STATE-CTR               PIC  9(2) COMP.

       01 WS-FLAGS.
          05 WS-LASTREC                 PIC X          VALUE SPACE.
          05 WS-LINE-KTR                PIC 9(4) COMP  VALUE 0.
          05 WS-SALARY-SUB-TOT          PIC 9(09)V99   VALUE 0.
          05 WS-NET-WORTH-SUB-TOT       PIC 9(09)V99   VALUE 0.

      *------------------
       PROCEDURE DIVISION.
      *------------------
           PERFORM 100-INIT-RTN *> Housekeeping, Initial Report Headings
           PERFORM 300-PROCESS-RECORDS UNTIL EOF-INPUT
           PERFORM 500-CONTROL-BREAK *> Final Control Break paragraphs
           PERFORM 900-WRAP-UP
           GOBACK
           .
       100-INIT-RTN.
           INITIALIZE TOTAL-SALARY-ACCUM-O.
           MOVE FUNCTION CURRENT-DATE TO RPT-DATE.
           PERFORM 200-OPEN-FILES
           MOVE SPACES TO PRINT-REC
           PERFORM 700-READ-RECORD
           PERFORM 500-CONTROL-BREAK *> Initial Control creates Rpt Headings
           .
       150-INIT-WS-FIELDS.
           INITIALIZE WS-COUNTERS-AND-ACCUMULATORS
           .
       200-OPEN-FILES.
           OPEN INPUT ACCT-REC
           OPEN OUTPUT PRINT-LINE
           .
       300-PROCESS-RECORDS.
           IF NOT EOF-INPUT   *> No duplicating last record
              IF WS-CONTROL-KEY = USA-STATE  *> Control Break Conditional
                 PERFORM 400-MOVE-DATA
                 PERFORM 600-WRITE-DATA
                 PERFORM 700-READ-RECORD
              ELSE
                 PERFORM 500-CONTROL-BREAK
              END-IF
           END-IF
           .
       401-COMPUTE-SALARY-ACCUM.
           INITIALIZE SAL-ACCUM-0.
      * Using the PIC 9 Version of the Years calculate the number of
      * years that the President earned (well was paid) for.
      * First Compute the Difference DIFF-CCYY between the two dates,
      * then use that calculation to calculate the SAL-ACCUM.
           IF START-CCYY IS NUMERIC
              IF END-CCYY IS NUMERIC
                 COMPUTE
                    DIFF-CCYY =(END-CCYY - START-CCYY)
                 END-COMPUTE
                 IF DIFF-CCYY IS NUMERIC
                    COMPUTE
                       SAL-ACCUM ROUNDED = DIFF-CCYY * ACCT-LIMIT
                    END-COMPUTE
                 ELSE
                    PERFORM 999-DEBUG-OUTPUT-EXIT
                 END-IF
              ELSE
                 PERFORM 999-DEBUG-OUTPUT-EXIT
              END-IF
           ELSE
              PERFORM 999-DEBUG-OUTPUT-EXIT
           END-IF

           MOVE SAL-ACCUM TO SAL-ACCUM-0
           .

       402-TEST-HIGH-LOW-SAL.
      * Reset Formated Fields.
           MOVE 0 TO TOTAL-SALARY-0.
           MOVE 0 TO HIGH-SALARY-0.
           MOVE 0 TO LOW-SALARY-0.
           MOVE 0 TO AVG-SALARY-0.
           MOVE 0 TO TOTAL-SALARY-0.

      * Add to the Accumulator and move to Formated Fields (Do not Reset).
           ADD TOTAL-SALARY-ACCUM-O-TMP TO TOTAL-SALARY-ACCUM-O.
           MOVE TOTAL-SALARY-ACCUM-O TO TOTAL-SALARY-0.
      * This is getting calculated each time, but not displayed until
      * the end.

           IF TOTAL-PRESIDENT-COUNT > 0
              IF TOTAL-SALARY-ACCUM-O IS NUMERIC
                 IF TOTAL-PRESIDENT-COUNT IS NUMERIC
                    COMPUTE AVG-SALARY-0
                       =(TOTAL-SALARY-ACCUM-O / TOTAL-PRESIDENT-COUNT)
                    END-COMPUTE
                 ELSE
                    PERFORM 999-DEBUG-OUTPUT-EXIT
                 END-IF
              ELSE
                 PERFORM 999-DEBUG-OUTPUT-EXIT
              END-IF
           END-IF
           .
      * Move to Formated Fields (Do not Reset).
           MOVE TOTAL-SALARY-ACCUM-O TO TOTAL-SALARY-0.
      * Set the INPUT-SAL for the Highest / Lowest Logic statements.
           MOVE ACCT-LIMIT TO INPUT-SAL.
      * Highest Salary Logic.
      *  First If.
      *  Set the inital Values of Highes / Lowest 1st President could be
      *  highest, could be lowest set both if a 0 Value.
      * Else If = or > Greater than.
      *  GT
      *   Move 0 to Reset the accumulator because a new high value found.
      *   Move the INPUT-SAL to the NEW-HIGH-SAL.
      *  Equal Add 1 as a duplicate has been found.
      * After out of Loop move the NEW-HIGH-SAL TO HIGH-SALARY-0, may
      * or may not have changed doesn't matter.
           IF NEW-HIGH-SAL = 0
              MOVE INPUT-SAL TO NEW-HIGH-SAL
           ELSE
              IF INPUT-SAL >= NEW-HIGH-SAL
                 IF INPUT-SAL > NEW-HIGH-SAL
                    MOVE 0 TO MULTIPLE-HIGH-SAL-FLAG
                    IF INPUT-SAL = NEW-HIGH-SAL
                       ADD 1 TO MULTIPLE-HIGH-SAL-FLAG
                    END-IF
                    MOVE INPUT-SAL TO NEW-HIGH-SAL
                 END-IF
              END-IF
              MOVE NEW-HIGH-SAL TO HIGH-SALARY-0
           .

      * Lowest Salary Logic.
      *  First If.
      *  Set the inital Values of Highes / Lowest 1st President could be
      *  highest, could be lowest set both if 0 Value.
      * Else If = or < less than.
      *  LT
      *   Move 0 to Reset the accumulator because a new low value found.
      *   Move the INPUT-SAL to the NEW-LOW-SAL.
      *  Equal Add 1 as a duplicate has been found.
      * After out of Loop move the NEW-LOW-SAL TO LOW-SALARY-0, may
      * or may not have changed doesn't matter.
           IF NEW-LOW-SAL = 0
              MOVE INPUT-SAL TO NEW-LOW-SAL
           ELSE
              IF INPUT-SAL <= NEW-LOW-SAL
                 IF INPUT-SAL < NEW-LOW-SAL
                    MOVE 0 TO MULTIPLE-LOW-SAL-FLAG
                 END-IF
                 IF INPUT-SAL = NEW-LOW-SAL
                    ADD 1 TO MULTIPLE-LOW-SAL-FLAG
                 END-IF
                 MOVE INPUT-SAL TO NEW-LOW-SAL
              END-IF
           END-IF
           MOVE NEW-LOW-SAL TO LOW-SALARY-0
           .

       400-MOVE-DATA.
           MOVE SPACES TO PRINT-REC
           ADD +1 TO WS-STATE-CTR
           IF WS-STATE-CTR > 1 *> Logic to create outline view in State column
              MOVE SPACES TO USA-STATE-O
           ELSE
              MOVE USA-STATE TO USA-STATE-O
                                STATE-TRLR-LINE
           END-IF
           ADD ACCT-LIMIT TO WS-SALARY-SUB-TOT.
      *HStone Total Accumulator
           MOVE ACCT-LIMIT TO TOTAL-SALARY-ACCUM-O-TMP
           ADD 1 TO TOTAL-PRESIDENT-COUNT
           ADD ACCT-BALANCE TO WS-NET-WORTH-SUB-TOT
      *** The ACCT file is actually a repurposed file for the presidents
      *** The first four bytes is their inaugural yr => last year in office
           MOVE ACCT-NO(1:4) TO ELECTED-O
      *HStone Added to a PIC 9 Value for 401-COMPUTE-SALARY-ACCUM.
           MOVE ACCT-NO(1:4) TO START-CCYY
           MOVE ACCT-NO(5:4) TO LAST-YEAR-O
      *HStone Added to a PIC 9 Value for 401-COMPUTE-SALARY-ACCUM.
           MOVE ACCT-NO(5:4) TO END-CCYY
           MOVE ACCT-LIMIT TO ACCT-LIMIT-O
           MOVE ACCT-BALANCE TO ACCT-BALANCE-O
           MOVE LAST-NAME TO LAST-NAME-O
           MOVE FIRST-NAME TO FIRST-NAME-O
           PERFORM 401-COMPUTE-SALARY-ACCUM
           PERFORM 402-TEST-HIGH-LOW-SAL
           .
       500-CONTROL-BREAK.
           IF WS-LINE-KTR > 0  *> Check for first time (beginning of program)
              MOVE WS-SALARY-SUB-TOT TO SALARY-SUB-TOT-OUT
              MOVE WS-NET-WORTH-SUB-TOT TO NET-WORTH-SUB-TOT-OUT
              WRITE PRINT-REC FROM WS-BLANK-LINE
              WRITE PRINT-REC FROM WS-TRLR-LINE-1
              WRITE PRINT-REC FROM WS-BLANK-LINE
              WRITE PRINT-REC FROM WS-BLANK-LINE
           END-IF
           IF NOT EOF-INPUT
              ADD +1 TO WS-LINE-KTR
              MOVE ZERO TO WS-SALARY-SUB-TOT, WS-NET-WORTH-SUB-TOT
              MOVE WS-LINE-KTR TO RPT-PAGE-NO
              MOVE USA-STATE TO WS-CONTROL-KEY   *> SET NEW CONTROL KEY
              WRITE PRINT-REC FROM WS-BLANK-LINE
              WRITE PRINT-REC FROM WS-HEADER-1
              WRITE PRINT-REC FROM WS-BLANK-LINE
              WRITE PRINT-REC FROM WS-HEADER-2
              WRITE PRINT-REC FROM WS-HEADER-3
              PERFORM 150-INIT-WS-FIELDS
           END-IF
      *** HStone.
      *** This is just adding logic for after the rest of the report has
      *** been written. If the EOF-INPUT returns true write the records
      *** that were calculated through the processing in the
      *** 402-TEST-HIGH-LOW-SAL.
           IF EOF-INPUT
              MOVE SPACES TO PRINT-REC
              WRITE PRINT-REC FROM ACCUM-RECORD-TOTAL-SAL-O
              WRITE PRINT-REC FROM ACCUM-RECORD-HIGH-SAL
              WRITE PRINT-REC FROM ACCUM-RECORD-LOW-SAL
              WRITE PRINT-REC FROM ACCUM-RECORD-AVERAGE-SAL-O
           END-IF
           .
      *** 2020-08-24 HStone. End

       600-WRITE-DATA.
           WRITE PRINT-REC
           .
       700-READ-RECORD.
           READ ACCT-REC
           AT END
              MOVE 'Y' TO WS-EOF-INPUT-SW
           END-READ
           .

       999-DEBUG-OUTPUT-EXIT.
           DISPLAY "ACCUM-RECORD-TOTAL-SAL-O:  "
                   ACCUM-RECORD-TOTAL-SAL-O.
           DISPLAY " ".
           DISPLAY "ACCUM-RECORD-HIGH-SAL:     "
                   ACCUM-RECORD-HIGH-SAL.
           DISPLAY " ".
           DISPLAY "MULTIPLE-HIGH-SAL-FLAG:    "
                   MULTIPLE-HIGH-SAL-FLAG.
           DISPLAY " ".
           DISPLAY "ACCUM-RECORD-LOW-SAL:      "
                   ACCUM-RECORD-LOW-SAL.
           DISPLAY " ".
           DISPLAY "MULTIPLE-LOW-SAL-FLAG:     "
                   MULTIPLE-LOW-SAL-FLAG.
           DISPLAY " ".
           DISPLAY "ACCUM-RECORD-AVERAGE-SAL-O:"
                   ACCUM-RECORD-AVERAGE-SAL-O.
           DISPLAY " ".
           DISPLAY "**************************************************".
           DISPLAY " ".
           GOBACK.

       900-WRAP-UP.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.