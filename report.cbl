       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPPGM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TO001-PS ASSIGN TO DD1
           ORGANIZATION IS SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS WS-FST-TO001.

       DATA DIVISION.

       FILE SECTION.
       FD TO001-PS
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
       01 TO001-PS-REC PIC X(80).

       WORKING-STORAGE SECTION.

       01 WS-VARS.
         05 WS-FST-TO001 PIC 9(02).
            88 C05-TO001-SUCCESS VALUE 00.
            88 C05-TO001-EOF     VALUE 10.

         05 WS-ERR-MSG.
            10 WS-ERR-LEN  PIC S9(04) COMP VALUE 800.
            10 WS-ERR-TEXT PIC X(80) OCCURS 10 TIMES.

         05 WS-LRECL      PIC S9(09) COMP VALUE 80.

         05 WS-LINE-COUNTER  PIC 9(02) VALUE 0.
         05 WS-PAGE-NUM      PIC 9(02) VALUE 1.

      * ---------------- HEADERS (RENAMED PROPERLY) ----------------

         05 HDR1.
            10 HDR1-F1     PIC X(24).
            10 HDR1-TITLE  PIC X(24)
               VALUE 'APPLICANTS DETAIL REPORT'.
            10 HDR1-F2     PIC X(24).

         05 HDR-FILLER.
            10 HDR-FILL    PIC X(80).

         05 HDR2.
            10 HDR2-DATE-LBL PIC X(06) VALUE 'DATE: '.
            10 HDR2-DATE.
               15 WS-YEAR  PIC 9(04).
               15 HDR2-D1  PIC X VALUE '-'.
               15 WS-MONTH PIC 9(02).
               15 HDR2-D2  PIC X VALUE '-'.
               15 WS-DAY   PIC 9(02).
            10 HDR2-SP1 PIC X(40).
            10 HDR2-PAGE-LBL PIC X(06) VALUE 'PAGE: '.
            10 HDR2-PAGE PIC 9(02).
            10 HDR2-SP2 PIC X(16).

         05 HDR3.
            10 HDR3-TIME-LBL PIC X(06) VALUE 'TIME: '.
            10 HDR3-TIME.
               15 WS-HOUR PIC 9(02).
               15 HDR3-T1 PIC X VALUE ':'.
               15 WS-MIN  PIC 9(02).
               15 HDR3-T2 PIC X VALUE ':'.
               15 WS-SEC  PIC 9(02).
            10 HDR3-SP1 PIC X(42).
            10 HDR3-REP PIC X(15) VALUE 'REPORT ID: 1001'.
            10 HDR3-SP2 PIC X(17).

         05 HDR4.
            10 HDR4-TEXT PIC X(33)
               VALUE 'APPROVED LOAN AMOUNT FOR NOVEMBER'.
            10 HDR4-SP PIC X(47).

         05 HDR5.
            10 HDR5-ID    PIC X(12) VALUE 'APPLICANT_ID'.
            10 HDR5-SP1   PIC X(03).
            10 HDR5-NAME  PIC X(14) VALUE 'APPLICANT_NAME'.
            10 HDR5-SP2   PIC X(09).
            10 HDR5-STATE PIC X(05) VALUE 'STATE'.
            10 HDR5-SP3   PIC X(08).
            10 HDR5-AMT   PIC X(20) VALUE 'APPROVED_LOAN_AMOUNT'.
            10 HDR5-SP4   PIC X(09).

         05 HDR-HYPHENS.
            10 HYP-ID    PIC X(12) VALUE ALL '-'.
            10 HYP-SP1   PIC X(03).
            10 HYP-NAME  PIC X(14) VALUE ALL '-'.
            10 HYP-SP2   PIC X(09).
            10 HYP-STATE PIC X(05) VALUE ALL '-'.
            10 HYP-SP3   PIC X(08).
            10 HYP-AMT   PIC X(20) VALUE ALL '-'.
            10 HYP-SP4   PIC X(09).

         05 HDR6.
            10 HDR6-SP1 PIC X(56).
            10 HDR6-PAGE-LBL PIC X(06) VALUE 'PAGE: '.
            10 HDR6-PAGE PIC 9(02).
            10 HDR6-SP2 PIC X(16).

         05 HDR7.
            10 HDR7-SP1 PIC X(56).
            10 HDR7-REP PIC X(15) VALUE 'REPORT ID: 1001'.
            10 HDR7-SP2 PIC X(09).

      * ---------------- DATA RECORD ----------------

         05 TO001-RECORD.
            10 TO001-APPID    PIC X(10).
            10 TO001-SP1      PIC X(03).
            10 TO001-APPNAME  PIC A(22).
            10 TO001-SP2      PIC X(03).
            10 TO001-STATE    PIC A(10).
            10 TO001-SP3      PIC X(03).
            10 TO001-ALAMOUNT PIC 9(08).
            10 TO001-SP4      PIC X(21).

      * ---------------- TRAILERS ----------------

         05 TRAILER1.
            10 TR1-HYP1 PIC X(30) VALUE ALL '-'.
            10 TR1-TEXT PIC X(11) VALUE 'END OF PAGE'.
            10 TR1-HYP2 PIC X(31) VALUE ALL '-'.

         05 TRAILER2.
            10 TR2-HYP1 PIC X(29) VALUE ALL '-'.
            10 TR2-TEXT PIC X(13) VALUE 'END OF REPORT'.
            10 TR2-HYP2 PIC X(30) VALUE ALL '-'.

       EXEC SQL
         INCLUDE SQLCA
       END-EXEC.

       EXEC SQL
         INCLUDE DCLAPDB
       END-EXEC.

       EXEC SQL
         INCLUDE DCLLDB
       END-EXEC.

       EXEC SQL
         DECLARE CUR1 CURSOR FOR
         SELECT A.APPLICANT_ID, APPLICANT_NAME, STATE,
                L.APPROVED_LOAN_AMOUNT
         FROM APPLICANT_DB A
         INNER JOIN LOAN_DB L
           ON A.APPLICANT_ID = L.APPLICANT_ID
         WHERE CITY = 'CHENNAI'
           AND MONTH(LOAN_APPROVAL_DATE) = 11
       END-EXEC.

       PROCEDURE DIVISION.

       0000-MAIN-PARA.
           PERFORM 1000-INIT-PARA
           PERFORM 3000-PROC-PARA
           PERFORM 9000-TERM-PARA.

       1000-INIT-PARA.
           CONTINUE.

       3000-PROC-PARA.
           PERFORM 3100-OPEN-PARA
           PERFORM 3200-FETCH-PARA
              UNTIL SQLCODE = 100
           PERFORM 3300-CLOSE-PARA.

       3100-OPEN-PARA.
           OPEN OUTPUT TO001-PS
           IF NOT C05-TO001-SUCCESS
               DISPLAY 'TO001 OPEN FAILED ' WS-FST-TO001
               GOBACK
           END-IF

           EXEC SQL
             OPEN CUR1
           END-EXEC

           IF SQLCODE NOT = 0
               DISPLAY 'CURSOR OPEN FAILED'
               CALL 'DSNTIAR'
                    USING SQLCA WS-ERR-MSG WS-LRECL
               DISPLAY WS-ERR-MSG
               GOBACK
           END-IF.

       3200-FETCH-PARA.
           INITIALIZE DCLAPPLICANT-DB
           INITIALIZE DCLLOAN-DB

           EXEC SQL
             FETCH CUR1
             INTO :HV-APPLICANT-ID,
                  :HV-APPLICANT-NAME,
                  :HV-STATE,
                  :HL-APPROVED-LOAN-AMOUNT
           END-EXEC

           EVALUATE TRUE
             WHEN SQLCODE = 0
               ADD 1 TO WS-LINE-COUNTER
               PERFORM 3250-WRITE-PARA
             WHEN SQLCODE = 100
               DISPLAY "ALL RECORDS FETCHED"
               WRITE TO001-PS-REC FROM HDR-FILLER
               WRITE TO001-PS-REC FROM TRAILER1
               WRITE TO001-PS-REC FROM HDR-FILLER
               WRITE TO001-PS-REC FROM TRAILER2
             WHEN OTHER
               DISPLAY "FETCH FAILED"
               CALL 'DSNTIAR'
                    USING SQLCA WS-ERR-MSG WS-LRECL
               DISPLAY WS-ERR-MSG
               GOBACK
           END-EVALUATE.

       3250-WRITE-PARA.

           IF WS-LINE-COUNTER = 1
              MOVE FUNCTION CURRENT-DATE(1:4)  TO WS-YEAR
              MOVE FUNCTION CURRENT-DATE(5:2)  TO WS-MONTH
              MOVE FUNCTION CURRENT-DATE(7:2)  TO WS-DAY
              MOVE FUNCTION CURRENT-DATE(9:2)  TO WS-HOUR
              MOVE FUNCTION CURRENT-DATE(11:2) TO WS-MIN
              MOVE FUNCTION CURRENT-DATE(13:2) TO WS-SEC

              MOVE WS-PAGE-NUM TO HDR2-PAGE
              MOVE WS-PAGE-NUM TO HDR6-PAGE

              WRITE TO001-PS-REC FROM HDR1
              WRITE TO001-PS-REC FROM HDR-FILLER
              WRITE TO001-PS-REC FROM HDR2
              WRITE TO001-PS-REC FROM HDR3
              WRITE TO001-PS-REC FROM HDR-FILLER
              WRITE TO001-PS-REC FROM HDR4
              WRITE TO001-PS-REC FROM HDR-FILLER
              WRITE TO001-PS-REC FROM HDR5
              WRITE TO001-PS-REC FROM HDR-HYPHENS
           END-IF

           MOVE HV-APPLICANT-ID         TO TO001-APPID
           MOVE HV-APPLICANT-NAME       TO TO001-APPNAME
           MOVE HV-STATE                TO TO001-STATE
           MOVE HL-APPROVED-LOAN-AMOUNT TO TO001-ALAMOUNT

           MOVE TO001-RECORD TO TO001-PS-REC
           WRITE TO001-PS-REC

           IF WS-LINE-COUNTER > 50
              ADD 1 TO WS-PAGE-NUM
              MOVE 0 TO WS-LINE-COUNTER
              WRITE TO001-PS-REC FROM HDR-FILLER
              WRITE TO001-PS-REC FROM TRAILER1
           END-IF.

       3300-CLOSE-PARA.
           EXEC SQL
             CLOSE CUR1
           END-EXEC

           CLOSE TO001-PS.

       9000-TERM-PARA.
           GOBACK.
