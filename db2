      ************** MINI CASE STUDY:ASSESSMENT **************          00010003
       IDENTIFICATION DIVISION.                                         00020003
       PROGRAM-ID.      COBOL1.                                         00030003
      ************ SELECTING INPUT/OUTPUT FILES **************          00040003
       ENVIRONMENT DIVISION.                                            00050003
       INPUT-OUTPUT SECTION.                                            00060003
       FILE-CONTROL.                                                    00070003
           SELECT TI001-PS1 ASSIGN TO DD1                               00080003
                  ORGANIZATION IS SEQUENTIAL                            00090003
                  ACCESS       IS SEQUENTIAL                            00100003
                  FILE STATUS  IS WS-TI001-FST.                         00110003
           SELECT TO001-PS2 ASSIGN TO DD2                               00120003
                  ORGANIZATION IS SEQUENTIAL                            00130003
                  ACCESS       IS SEQUENTIAL                            00140003
                  FILE STATUS  IS WS-TO001-FST.                         00150003
           SELECT TO002-KSDS ASSIGN TO DD3                              00151015
           ORGANIZATION  IS        INDEXED                              00152015
           ACCESS        IS         RANDOM                              00153015
           RECORD KEY    IS      TO002-KEY                              00154016
           FILE STATUS   IS   WS-TO002-FST                              00155016
           .                                                            00156015
      ************** FILE LAYOUT OF INPUT FILE ***************          00160003
       DATA DIVISION.                                                   00170003
       FILE SECTION.                                                    00180003
       FD TI001-PS1.                                                    00190003
       01 TI001-PS1-REC.                                                00200003
              10 TI001-APPLICANT-ID      PIC X(10).                     00210003
              10 FILLER                  PIC X(01).                     00220003
              10 TI001-APPLICANT-NAME    PIC X(32).                     00230003
              10 FILLER                  PIC X(01).                     00240003
              10 TI001-DOORNO            PIC X(10).                     00250003
              10 FILLER                  PIC X(01).                     00260003
              10 TI001-STREET            PIC X(20).                     00270003
              10 FILLER                  PIC X(01).                     00280003
              10 TI001-CITY              PIC X(20).                     00290003
              10 FILLER                  PIC X(01).                     00300003
              10 TI001-STATE             PIC X(20).                     00310003
              10 FILLER                  PIC X(01).                     00320003
              10 TI001-PIN               PIC 9(06).                     00330003
              10 FILLER                  PIC X(01).                     00340003
              10 TI001-PHONENO           PIC X(10).                     00350003
              10 FILLER                  PIC X(01).                     00360003
              10 TI001-LOANID            PIC 9(02).                     00370003
              10 FILLER                  PIC X(01).                     00380003
              10 TI001-LOANTYPE          PIC X(10).                     00390003
              10 FILLER                  PIC X(01).                     00400003
              10 TI001-APPLOAMT          PIC 9(08).                     00410003
              10 FILLER                  PIC X(01).                     00420003
              10 TI001-APPDATE           PIC X(10).                     00430003
              10 FILLER                  PIC X(01).                     00440003
              10 TI001-LOTENURE          PIC 9(02).                     00450016
              10 FILLER                  PIC X(01).                     00460003
              10 TI001-INTRATE           PIC 9(02).9(01).               00470003
              10 FILLER                  PIC X(01).                     00480003
              10 TI001-LOINDCATOR        PIC X(05).                     00490003
              10 FILLER                  PIC X(01).                     00500003
              10 TI001-REPAYDATE         PIC X(02).                     00510003
              10 FILLER                  PIC X(114).                    00520003
      ************** FILE LAYOUT OF OUTPUT FILE **************          00530003
       FD TO001-PS2.                                                    00540003
       01 TO001-PS2-REC.                                                00550003
              10 TO001-APPLICANT-ID      PIC X(10).                     00560003
              10 FILLER                  PIC X(01).                     00570003
              10 TO001-APPLICANT-NAME    PIC X(32).                     00580003
              10 FILLER                  PIC X(01).                     00590003
              10 TO001-DOORNO            PIC X(10).                     00600003
              10 FILLER                  PIC X(01).                     00610003
              10 TO001-STREET            PIC X(20).                     00620003
              10 FILLER                  PIC X(01).                     00630003
              10 TO001-CITY              PIC X(20).                     00640003
              10 FILLER                  PIC X(01).                     00650003
              10 TO001-STATE             PIC X(20).                     00660003
              10 FILLER                  PIC X(01).                     00670003
              10 TO001-PIN               PIC 9(06).                     00680003
              10 FILLER                  PIC X(01).                     00690003
              10 TO001-PHONENO           PIC X(10).                     00700004
              10 FILLER                  PIC X(01).                     00710003
              10 TO001-LOANID            PIC 9(02).                     00720003
              10 FILLER                  PIC X(01).                     00730003
              10 TO001-LOANTYPE          PIC X(10).                     00740003
              10 FILLER                  PIC X(01).                     00750003
              10 TO001-APPLOAMT          PIC 9(08).                     00760003
              10 FILLER                  PIC X(01).                     00770003
              10 TO001-APPDATE           PIC X(10).                     00780003
              10 FILLER                  PIC X(01).                     00790003
              10 TO001-LOTENURE          PIC X(02).                     00800003
              10 FILLER                  PIC X(01).                     00810003
              10 TO001-INTRATE           PIC 9(02).9(01).               00820003
              10 FILLER                  PIC X(01).                     00830003
              10 TO001-LOINDCATOR        PIC X(05).                     00840003
              10 FILLER                  PIC X(01).                     00850003
              10 TO001-REPAYDATE         PIC X(02).                     00860003
              10 FILLER                  PIC X(01).                     00870003
              10 TO001-REASON            PIC X(40).                     00880003
              10 FILLER                  PIC X(73).                     00890003
       FD TO002-KSDS.                                                   00891015
       01 TO002-KSDS-REC.                                               00892015
         05 TO002-KEY        PIC X(12).                                 00893015
         05 F                PIC X(01).                                 00894015
         05 TO002-LOANTYPE   PIC X(10).                                 00895015
         05 F                PIC X(67).                                 00896015
      ***** DECLARING THE VARIABLES USED IN THE PROGRAM *****           00900003
       WORKING-STORAGE SECTION.                                         00910003
           EXEC SQL INCLUDE SQLCA END-EXEC.                             00911006
           EXEC SQL INCLUDE APPDB END-EXEC.                             00912013
           EXEC SQL INCLUDE LONDB END-EXEC.                             00913012
       01 WS-VARS.                                                      00920003
           05 WS-TI001-FST PIC 9(02).                                   00930003
              88 WS-TI001-FST-SUCCESS        VALUE ZEROES.              00940003
              88 WS-TI001-FST-EOF            VALUE 10.                  00950003
           05 WS-TO001-FST PIC 9(02).                                   00960003
              88 WS-TO001-FST-SUCCESS        VALUE ZEROES.              00970003
              88 WS-TO001-FST-EOF            VALUE 10.                  00980003
           05 WS-TO002-FST PIC 9(02).                                   00980115
              88 WS-TO002-FST-SUCCESS        VALUE ZEROES.              00980215
              88 WS-TO002-FST-EOF            VALUE 10.                  00980315
           05 WS-FIR                PIC 9(02).                          00980415
           05 WS-MONTH              PIC 9(02).                          00980515
           05 WS-YEAR               PIC 9(04).                          00980615
           05 WS-TOT-REP-AMNT       PIC 9(09).                          00980715
           05 WS-MON-REP-AMNT       PIC 9(09).                          00980815
           05 WS-NEXT-REP-DUE-DATE  PIC X(10).                          00980915
           05 WS-BALANCE-AMNT       PIC 9(09).                          00981015
           05 WS-ERR-MSG.                                               00981114
              10 WS-ERR-LEN     PIC S9(4) COMP VALUE 800.               00982014
              10 WS-ERR-TEXT    PIC X(80) OCCURS 10 TIMES.              00983014
           05 WS-ERR-LRECL      PIC S9(9) COMP VALUE 80.                00984014
       01 WS-COUNTER       PIC 9(03)         VALUE ZERO.                00990003
       01 WS-VALID-REC     PIC 9(02).                                   01000003
       01 WS-STRING-PTR    PIC 9(02).                                   01010003
       01 WS-INVALID-REC   PIC 9(02).                                   01020003
       01 WS-REASON-OUT    PIC X(40).                                   01030003
       PROCEDURE DIVISION.                                              01040003
       0000-MAIN-PARA.                                                  01050003
           PERFORM 1000-INIT-PARA                                       01060003
           THRU    1000-INIT-PARA-EXIT                                  01070003
           PERFORM 3000-PROC-PARA                                       01080003
           THRU    3000-PROC-PARA-EXIT                                  01090003
           PERFORM 9000-TERM-PARA                                       01100003
           .                                                            01110003
       0000-MAIN-PARA-EXIT.                                             01120003
           EXIT                                                         01130003
           .                                                            01140003
      ************** INITIALIZING THE VARIABLES **************          01150003
       1000-INIT-PARA.                                                  01160003
           INITIALIZE  WS-VALID-REC                                     01170003
           INITIALIZE  WS-INVALID-REC                                   01180003
           .                                                            01190003
       1000-INIT-PARA-EXIT.                                             01200003
           EXIT                                                         01210003
           .                                                            01220003
       3000-PROC-PARA.                                                  01230003
           PERFORM 3100-OPEN-PARA                                       01240003
           THRU    3100-OPEN-PARA-EXIT                                  01250003
           PERFORM 3200-READ-PARA                                       01260003
           THRU    3200-READ-PARA-EXIT                                  01270003
           UNTIL   WS-TI001-FST-EOF                                     01280003
           PERFORM 3300-CLOSE-PARA                                      01290003
           THRU    3300-CLOSE-PARA-EXIT                                 01300003
           .                                                            01310003
       3000-PROC-PARA-EXIT.                                             01320003
           EXIT                                                         01330003
           .                                                            01340003
      ********** OPENING THE INPUT AND OUTPUT FILES **********          01350003
       3100-OPEN-PARA.                                                  01360003
           OPEN INPUT TI001-PS1                                         01370003
           EVALUATE TRUE                                                01380003
                WHEN WS-TI001-FST-SUCCESS                               01390003
                     DISPLAY "INPUT FILE OPENED"                        01400003
                WHEN OTHER                                              01410003
                     DISPLAY "INPUT FILE OPEN FAILED" WS-TI001-FST      01420003
                     PERFORM 9000-TERM-PARA                             01430003
           END-EVALUATE                                                 01440003
           OPEN OUTPUT TO001-PS2                                        01450003
           EVALUATE TRUE                                                01460003
                WHEN WS-TO001-FST-SUCCESS                               01470003
                     DISPLAY "OUTPUT OPENED"                            01480003
                WHEN OTHER                                              01490003
                     DISPLAY "OUTPUT OPEN FAILED" WS-TO001-FST          01500003
                     PERFORM 9000-TERM-PARA                             01510003
           END-EVALUATE                                                 01520003
           OPEN OUTPUT TO002-KSDS                                       01521015
           EVALUATE TRUE                                                01522015
                WHEN WS-TO002-FST-SUCCESS                               01523016
                     DISPLAY 'TO002 OPENED'                             01524016
                WHEN OTHER                                              01525016
                     DISPLAY 'TO002 OPEN FAILED ' WS-TO002-FST          01526016
                     PERFORM 9000-TERM-PARA                             01527016
           END-EVALUATE                                                 01528015
           .                                                            01529015
       3100-OPEN-PARA-EXIT.                                             01540003
           EXIT                                                         01550003
           .                                                            01560003
      **************** READING THE INPUT FILE ****************          01570003
       3200-READ-PARA.                                                  01580003
           MOVE SPACES TO TI001-PS1-REC TO001-PS2-REC WS-REASON-OUT     01590003
                          TO002-KSDS-REC                                01591016
           MOVE 1 TO WS-STRING-PTR                                      01600003
           READ TI001-PS1                                               01610003
           EVALUATE TRUE                                                01620003
           WHEN WS-TI001-FST-SUCCESS                                    01630003
                  ADD 1 TO WS-COUNTER                                   01640003
                  PERFORM 3210-VALID-PARA                               01650003
                  THRU    3210-VALID-PARA-EXIT                          01660003
           WHEN WS-TI001-FST-EOF                                        01670003
                IF WS-COUNTER = 00                                      01680003
                  DISPLAY "FILE IS EMPTY"                               01690003
                ELSE                                                    01700003
                  DISPLAY "END OF FILE REACHED"                         01710003
                  DISPLAY "RECORD PROCCESSED:" WS-COUNTER               01720003
                END-IF                                                  01730003
           WHEN OTHER                                                   01740003
                  DISPLAY "INPUT READ FAILED" WS-TI001-FST              01750003
           END-EVALUATE                                                 01760003
           .                                                            01770003
       3200-READ-PARA-EXIT.                                             01780003
           EXIT                                                         01790003
           .                                                            01800003
      ************** CHECKING THE INPUT RECORD ***************          01810003
       3210-VALID-PARA.                                                 01820003
           EVALUATE TRUE                                                01830003
              WHEN                                                      01840003
                   TI001-APPLICANT-ID   IS GREATER THAN SPACES AND      01850003
      *            TI001-APPLICANT-NAME IS GREATER THAN SPACES AND      01860003
      *            TI001-DOORNO         IS GREATER THAN SPACES AND      01870003
      *            TI001-STREET         IS GREATER THAN SPACES AND      01880003
      *            TI001-CITY           IS GREATER THAN SPACES AND      01890003
      *            TI001-STATE          IS GREATER THAN SPACES AND      01900003
      *            TI001-PIN            IS NUMERIC             AND      01910003
                   TI001-PHONENO        IS NUMERIC             AND      01920003
      *            TI001-LOANID         IS NUMERIC             AND      01930003
                   TI001-LOANTYPE       = "HOUSING"            AND      01940003
      *            TI001-APPLOAMT       IS NUMERIC             AND      01950003
                   TI001-APPDATE(1:4)   IS NUMERIC             AND      01960003
                   TI001-APPDATE(5:1)   = "-" OR "/"           AND      01970003
                   TI001-APPDATE(6:2)   IS NUMERIC             AND      01980003
                   TI001-APPDATE(8:1)   = "-" OR "/"           AND      01990003
                   TI001-APPDATE(9:2)   IS NUMERIC             AND      02000003
      *            TI001-LOTENURE       IS GREATER THAN SPACES AND      02010003
                   TI001-INTRATE(1:2)   IS NUMERIC             AND      02020003
                   TI001-INTRATE(3:1)   = "."                  AND      02030003
                   TI001-INTRATE(4:1)   IS NUMERIC             AND      02040003
                   TI001-LOINDCATOR     = "OPEN " OR "CLOSE"            02050004
      *            TI001-REPAYDATE      IS NUMERIC                      02060003
                   ADD 1 TO WS-VALID-REC                                02070003
                   DISPLAY WS-COUNTER ":VALID RECORD"                   02080003
                   PERFORM 3211-LOAD-PARA                               02081007
                   THRU 3211-LOAD-PARA-EXIT                             02082007
              WHEN OTHER                                                02090003
                      ADD 1 TO WS-INVALID-REC                           02100003
                        PERFORM 3212-INVALID-PARA                       02110003
                        THRU 3212-INVALID-PARA-EXIT                     02120003
           END-EVALUATE                                                 02130003
           .                                                            02140003
       3210-VALID-PARA-EXIT.                                            02150003
           EXIT                                                         02160003
           .                                                            02170003
      *    ------------------------------------------------------       02170116
       3211-LOAD-PARA.                                                  02171007
           MOVE SPACES TO DCLAPPLICANT-DB                               02172013
           MOVE TI001-APPLICANT-ID         TO HV-APPLICANT-ID-TEXT      02173016
           MOVE LENGTH OF HV-APPLICANT-ID-TEXT TO                       02173113
                          HV-APPLICANT-ID-LEN                           02173213
           MOVE TI001-APPLICANT-NAME     TO HV-APPLICANT-NAME-TEXT      02173313
           MOVE LENGTH OF HV-APPLICANT-NAME-TEXT TO                     02173413
                          HV-APPLICANT-NAME-LEN                         02173513
           MOVE TI001-DOORNO             TO HV-DOORNO-TEXT              02173613
           MOVE LENGTH OF HV-DOORNO-TEXT TO                             02173713
                          HV-DOORNO-LEN                                 02173813
           MOVE TI001-STREET                TO HV-STREET-TEXT           02173916
           MOVE LENGTH OF HV-STREET-TEXT    TO HV-STREET-LEN            02174016
           MOVE TI001-CITY                  TO HV-CITY-TEXT             02176016
           MOVE LENGTH OF HV-CITY-TEXT      TO HV-CITY-LEN              02177016
           MOVE TI001-STATE                 TO HV-STATE-TEXT            02178016
           MOVE LENGTH OF HV-STATE-TEXT     TO HV-STATE-LEN             02179016
           MOVE TI001-PIN                   TO HV-PIN                   02180016
           MOVE TI001-PHONENO               TO HV-PHONE-NO              02180116
      *    ------------------------------------------------------       02180216
           MOVE SPACES TO DCLLOAN-DB                                    02180312
           MOVE TI001-APPLICANT-ID          TO HV-APP-ID-TEXT           02180416
           MOVE LENGTH OF HV-APP-ID-TEXT    TO                          02180516
                          HV-APP-ID-LEN                                 02180612
           MOVE TI001-LOANID                TO HV-LOAN-ID-TEXT          02180716
           MOVE LENGTH OF HV-LOAN-ID-TEXT   TO HV-LOAN-ID-LEN           02180816
           MOVE TI001-LOANTYPE              TO HV-LOAN-TYPE-TEXT        02180916
           MOVE LENGTH OF HV-LOAN-TYPE-TEXT TO HV-LOAN-TYPE-LEN         02181012
           MOVE TI001-APPLOAMT              TO HV-APRVED-LN-AMNT        02181116
           MOVE TI001-APPDATE               TO HV-LN-APRD-DATE          02181216
           MOVE TI001-LOTENURE              TO HV-LOAN-TENURE-TEXT      02181316
           MOVE LENGTH OF HV-LOAN-TENURE-TEXT TO HV-LOAN-TENURE-LEN     02181412
           MOVE TI001-INTRATE                 TO HV-FD-INT-RATE         02181516
           MOVE TI001-LOINDCATOR              TO HV-LOAN-INDICATOR-TEXT 02181616
           MOVE LENGTH OF HV-LOAN-INDICATOR-TEXT TO                     02181712
                          HV-LOAN-INDICATOR-LEN                         02181812
           MOVE TI001-REPAYDATE                  TO HV-REPAY-DAY-TEXT   02181916
           MOVE LENGTH OF HV-REPAY-DAY-TEXT      TO HV-REPAY-DAY-LEN    02182016
      *    ------------------------------------------------------       02182116
           MOVE TI001-INTRATE(1:2)  TO WS-FIR                           02182216
           COMPUTE WS-TOT-REP-AMNT = TI001-APPLOAMT +                   02182316
                  ((TI001-APPLOAMT * TI001-LOTENURE * WS-FIR) / 100)    02182416
           MOVE WS-TOT-REP-AMNT TO HV-TOT-REPAY-AMNT                    02182516
      *    ------------------------------------------------------       02182616
           COMPUTE WS-MON-REP-AMNT = WS-TOT-REP-AMNT /                  02182716
                                     (TI001-LOTENURE * 12)              02182816
           MOVE WS-MON-REP-AMNT TO HV-MONTH-REPAY-AMNT                  02182916
      *    ------------------------------------------------------       02183016
           EVALUATE TRUE                                                02184017
           WHEN (TI001-APPDATE(6:2) = 01)                               02185017
             IF TI001-APPDATE(9:2) > 28                                 02185117
               MOVE TI001-APPDATE(1:4) TO WS-NEXT-REP-DUE-DATE(1:4)     02185217
               MOVE TI001-APPDATE(5:1) TO WS-NEXT-REP-DUE-DATE(5:1)     02185317
               MOVE TI001-APPDATE(6:2) TO WS-MONTH                      02185417
               COMPUTE WS-MONTH = WS-MONTH + 1                          02185517
               MOVE WS-MONTH          TO WS-NEXT-REP-DUE-DATE(6:2)      02185617
               MOVE TI001-APPDATE(8:1) TO WS-NEXT-REP-DUE-DATE(8:1)     02185717
               MOVE 28                TO TI001-APPDATE(9:2)             02185817
               MOVE TI001-APPDATE(9:2) TO WS-NEXT-REP-DUE-DATE(9:2)     02185917
               MOVE WS-NEXT-REP-DUE-DATE TO HV-NXT-REPAY-DUEDATE        02186017
      *                                                                 02186117
             ELSE                                                       02186217
               MOVE TI001-APPDATE(1:4) TO WS-NEXT-REP-DUE-DATE(1:4)     02186317
               MOVE TI001-APPDATE(5:1) TO WS-NEXT-REP-DUE-DATE(5:1)     02186417
               MOVE TI001-APPDATE(6:2) TO WS-MONTH                      02186517
               COMPUTE WS-MONTH = WS-MONTH + 1                          02186617
               MOVE WS-MONTH           TO WS-NEXT-REP-DUE-DATE(6:2)     02186717
               MOVE TI001-APPDATE(8:1) TO WS-NEXT-REP-DUE-DATE(8:1)     02186817
               MOVE TI001-APPDATE(9:2) TO WS-NEXT-REP-DUE-DATE(9:2)     02186917
               MOVE WS-NEXT-REP-DUE-DATE TO HV-NXT-REPAY-DUEDATE        02187017
             END-IF                                                     02187117
           WHEN (TI001-APPDATE(6:2) = 03 OR 05 OR 07 OR 08 OR 10)       02187217
             IF TI001-APPDATE(9:2) = 31                                 02187317
               MOVE TI001-APPDATE(1:4) TO WS-NEXT-REP-DUE-DATE(1:4)     02187417
               MOVE TI001-APPDATE(5:1) TO WS-NEXT-REP-DUE-DATE(5:1)     02187517
               MOVE TI001-APPDATE(6:2) TO WS-MONTH                      02187617
               COMPUTE WS-MONTH = WS-MONTH + 1                          02187717
               MOVE WS-MONTH          TO WS-NEXT-REP-DUE-DATE(6:2)      02187817
               MOVE TI001-APPDATE(8:1) TO WS-NEXT-REP-DUE-DATE(8:1)     02187917
               MOVE 30                TO TI001-APPDATE(9:2)             02188017
               MOVE TI001-APPDATE(9:2) TO WS-NEXT-REP-DUE-DATE(9:2)     02188117
               MOVE WS-NEXT-REP-DUE-DATE TO HV-NXT-REPAY-DUEDATE        02188217
      *                                                                 02188317
             ELSE                                                       02188417
               MOVE TI001-APPDATE(1:4) TO WS-NEXT-REP-DUE-DATE(1:4)     02188517
               MOVE TI001-APPDATE(5:1) TO WS-NEXT-REP-DUE-DATE(5:1)     02188617
               MOVE TI001-APPDATE(6:2) TO WS-MONTH                      02188717
               COMPUTE WS-MONTH = WS-MONTH + 1                          02188817
               MOVE WS-MONTH          TO WS-NEXT-REP-DUE-DATE(6:2)      02188917
               MOVE TI001-APPDATE(8:1) TO WS-NEXT-REP-DUE-DATE(8:1)     02189017
               MOVE TI001-APPDATE(9:2) TO WS-NEXT-REP-DUE-DATE(9:2)     02189117
               MOVE WS-NEXT-REP-DUE-DATE TO HV-NXT-REPAY-DUEDATE        02189217
             END-IF                                                     02189317
           WHEN (TI001-APPDATE(6:2) = 02 OR 04 OR 06 OR 09 OR 11)       02189417
               MOVE TI001-APPDATE(1:4) TO WS-NEXT-REP-DUE-DATE(1:4)     02189517
               MOVE TI001-APPDATE(5:1) TO WS-NEXT-REP-DUE-DATE(5:1)     02189617
               MOVE TI001-APPDATE(6:2) TO WS-MONTH                      02189717
               COMPUTE WS-MONTH = WS-MONTH + 1                          02189817
               MOVE WS-MONTH          TO WS-NEXT-REP-DUE-DATE(6:2)      02189917
               MOVE TI001-APPDATE(8:1) TO WS-NEXT-REP-DUE-DATE(8:1)     02190017
               MOVE TI001-APPDATE(9:2) TO WS-NEXT-REP-DUE-DATE(9:2)     02190117
               MOVE WS-NEXT-REP-DUE-DATE TO HV-NXT-REPAY-DUEDATE        02190218
           WHEN (TI001-APPDATE(6:2) = 12)                               02190317
             MOVE TI001-APPDATE(1:4) TO WS-YEAR                         02190417
             COMPUTE WS-YEAR = WS-YEAR + 1                              02190517
             MOVE WS-YEAR           TO WS-NEXT-REP-DUE-DATE(1:4)        02190617
             MOVE TI001-APPDATE(5:1) TO WS-NEXT-REP-DUE-DATE(5:1)       02190717
             MOVE 01                TO WS-NEXT-REP-DUE-DATE(6:2)        02190817
             MOVE TI001-APPDATE(8:1) TO WS-NEXT-REP-DUE-DATE(8:1)       02190917
             MOVE TI001-APPDATE(9:2) TO WS-NEXT-REP-DUE-DATE(9:2)       02191017
             MOVE WS-NEXT-REP-DUE-DATE TO HV-NXT-REPAY-DUEDATE          02191118
           END-EVALUATE                                                 02191217
      *    -----------------------------------------------------------  02191316
           COMPUTE WS-BALANCE-AMNT = WS-TOT-REP-AMNT - WS-MON-REP-AMNT  02191416
           MOVE WS-BALANCE-AMNT     TO HV-BLNCE-AMNT                    02191516
                  PERFORM 3215-INSERT-PARA                              02191607
                  THRU 3215-INSERT-PARA-EXIT.                           02191707
       3211-LOAD-PARA-EXIT.                                             02191809
           EXIT                                                         02191907
           .                                                            02192007
      *    ------------------------------------------------------       02192116
       3215-INSERT-PARA.                                                02192207
           EXEC SQL                                                     02192313
           INSERT INTO APPLICANT_DB                                     02192413
           VALUES(:HV-APPLICANT-ID,:HV-APPLICANT-NAME,:HV-DOORNO,       02192513
            :HV-STREET,:HV-CITY,:HV-STATE,:HV-PIN,:HV-PHONE-NO)         02192613
           END-EXEC                                                     02192713
           EXEC SQL                                                     02192812
           INSERT INTO LOAN_DB(APP_ID,LOAN_ID,LOAN_TYPE,                02192913
           APRVED_LN_AMNT,LN_APRD_DATE,LOAN_TENURE,FD_INT_RATE,         02193013
           LOAN_INDICATOR,REPAY_DAY)                                    02193113
           VALUES(:HV-APP-ID,:HV-LOAN-ID,:HV-LOAN-TYPE,                 02193212
            :HV-APRVED-LN-AMNT,:HV-LN-APRD-DATE,:HV-LOAN-TENURE,        02193312
            :HV-FD-INT-RATE,:HV-LOAN-INDICATOR,:HV-REPAY-DAY)           02193412
           END-EXEC.                                                    02193512
           EVALUATE TRUE                                                02193612
           WHEN SQLCODE = 000                                           02193712
           DISPLAY "INSERT IS SUCCESSFUL"                               02193812
           WHEN OTHER                                                   02193912
           DISPLAY "INSERT FAILED"                                      02194012
           CALL 'DSNTIAR' USING SQLCA WS-ERR-MSG WS-ERR-LRECL           02194112
           DISPLAY WS-ERR-MSG.                                          02194212
           PERFORM 3230-KSDS-MOVE-PARA                                  02194316
              THRU 3230-KSDS-MOVE-PARA-EXIT                             02194416
           .                                                            02194516
       3215-INSERT-PARA-EXIT.                                           02194609
           EXIT                                                         02194708
           .                                                            02194808
      *    ------------------------------------------------------       02194916
       3230-KSDS-MOVE-PARA.                                             02195016
           EVALUATE TRUE                                                02195116
           WHEN TI001-LOINDCATOR = 'OPEN'                               02195216
             MOVE TI001-APPLICANT-ID    TO TO002-KEY(1:10)              02195316
             MOVE TI001-LOANID   TO TO002-KEY(11:2)                     02195416
             MOVE TI001-LOANTYPE TO TO002-LOANTYPE                      02195516
           END-EVALUATE                                                 02195616
           WRITE TO002-KSDS-REC                                         02195716
           .                                                            02195816
       3230-KSDS-MOVE-PARA-EXIT.                                        02195916
           EXIT                                                         02196016
           .                                                            02196116
      *    ------------------------------------------------------       02196216
       3212-INVALID-PARA.                                               02196303
              DISPLAY WS-COUNTER ":INVALID RECORD"                      02197003
           EVALUATE TRUE                                                02200003
            WHEN TI001-APPLICANT-ID IS NOT NUMERIC                      02210003
               STRING "APPID" DELIMITED BY SIZE INTO WS-REASON-OUT      02220003
                              WITH POINTER WS-STRING-PTR                02230003
               END-STRING                                               02240003
                              DISPLAY WS-STRING-PTR                     02250003
           END-EVALUATE                                                 02260003
           EVALUATE TRUE                                                02270003
            WHEN TI001-PHONENO IS NOT NUMERIC                           02280003
               IF WS-STRING-PTR = 1                                     02290003
                 STRING "PHONENO" DELIMITED BY SIZE INTO WS-REASON-OUT  02300003
                              WITH POINTER WS-STRING-PTR                02310003
                 END-STRING                                             02320003
                              DISPLAY WS-STRING-PTR                     02330003
               ELSE                                                     02340003
               STRING ",PHONENO" DELIMITED BY SIZE INTO WS-REASON-OUT   02350003
                              WITH POINTER WS-STRING-PTR                02360003
               END-STRING                                               02370003
               DISPLAY WS-STRING-PTR                                    02380003
             END-IF                                                     02390003
           END-EVALUATE                                                 02400003
           EVALUATE TRUE                                                02410003
            WHEN TI001-LOANTYPE IS NOT EQUAL TO "HOUSING"               02420003
               IF WS-STRING-PTR = 1                                     02430003
                 STRING "LOANTYPE" DELIMITED BY SIZE INTO WS-REASON-OUT 02440003
                              WITH POINTER WS-STRING-PTR                02450003
                 END-STRING                                             02460003
                              DISPLAY WS-STRING-PTR                     02470003
               ELSE                                                     02480003
               STRING ",LOANTYPE" DELIMITED BY SIZE INTO WS-REASON-OUT  02490003
                              WITH POINTER WS-STRING-PTR                02500003
               END-STRING                                               02510003
               DISPLAY WS-STRING-PTR                                    02520003
             END-IF                                                     02530003
           END-EVALUATE                                                 02540003
           EVALUATE TRUE                                                02550003
            WHEN TI001-APPDATE(1:4) IS NOT NUMERIC OR                   02560003
                 TI001-APPDATE(5:1) NOT EQUAL TO "-" OR                 02570003
                 TI001-APPDATE(6:2) IS NOT NUMERIC OR                   02580003
                 TI001-APPDATE(8:1) NOT EQUAL TO "-" OR                 02590003
                 TI001-APPDATE(9:2) IS NOT NUMERIC                      02600003
               IF WS-STRING-PTR = 1                                     02610003
                 STRING "APPDATE" DELIMITED BY SIZE INTO WS-REASON-OUT  02620003
                              WITH POINTER WS-STRING-PTR                02630003
                 END-STRING                                             02640003
                              DISPLAY WS-STRING-PTR                     02650003
               ELSE                                                     02660003
               STRING ",APPDATE" DELIMITED BY SIZE INTO WS-REASON-OUT   02670003
                              WITH POINTER WS-STRING-PTR                02680003
               END-STRING                                               02690003
               DISPLAY WS-STRING-PTR                                    02700003
             END-IF                                                     02710003
           END-EVALUATE                                                 02720003
           EVALUATE TRUE                                                02730003
            WHEN TI001-INTRATE(1:2) IS NOT NUMERIC OR                   02740003
                 TI001-INTRATE(3:1) IS NOT EQUAL TO "." OR              02750003
                 TI001-INTRATE(4:1) IS NOT NUMERIC                      02760003
               IF WS-STRING-PTR = 1                                     02770003
                 STRING "INTRATE" DELIMITED BY SIZE INTO WS-REASON-OUT  02780003
                              WITH POINTER WS-STRING-PTR                02790003
                 END-STRING                                             02800003
                              DISPLAY WS-STRING-PTR                     02810003
               ELSE                                                     02820003
               STRING ",INTRATE" DELIMITED BY SIZE INTO WS-REASON-OUT   02830003
                              WITH POINTER WS-STRING-PTR                02840003
               END-STRING                                               02850003
               DISPLAY WS-STRING-PTR                                    02860003
             END-IF                                                     02870003
           END-EVALUATE                                                 02880003
           EVALUATE TRUE                                                02890003
            WHEN TI001-LOINDCATOR IS NOT EQUAL TO "OPEN " OR            02900016
                 TI001-LOINDCATOR IS NOT EQUAL TO "CLOSE"               02910004
               IF WS-STRING-PTR = 1                                     02920003
             STRING "LOINDCATOR" DELIMITED BY SPACE INTO WS-REASON-OUT  02930004
                              WITH POINTER WS-STRING-PTR                02940003
                 END-STRING                                             02950003
                              DISPLAY WS-STRING-PTR                     02960003
               ELSE                                                     02970003
             STRING ",LOINDCATOR" DELIMITED BY SPACE INTO WS-REASON-OUT 02980004
                              WITH POINTER WS-STRING-PTR                02990003
               END-STRING                                               03000003
               DISPLAY WS-STRING-PTR                                    03010003
             END-IF                                                     03020003
           END-EVALUATE                                                 03030003
                         PERFORM 3220-MOVE-PARA                         03040003
                         THRU 3220-MOVE-PARA-EXIT.                      03050003
       3212-INVALID-PARA-EXIT.                                          03060003
           EXIT                                                         03070003
           .                                                            03080003
      ********* MOVING RECORDS TO OUTPUT FILE LAYOUT *********          03090003
       3220-MOVE-PARA.                                                  03110003
           MOVE TI001-APPLICANT-ID    TO TO001-APPLICANT-ID             03120003
           MOVE TI001-APPLICANT-NAME  TO TO001-APPLICANT-NAME           03130003
           MOVE TI001-DOORNO          TO TO001-DOORNO                   03140003
           MOVE TI001-STREET          TO TO001-STREET                   03150003
           MOVE TI001-CITY            TO TO001-CITY                     03160003
           MOVE TI001-STATE           TO TO001-STATE                    03170003
           MOVE TI001-PIN             TO TO001-PIN                      03180003
           MOVE TI001-PHONENO         TO TO001-PHONENO                  03190003
           MOVE TI001-LOANID          TO TO001-LOANID                   03200003
           MOVE TI001-LOANTYPE        TO TO001-LOANTYPE                 03201005
           MOVE TI001-APPLOAMT        TO TO001-APPLOAMT                 03210003
           MOVE TI001-APPDATE         TO TO001-APPDATE                  03220003
           MOVE TI001-LOTENURE        TO TO001-LOTENURE                 03230003
           MOVE TI001-INTRATE         TO TO001-INTRATE                  03240003
           MOVE TI001-LOINDCATOR      TO TO001-LOINDCATOR               03250003
           MOVE TI001-REPAYDATE       TO TO001-REPAYDATE                03260003
           MOVE WS-REASON-OUT         TO TO001-REASON                   03270003
                 PERFORM 3230-WRITE-PARA                                03280003
                 THRU 3230-WRITE-PARA-EXIT                              03290003
           .                                                            03300003
       3220-MOVE-PARA-EXIT.                                             03310003
           EXIT                                                         03320003
           .                                                            03330003
      ************** WRITING IN THE OUTPUT FILE **************          03340003
       3230-WRITE-PARA.                                                 03350003
           WRITE TO001-PS2-REC                                          03360003
           EVALUATE TRUE                                                03370003
              WHEN WS-TO001-FST-SUCCESS                                 03380003
                 DISPLAY "WRITE IS SUCCESSFUL"                          03390003
              WHEN OTHER                                                03400003
                 DISPLAY "WRITE FAILED" WS-TO001-FST                    03410003
           END-EVALUATE                                                 03420003
           .                                                            03430003
       3230-WRITE-PARA-EXIT.                                            03440003
           EXIT                                                         03450003
           .                                                            03460003
      ********** CLOSING THE INPUT AND OUTPUT FILES **********          03470003
       3300-CLOSE-PARA.                                                 03480003
           CLOSE TI001-PS1 TO001-PS2                                    03490003
           .                                                            03500003
       3300-CLOSE-PARA-EXIT.                                            03510003
           EXIT                                                         03520003
           .                                                            03530003
       9000-TERM-PARA.                                                  03540003
           STOP RUN                                                     03550003
           .                                                            03560003
