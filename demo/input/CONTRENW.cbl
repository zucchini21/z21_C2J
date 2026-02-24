       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTRENW.
      *================================================================*
      * 契約更新バッチプログラム
      * 満期到来契約を判定し、更新処理または満期通知を出力する
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-CONTRACT-FILE
               ASSIGN TO 'CONTIN'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT OUT-RENEW-FILE
               ASSIGN TO 'RENWOUT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-RNW-STATUS.
           SELECT OUT-EXPIRE-FILE
               ASSIGN TO 'EXPOUT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-EXP-STATUS.
           SELECT OUT-ERROR-FILE
               ASSIGN TO 'ERROUT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-ERR-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  IN-CONTRACT-FILE.
       01  IN-CONTRACT-REC.
           05  IC-CONTRACT-NO        PIC X(10).
           05  IC-HOLDER-NAME        PIC N(20).
           05  IC-PLAN-CODE          PIC X(3).
           05  IC-START-DATE         PIC 9(8).
           05  IC-END-DATE           PIC 9(8).
           05  IC-TERM-YEARS         PIC 9(2).
           05  IC-RENEW-TYPE         PIC X(1).
               88  IC-AUTO-RENEW     VALUE 'A'.
               88  IC-MANUAL-RENEW   VALUE 'M'.
               88  IC-NO-RENEW       VALUE 'N'.
           05  IC-PREMIUM-AMOUNT     PIC 9(7)V99.
           05  IC-UNPAID-FLAG        PIC X(1).
               88  IC-HAS-UNPAID     VALUE 'Y'.
           05  IC-RENEW-COUNT        PIC 9(2).
           05  IC-MAX-RENEW          PIC 9(2).
           05  FILLER                PIC X(27).

       FD  OUT-RENEW-FILE.
       01  OUT-RENEW-REC.
           05  RN-CONTRACT-NO        PIC X(10).
           05  RN-HOLDER-NAME        PIC N(20).
           05  RN-OLD-END-DATE       PIC 9(8).
           05  RN-NEW-START-DATE     PIC 9(8).
           05  RN-NEW-END-DATE       PIC 9(8).
           05  RN-RENEW-COUNT        PIC 9(2).
           05  RN-PREMIUM-AMOUNT     PIC 9(7)V99.
           05  RN-RENEW-TYPE         PIC X(1).
           05  FILLER                PIC X(24).

       FD  OUT-EXPIRE-FILE.
       01  OUT-EXPIRE-REC.
           05  EX-CONTRACT-NO        PIC X(10).
           05  EX-HOLDER-NAME        PIC N(20).
           05  EX-END-DATE           PIC 9(8).
           05  EX-EXPIRE-REASON      PIC X(2).
           05  EX-EXPIRE-MSG         PIC X(40).
           05  FILLER                PIC X(20).

       FD  OUT-ERROR-FILE.
       01  OUT-ERROR-REC.
           05  ER-CONTRACT-NO        PIC X(10).
           05  ER-ERROR-CODE         PIC X(4).
           05  ER-ERROR-MSG          PIC X(50).
           05  FILLER                PIC X(36).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-IN-STATUS          PIC X(2).
           05  WS-RNW-STATUS         PIC X(2).
           05  WS-EXP-STATUS         PIC X(2).
           05  WS-ERR-STATUS         PIC X(2).

       01  WS-FLAGS.
           05  WS-EOF-FLAG           PIC X(1) VALUE 'N'.
               88  WS-EOF            VALUE 'Y'.
           05  WS-ERROR-FLAG         PIC X(1) VALUE 'N'.
               88  WS-HAS-ERROR      VALUE 'Y'.

       01  WS-COUNTERS.
           05  WS-READ-CNT           PIC 9(7) VALUE ZERO.
           05  WS-RENEW-CNT          PIC 9(7) VALUE ZERO.
           05  WS-EXPIRE-CNT         PIC 9(7) VALUE ZERO.
           05  WS-SKIP-CNT           PIC 9(7) VALUE ZERO.
           05  WS-ERROR-CNT          PIC 9(7) VALUE ZERO.

       01  WS-DATE-WORK.
           05  WS-CURRENT-DATE       PIC 9(8).
           05  WS-CHECK-DATE         PIC 9(8).
           05  WS-NEW-START          PIC 9(8).
           05  WS-NEW-END            PIC 9(8).
           05  WS-WORK-YEAR          PIC 9(4).
           05  WS-WORK-MONTH         PIC 9(2).
           05  WS-WORK-DAY           PIC 9(2).

       01  WS-CONSTANTS.
           05  WS-ADVANCE-DAYS       PIC 9(3) VALUE 060.
           05  WS-PREMIUM-INCREASE   PIC 9(1)V9(4) VALUE 1.0300.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INIT-PROCESS.
           PERFORM READ-CONTRACT.
           PERFORM RENEW-LOOP
               UNTIL WS-EOF.
           PERFORM TERM-PROCESS.
           STOP RUN.

       INIT-PROCESS.
           OPEN INPUT  IN-CONTRACT-FILE.
           OPEN OUTPUT OUT-RENEW-FILE
                        OUT-EXPIRE-FILE
                        OUT-ERROR-FILE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.
      *    満期判定基準日 = 現在日 + 60日
           PERFORM CALC-CHECK-DATE.

       READ-CONTRACT.
           READ IN-CONTRACT-FILE
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ.

       CALC-CHECK-DATE.
           MOVE WS-CURRENT-DATE(1:4) TO WS-WORK-YEAR.
           MOVE WS-CURRENT-DATE(5:2) TO WS-WORK-MONTH.
           MOVE WS-CURRENT-DATE(7:2) TO WS-WORK-DAY.
           ADD WS-ADVANCE-DAYS TO WS-WORK-DAY.
           PERFORM ADJUST-DATE.
           STRING WS-WORK-YEAR WS-WORK-MONTH WS-WORK-DAY
               DELIMITED BY SIZE
               INTO WS-CHECK-DATE.

       ADJUST-DATE.
           PERFORM UNTIL WS-WORK-DAY < 31
               SUBTRACT 30 FROM WS-WORK-DAY
               ADD 1 TO WS-WORK-MONTH
               IF WS-WORK-MONTH > 12
                   SUBTRACT 12 FROM WS-WORK-MONTH
                   ADD 1 TO WS-WORK-YEAR
               END-IF
           END-PERFORM.

       RENEW-LOOP.
           MOVE 'N' TO WS-ERROR-FLAG.
           PERFORM VALIDATE-CONTRACT.
           IF NOT WS-HAS-ERROR
               PERFORM CHECK-EXPIRY
           END-IF.
           PERFORM READ-CONTRACT.

       VALIDATE-CONTRACT.
           IF IC-CONTRACT-NO = SPACES
               MOVE 'E001' TO ER-ERROR-CODE
               MOVE '契約番号が空白です' TO ER-ERROR-MSG
               PERFORM WRITE-ERROR
           END-IF.
           IF IC-END-DATE < IC-START-DATE
               MOVE IC-CONTRACT-NO TO ER-CONTRACT-NO
               MOVE 'E002' TO ER-ERROR-CODE
               MOVE '終了日が開始日より前です' TO ER-ERROR-MSG
               PERFORM WRITE-ERROR
           END-IF.

       CHECK-EXPIRY.
           IF IC-END-DATE > WS-CHECK-DATE
               ADD 1 TO WS-SKIP-CNT
           ELSE
               EVALUATE TRUE
                   WHEN IC-HAS-UNPAID
                       PERFORM WRITE-EXPIRE-UNPAID
                   WHEN IC-NO-RENEW
                       PERFORM WRITE-EXPIRE-NO-RENEW
                   WHEN IC-RENEW-COUNT >= IC-MAX-RENEW
                       PERFORM WRITE-EXPIRE-MAX-RENEW
                   WHEN IC-AUTO-RENEW
                       PERFORM PROCESS-AUTO-RENEW
                   WHEN IC-MANUAL-RENEW
                       PERFORM PROCESS-MANUAL-RENEW
               END-EVALUATE
           END-IF.

       PROCESS-AUTO-RENEW.
           PERFORM CALC-NEW-DATES.
           MOVE IC-CONTRACT-NO      TO RN-CONTRACT-NO.
           MOVE IC-HOLDER-NAME      TO RN-HOLDER-NAME.
           MOVE IC-END-DATE         TO RN-OLD-END-DATE.
           MOVE WS-NEW-START        TO RN-NEW-START-DATE.
           MOVE WS-NEW-END          TO RN-NEW-END-DATE.
           ADD 1 TO IC-RENEW-COUNT
               GIVING RN-RENEW-COUNT.
           COMPUTE RN-PREMIUM-AMOUNT =
               IC-PREMIUM-AMOUNT * WS-PREMIUM-INCREASE.
           MOVE 'A' TO RN-RENEW-TYPE.
           WRITE OUT-RENEW-REC.
           ADD 1 TO WS-RENEW-CNT.

       PROCESS-MANUAL-RENEW.
           PERFORM CALC-NEW-DATES.
           MOVE IC-CONTRACT-NO      TO RN-CONTRACT-NO.
           MOVE IC-HOLDER-NAME      TO RN-HOLDER-NAME.
           MOVE IC-END-DATE         TO RN-OLD-END-DATE.
           MOVE WS-NEW-START        TO RN-NEW-START-DATE.
           MOVE WS-NEW-END          TO RN-NEW-END-DATE.
           ADD 1 TO IC-RENEW-COUNT
               GIVING RN-RENEW-COUNT.
           MOVE IC-PREMIUM-AMOUNT   TO RN-PREMIUM-AMOUNT.
           MOVE 'M' TO RN-RENEW-TYPE.
           WRITE OUT-RENEW-REC.
           ADD 1 TO WS-RENEW-CNT.

       CALC-NEW-DATES.
           MOVE IC-END-DATE TO WS-NEW-START.
           MOVE IC-END-DATE(1:4) TO WS-WORK-YEAR.
           ADD IC-TERM-YEARS TO WS-WORK-YEAR.
           STRING WS-WORK-YEAR
                  IC-END-DATE(5:4)
               DELIMITED BY SIZE
               INTO WS-NEW-END.

       WRITE-EXPIRE-UNPAID.
           MOVE IC-CONTRACT-NO  TO EX-CONTRACT-NO.
           MOVE IC-HOLDER-NAME  TO EX-HOLDER-NAME.
           MOVE IC-END-DATE     TO EX-END-DATE.
           MOVE 'UP' TO EX-EXPIRE-REASON.
           MOVE '未払い保険料あり - 更新不可' TO EX-EXPIRE-MSG.
           WRITE OUT-EXPIRE-REC.
           ADD 1 TO WS-EXPIRE-CNT.

       WRITE-EXPIRE-NO-RENEW.
           MOVE IC-CONTRACT-NO  TO EX-CONTRACT-NO.
           MOVE IC-HOLDER-NAME  TO EX-HOLDER-NAME.
           MOVE IC-END-DATE     TO EX-END-DATE.
           MOVE 'NR' TO EX-EXPIRE-REASON.
           MOVE '更新不可契約（契約者意思）' TO EX-EXPIRE-MSG.
           WRITE OUT-EXPIRE-REC.
           ADD 1 TO WS-EXPIRE-CNT.

       WRITE-EXPIRE-MAX-RENEW.
           MOVE IC-CONTRACT-NO  TO EX-CONTRACT-NO.
           MOVE IC-HOLDER-NAME  TO EX-HOLDER-NAME.
           MOVE IC-END-DATE     TO EX-END-DATE.
           MOVE 'MX' TO EX-EXPIRE-REASON.
           MOVE '最大更新回数到達' TO EX-EXPIRE-MSG.
           WRITE OUT-EXPIRE-REC.
           ADD 1 TO WS-EXPIRE-CNT.

       WRITE-ERROR.
           SET WS-HAS-ERROR TO TRUE.
           MOVE IC-CONTRACT-NO TO ER-CONTRACT-NO.
           WRITE OUT-ERROR-REC.
           ADD 1 TO WS-ERROR-CNT.

       TERM-PROCESS.
           DISPLAY '読込件数:   ' WS-READ-CNT.
           DISPLAY '更新件数:   ' WS-RENEW-CNT.
           DISPLAY '満期件数:   ' WS-EXPIRE-CNT.
           DISPLAY 'スキップ件数: ' WS-SKIP-CNT.
           DISPLAY 'エラー件数:  ' WS-ERROR-CNT.
           CLOSE IN-CONTRACT-FILE
                 OUT-RENEW-FILE
                 OUT-EXPIRE-FILE
                 OUT-ERROR-FILE.
