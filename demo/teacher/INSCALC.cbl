       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSCALC.
      *================================================================*
      * 保険料計算バッチプログラム
      * 契約者マスタを読み込み、保険料を計算して結果ファイルに出力
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-CONTRACT-FILE
               ASSIGN TO 'CONTIN'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT IN-RATE-FILE
               ASSIGN TO 'RATEIN'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS RT-GRADE-KEY
               FILE STATUS IS WS-RT-STATUS.
           SELECT OUT-RESULT-FILE
               ASSIGN TO 'RESOUT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.
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
           05  IC-BIRTH-DATE         PIC 9(8).
           05  IC-GENDER             PIC X(1).
               88  IC-GENDER-MALE    VALUE 'M'.
               88  IC-GENDER-FEMALE  VALUE 'F'.
           05  IC-PLAN-CODE          PIC X(3).
           05  IC-GRADE              PIC 9(2).
           05  IC-SUM-INSURED        PIC 9(9).
           05  IC-START-DATE         PIC 9(8).
           05  IC-RIDER-FLAGS.
               10  IC-RIDER-HOSP     PIC X(1).
                   88  IC-RIDER-HOSP-YES VALUE 'Y'.
               10  IC-RIDER-SURG     PIC X(1).
                   88  IC-RIDER-SURG-YES VALUE 'Y'.
               10  IC-RIDER-CANC     PIC X(1).
                   88  IC-RIDER-CANC-YES VALUE 'Y'.
           05  FILLER                PIC X(37).

       FD  IN-RATE-FILE.
       01  IN-RATE-REC.
           05  RT-GRADE-KEY          PIC 9(2).
           05  RT-BASE-RATE          PIC 9(3)V9(4).
           05  RT-MALE-ADJ           PIC S9(1)V9(4).
           05  RT-FEMALE-ADJ         PIC S9(1)V9(4).
           05  RT-AGE-BAND-RATES.
               10  RT-AGE-RATE       PIC 9(1)V9(4)
                                     OCCURS 6 TIMES.
           05  FILLER                PIC X(30).

       FD  OUT-RESULT-FILE.
       01  OUT-RESULT-REC.
           05  OR-CONTRACT-NO        PIC X(10).
           05  OR-HOLDER-NAME        PIC N(20).
           05  OR-BASE-PREMIUM       PIC 9(7)V99.
           05  OR-RIDER-PREMIUM      PIC 9(7)V99.
           05  OR-TOTAL-PREMIUM      PIC 9(7)V99.
           05  OR-CALC-DATE          PIC 9(8).
           05  FILLER                PIC X(35).

       FD  OUT-ERROR-FILE.
       01  OUT-ERROR-REC.
           05  ER-CONTRACT-NO        PIC X(10).
           05  ER-ERROR-CODE         PIC X(4).
           05  ER-ERROR-MSG          PIC X(50).
           05  FILLER                PIC X(36).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-IN-STATUS          PIC X(2).
           05  WS-RT-STATUS          PIC X(2).
           05  WS-OUT-STATUS         PIC X(2).
           05  WS-ERR-STATUS         PIC X(2).

       01  WS-FLAGS.
           05  WS-EOF-FLAG           PIC X(1) VALUE 'N'.
               88  WS-EOF            VALUE 'Y'.
           05  WS-ERROR-FLAG         PIC X(1) VALUE 'N'.
               88  WS-HAS-ERROR      VALUE 'Y'.

       01  WS-COUNTERS.
           05  WS-READ-CNT           PIC 9(7) VALUE ZERO.
           05  WS-WRITE-CNT          PIC 9(7) VALUE ZERO.
           05  WS-ERROR-CNT          PIC 9(7) VALUE ZERO.

       01  WS-CALC-WORK.
           05  WS-AGE                PIC 9(3).
           05  WS-AGE-BAND           PIC 9(1).
           05  WS-BASE-PREMIUM       PIC 9(9)V9(4).
           05  WS-GENDER-FACTOR      PIC S9(1)V9(4).
           05  WS-AGE-FACTOR         PIC 9(1)V9(4).
           05  WS-RIDER-PREMIUM      PIC 9(9)V9(4).
           05  WS-TOTAL-PREMIUM      PIC 9(9)V9(4).

       01  WS-DATE-WORK.
           05  WS-CURRENT-DATE       PIC 9(8).
           05  WS-CURRENT-YEAR       PIC 9(4).
           05  WS-BIRTH-YEAR         PIC 9(4).

       01  WS-RIDER-RATES.
           05  WS-HOSP-RATE          PIC 9(3)V9(4) VALUE 0.0200.
           05  WS-SURG-RATE          PIC 9(3)V9(4) VALUE 0.0150.
           05  WS-CANC-RATE          PIC 9(3)V9(4) VALUE 0.0350.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INIT-PROCESS.
           PERFORM READ-CONTRACT.
           PERFORM CALC-LOOP
               UNTIL WS-EOF.
           PERFORM TERM-PROCESS.
           STOP RUN.

       INIT-PROCESS.
           OPEN INPUT  IN-CONTRACT-FILE
                        IN-RATE-FILE.
           OPEN OUTPUT OUT-RESULT-FILE
                        OUT-ERROR-FILE.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD.
           MOVE WS-CURRENT-DATE(1:4) TO WS-CURRENT-YEAR.

       READ-CONTRACT.
           READ IN-CONTRACT-FILE
               AT END
                   SET WS-EOF TO TRUE
               NOT AT END
                   ADD 1 TO WS-READ-CNT
           END-READ.

       CALC-LOOP.
           MOVE 'N' TO WS-ERROR-FLAG.
           PERFORM VALIDATE-INPUT.
           IF NOT WS-HAS-ERROR
               PERFORM CALC-AGE
               PERFORM CALC-BASE-PREMIUM
               PERFORM CALC-RIDER-PREMIUM
               PERFORM CALC-TOTAL
               PERFORM WRITE-RESULT
           END-IF.
           PERFORM READ-CONTRACT.

       VALIDATE-INPUT.
           IF IC-CONTRACT-NO = SPACES
               MOVE 'E001' TO ER-ERROR-CODE
               MOVE '契約番号が空白です' TO ER-ERROR-MSG
               PERFORM WRITE-ERROR
           END-IF.
           IF IC-GRADE < 01 OR IC-GRADE > 20
               MOVE IC-CONTRACT-NO TO ER-CONTRACT-NO
               MOVE 'E002' TO ER-ERROR-CODE
               MOVE '等級が範囲外です(01-20)' TO ER-ERROR-MSG
               PERFORM WRITE-ERROR
           END-IF.
           IF IC-SUM-INSURED = ZERO
               MOVE IC-CONTRACT-NO TO ER-CONTRACT-NO
               MOVE 'E003' TO ER-ERROR-CODE
               MOVE '保険金額がゼロです' TO ER-ERROR-MSG
               PERFORM WRITE-ERROR
           END-IF.

       CALC-AGE.
           MOVE IC-BIRTH-DATE(1:4) TO WS-BIRTH-YEAR.
           COMPUTE WS-AGE = WS-CURRENT-YEAR - WS-BIRTH-YEAR.
           EVALUATE TRUE
               WHEN WS-AGE < 20
                   MOVE 1 TO WS-AGE-BAND
               WHEN WS-AGE < 30
                   MOVE 2 TO WS-AGE-BAND
               WHEN WS-AGE < 40
                   MOVE 3 TO WS-AGE-BAND
               WHEN WS-AGE < 50
                   MOVE 4 TO WS-AGE-BAND
               WHEN WS-AGE < 60
                   MOVE 5 TO WS-AGE-BAND
               WHEN OTHER
                   MOVE 6 TO WS-AGE-BAND
           END-EVALUATE.

       CALC-BASE-PREMIUM.
           MOVE IC-GRADE TO RT-GRADE-KEY.
           READ IN-RATE-FILE
               INVALID KEY
                   MOVE IC-CONTRACT-NO TO ER-CONTRACT-NO
                   MOVE 'E004' TO ER-ERROR-CODE
                   MOVE '料率マスタに該当等級なし' TO ER-ERROR-MSG
                   PERFORM WRITE-ERROR
               NOT INVALID KEY
                   COMPUTE WS-BASE-PREMIUM =
                       IC-SUM-INSURED * RT-BASE-RATE
                   IF IC-GENDER-MALE
                       MOVE RT-MALE-ADJ TO WS-GENDER-FACTOR
                   ELSE
                       MOVE RT-FEMALE-ADJ TO WS-GENDER-FACTOR
                   END-IF
                   MOVE RT-AGE-RATE(WS-AGE-BAND)
                       TO WS-AGE-FACTOR
                   COMPUTE WS-BASE-PREMIUM =
                       WS-BASE-PREMIUM
                       * (1 + WS-GENDER-FACTOR)
                       * (1 + WS-AGE-FACTOR)
           END-READ.

       CALC-RIDER-PREMIUM.
           MOVE ZERO TO WS-RIDER-PREMIUM.
           IF IC-RIDER-HOSP-YES
               COMPUTE WS-RIDER-PREMIUM =
                   WS-RIDER-PREMIUM +
                   (IC-SUM-INSURED * WS-HOSP-RATE)
           END-IF.
           IF IC-RIDER-SURG-YES
               COMPUTE WS-RIDER-PREMIUM =
                   WS-RIDER-PREMIUM +
                   (IC-SUM-INSURED * WS-SURG-RATE)
           END-IF.
           IF IC-RIDER-CANC-YES
               COMPUTE WS-RIDER-PREMIUM =
                   WS-RIDER-PREMIUM +
                   (IC-SUM-INSURED * WS-CANC-RATE)
           END-IF.

       CALC-TOTAL.
           COMPUTE WS-TOTAL-PREMIUM =
               WS-BASE-PREMIUM + WS-RIDER-PREMIUM.

       WRITE-RESULT.
           MOVE IC-CONTRACT-NO    TO OR-CONTRACT-NO.
           MOVE IC-HOLDER-NAME    TO OR-HOLDER-NAME.
           MOVE WS-BASE-PREMIUM   TO OR-BASE-PREMIUM.
           MOVE WS-RIDER-PREMIUM  TO OR-RIDER-PREMIUM.
           MOVE WS-TOTAL-PREMIUM  TO OR-TOTAL-PREMIUM.
           MOVE WS-CURRENT-DATE   TO OR-CALC-DATE.
           WRITE OUT-RESULT-REC.
           ADD 1 TO WS-WRITE-CNT.

       WRITE-ERROR.
           SET WS-HAS-ERROR TO TRUE.
           MOVE IC-CONTRACT-NO TO ER-CONTRACT-NO.
           WRITE OUT-ERROR-REC.
           ADD 1 TO WS-ERROR-CNT.

       TERM-PROCESS.
           DISPLAY '処理件数: ' WS-READ-CNT.
           DISPLAY '出力件数: ' WS-WRITE-CNT.
           DISPLAY 'エラー件数: ' WS-ERROR-CNT.
           CLOSE IN-CONTRACT-FILE
                 IN-RATE-FILE
                 OUT-RESULT-FILE
                 OUT-ERROR-FILE.
