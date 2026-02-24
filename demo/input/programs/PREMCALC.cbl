       IDENTIFICATION DIVISION.
       PROGRAM-ID. PREMCALC.
      *================================================================*
      * 保険料再計算サブプログラム
      * 更新時の新保険料を算出する
      * CALL元: CONTRENW（契約更新バッチ）
      *================================================================*
      * CALL USING:
      *   PM-RENEW-TYPE       X(1)  入力  更新区分 A=自動/M=手動
      *   PM-CURRENT-PREMIUM  9(7)V99  入力  現行保険料
      *   PM-PLAN-CODE        X(3)  入力  保険種別コード
      *   PM-RENEW-COUNT      9(2)  入力  更新回数
      *   PM-NEW-PREMIUM      9(7)V99  出力  新保険料
      *   PM-RETURN-CODE      9(2)  出力  リターンコード 00=正常
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CALC-WORK.
           05  WS-BASE-RATE         PIC 9(1)V9(4).
           05  WS-COUNT-FACTOR      PIC 9(1)V9(4).
           05  WS-WORK-PREMIUM      PIC 9(9)V99.

       01  WS-RATE-TABLE.
           05  WS-AUTO-INCREASE     PIC 9(1)V9(4) VALUE 1.0300.
           05  WS-MANUAL-KEEP       PIC 9(1)V9(4) VALUE 1.0000.
           05  WS-COUNT-SURCHARGE   PIC 9(1)V9(4) VALUE 0.0050.
           05  WS-MAX-SURCHARGE     PIC 9(1)V9(4) VALUE 0.0500.

       LINKAGE SECTION.
       01  PM-RENEW-TYPE            PIC X(1).
       01  PM-CURRENT-PREMIUM       PIC 9(7)V99.
       01  PM-PLAN-CODE             PIC X(3).
       01  PM-RENEW-COUNT           PIC 9(2).
       01  PM-NEW-PREMIUM           PIC 9(7)V99.
       01  PM-RETURN-CODE           PIC 9(2).

       PROCEDURE DIVISION USING PM-RENEW-TYPE
                                PM-CURRENT-PREMIUM
                                PM-PLAN-CODE
                                PM-RENEW-COUNT
                                PM-NEW-PREMIUM
                                PM-RETURN-CODE.

       MAIN-CALC.
           MOVE ZERO TO PM-RETURN-CODE.
           PERFORM DETERMINE-BASE-RATE.
           PERFORM CALC-COUNT-FACTOR.
           PERFORM CALC-NEW-PREMIUM.
           GOBACK.

       DETERMINE-BASE-RATE.
           EVALUATE PM-RENEW-TYPE
               WHEN 'A'
                   MOVE WS-AUTO-INCREASE TO WS-BASE-RATE
               WHEN 'M'
                   MOVE WS-MANUAL-KEEP TO WS-BASE-RATE
               WHEN OTHER
                   MOVE 99 TO PM-RETURN-CODE
                   GOBACK
           END-EVALUATE.

       CALC-COUNT-FACTOR.
      *    更新回数に応じた割増率（回数 × 0.5%、上限5%）
           COMPUTE WS-COUNT-FACTOR =
               PM-RENEW-COUNT * WS-COUNT-SURCHARGE.
           IF WS-COUNT-FACTOR > WS-MAX-SURCHARGE
               MOVE WS-MAX-SURCHARGE TO WS-COUNT-FACTOR
           END-IF.

       CALC-NEW-PREMIUM.
           COMPUTE WS-WORK-PREMIUM =
               PM-CURRENT-PREMIUM * (WS-BASE-RATE + WS-COUNT-FACTOR).
           IF WS-WORK-PREMIUM > 9999999.99
               MOVE 9999999.99 TO PM-NEW-PREMIUM
               MOVE 01 TO PM-RETURN-CODE
           ELSE
               MOVE WS-WORK-PREMIUM TO PM-NEW-PREMIUM
           END-IF.
