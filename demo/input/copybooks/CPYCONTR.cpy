      *================================================================*
      * CPYCONTR - 契約マスタ共通レコードレイアウト
      * 契約管理系バッチで共通使用する契約レコード定義
      *================================================================*
       01  CONTRACT-REC.
           05  CT-CONTRACT-NO        PIC X(10).
           05  CT-HOLDER-NAME        PIC N(20).
           05  CT-PLAN-CODE          PIC X(3).
           05  CT-START-DATE         PIC 9(8).
           05  CT-END-DATE           PIC 9(8).
           05  CT-TERM-YEARS         PIC 9(2).
           05  CT-RENEW-TYPE         PIC X(1).
               88  CT-AUTO-RENEW     VALUE 'A'.
               88  CT-MANUAL-RENEW   VALUE 'M'.
               88  CT-NO-RENEW       VALUE 'N'.
           05  CT-PREMIUM-AMOUNT     PIC 9(7)V99.
           05  CT-UNPAID-FLAG        PIC X(1).
               88  CT-HAS-UNPAID     VALUE 'Y'.
           05  CT-RENEW-COUNT        PIC 9(2).
           05  CT-MAX-RENEW          PIC 9(2).
           05  FILLER                PIC X(27).
