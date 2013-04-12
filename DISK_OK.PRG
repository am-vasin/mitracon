*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Disk_OK       งเ กฎโ็จช €ญคเฅฉ  แจญ           13.02.97 15:05:58 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ    เฎขฅเช  ใแโเฎฉแโข  ญ  ฏเฅคฌฅโ:   ญฅ คจแชฅโ  ซจ จ ใแโ ญฎขซฅญ  ซจ?   บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Disk_OK
PARAMETER pth

PRIVATE mss, drv

IF LEN(pth) < 2
  RETURN .T.
ENDIF

IF INLIST(UPPER(LEFT(pth,2)), "A:", "B:")   && Floppy disk!
  DO WHILE .T.
    drv = UPPER(LEFT(pth,1))
    CALL IsDisk WITH drv
    IF drv # UPPER(LEFT(pth,1))
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"  € คจแชฅโใ กใคฅฌ ใแโ ญ ขซจข โ์? [   ] [ ฅโ ]"
      mss(3) = ""
      IF Out_Mess(7, "mss") = 1
        LOOP
      ENDIF
      RETURN .F.
    ENDIF
    EXIT
  ENDDO
ENDIF

RETURN .T.
