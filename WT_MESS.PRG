*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎๆฅคใเ  ข๋ค ็จ ง แโ ขชจ.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Wt_Mess
PARAMETER mes  && แฎฎก่ฅญจฅ

PRIVATE mes_w, l1, c1, l2, c2, i

IF WEXIST("Wt_Mess")
  RELEASE WINDOW Wt_Mess
  RETURN
ENDIF
mes_w = ""
IF TYPE("mes") = "C"
  mes_w = mes
ELSE
  IF WEXIST("Wt_Mess")
    RELEASE WINDOW Wt_Mess
  ENDIF
  RETURN
ENDIF

l2 = IIF(EMPTY(mes_w), 5, 6)
l1 = FLOOR((WROWS("")-l2)/2)
l2 = l1+l2-1

c2 = MAX(LEN(mes_w),12)+8
c1 = FLOOR((WCOLS("")-c2)/2)
c2 = c1+c2-1

DEFINE WINDOW Wt_Mess FROM l1, c1 TO l2, c2 NONE SHADOW COLOR SCHEME 5
ACTIVATE WINDOW Wt_Mess

i = 2
IF .NOT. EMPTY(mes_w)
  @ i, 0 SAY PADC(mes_w,WCOLS())
  i = i+1
ENDIF
@ i, 0 SAY PADC("คฅฌ-แ...",WCOLS())
RETURN
