*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Set_Hol       งเ กฎโ็จช €ญคเฅฉ + €ซซ           08.04.97 21:18:48 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                       แแโ ญฎขช  ญฅเ กฎ็จๅ คญฅฉ.                       บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Hol
PARAMETERS ye, mn

PRIVATE point, col0, col1, col0_i, col1_i, s, l0, c0, i
PRIVATE t_month, t_year, menu_name, sel_sav
EXTERNAL ARRAY rw, cl, h_sw
point = 1

col0   = SCHEME(13,1)
col0_i = SCHEME(13,2)
col1   = SCHEME(14,2)
col1_i = SCHEME(14,1)

menu_name  = PROGRAM()

sel_sav = SELECT()
SELECT 0
USE (base_path+"MOULD") ORDER TAG MONTH ALIAS T5678 AGAIN
IF .NOT. SEEK(STR(ye,4)+STR(mn,2))
  USE
  SELECT (sel_sav)
  RELEASE WINDOW CALENDAR
  RETURN
ENDIF

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF RECNO("T5678") = RECCOUNT("T5678")
  DEFINE PAD Switch OF (menu_name) PROMPT "ฅเชซ." KEY Ctrl-A, "Enter"
  ON SELECTION PAD Switch OF (menu_name) DO Chs_M WITH 13
ENDIF

DEFINE PAD Next_M OF (menu_name) PROMPT "‘ซฅค. ฌฅแ๏ๆ" KEY Ctrl-A, "PgDn"
ON SELECTION PAD Next_M OF (menu_name) DO Chs_M WITH 3

DEFINE PAD Prev_M OF (menu_name) PROMPT "เฅค. ฌฅแ๏ๆ" KEY Ctrl-A, "PgUp"
ON SELECTION PAD Prev_M OF (menu_name) DO Chs_M WITH 18

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Chs_M WITH 27

ACTIVATE SCREEN
@ 0, 0 SAY PADL("ฅญ๎ - F10 ", WCOLS()) COLOR (SCHEME(1,2))
ACTIVATE WINDOW Calendar
DO WHILE .T.
  SHOW MENU (menu_Name)
  point = MIN(point, ALEN(rw))
  @ rw(point), cl(point) FILL TO rw(point), cl(point)+1  ;
               COLOR (IIF(h_sw(point), col1_i, col0_i))
  s = INKEY(0, "HM")
  @ rw(point), cl(point) FILL TO rw(point), cl(point)+1  ;
               COLOR (IIF(h_sw(point), col1, col0))
  IF s = 151      && ๋่ช !
    l0 = MROW()
    c0 = MCOL()
    IF l0 >= 0 .AND. c0 >= 0  &&   ช ซฅญค เฅ
      FOR i = 1 TO ALEN(rw)
        IF l0 = rw(i) .AND. BETWEEN(c0, cl(i), cl(i)+1)
          point = i
          s = 13
          EXIT
        ENDIF
      ENDFOR
      IF s = 151
        LOOP
      ENDIF
    ELSE
      IF MROW("") = 0   &&   แโเฎชฅ ฌฅญ๎...
        s = -9
      ELSE
        LOOP
      ENDIF
    ENDIF
  ENDIF
  IF s = -9
    s = 0
    ACTIVATE MENU (menu_name)
  ENDIF
  DO CASE
  CASE s = 27    && Esc
    EXIT
  CASE s = 19    && Left
    point = MAX(point-7, 1)
  CASE s =  4    && Right
    point = MIN(point+7, ALEN(h_sw))
  CASE s =  5    && Up
    point = MAX(point-1, 1)
  CASE s = 24    && Down
    point = MIN(point+1, ALEN(h_sw))
  CASE s = 13 .AND. RECNO("T5678") = RECCOUNT("T5678")    && Enter
    h_sw(point) = .NOT. h_sw(point)
    REPLACE T5678.DAYS WITH STUFF(T5678.DAYS, point, 1, IIF(h_sw(point), "", ""))
  CASE s = 18    && PageUp
  SKIP -1
    IF BOF()
      WAIT WINDOW "ฎแโจฃญใโฎ ญ ็ ซฎ ไ ฉซ " NOWAIT
      GO TOP
    ELSE
      DO Calendar WITH T5678.YEAR, T5678.MONTH
    ENDIF
    RELEASE PAD Switch OF (menu_name)
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ - F10 ", WCOLS()) COLOR (SCHEME(1,2))
    ACTIVATE WINDOW Calendar
  CASE s =  3    && PageDown  
    SKIP 1
      IF EOF()
        WAIT WINDOW "ฎแโจฃญใโ ชฎญฅๆ ไ ฉซ " NOWAIT
        GO BOTTOM
      ELSE
        DO Calendar	WITH T5678.YEAR, T5678.MONTH
        IF RECNO() = RECCOUNT()
          DEFINE PAD Switch OF (menu_name) BEFORE Next_M PROMPT "ฅเชซ." KEY Ctrl-A, "Enter"
          ON SELECTION PAD Switch OF (menu_name) DO Chs_M WITH 13
        ENDIF
        ACTIVATE SCREEN
        @ 0, 0 SAY PADL("ฅญ๎ - F10 ", WCOLS()) COLOR (SCHEME(1,2))
        ACTIVATE WINDOW Calendar
      ENDIF
  ENDCASE
ENDDO
RELEASE MENU (menu_name) EXTENDED
ye = T5678.YEAR
mn = T5678.MONTH
USE
SELECT (sel_sav)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ๋กฎเ ฏใญชโ  ฌฅญ๎.                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 29.09.97 17:11:53 อผ
PROCEDURE Chs_M
PARAMETER v

s = v
DEACTIVATE MENU (menu_name)
RETURN
