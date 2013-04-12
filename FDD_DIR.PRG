*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                   บ
*บ           เฎฃเ ฌฌ  ฏฎซใ็ฅญจ๏ แฎคฅเฆจฌฎฃฎ คจแช .                  บ
*บ                                                                   บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Fdd_Dir
PARAMETERS pth, s_need
PRIVATE aw_dir, aw, ln, free, p_rem, i, lnsc, l, c, t, t1, t2, pthw, s_n

p_rem = SET("DEFAULT")+CURDIR()

pthw = pth
s_n = IIF(TYPE("s_need") = "N", s_need, 0)
s_n = MAX(0,s_n)

*IF INLIST(LEFT(pth,2), "a:", "A:", "b:", "B:")
*  DO WHILE .T.
*    IF .NOT. FLOP(LEFT(pth,1))
*      DIMENSION mss(3)
*      mss(1) = ""
*      mss(2) = CHR(0)+"  € ็โฎ แ คจแชฅโฎฉ? ใคฅฌ แโ ขจโ์? [   ] [ ฅโ ]"
*      mss(3) = ""
*      IF Out_Mess(7,"mss") = 2
*        RETURN ""
*      ENDIF
*      LOOP
*    ENDIF
*    EXIT
*  ENDDO
*ENDIF

DO WHILE .T.

  SET DEFAULT TO (pthw)
  pthw = SET("DEFAULT")+CURDIR()
  free = DISKSPACE()
  ln = ADIR(aw_dir,"*.*","D")
  IF ln = 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"จแชฅโ  ฏใแโ !"
    mss(3) = ""
    DO Out_Mess WITH 7,"mss"
    t = ""
    EXIT
  ENDIF
  DIMENSION aw(ln)
  FOR i = 1 TO ln
    aw_dir(i,1) = IIF("D" $ aw_dir(i,5), " ", "*")+aw_dir(i,1)
  ENDFOR

  = ASORT(aw_dir)

  FOR i = 1 TO ln
    IF "D" $ aw_dir(i,5)
      aw(i) = SUBSTR(PADR(aw_dir(i,1),14),2)+"  <DIR>   "
    ELSE
      aw(i) = SUBSTR(PADR(aw_dir(i,1),14),2)+STR(aw_dir(i,2),10)
    ENDIF
  ENDFOR

  RELEASE aw_dir

  lnsc = MIN(WROWS("")-IIF(s_n = 0, 9, 10),ln)
  l    = CEILING((WROWS("")-lnsc-5)/2)
  c    = FLOOR((WCOLS("")-LEN(aw(1))-2)/2)

  i = 1
  t1 = "‘ขฎกฎคญฎ  "+ALLTRIM(STR(free,10))
  IF s_n # 0
    t2 = "’เฅกใฅโแ๏ "+ALLTRIM(STR(s_n,10))
  ENDIF
  DEFINE WINDOW Fdd_Dir FROM l, c TO l+lnsc+IIF(s_n = 0, 3, 4), c+LEN(aw(1))+3  ;
                NONE SHADOW COLOR SCHEME 13
  ACTIVATE WINDOW Fdd_Dir
  t = UPPER(pthw)
  IF LEN(t) > WCOLS()
    t = LEFT(t,WCOLS()-3)+"..."
  ENDIF
  @ 0,0 SAY PADC(t,WCOLS()) COLOR SCHEME 5
  @ 1,2 SAY t1
  IF s_n # 0
    @ 2,2 SAY t2
  ENDIF
  DO WHILE .T.
    i = MAX(i,1)
    i = MIN(i,ln)
    @ IIF(s_n=0, 2, 3), 1 MENU aw, ln, lnsc TILE t SHADOW
    READ MENU TO i
    IF MOD(READKEY(),256) = 12
      t = ""
      EXIT
    ENDIF
    IF MOD(READKEY(),256) = 15
      t = ALLTRIM(LEFT(aw(i),13))
      EXIT
    ENDIF
  ENDDO
  RELEASE WINDIW Fdd_Dir
  IF .NOT. EMPTY(t) .AND. "  <DIR>   " = RIGHT(aw(i),10)
    DO CASE
    CASE ALLTRIM(t) == "."
      pthw = LEFT(pthw,3)
    CASE ALLTRIM(t) == ".."
      i = RAT("\",pthw,2)
      pthw = IIF(i=0, pthw, LEFT(pthw,i))
    OTHERWISE
      pthw = pthw+t+"\"
    ENDCASE
    LOOP
  ENDIF
  EXIT

ENDDO

RELEASE WINDOW Fdd_Dir
SET DEFAULT TO (p_rem)
RETURN IIF(EMPTY(t), t, pthw+t)
