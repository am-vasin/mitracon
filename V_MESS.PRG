*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ V_Mess        งเ กฎโ็จช €ญคเฅฉ  แจญ           16.01.98 11:43:28 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ เฎแฌฎโเ โฅชแโ , ฎไฎเฌซฅญญฎฃฎ ข ขจคฅ DBF-ไ ฉซ  แ ฎคญจฌ โฅชแโฎข๋ฌ ฏฎซฅฌ บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE V_Mess
PARAMETERS als, tit

*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*

PRIVATE stat_type     && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
                                        1 - กซ ญช;
                                        2 - BROWSE - โ กซจๆ ;
                                        3 - BROWSE - แฏจแฎช.
PRIVATE what_do       && ฌ๏ เฅฆจฌ .
PRIVATE menu_name     && ฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎.
PRIVATE last_mouse    && เฅฌ๏ ฏฎแซฅคญฅฃฎ ญ ฆ โจ๏ งขฅเจญฎฉ ชญฎฏชจ.
PRIVATE win_name      && ฌ๏ ฎชญ  ( ฎชฎญ คซ๏ BROWSE ).

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*

PRIVATE ln, wd, lin, pages, point, nav_arr, ex, sel_sav, i, pic, p_sav
sel_sav = SELECT()
SELECT (als)
ln = MIN(WROWS("")-4,RECCOUNT()+4)
wd = FSIZE(FIELD(1))+4

pages = CEILING(RECCOUNT()/(ln-4))
point = 1
p_sav = 0
ex    = 1

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2

DO D_Win_N WITH ln, wd, tit
SCROLL WROWS()-2, 0, WROWS()-1, WCOLS()-1, 1
ln = ln-4

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
DO WHILE .T.
  *
  *   ฎช ง โ์ แโเ ญจๆใ
  *
  IF p_sav # point
    GO point
    @ 2, 2 CLEAR TO WROWS()-3, WCOLS()-3
    FOR i = 1 TO ln
      SCATTER TO lin
      @ 1+i, 2 SAY lin(1)
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  p_sav = point
  pic = "@*HT "
  i = 1
  IF point < RECCOUNT()-ln+1
    ON KEY LABEL PgDn DO Exit_N WITH "Next"
    pic = pic+"\ ‘ซฅค. PgDn ;"
    DIMENSION nav_arr(i)
    nav_arr(i) = "Next"
    i = i+1
  ENDIF
  IF point # 1
    ON KEY LABEL PgUp DO Exit_N WITH "Prev"
    pic = pic+"\ เฅค. PgUp ;"
    DIMENSION nav_arr(i)
    nav_arr(i) = "Prev"
    i = i+1
  ENDIF
  ON KEY LABEL UpArrow DO Exit_N WITH "Up"
  ON KEY LABEL DnArrow DO Exit_N WITH "Dn"
  DIMENSION nav_arr(i)
  nav_arr(i) = "Exit"
  pic = pic+"\? ๋ๅฎค Esc "
  what_do = ""
  @ WROWS()-1, 0 SAY SPACE(WCOLS())
  @ WROWS()-1, 2 GET ex PICTURE pic
  READ CYCLE
  ON KEY
  IF EMPTY(what_do)
    what_do = nav_arr(ex)
  ENDIF
  DO CASE
  CASE what_do = "Next"
    point = MIN(point+ln, RECCOUNT()-ln+1)
  CASE what_do = "Prev"
    point = MAX(point-ln,1)
  CASE what_do = "Up"
    point = MAX(point-1,1)
  CASE what_do = "Dn"
    point = MIN(point+1, RECCOUNT()-ln+1)
  OTHERWISE
    EXIT
  ENDCASE
ENDDO
*-------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                         เฎฃเ ฌฌ  ฎกเ กฎโ็จช.                          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 16.01.98 12:36:16 อผ
PROCEDURE Exit_N
PARAMETERS p

CLEAR READ
what_do = p
RETURN
