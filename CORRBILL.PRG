*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  แฎงค ญจ๏ จ ชฎเเฅชๆจจ ชฎเเ. แ็ฅโฎข.              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE CorrBill

*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃขๆจจ
*

PRIVATE stat_type   && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "ฎเเฅชๆจ๏" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "corr"

DEFINE PAD Add OF (menu_name) PROMPT "ฎก ขซฅญจฅ" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "add"

DEFINE PAD Del OF (menu_name) PROMPT "“ค ซฅญจฅ" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) Do Sw_Mode WITH "del"

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   โชเ๋ข ฅฌ ไ ฉซ๋ .., ใแโ ญ ขซจข ฅฌ แข๏งจ จ โ. ค.
*

DO Use_Dummy

SELECT 0
USE CORRBILL ORDER TAG ORD
PRIVATE n_p
COUNT TO n_p
GO TOP
*IF n_p = 0
*  DO Corr_R WITH "a"
*ENDIF
*IF n_p = 0
*  RETURN
*ENDIF

*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd
ln = 100            && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "ฎเเ. แ็ฅโ ", 0, 0
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && เฎแฌฎโเ แฏจแช 

    SELECT CORRBILL
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del Do Sw_Mode WITH "del"
    ON KEY LABEL Ins Do Sw_Mode WITH "add"

*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS NAME,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "corr")
    ENDIF

  CASE what_do = "corr"    && ฎเเฅชๆจ๏ ชฎเเ. แ็ฅโ 

    IF n_p # 0
      DO Corr_R WITH "c"
    ENDIF
    what_do = "list"

  CASE what_do = "add"     && ฎก ขซฅญจฅ ชฎเเ. แ็ฅโ 

    DO Corr_R WITH "a"
    what_do = "list"

  CASE what_do = "del"     && “ค ซฅญจฅ ชฎเเ. แ็ฅโ 

    REPLACE DATE_OFF WITH DATE()
    n_p = n_p - 1
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN


*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  แฎงค ญจ๏/ชฎเเฅชๆจจ ชฎเเ. แ็ฅโ .              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Corr_R
PARAMETERS tp

PRIVATE new

new = tp = "a"
*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃขๆจจ
*

PRIVATE stat_type   && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
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

PRIVATE ex, nm_w         &&
PRIVATE f1, f2, f3, f4   &&   ”ซ ฆชจ...
                         &&   ก๊๏ขซ๏ฅฌ จ ง ฏฎซญ๏ฅฌ ฏฎซ๏ กซ ญช 
ex   = 1                 &&
IF new
  nm_w = SPACE(FSIZE("NAME"))
  STORE .F. TO f1, f2, f3, f4
ELSE
  nm_w = NAME
  f1   = .NOT. EMPTY(SUBSTR(FLAGS,1,1))
  f2   = .NOT. EMPTY(SUBSTR(FLAGS,2,1))
  f3   = .NOT. EMPTY(SUBSTR(FLAGS,3,1))
  f4   = .NOT. EMPTY(SUBSTR(FLAGS,4,1))
ENDIF
  
*------------------------------------------------------------------------

PUSH KEY CLEAR
DO Prp_Nav_2
DO D_Win_N WITH 13, 51, "ฎเเ. แ็ฅโ"

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*

@  2, 2 SAY "                                               "
@  3, 2 SAY "            งข ญจฅ ____.____!____._           "
@  4, 2 SAY "                                               "
@  5, 2 SAY "  ฺฤ ”ซ ฆชจ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ  " 
@  6, 2 SAY "  ณ  [ ] ”ซ ฃ 1          [ ] ”ซ ฃ 2         ณ  "
@  7, 2 SAY "  ณ  [ ] ”ซ ฃ 3          [ ] ”ซ ฃ 3         ณ  "
@  8, 2 SAY "  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู  "
@  9, 2 SAY "                                               "
@ 10, 2 SAY "       <  Ctrl-W > < โช ง โ์แ๏ Esc >        "
@ 11, 2 SAY "                                               "

@  3, 22 GET nm_w
@  6,  7 GET f1 PICTURE "@*C ”ซ ฃ 1"
@  6, 27 GET f2 PICTURE "@*C ”ซ ฃ 2"
@  7,  7 GET f3 PICTURE "@*C ”ซ ฃ 3"
@  7, 27 GET f4 PICTURE "@*C ”ซ ฃ 4"
@ 10,  9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(nm_w)
  f1 = IIF(f1,"*"," ")+IIF(f2,"*"," ")+IIF(f3,"*"," ")+IIF(f4,"*"," ")
  IF new
    PRIVATE cd
    rc = RECNO()
    SET ORDER TO TAG CODE
    GO BOTTOM
    cd = CODE+1
    SET ORDER TO TAG ORD
    n_p = n_p+1
    APPEND BLANK
  ENDIF
  REPLACE CODE  WITH IIF(new, cd, CODE), ;
          NAME  WITH nm_w,      ;
          FLAGS WITH f1
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ฎชฎญ คซ๏ BROWSE.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE D_Wins
PARAMETERS ln, wd, m_tit, l_tit, l_foot
PRIVATE ln1, l, c, l_tit_w, l_foot_w

l_tit_w  = IIF(TYPE("l_tit")  = "N", l_tit,  0)
l_foot_w = IIF(TYPE("l_foot") = "N", l_foot, 0)
l_foot_w = IIF(l_foot_w = 0, 0, l_foot_w+2)

ln1 = ln+3+l_tit_w+l_foot_w
ln1 = MIN(ln1, WROWS("")-3)

l = CEILING((WROWS("")-ln1)/2)
c = FLOOR((WCOLS("")-wd-4)/2)

*
*    ‘ฎงค ฅฌ จ ง ฏฎซญ๏ฅฌ ฎชญฎ - ฏฎคซฎฆชใ
*
DEFINE WINDOW (win_name+"_M") FROM l, c TO l+ln1-1, c+wd+3 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (win_name+"_M")
@ 0, 0 SAY PADC(m_tit,WCOLS()) COLOR SCHEME 5

@ l_tit_w+1, 1 TO WROWS()-l_foot_w-1, WCOLS()-2
@ WROWS()-l_foot_w-1, 2 FILL TO WROWS()-l_foot_w-1, WCOLS()-2 COLOR (SCHEME(13,6))
@ l_tit_w+1, WCOLS()-2 FILL TO WROWS()-l_foot_w-1, WCOLS()-2 COLOR (SCHEME(13,6))

IF l_foot_w # 0
  @ WROWS()-l_foot_w, 1 TO WROWS()-1, WCOLS()-2
  @ WROWS()-1, 2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))
  @ WROWS()-l_foot_w, WCOLS()-2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))
ENDIF

*
*  ‘ฎงค ฅฌ ขญฅ่ญฅฅ จ ขญใโเฅญญฅฅ ฎชญ  คซ๏ BROWSE.
*
l   = l+2+l_tit_w
c   = c+2
ln1 = ln1-3-l_foot_w-l_tit_w

DEFINE WINDOW (win_name+"_E") FROM l, c TO l+ln1-1, c+wd-1 NONE COLOR SCHEME 13
DEFINE WINDOW (win_name+"_I") FROM l-3, c-2 TO l+ln1, c+wd+20   ;
                           NONE COLOR SCHEME 13
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ญ ขจฃ ๆจฎญญฎฉ แเฅค๋ คซ๏ BROWSE.           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prp_Nav_1

ACTIVATE SCREEN
IF EMPTY(menu_name)
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ELSE
  @ 0, 0 SAY PADL("ฅญ๎ - F10 ",WCOLS()) COLOR SCHEME 13
  SHOW MENU (menu_name)
  ON KEY LABEL F10 DO To_Menu
ENDIF

ON KEY LABEL LeftMouse DO Mouse
ACTIVATE WINDOW (win_name+"_E")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎๆฅคใเ   ชโจขจง ๆจจ  แจญๅเฎญญฎฃฎ ฌฅญ๎.               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE To_Menu

IF EMPTY(menu_name)
  RETURN
ENDIF

PUSH KEY CLEAR
ACTIVATE MENU (menu_name)
DEACTIVATE MENU (menu_name)
POP KEY

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎๆฅคใเ  - ฎกเ กฎโ็จช ฌ๋่ชจ.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Mouse
PRIVATE ml, mc, s

PUSH KEY CLEAR

IF INLIST(stat_type, 2, 3)   && BROWSE
  *
  *     ฎคญฎฉ จง แโเฎช Browse
  *
  ml = MROW(win_name+"_E")
  mc = MCOL(win_name+"_E")
  IF ml >= 0 .AND. mc >= 0
    IF stat_type = 3         && ซ๏ ฌฅญ๎ ฏเฎขฅเ๏ฅฌ ญ  คขฎฉญฎฅ ญ ฆ โจฅ
      s = SECONDS()
      IF s - last_mouse < _DBLCLICK
        KEYBOARD CHR(13)
      ELSE
        last_mouse = s
      ENDIF
    ENDIF
    POP KEY
    RETURN
  ENDIF


  ml = MROW("")
  mc = MCOL("")
  IF BETWEEN(mc, WLCOL(win_name+"_E"), WLCOL(win_name+"_E")+WCOLS(win_name+"_E")-1)
    IF ml = WLROW(win_name+"_E")-1
      = INKEY("M")
      KEYBOARD "{PgUp}"
      POP KEY
      RETURN
    ENDIF
    IF ml = WLROW(win_name+"_E")+WROWS(win_name+"_E")
      = INKEY("M")
      KEYBOARD "{PgDn}"
      POP KEY
      RETURN
    ENDIF
  ENDIF
  IF ml = 0 .AND. .NOT. EMPTY(menu_name)
    DO To_Menu
  ELSE
    = INKEY("M")
  ENDIF
  POP KEY
  RETURN
ENDIF
IF stat_type = 1       && ซ ญช
  IF MROW() < 0 .OR. MCOL() < 0
    = INKEY("M")
  ENDIF
ENDIF
POP KEY
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎๆฅคใเ  ฏฅเฅชซ๎็ฅญจ๏ เฅฆจฌ .                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sw_Mode
PARAMETERS  mode

what_do = mode
KEYBOARD CHR(23)
IF .NOT. EMPTY(menu_name)
  DEACTIVATE MENU (menu_name)
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ฎชญ  คซ๏ กซ ญช .                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE D_Win
PARAMETERS ln, wd, m_tit
PRIVATE l, c

l = CEILING((WROWS("")-ln)/2)
c = FLOOR((WCOLS("")-wd)/2)

*
*    ‘ฎงค ฅฌ จ ง ฏฎซญ๏ฅฌ ฎชญฎ
*
DEFINE WINDOW (win_name) FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (win_name)
@ 0, 0 SAY PADC(m_tit,WCOLS()) COLOR SCHEME 5

@ 1, 1 TO WROWS()-1, WCOLS()-2
@ WROWS()-1, 2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))
@ 1, WCOLS()-2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ญ ขจฃ ๆจฎญญฎฉ แเฅค๋ คซ๏ กซ ญช .           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prp_Nav_2

ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ON KEY LABEL LeftMouse DO Mouse

RETURN
