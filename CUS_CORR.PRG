*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ชฎเเฅชๆจจ จฌฅญ ชซจฅญโฎข.                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cus_Corr

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

DEFINE PAD Edit OF (menu_name) PROMPT "ฎเเฅชๆจ๏" KEY Ctrl-A, "Enter"
ON SELECTION PAD Edit OF (menu_name) Do Sw_Mode WITH "edit"

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   โชเ๋ข ฅฌ ไ ฉซ๋ .., ใแโ ญ ขซจข ฅฌ แข๏งจ จ โ. ค.
*
DO Use_Dummy

SELECT 0
USE ACCOUNT ORDER TAG CUS_NAME

PRIVATE ln, wd
ln = MIN(100, RECCOUNT())
wd = FSIZE("CUS_NAME")

DO D_Wins WITH ln, wd, "ซจฅญโ๋", 0, 0
what_do = "list"
DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && เฎแฌฎโเ แฏจแช 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

    BROWSE FIELDS CUS_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "edit")
    ENDIF

  CASE what_do = "edit"    && เฎแฌฎโเ แฏจแช 

    DO Corr_Name
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name)

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
*บ                  เฎฃเ ฌฌ  ชฎเเฅชๆจจ จฌฅญจ ชซจฅญโ .                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Corr_Name

*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃขๆจจ
*

PRIVATE stat_type   && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
                                        1 - กซ ญช;
                                        2 - BROWSE - โ กซจๆ ;
                                        3 - BROWSE - แฏจแฎช.
PRIVATE menu_name     && ฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎.
PRIVATE last_mouse    && เฅฌ๏ ฏฎแซฅคญฅฃฎ ญ ฆ โจ๏ งขฅเจญฎฉ ชญฎฏชจ.
PRIVATE win_name      && ฌ๏ ฎชญ  ( ฎชฎญ คซ๏ BROWSE ).

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 1
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

PRIVATE ex, nm_w

ex   = 1
nm_w = CUS_NAME

PUSH KEY CLEAR
DO Prp_Nav_2
DO D_Win WITH 7, 60, "ซจฅญโ"

@ 3,  5 GET nm_w
@ 5, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(nm_w)
  REPLACE CUS_NAME WITH nm_w
ENDIF

POP KEY
RELEASE WINDOW (win_name)
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

