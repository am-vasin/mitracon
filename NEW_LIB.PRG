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
