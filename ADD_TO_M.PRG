*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎๅเ ญฅญจฉ ฏเจ ขๅฎคฅ ข ฏเฎฃเ ฌฌใ.                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Entry_P
PARAMETERS ext_menu, own_menu
*
*    ext_menu - จฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎ ข๋ง๋ข ๎้ฅฉ ฏเฎฃเ ฌฌ๋;
*    own_menu - จฌ๏ แฎกแโขฅญญฎฃฎ  แจญๅเฎญญฎฃฎ ฌฅญ๎.
*
PRIVATE e_m, o_m, s

e_m = ""
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    e_m = ext_menu
  ENDIF
ENDIF

o_m = ""
IF TYPE("own_menu") = "C"
  IF .NOT. EMPTY(own_menu)
    o_m = own_menu
  ENDIF
ENDIF

ACTIVATE SCREEN

IF .NOT. EMPTY(e_m)
  HIDE MENU (e_m)
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ENDIF

PUSH KEY CLEAR
IF .NOT. EMPTY(o_m)
  @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
  SHOW MENU (o_m)
  s = "'"+o_m+"'"
  ON KEY LABEL F10 DO F10_Proc WITH &s
ENDIF

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ   ชโจขจง ๆจจ  แจญๅเฎญญฎฃฎ ฌฅญ๎.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE F10_Proc
PARAMETER m_name

PUSH KEY CLEAR
ACTIVATE MENU (m_name)
DEACTIVATE MENU (m_name)
POP KEY

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ขฎแแโ ญฎขซฅญจ๏ ฏเจ ข๋ๅฎคฅ จง ฏเฎฃเ ฌฌ๋.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Exit_P
PARAMETERS ext_menu, own_menu
*
*    ext_menu - จฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎ ข๋ง๋ข ๎้ฅฉ ฏเฎฃเ ฌฌ๋;
*    own_menu - จฌ๏ แฎกแโขฅญญฎฃฎ  แจญๅเฎญญฎฃฎ ฌฅญ๎.
*
PRIVATE e_m, o_m

e_m = ""
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    e_m = ext_menu
  ENDIF
ENDIF

o_m = ""
IF TYPE("own_menu") = "C"
  IF .NOT. EMPTY(own_menu)
    o_m = own_menu
  ENDIF
ENDIF

ACTIVATE SCREEN

IF .NOT. EMPTY(o_m)
  DEACTIVATE MENU (o_m)
  RELEASE MENU (o_m) EXTENDED
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ENDIF

POP KEY

IF .NOT. EMPTY(e_m)
  @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
  DEACTIVATE MENU (e_m)
  SHOW MENU (e_m)
ENDIF

RETURN

