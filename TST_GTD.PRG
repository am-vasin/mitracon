*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Tst_GTD       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                  เฎขฅเช  ฏฎคฎงเจโฅซ์ญ๋ๅ ฏฎงจๆจฉ ’.                  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.08.2001 ออผ
PROCEDURE Tst_GTD
PRIVATE c, n_line, escape

USE path_comm+"GTD" ORDER TAG NAME 
DEFINE WINDOW tmp_win FROM 1, 1 TO WROWS()-2, WCOLS()-2 SHADOW NONE COLOR SCHEME 13
ACTIVATE WINDOW tmp_win
@ 0, 0 SAY PADC("ฎงจๆจจ ’ แ ชจเจซซจๆฅฉ",WCOLS()) COLOR SCHEME 5
DO Sun_Bord WITH 1, 1, WROWS()-2, WCOLS()-2
@ 1,  2 SAY "ฎฌฅเ ’"
@ 1, 23 SAY " จฌฅญฎข ญจฅ"

escape = .F.
n_line = 2
SCAN FOR Is_Cyr(NAME)
  @ n_line, 2 SAY GTD_NUM+"ณ"
  DO Show_Cyr WITH NAME, n_line, COL()
  n_line = n_line+1
  IF n_line = WROWS()-2
    @ WROWS()-1, 2 SAY PADC(" Esc - ข๋ๅฎค, ซ๎ก ๏ คเใฃ ๏ ชซ ขจ่  - ฏเฎคฎซฆจโ์ ", WCOLS()-4)
    c = INKEY(0, "H")
    @ 2, 2 CLEAR TO WROWS()-3, WCOLS()-3
    @ WROWS()-1, 0 SAY SPACE(WCOLS())
    n_line = 2
    IF c = 27
      escape = .T.
      EXIT
    ENDIF
  ENDIF
ENDSCAN

IF .NOT. escape
  @ WROWS()-1, 2 SAY PADC(" ฎญฅๆ ฏเฎแฌฎโเ , ญ ฆฌจโฅ ซ๎กใ๎ ชซ ขจ่ใ ", WCOLS()-4)
  = INKEY(0, "H")
ENDIF
USE
RELEASE WINDOW tmp_win

RETURN
