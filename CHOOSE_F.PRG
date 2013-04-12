*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Choose_F      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                        ๋กฎเ ฏเฅคฏเจ๏โจ๏ ฏฎ F1.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 15.01.1999 ออผ
PROCEDURE Choose_F
PARAMETER n_f
PRIVATE nm_w
nm_w = ""

DO Ch_Firm WITH f_c000, f_n000, nm_w
IF f_c_old # f_c000
  b_c000 = "   "
  b_n000 = SPACE(16)
  SHOW GET b_n000
  f_c_old = f_c000
ENDIF
SHOW GET f_c000
SHOW GET f_n000
IF .NOT. EMPTY(n_f)
  DO (n_f)
ENDIF
IF EMPTY(ON("KEY","Tab"))
  KEYBOARD "{Tab}"
ELSE
  IF EMPTY(ON("KEY","DnArrow"))
    KEYBOARD "{DnArrow}"
  ENDIF
ENDIF
RETURN
