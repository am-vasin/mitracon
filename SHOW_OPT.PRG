*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Show_Opt      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      ญไฎเฌ ๆจ๏ ฎก ฎฏโฎข๋ๅ ๆฅญ ๅ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 25.04.2001 ออผ
PROCEDURE Show_Opt
PARAMETERS line_no

PRIVATE line_wd, tmpLine, o_prc, n_cols, tmpQnt, let_modi
let_modi = .T.
IF sale_mode # "Mitracon"
  RETURN
ENDIF
tmpQnt = LD.QNT
IF STOCK.CODE # LD.CODE .OR. EMPTY(LD.CODE)
  tmpQnt = 0
ENDIF
n_cols = 0

line_wd = WCOLS()

IF STOCK.QNT_OPT <= 1
  tmpLine = "ฅโ ฎฏโฎขฎฉ ๆฅญ๋"
ELSE
  IF tmpQnt >= STOCK.QNT_OPT
    tmpLine = " ฏโฎข ๏ ๆฅญ  ใแโ ญฎขซฅญ "
    n_cols  = WCOLS()-1
  ELSE
    o_prc   = IIF(LIST_TIT.IS_USD, ;
                  TRANSFORM(STOCK.PRICE_O1, "99999.9999")+"$", ;
                  TRANSFORM(STOCK.PRICE_O1_R, "99999.99999")+"เ")
    tmpLine = " ฏโฎข ๏ ๆฅญ  "+o_prc+", ฎฏโฎขฎฅ ชฎซจ็. "+STR(STOCK.QNT_OPT,6)
    IF tmpQnt > 0
      tmpLine = tmpLine+", ญฅ ๅข โ ฅโ "+STR(STOCK.QNT_OPT-tmpQnt,6)
      n_cols = ROUND(LD.QNT/STOCK.QNT_OPT*WCOLS()-1, 0)
      IF LD.QNT/STOCK.QNT_OPT > opt_part
        ?? REPLICATE(CHR(7), 3)
      ENDIF
      n_cols = MIN(n_cols, WCOLS()-2)
    ENDIF
  ENDIF
ENDIF

IF TYPE("let_edit") = "L"
  let_modi = let_edit
ENDIF

IF let_modi
  DO Set_Opt
ENDIF

tmpLine = PADR(tmpLine, line_wd)

@ line_no, 0 SAY tmpLine
IF n_cols > 0
  @ line_no, 0 FILL TO line_no, n_cols COLOR SCHEME 7
ENDIF

RETURN