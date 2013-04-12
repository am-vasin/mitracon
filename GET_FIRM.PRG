*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Get_Firm      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                        ฅญฅเ ๆจ๏ GET คซ๏ ไจเฌ๋.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 15.01.1999 ออผ
PROCEDURE Get_Firm
PARAMETERS l_01, c_01, is_enab, exit_proc
PRIVATE s_sav, n_f
n_f = IIF(EMPTY(exit_proc), "", '"'+exit_proc+'"')

IF EMPTY(f_c000)
  f_n000 = SPACE(16)
ELSE
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FRM_9120 AGAIN
  IF SEEK(f_c000)
    f_n000 = B_NAME
  ELSE
    f_c000 = "   "
    f_n000 = SPACE(16)
  ENDIF
  USE
  SELECT (s_sav)
ENDIF

@ l_01, c_01 SAY "เฅคฏเจ๏โจฅ "
IF is_enab
  @ ROW(), COL() GET f_c000 WHEN S_F1_(&n_f) VALID C_F1_(&n_f)
  @ ROW(), COL()+1 GET f_n000 WHEN .F.
  @ ROW(), COL()+1 SAY "(F1)"
ELSE
  @ ROW(), COL() GET f_c000 DISABLE
  @ ROW(), COL()+1 GET f_n000 DISABLE
ENDIF

RETURN
