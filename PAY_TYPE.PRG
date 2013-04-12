*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Pay_Type      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                              ’จฏ ฏซ โฅฆ                                บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 31.08.2001 ออผ
PROCEDURE Pay_Type
PARAMETERS p_code, sgn

DO CASE
CASE p_code = " "
  IF TYPE("sgn") = "N"
    IF sgn < 0
      RETURN " แๅฎคญ๋ฉ ฎเคฅเ"
    ELSE
      RETURN "เจๅฎคญ๋ฉ ฎเคฅเ"
    ENDIF
  ELSE
    RETURN "เจๅฎคญ๋ฉ ฎเคฅเ"
  ENDIF
CASE p_code = "1"
  RETURN "/ ฎฏซ โ "
CASE p_code = "—"
  RETURN "—ฅช"
CASE p_code = "3"
  RETURN "ญคฅชแ. ฏซ โฅฆ"
CASE p_code = ""
  RETURN "ฏซ โ  ็ฅเฅง ‘/"
ENDCASE

RETURN ""
