*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Saled         งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                            ‘ฏจแแฎช ฏเฎค ญ?                             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 10/31/2007 ออผ
PROCEDURE Saled
PARAMETERS l_num

PRIVATE tmpNum, s_sav

IF TYPE("m.l_num") = "N"
  m.tmpNum = STR(m.l_num, 6)
ELSE
  m.tmpNum = PADL(ALLTRIM(m.l_num), 6)
ENDIF

USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS SLD_L IN 0
IF .NOT. SEEK(VAL(ALLTRIM(m.tmpNum)), "SLD_L")
  USE IN SLD_L
  RETURN .T.
ENDIF

IF .NOT. EMPTY(SLD_L.SAL_NUM)
  USE IN SLD_L
  RETURN .T.
ENDIF

USE (m.base_path+"SALE_TIT") SHARED AGAIN ORDER TAG LIST_NUM ALIAS SLD_S IN 0
IF SEEK(m.tmpNum, "SLD_S")
  REPLACE SLD_L.SAL_NUM  WITH SLD_S.DOC_NUM, ;
          SLD_L.SAL_DATE WITH SLD_S.DOC_DATE
  USE IN SLD_S
  USE IN SLD_L
  RETURN .T.
ENDIF

USE IN SLD_S
USE IN SLD_L
RETURN .F.
