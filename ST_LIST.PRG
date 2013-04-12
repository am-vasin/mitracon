*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  St_List       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                          ‘โ ญค เโญ๋ฉ แฏจแฎช.                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 07.09.1999 ออผ
PROCEDURE St_List
PARAMETER new

PRIVATE c1, c2, c3, c4, c5, c6, c7, l_n, sw
STORE "" TO c1, c2, c3, c4, c5, c6, c7
IF .NOT. new .AND. .NOT. EMPTY(LIST_TIT.SAL_NUM)
  DIMENSION l_n(3)
  l_n(1) = ""
  l_n(2) = CHR(0)+"‘ฏจแฎช ใฆฅ ฏเฎค ญ!"
  l_n(3) = ""
  DO Out_Mess WITH 7, "l_n"
  RETURN
ENDIF

DO Ch_Lst WITH c1, c2, c3, c4, c5, c6, c7
IF LEN(c1) = 0
  RETURN
ENDIF
sw = .F.
IF new
  l_n = Doc_Num("LISTS", "", arm, DATE())
  SELECT LIST_TIT
  APPEND BLANK
  REPLACE LST_ARM   WITH arm,   ;
          LST_NUM   WITH l_n,   ;
          CR_DAT    WITH DATE(),;
          WHO_CR    WITH user,  ;
          ORD_QUEUE WITH queuetype,;
          WHERE_CR  WITH arm
ELSE
  sw = IS_CHECK # c7
ENDIF

IF c7
  c5 = LEFT(c5,25)+"—ฅช"
ENDIF
     
REPLACE LST_NAME  WITH c5, ;
        LST_FLAGS WITH c1, ;
        CLI_CODE  WITH c2, ;
        FIRM      WITH c3, ;
        IS_USD    WITH IIF(new, c6, IS_USD), ;
        IS_CHECK  WITH c7, ;
        BANK      WITH c4

IF sw .AND. .NOT. new .AND. .NOT. IS_USD
*  IF IS_CHECK
*    PRIVATE nds_flt
*    nds_flt = 0
*  ENDIF
  SELECT 0
  USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS LD_9B10 AGAIN
  c1 = STR(LIST_TIT.LST_NUM,6)
  c2 = 0
  SEEK c1
  SCAN REST WHILE LIST_NUM = c1
    REPLACE SAL_PRICE WITH Round_6(SAL_PRICE)
    c2 = c2 +ROUND(QNT*SAL_PRICE,2)
  ENDSCAN
  USE
  SELECT LIST_TIT
  REPLACE R_TOTAL WITH c2
ENDIF

RETURN
