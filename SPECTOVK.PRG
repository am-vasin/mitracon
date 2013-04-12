*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ SpecToVK      งเ กฎโ็จช €ญคเฅฉ  แจญ           24.03.98 11:14:35 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                  ฎแ๋ซช  ใโข. แฏฅๆคขจฆฅญจ๏ ข  เๅจข.                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE SpecToVK
PARAMETERS d_num, d_date

PRIVATE sel_rem, d_year, c_c, c_sav, who_0, who_1

IF EMPTY(vk_spec)
  RETURN
ENDIF

IF TYPE("d_date") = "D"
  d_year = LEFT(DTOS(d_date),4)
ENDIF
IF TYPE("d_date") = "N"
  d_year = STR(d_date,4)
ENDIF
IF TYPE("d_date") = "C"
  d_year = d_date
ENDIF

sel_rem = SELECT()
SELECT 0
USE (base_path+"LET_SP_T") ORDER TAG DOC_NUM ALIAS SEND_TMP AGAIN
IF .NOT. SEEK(d_year+d_num)
  USE
  SELECT (sel_sav)
  RETURN
ENDIF

c_c = CUS_CODE
who_0 = WHO
who_1 = WHO_LET

USE (vk_spec) ALIAS SEND_RES AGAIN

SELECT 0
USE (base_path+"LET_SPEC") ORDER TAG DOC_NUM ALIAS SEND_TMP AGAIN

SEEK d_year+d_num
c_sav = 0
SCAN REST WHILE d_num = DOC_NUM .AND. d_year = LEFT(DTOS(DOC_DATE),4)
  SELECT SEND_RES
  IF c_sav # SEND_TMP.CODE
    APPEND BLANK
    REPLACE DOC_NUM  WITH SEND_TMP.DOC_NUM,  ;
            DATE     WITH SEND_TMP.DOC_DATE, ;
            SUB_SYS  WITH sys_char,          ;
            CUS_CODE WITH c_c,               ;
            STO_CODE WITH SEND_TMP.CODE,     ;
            QTY      WITH SEND_TMP.QNT,      ;
            WHO      WITH who_0,             ;
            WHO_LET  WITH who_1

    c_sav = SEND_TMP.CODE
  ELSE
    REPLACE QTY WITH QTY+SEND_TMP.QNT
  ENDIF
  SELECT SEND_TMP
ENDSCAN

USE
SELECT SEND_RES
USE

SELECT (sel_rem)

RETURN
