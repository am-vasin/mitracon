*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  To_URM        งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ       ”ฎเฌจเฎข ญจฅ ไ ฉซ  จง แฏจแช  คซ๏ ใค ซฅญญฎฃฎ เ กฎ็ฅฃฎ ฌฅแโ        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 24.10.2007 ออผ
PROCEDURE To_URM
PARAMETERS l_num

PRIVATE tmpNm, s_sav, mss, s_list

m.s_list = Seg_Lst()
IF EMPTY(m.s_list)
  RETURN
ENDIF
m.s_sav = SELECT()
SELECT 0
USE m.path_comm+"SUBJECT" SHARED AGAIN ORDER TAG STO_CODE ALIAS SJ_07A24
SELECT 0
USE m.base_path+"LIST_DET" SHARED AGAIN ORDER TAG L_NAME ALIAS LS_07A24
SET RELATION TO CODE INTO SJ_07A24
SEEK m.l_num
LOCATE REST WHILE m.l_num = LIST_NUM FOR QNT # 0 .AND. CODE # 0 .AND. ;
                  FOUND("SJ_07A24") .AND. SJ_07A24.SEGMENT $ m.s_list
IF .NOT. FOUND()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"‘ฏจแฎช ฏใแโ..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN SJ_07A24
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF
m.tmpNm = New_File("", "DBF", "คฅ แฎๅเ ญจโ์ ง ช ง")
IF EMPTY(m.tmpNm)
  USE IN SJ_07A24
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF

COPY REST WHILE m.l_num = LIST_NUM FOR QNT # 0 .AND. CODE # 0 .AND. ;
                FOUND("SJ_07A24") .AND. SJ_07A24.SEGMENT $ m.s_list ;
       TO (m.tmpNm) FIELDS CODE, QNT

USE IN SJ_07A24
USE
SELECT (m.s_sav)
RETURN
