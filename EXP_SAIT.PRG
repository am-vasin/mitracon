*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Exp_Sait     맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�          씕召�設 召ⓤ첓, 宋�席ⓣ�쥯���． �� Þ收惜β-쭬첓㎯.           �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 30.08.2002 컴�
PROCEDURE Exp_Sait

*
*  룯誓Д��瑜 貰飡�輾⑨ ㄻ� 췅˘짛與�
*

PRIVATE stat_type     && 뮜� 췅˘짛與�: 0 - �α�젺쩆設�硫;
                                        1 - ∥젺�;
                                        2 - BROWSE - �젩エ��;
                                        3 - BROWSE - 召ⓤ��.
PRIVATE what_do       && 닾� 誓┬쵟.
PRIVATE menu_name     && 닾� 졹Þ魚����． Д��.
PRIVATE last_mouse    && 귖�э ��笹ⅳ�ⅲ� 췅쬊殊� ㎖�黍��� き��え.
PRIVATE win_name      && 닾� �き� ( �ぎ� ㄻ� BROWSE ).
PRIVATE s_sav         && 뜮Д� �젩�曄� �∥졹殊 ㄻ� 貰魚젺��⑨ � ¡遜�젺�˙��⑨!

*
*   뇿��キ畑� ㎛좂��⑨Ж ��誓Д��瑜 貰飡�輾⑨...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

PRIVATE exp_ord, nn

*
*   롡む猶젰� �젵ル �.�., 信�젺젪エ쥯�� 聲筌� � �. �.
*
s_sav = SELECT()

SELECT 0
DELETE FILE(tmpo_path+"EXP_SAIT.DBF")
CREATE DBF(tmpo_path+"EXP_SAIT");
       (CUST_CODE N( 8),  ;
        ORD_ID    N( 9),  ;
        CODE      N( 7),  ;  
        PREFIX    C( 4),  ;
        NAME      C(35),  ;
        PRODUCER  C( 8),  ;
        QTY       N(10),  ;
        BILL_NUM  C(10),  ;
        BILL_DATE D,      ;
        QNT_SAVE  N(10),  ;
        MONEY_R   N(12,2),;
        STATUS    C( 1) ) 

SELECT 0
USE(base_path+"STOCK") ORDER TAG CODE SHARED ALIAS ST_2830

SELECT 0
USE(base_path+"INCMONEY") ORDER TAG BILL_NUM SHARED ALIAS MON_2830

SELECT 0
USE(base_path+"BILLS") ORDER TAG DOC_NUM SHARED ALIAS BIL_2830

SELECT 0
USE(base_path+"LIST_DET") ORDER TAG LIST SHARED ALIAS LD_2830
SET RELATION TO CODE INTO ST_2830

SELECT 0
USE(base_path+"LIST_TIT") ORDER TAG LST_NUM SHARED ALIAS LT_2830
COUNT TO exp_ord FOR REQ_TYPE = "E"
IF exp_ord = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"뜢� �� �ㄽ�． 召ⓤ첓 �� Þ收惜β-쭬첓쭬�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"

  SELECT ST_2830
  USE
  SELECT BIL_2830
  USE
  SELECT LD_2830
  USE
  SELECT LT_2830
  USE
  SELECT EXP_SAIT
  USE
  DELETE FILE(tmpo_path+"EXP_SAIT.DBF")
  SELECT(s_sav)
  RETURN
ENDIF

SET RELATION TO BILL_FIRM+BILL_YEAR+BILL_NUM INTO BIL_2830
SET FILTER TO REQ_TYPE ="E"
nn = 0
SCAN
  m.nn = m.nn+1
  IF m.nn%10 = 0
    WAIT WINDOW ALLTRIM(STR(m.nn))+"/"+ALLTRIM(STR(m.exp_ord)) NOWAIT
  ENDIF
  SELECT LD_2830
  SCAN FOR LIST_NUM = STR(LT_2830.LST_NUM,6)
    SELECT EXP_SAIT
    APPEND BLANK
    REPLACE CUST_CODE WITH LT_2830.CLI_CODE, ;
            ORD_ID    WITH VAL(LT_2830.REQ_DOC),;
            CODE      WITH LD_2830.CODE,     ;  
            PREFIX    WITH ST_2830.PREFIX,   ;
            NAME      WITH ST_2830.NAME,     ;
            PRODUCER  WITH ST_2830.PRODUCER, ;
            QTY       WITH LD_2830.QNT,      ;
            BILL_NUM  WITH LT_2830.BILL_NUM, ;
            BILL_DATE WITH BIL_2830.DOC_DATE,;
            QNT_SAVE  WITH LD_2830.QNT_REQ,  ;
            STATUS    WITH IIF(EMPTY(LT_2830.SAL_NUM),"","*")
    SELECT BIL_2830
    IF SEEK(LT_2830.BILL_FIRM+LT_2830.BILL_YEAR+LT_2830.BILL_NUM, "BIL_2830")
      IF STATUS = "렞쳽曄��"
        SELECT MON_2830
        IF SEEK(BIL_2830.FIRM+LEFT(DTOS(BIL_2830.DOC_DATE),4)+BIL_2830.DOC_NUM, "MON_2830")
          SELECT EXP_SAIT
          REPLACE MONEY_R WITH MON_2830.MONEY_R
        ENDIF
      ENDIF
    ENDIF
    SELECT LD_2830
  ENDSCAN
  SELECT LT_2830
ENDSCAN

*
*   �お侁졻���履� (젶ゃ�졻���履�!) 쭬む猶젰� DBF-�젵ル.
*
SELECT LT_2830
USE
SELECT LD_2830
USE
SELECT ST_2830
USE
SELECT BIL_2830
USE
SELECT EXP_SAIT
DO Save_DBF
USE
DELETE FILE(tmpo_path+"EXP_SAIT.DBF")

SELECT (s_sav)

RETURN

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Save_DBF     맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                           뫌魚젺���� �젵쳽.                            �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 13.11.2001 컴�
PROCEDURE Save_DBF
PRIVATE tmpDir, tmpFile, mss, tmpName, r_sav
tmpDir = ""
tmpFile = ""

DO WHILE .T.
  tmpName = New_File(tmpDir, "dbf", "벆젲ⓥ� �젵�",tmpFile)
  IF EMPTY(tmpName)
    RETURN
  ENDIF
  IF File_O(tmpName)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  뮔ぎ� �젵� 拾� 率耀飡㏂β. 뱾젷º? [ 꽑 ] [ 뜢� ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      DELETE FILE (tmpName)
    ELSE
      LOOP
    ENDIF
  ENDIF
  EXIT
ENDDO

r_sav = RECNO()
COPY TO (tmpName)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN