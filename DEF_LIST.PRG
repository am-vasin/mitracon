*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Def_List      ��о�｀砒┴ ���┃� ���矗┘���                �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�            ��爼�牀���┘ 甎�瓷� 瓷��め���� ぅ筥罔��.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02.07.2002 様�
PROCEDURE Def_List
PARAMETERS l_2702
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

PRIVATE s_sav
s_sav = SELECT() 

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_2702 AGAIN

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_CODE ALIAS LD_2702 AGAIN
SET RELATION TO CODE INTO ST_2702

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_2702 AGAIN

DO Prep_Def

SELECT ST_2702
USE
SELECT LT_2702
USE
SELECT LD_2702
USE

SELECT (s_sav)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�               ��Г��┘ ∇ガキ���� 甎�瓷� 瓷��め���� ぅ筥罔��.          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02.07.2002 様�
PROCEDURE Prep_Def
PRIVATE n_list
n_list = ""

SELECT 0
DELETE FILE(tmpo_path+"TMP_T.DBF")
CREATE DBF (tmpo_path+"TMP_T");
           (LST_ARM   C( 3),;
            CR_DAT    D,    ;
            LST_NAME  C(32),;
            LST_FLAGS C(25),;
            REQ_DOC   C( 8),;
            REQ_DATE  D,    ;
            IS_USD    L,    ;
            IS_CHECK  L,    ;
            CLI_CODE  N( 8),;
            CLI_TYPE  N( 1),;
            MAIL      C( 1),;
            FIRM      C( 3),;
            BANK      C( 3),;
            IS_DIS    C( 1),;
            IS_SPEC   C( 1),;
            WHO_CR    N( 3),;
            WHERE_CR  C( 3))
SELECT 0
DELETE FILE(tmpo_path+"TMP_D.DBF")
DELETE FILE(tmpo_path+"TMP_D.FPT")
CREATE DBF (tmpo_path+"TMP_D");
           (CODE      N( 7),  ;
            PREFIX    C( 4),  ;
            NAME      C(35),  ;
            PRODUCER  C( 8),  ;
            QNT       N( 6),  ;
            IS_SPEC   C( 1),  ;
            USD_DEF   N(10,4),;
            DEF_PRICE N(12,5),;
            USD_PRICE N(10,4),;
            SAL_PRICE N(12,5),;
            NOTE      M       )
INDEX ON NAME TAG NAME

SELECT LT_2702
IF SEEK(l_2702, "LT_2702")
  SELECT TMP_T
    APPEND BLANK
    REPLACE LST_ARM   WITH arm,     ;
            CR_DAT    WITH DATE(),  ;
            LST_NAME  WITH LEFT(LT_2702.LST_NAME,24)+" ぅ筥罔�",;
            LST_FLAGS WITH LT_2702.LST_FLAGS, ;
            IS_USD    WITH LT_2702.IS_USD,    ;
            IS_CHECK  WITH LT_2702.IS_CHECK,  ;
            CLI_CODE  WITH LT_2702.CLI_CODE,  ;
            CLI_TYPE  WITH LT_2702.CLI_TYPE,  ;
            MAIL      WITH LT_2702.MAIL,      ;
            FIRM      WITH LT_2702.FIRM,      ;
            BANK      WITH LT_2702.BANK,      ;
            IS_DIS    WITH LT_2702.IS_DIS,    ;
            IS_SPEC   WITH LT_2702.IS_SPEC,   ;
            WHO_CR    WITH user,              ;
            WHERE_CR  WITH arm

  SELECT LD_2702
  IF SEEK(STR(l_2702,6), "LD_2702")
    SCAN REST WHILE LIST_NUM = STR(l_2702,6)
      IF SEEK(CODE, "ST_2702")
        IF QNT - QNT_REQ > ST_2702.QNT
          SELECT TMP_D
          APPEND BLANK
          REPLACE CODE      WITH LD_2702.CODE,     ;
                  PREFIX    WITH ST_2702.PREFIX,   ;
                  NAME      WITH ST_2702.NAME,     ;
                  PRODUCER  WITH ST_2702.PRODUCER, ;
                  QNT       WITH LD_2702.QNT-LD_2702.QNT_REQ-ST_2702.QNT,;
                  IS_SPEC   WITH LD_2702.IS_SPEC,  ; 
                  USD_DEF   WITH LD_2702.USD_DEF,  ;
                  DEF_PRICE WITH LD_2702.DEF_PRICE,;
                  USD_PRICE WITH LD_2702.USD_PRICE,;
                  SAL_PRICE WITH LD_2702.SAL_PRICE,;
                  NOTE      WITH LD_2702.NOTE
        ENDIF
        SELECT LD_2702
      ENDIF
    ENDSCAN
  ENDIF

  n_list = DOC_NUM("LISTS", "", "", "")

  SELECT LT_2702
  APPEND BLANK
  REPLACE LST_ARM  WITH arm,           ;
          LST_NUM  WITH n_list,        ;
          IS_USD   WITH TMP_T.IS_USD,  ;
          IS_CHECK WITH TMP_T.IS_CHECK,;
          CR_DAT   WITH DATE(),        ;
          MAIL     WITH TMP_T.MAIL,    ;
          LST_NAME WITH TMP_T.LST_NAME,;
          CLI_CODE WITH TMP_T.CLI_CODE,;
          CLI_TYPE WITH TMP_T.CLI_TYPE,;
          REQ_DOC  WITH TMP_T.REQ_DOC, ;
          REQ_DATE WITH TMP_T.REQ_DATE,;
          FIRM     WITH TMP_T.FIRM,    ;
          BANK     WITH TMP_T.BANK,    ;
          IS_DIS   WITH TMP_T.IS_DIS,  ;
          IS_SPEC  WITH TMP_T.IS_SPEC, ;
          WHO_CR   WITH user,          ;
          WHERE_CR WITH arm

  SELECT TMP_D
  SCAN
    SELECT LD_2702
    APPEND BLANK
    REPlACE LIST_NUM  WITH STR(n_list,6), ;
            CODE      WITH TMP_D.CODE,    ;
            PREFIX    WITH TMP_D.PREFIX,  ;
            NAME      WITH TMP_D.NAME,    ;
            PRODUCER  WITH TMP_D.PRODUCER,;
            QNT       WITH TMP_D.QNT,     ;
            IS_SPEC   WITH TMP_D.IS_SPEC, ;
            USD_DEF   WITH TMP_D.USD_DEF,  ;
            DEF_PRICE WITH TMP_D.DEF_PRICE,;
            USD_PRICE WITH TMP_D.USD_PRICE,;
            SAL_PRICE WITH TMP_D.SAL_PRICE,;
            NOTE      WITH TMP_D.NOTE
    SELECT TMP_D
  ENDSCAN

  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+'�筮爼�牀��� ���覃 甎�甌� " ぅ筥罔�", N '+ALLTRIM(STR(n_list))
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"

  SELECT TMP_T
  USE
  DELETE FILE(tmpo_path+"TMP_T.DBF")
ENDIF
SELECT TMP_D
USE
DELETE FILE(tmpo_path+"TMP_D.DBF")
DELETE FILE(tmpo_path+"TMP_D.FPT")

RETURN
