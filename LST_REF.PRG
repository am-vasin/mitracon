*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Lst_Ref      맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                    뫍ⓤ�� "롡첓㏓" � DBF-兒席졻�.                      �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 18.06.2002 컴�
PROCEDURE Lst_Ref
PARAMETERS l_num

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

*
*   롡む猶젰� �젵ル �.�., 信�젺젪エ쥯�� 聲筌� � �. �.
*

s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_2618 AGAIN
IF .NOT. SEEK(l_num)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"뫍ⓤ�� � "+ALLTRIM(STR(l_num,6))+" �� 췅ħ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ELSE
  IF "�洙젳�" $ LST_NAME
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"귣〃�졻� 쩆栒 ��笹ⅳ�ⅸ ��ゃ�え ��㎤與� か���獸� ?"+"  [ 뜢� ] [ 꽑 ]"
      mss(3) = ""
      IF Out_Mess(5, "mss") = 1
        DO Wt_Mess WITH "귣〃�젰�... 묊ⓥ젰�..."
        DO Prep_Ref
        DO Wt_Mess
      ELSE
        DO Wt_Mess WITH "귣〃�젰�... 묊ⓥ젰�..."
        DO Prep_Ref1
        DO Wt_Mess
      ENDIF  
  ELSE
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"뫍ⓤ�� � "+ALLTRIM(STR(l_num,6))+" �� 貰ㄵ逝ⓥ �洙젳�!"
    mss(3) = ""
    DO Out_Mess WITH 5, "mss"
    what_do = "list"
  ENDIF
ENDIF

SELECT LT_2618
USE

SELECT(s_sav)
RETURN

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Prep_Ref                                                   �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                    룼ㄳ�獸˚� №�Д���． �젵쳽.                        �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 18.06.2002 컴�
PROCEDURE Prep_Ref

SELECT 0
DELETE FILE(tmpo_path+"TTT_REF.DBF")
CREATE DBF(tmpo_path+"TTT_REF");
          (CODE     N( 7),  ;
           PREFIX   C( 4),  ;
           NAME     C(35),  ;
           PRODUCER C( 8),  ;
           PROD_MAN C(15),  ;
           Q_ZAKAZ  N(10),  ;
           Q_ZAPROS N(10),  ;
           Q_OTKAZ  N(10),  ;
           S_ROUND  N(10),  ;
           PRICE    N(12,5),;
           SUMM     N(10,5),;
           NOTE     C(10) )
INDEX ON NAME TAG NAME

SELECT 0
USE (path_comm+"SUBJECT") ORDER TAG STO_CODE ALIAS SJ_2618 AGAIN

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_2618 AGAIN
SET RELATION TO CODE INTO SJ_2618

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS LD_2618 AGAIN
SET RELATION TO CODE INTO S_2618
IF SEEK(STR(l_num,6))
  SCAN REST WHILE LIST_NUM = STR(l_num,6) FOR .NOT.EMPTY(QNT)
    IF S_2618.QNT >= QNT - QNT_REQ
      SELECT TTT_REF
      APPEND BLANK
      REPLACE CODE     WITH LD_2618.CODE,     ;
              PREFIX   WITH S_2618.PREFIX,    ;
              NAME     WITH S_2618.NAME,      ;
              PRODUCER WITH S_2618.PRODUCER,  ;
              PROD_MAN WITH SJ_2618.AUTHOR,   ;
              Q_ZAKAZ  WITH LD_2618.QNT_ORD-LD_2618.QNT,;
              Q_ZAPROS WITH LD_2618.QNT_ORD,  ;
              Q_OTKAZ  WITH LD_2618.QNT,      ;
              S_ROUND  WITH S_2618.QNT_PACK,  ;
              PRICE    WITH IIF(LT_2618.IS_USD,LD_2618.USD_PRICE,LD_2618.SAL_PRICE),;
              SUMM     WITH IIF(LT_2618.IS_USD,LD_2618.USD_PRICE*LD_2618.QNT,LD_2618.SAL_PRICE*LD_2618.QNT),;
              NOTE     WITH IIF(EMPTY(SJ_2618.NODISC),"","뫍ζ.璵췅")
    ENDIF
  ENDSCAN
ENDIF

SELECT SJ_2618
USE
SELECT S_2618
USE
SELECT LD_2618
USE
SELECT TTT_REF
DO Save_DBF
USE

DELETE FILE(tmpo_path+"TTT_REF.DBF")

RETURN
*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Prep_Ref1                                                  �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                    룼ㄳ�獸˚� №�Д���． �젵쳽.                        �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 18.06.2002 컴�
PROCEDURE Prep_Ref1
PRIVATE d001, d002, doc000, doc001
d002 = DATE()
d001 = DATE() - 180

IF .NOT. G_PERIOD(d001, d002)
  RETURN
ENDIF

SELECT 0
DELETE FILE(tmpo_path+"TTT_REF.DBF")
CREATE DBF(tmpo_path+"TTT_REF");
          (CODE     N( 7),  ;
           PREFIX   C( 4),  ;
           NAME     C(35),  ;
           PRODUCER C( 8),  ;
           PROD_MAN C(15),  ;
           Q_ZAKAZ  N(10),  ;
           Q_ZAPROS N(10),  ;
           Q_OTKAZ  N(10),  ;
           S_ROUND  N(10),  ;
           PRICE    N(12,5),;
           SUMM     N(10,4),;
           DAT_RSAL D,      ;
           NOTE     C(10))
INDEX ON NAME TAG NAME

SELECT 0
USE (path_comm+"SUBJECT") ORDER TAG STO_CODE ALIAS SJ_2618 AGAIN

SELECT 0
USE (base_path+"SALE") ORDER TAG CODE ALIAS SD_2618 AGAIN

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS SD_2620 AGAIN

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON DESCENDING ALIAS ST_2618 AGAIN
SET FILTER TO DOC_DATE >= d001 .AND. DOC_DATE <= d002
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO SD_2620
SET SKIP TO SD_2620

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_2618 AGAIN
SET RELATION TO CODE INTO SJ_2618

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS LD_2618 AGAIN
SET RELATION TO CODE INTO S_2618
IF SEEK(STR(l_num,6))
  SCAN REST WHILE LIST_NUM = STR(l_num,6) FOR .NOT.EMPTY(QNT)
    SELECT TTT_REF
    APPEND BLANK
    REPLACE CODE     WITH LD_2618.CODE,     ;
            PREFIX   WITH S_2618.PREFIX,    ;
            NAME     WITH S_2618.NAME,      ;
            PRODUCER WITH S_2618.PRODUCER,  ;
            PROD_MAN WITH SJ_2618.AUTHOR,   ;
            Q_ZAKAZ  WITH LD_2618.QNT_ORD-LD_2618.QNT,;
            Q_ZAPROS WITH LD_2618.QNT_ORD,  ;
            Q_OTKAZ  WITH LD_2618.QNT,      ;
            S_ROUND  WITH S_2618.QNT_PACK,  ;
            PRICE    WITH IIF(LT_2618.IS_USD,LD_2618.USD_PRICE,LD_2618.SAL_PRICE),;
            SUMM     WITH IIF(LT_2618.IS_USD,LD_2618.USD_PRICE*LD_2618.QNT,LD_2618.SAL_PRICE*LD_2618.QNT),;
            NOTE     WITH IIF(EMPTY(SJ_2618.NODISC),"","뫍ζ.璵췅")
    SELECT ST_2618
    GO TOP
    doc000 = FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM
    SCAN FOR CUS_CODE = LT_2618.CLI_CODE
      SELECT SD_2620
      doc001 = FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM
      SCAN FOR doc001 = doc000
      LOCATE FOR CODE = LD_2618.CODE
      IF FOUND()
        SELECT TTT_REF
        REPLACE DAT_RSAL WITH SD_2620.DOC_DATE
      ENDIF
      EXIT
     ENDSCAN
    ENDSCAN
    SELECT LD_2618      
  ENDSCAN
ENDIF

SELECT SJ_2618
USE
SELECT SD_2618
USE
SELECT SD_2620
USE
SELECT ST_2618
USE
SELECT S_2618
USE
SELECT LD_2618
USE
SELECT TTT_REF
DO Save_DBF
USE

DELETE FILE(tmpo_path+"TTT_REF.DBF")

RETURN

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Save_DBF     맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                           뫌魚젺���� �젵쳽.                            �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 13.11.2001 컴�
PROCEDURE Save_DBF

PRIVATE tmp_dir, mss, tmpName, r_sav

tmp_dir = IIF(Is_Dir("D:"), "D:\", "C:\")

DO WHILE .T.
  tmpName = New_File(tmp_dir, "dbf", "벆젲ⓥ� �젵�")
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