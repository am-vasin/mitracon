*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Sait_MTK      ��о�｀砒┴ ���┃� ���矗┘���                �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ��爼�牀���┘ ����� か� ����� ���.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 22.07.2002 様�
PROCEDURE Sait_MTK
PRIVATE is_file, tmpCount
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
DELETE FILE(tmpo_path+"TMP_SAIT.DBF")
DELETE FILE(tmpo_path+"TMP_SAIT.CDX")
CREATE DBF (tmpo_path+"TMP_SAIT");
           (CODE      N( 7),  ;
            PREFIX    C( 4),  ;
            NAME      C(35),  ;
            PRODUCER  C( 8),  ;
            CORPUS    C(16),  ;
            CLASS     C(12),  ;
            QNT       N(10),  ;
            QNT_OPT   N(10),  ;
            QNT_C     N(10),  ;
            QNT_PACK  N(10),  ;
            PRICE_R_D N(10,4),;
            PRICE_R_R N(12,2),;
            PRICE_O_D N(10,4),;
            PRICE_O_R N(12,2),;
            SEGMENT   C( 6),  ;
            DATE_ON   D,      ;
            WEIGHT    N(6,2), ;
            OBSOLETE  C( 1),  ;
            DATASHEET C(32) )
INDEX ON NAME TAG NAME            

SELECT 0
USE (path_comm+"CORPUS") ORDER TAG CODE ALIAS CRP_2722 AGAIN

SELECT 0
USE (path_comm+"SUBJECT") ORDER TAG STO_CODE ALIAS SJ_2722 AGAIN

SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME ALIAS ST_2722 AGAIN
SET RELATION TO CODE INTO SJ_2722
SET RELATION TO CODE INTO CRP_2722 ADDITIVE
m.tmpCount = 0
SCAN
*  IF SJ_2722.COMP_ONLY = "X"
*    LOOP
*  ENDIF
  m.tmpCount = m.tmpCount+1
  IF m.tmpCount % 100 = 0
    WAIT WINDOW STR(m.tmpCount) NOWAIT
  ENDIF
  SELECT TMP_SAIT
  APPEND BLANK
  REPLACE CODE      WITH ST_2722.CODE,      ;
          PREFIX    WITH ST_2722.PREFIX,    ;
          NAME      WITH ST_2722.NAME,      ;
          PRODUCER  WITH ST_2722.PRODUCER,  ;
          CORPUS    WITH SJ_2722.CORPUS,    ;
          CLASS     WITH ST_2722.CLASS,     ;
          QNT       WITH ST_2722.QNT,       ;
          QNT_C     WITH ST_2722.QNT_STORE, ;
          QNT_OPT   WITH 999999999,         ;
          QNT_PACK  WITH ST_2722.QNT_ROUND, ;
          PRICE_R_D WITH ST_2722.PRICE,     ;
          PRICE_R_R WITH ST_2722.PRICE_R,   ;
          PRICE_O_D WITH ST_2722.PRICE_O1,  ;
          PRICE_O_R WITH ST_2722.PRICE_O1_R,;
          SEGMENT   WITH SJ_2722.SEGMENT,   ;
          DATE_ON   WITH IIF(EMPTY(ST_2722.DATE_ON), DATE(), ST_2722.DATE_ON), ;
          WEIGHT    WITH CRP_2722.WEIGHT,   ;
          OBSOLETE  WITH IIF(SJ_2722.OBSOLETE,"*",""),;
          DATASHEET WITH SJ_2722.DATASHEET
  SELECT ST_2722
ENDSCAN

SELECT SJ_2722
USE
SELECT ST_2722
USE
SELECT CRP_2722
USE

SELECT TMP_SAIT
DO Wt_Mess WITH "��茱�..."
DO Save_DBF
DO Wt_Mess

USE
DELETE FILE(tmpo_path+"TMP_SAIT.DBF")
DELETE FILE(tmpo_path+"TMP_SAIT.CDX")

SELECT(s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Save_DBF     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��縲��キ┘ �����.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.11.2001 陳�
PROCEDURE Save_DBF
PRIVATE tmpDir, tmpFile, mss, tmpName, r_sav
tmpDir = ""
tmpFile = ""

DO WHILE .T.
  tmpName = New_File(tmpDir, "dbf", "���Θ皀 ����",tmpFile)
  IF EMPTY(tmpName)
    RETURN
  ENDIF
  IF File_O(tmpName)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ����� ���� 礒� 痺薀痰≪モ. ����━? [ �� ] [ �モ ]"
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