*������������������������������������������������������������������������Ŀ
*�   ��楤�� Exp_Sait     ���ࠡ��稪 ������ ����ਥ���                 �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�          ��ᯮ�� ᯨ᪠, ��ନ஢������ �� ���୥�-������.           �
*�                                                                        �
*����������������������������������������������������������� 30.08.2002 ���
PROCEDURE Exp_Sait

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

PRIVATE exp_ord, nn

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
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
  mss(2) = CHR(0)+"��� �� ������ ᯨ᪠ �� ���୥�-�������!"
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
      IF STATUS = "����祭�"
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
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Save_DBF     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ���࠭���� 䠩��.                            �
*�                                                                        �
*����������������������������������������������������������� 13.11.2001 ���
PROCEDURE Save_DBF
PRIVATE tmpDir, tmpFile, mss, tmpName, r_sav
tmpDir = ""
tmpFile = ""

DO WHILE .T.
  tmpName = New_File(tmpDir, "dbf", "������ 䠩�",tmpFile)
  IF EMPTY(tmpName)
    RETURN
  ENDIF
  IF File_O(tmpName)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ����� 䠩� 㦥 �������. ������? [ �� ] [ ��� ]"
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