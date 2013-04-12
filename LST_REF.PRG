*������������������������������������������������������������������������Ŀ
*�   ��楤�� Lst_Ref      ���ࠡ��稪 ������ ����ਥ���                 �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    ���᮪ "�⪠��" � DBF-�ଠ�.                      �
*�                                                                        �
*����������������������������������������������������������� 18.06.2002 ���
PROCEDURE Lst_Ref
PARAMETERS l_num

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

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
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
  mss(2) = CHR(0)+"���᮪ � "+ALLTRIM(STR(l_num,6))+" �� ������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ELSE
  IF "�⪠��" $ LST_NAME
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"�롨��� ���� ��᫥���� ���㯪� ����樨 �����⮬ ?"+"  [ ��� ] [ �� ]"
      mss(3) = ""
      IF Out_Mess(5, "mss") = 1
        DO Wt_Mess WITH "�롨ࠥ�... ��⠥�..."
        DO Prep_Ref
        DO Wt_Mess
      ELSE
        DO Wt_Mess WITH "�롨ࠥ�... ��⠥�..."
        DO Prep_Ref1
        DO Wt_Mess
      ENDIF  
  ELSE
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"���᮪ � "+ALLTRIM(STR(l_num,6))+" �� ᮤ�ন� �⪠��!"
    mss(3) = ""
    DO Out_Mess WITH 5, "mss"
    what_do = "list"
  ENDIF
ENDIF

SELECT LT_2618
USE

SELECT(s_sav)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Ref                                                   �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    �����⮢�� �६������ 䠩��.                        �
*�                                                                        �
*����������������������������������������������������������� 18.06.2002 ���
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
              NOTE     WITH IIF(EMPTY(SJ_2618.NODISC),"","����.業�")
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
*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Ref1                                                  �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    �����⮢�� �६������ 䠩��.                        �
*�                                                                        �
*����������������������������������������������������������� 18.06.2002 ���
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
            NOTE     WITH IIF(EMPTY(SJ_2618.NODISC),"","����.業�")
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Save_DBF     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ���࠭���� 䠩��.                            �
*�                                                                        �
*����������������������������������������������������������� 13.11.2001 ���
PROCEDURE Save_DBF

PRIVATE tmp_dir, mss, tmpName, r_sav

tmp_dir = IIF(Is_Dir("D:"), "D:\", "C:\")

DO WHILE .T.
  tmpName = New_File(tmp_dir, "dbf", "������ 䠩�")
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