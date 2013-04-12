*������������������������������������������������������������������������Ŀ
*�   ��楤�� Lst_Def      ���ࠡ��稪 ������ ����ਥ���                 �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    ���᮪ "�����" � DBF-�ଠ�.                     �
*�                                                                        �
*����������������������������������������������������������� 09.07.2002 ���
PROCEDURE Lst_Def
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
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_2709 AGAIN
IF .NOT. SEEK(l_num)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���᮪ � "+ALLTRIM(STR(l_num,6))+" �� ������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ELSE
  IF "�����" $ LST_NAME
    DO Wt_Mess WITH "�롨ࠥ�... ��⠥�..."
    DO Prep_Def
    DO Wt_Mess
  ELSE
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"���᮪ � "+ALLTRIM(STR(l_num,6))+" �� ᮤ�ন� �����!"
    mss(3) = ""
    DO Out_Mess WITH 5, "mss"
    what_do = "list"
  ENDIF
ENDIF

SELECT LT_2709
USE

SELECT(s_sav)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Def                                                   �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    �����⮢�� �६������ 䠩��.                        �
*�                                                                        �
*����������������������������������������������������������� 09.07.2002 ���
PROCEDURE Prep_Def
PRIVATE dat_wait

SELECT 0
DELETE FILE(tmpo_path+"TTT_DEF.DBF")
CREATE DBF(tmpo_path+"TTT_DEF");
          (CODE     N( 7),  ;
           PREFIX   C( 4),  ;
           NAME     C(35),  ;
           PRODUCER C( 8),  ;
           QNT      N(10),  ;
           PRICE    N(12,5),;
           SUMM     N(10,5),;
           P_MAN    C(15),  ;
           X        C( 1),  ;
           OBSOLETE C( 1),  ;
           W_DATE   D,      ;
           QNT_FIL  N(10),  ;
           QNT_REZ  N(10) )
INDEX ON NAME TAG NAME

SELECT 0
IF File_O(path_comm+"PURCHASE.DBF")
  USE (path_comm+"PURCHASE") ALIAS PUR_2709 ORDER TAG NAME_DATE AGAIN
  SET FILTER TO PURFLAG = "K"
ENDIF

SELECT 0
USE (path_comm+"SUBJECT") ORDER TAG STO_CODE ALIAS SJ_2709 AGAIN

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_2709 AGAIN
SET RELATION TO CODE INTO SJ_2709

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_CODE ALIAS LD_2709 AGAIN
SET RELATION TO CODE INTO S_2709
IF SEEK (STR(l_num,6), "LD_2709")
  SCAN REST WHILE LIST_NUM = STR(l_num,6) FOR .NOT.EMPTY(QNT)
    SELECT S_2709
    IF SEEK(CODE, "S_2709")
      dat_wait = {}
      IF USED("PUR_2709")
        SELECT PUR_2709
        IF SEEK(S_2709.NAME+S_2709.PREFIX+S_2709.PRODUCER, "PUR_2709")
          dat_wait =  PUR_2709.DATE
        ENDIF
      ENDIF
      SELECT TTT_DEF
      APPEND BLANK
      REPLACE CODE     WITH LD_2709.CODE,    ;
              PREFIX   WITH S_2709.PREFIX,   ;
              NAME     WITH S_2709.NAME,     ;
              PRODUCER WITH S_2709.PRODUCER, ;
              QNT      WITH LD_2709.QNT,     ;
              PRICE    WITH IIF(LT_2709.IS_USD,LD_2709.USD_PRICE,LD_2709.SAL_PRICE),;
              SUMM     WITH IIF(LT_2709.IS_USD,LD_2709.USD_PRICE*LD_2709.QNT,LD_2709.SAL_PRICE*LD_2709.QNT),;
              P_MAN    WITH SJ_2709.AUTHOR,  ;
              X        WITH SJ_2709.NOPUR,   ;
              OBSOLETE WITH IIF(SJ_2709.OBSOLETE,"0",""),;
              W_DATE   WITH dat_wait,        ;
              QNT_REZ  WITH S_2709.QNT_REZ,  ;
              QNT_FIL  WITH S_2709.QNT_REAL
      SELECT LD_2709
    ENDIF
  ENDSCAN
ENDIF

IF USED("PUR_2709")
  SELECT PUR_2709
  USE
ENDIF

SELECT SJ_2709
USE
SELECT S_2709
USE
SELECT LD_2709
USE
SELECT TTT_DEF
DO Save_DBF
USE

DELETE FILE(tmpo_path+"TTT_DEF.DBF")

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