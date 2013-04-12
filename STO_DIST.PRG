*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Sto_Dist     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�              �����⮢�� ����ਡ�⨢� ��� �ணࠬ�� STORE.              �
*�                                                                        �
*����������������������������������������������������������� 05/19/1999 �ͼ
PROCEDURE Sto_Dist

PRIVATE dist_path       &&
PRIVATE dist_c_c        &&
PRIVATE dist_c_n        &&
PRIVATE dist_sto        &&
PRIVATE dist_cash       &&
PRIVATE dist_val        &&
PRIVATE dist_own        &&
PRIVATE dist_ver        &&
PRIVATE dist_reg        &&
PRIVATE dist_note       &&
PRIVATE scr_path        &&

dist_path = "Z:\STORE\DIST\"  && ��� �࠭���� ����ਡ�⨢
dist_path = PADR(dist_path,60)
dist_c_c  = 0               && ��� ������
dist_c_n  = SPACE(60)       && ��� ������
dist_note = SPACE(60)       && ��� ������
dist_sto  = 1               && ����������� (�����, ᢮�, ���)
dist_cash = .F.             && ����稥
dist_val  = 1               && ����� (�㡫�, �������)

IF .NOT. Fil_Blank()
  RETURN
ENDIF

DO Wt_Mess WITH "����஢����"
dist_path = ALLTRIM(dist_path)

scr_path = tmpo_path+SYS(3)

PRIVATE s, d_sav
s = "!mkdir "+scr_path+" > NUL"
&s
s = "! COPY "+dist_path+"*.* "+scr_path+"\*.* > NUL"
&s

s = "!mkdir "+scr_path+"\BASE > NUL"
&s
s = "! COPY "+dist_path+"BASE\*.* "+scr_path+"\BASE\*.* > NUL"
&s

s = "!mkdir "+scr_path+"\BASE\TMP > NUL"
&s
s = "! COPY "+dist_path+"BASE\TMP\*.* "+scr_path+"\BASE\TMP\*.* > NUL"
&s

s = "!mkdir "+scr_path+"\LOG > NUL"
&s
s = "! COPY "+dist_path+"LOG\*.* "+scr_path+"\LOG\*.* > NUL"
&s

s = "!mkdir "+scr_path+"\OWN > NUL"
&s
s = "! COPY "+dist_path+"OWN\*.* "+scr_path+"\OWN\*.* > NUL"
&s

s = "!mkdir "+scr_path+"\OWN\PRMS > NUL"
&s
s = "! COPY "+dist_path+"OWN\PRMS\*.* "+scr_path+"\OWN\PRMS\*.* > NUL"
&s

s = "!mkdir "+scr_path+"\OWN\TMP > NUL"
&s
s = "! COPY "+dist_path+"OWN\TMP\*.* "+scr_path+"\OWN\TMP\*.* > NUL"
&s

dist_reg = PADR(SYS(3),8)
= F_Stuff(scr_path+"\STORE.APP", "��� ���.", dist_reg)
dist_ver = F_Seek(scr_path+"\STORE.APP", "$�����$")
s = FOPEN(scr_path+"\STORE.APP")
= FSEEK(s, dist_ver+8)
dist_ver = FREAD(s, 8)
= FCLOSE(s)
DO Wt_Mess

DO Wt_Mess WITH "�����⮢�� ������"
USE (scr_path+"\BASE\CUSTOMER")
IF EMPTY(dist_c_c)
  APPEND BLANK
  REPLACE CODE   WITH 100,     ;
          SELF   WITH "*",     ;
          L_NAME WITH dist_c_n,;
          B_NAME WITH dist_c_n
ELSE
  APPEND BLANK
  REPLACE CODE   WITH dist_c_c, ;
          SELF   WITH "*",      ;
          L_NAME WITH dist_c_n, ;
          B_NAME WITH dist_c_n
  APPEND BLANK
  REPLACE CODE   WITH 10,           ;
          SUP    WITH "*",          ;
          L_NAME WITH "��� ������", ;
          B_NAME WITH "������"
ENDIF

USE (scr_path+"\BASE\PARMS")
REPLACE VALUTA WITH IIF(dist_val=1, "�", "$"), DAT_START WITH DATE()

USE (scr_path+"\BASE\STOCK")

SELECT 0
USE (base_path+"\STOCK") ORDER TAG CODE ALIAS ST000

DO CASE
CASE dist_sto = 2   && ���� �����������
  SELECT 0
  USE (base_path+"\RYN_L") ORDER TAG CUS_CODE
  SET RELATION TO CODE INTO ST000
  SEEK dist_c_c
  SCAN REST WHILE CUS_CODE = dist_c_c
    DO C_B_Name WITH "ST000", "STOCK"
    SELECT STOCK
    REPLACE QNT_START WITH 0, ;
            QNT_S_REP WITH 0, ;
            QNT_S_RYN WITH 0, ;
            QNT       WITH 0, ;
            QNT_REP   WITH 0, ;
            QNT_STORE WITH 0, ;
            QNT_SAVE  WITH 0, ;
            QNT_REAL  WITH 0, ;
            PRICE_B   WITH 0, ;
            PRICE_B_R WITH 0, ;
            PRICE     WITH 0, ;
            PRICE_R   WITH 0, ;
            QNT_DAY   WITH 0, ;
            QNT_MIN   WITH 0, ;
            ADDRESS   WITH "",;
            MEMO_ST   WITH ""
    IF dist_cash
      REPLACE QNT_START WITH RYN_L.QNT, ;
              QNT_S_REP WITH RYN_L.QNT, ;
              QNT       WITH RYN_L.QNT, ;
              PRICE_B   WITH RYN_L.PRICE_D, ;
              PRICE_B_R WITH RYN_L.PRICE_R, ;
              QNT_REP   WITH RYN_L.QNT
    ENDIF
    SELECT RYN_L
  ENDSCAN
  SELECT STOCK
  APPEND BLANK
  REPLACE CODE WITH 9000000, NAME WITH "==========", DATE_OFF WITH DATE()
  SELECT RYN_L
  USE
CASE dist_sto = 3   && ��� �����������
  SELECT 0
  USE (base_path+"\RYN_L") ORDER TAG C_CODE
  SELECT ST000
  SCAN
    DO C_B_Name WITH "ST000", "STOCK"
    SELECT STOCK
    REPLACE QNT_START WITH 0, ;
            QNT_S_REP WITH 0, ;
            QNT_S_RYN WITH 0, ;
            QNT       WITH 0, ;
            QNT_REP   WITH 0, ;
            QNT_STORE WITH 0, ;
            QNT_SAVE  WITH 0, ;
            QNT_REAL  WITH 0, ;
            PRICE_B   WITH 0, ;
            PRICE_B_R WITH 0, ;
            PRICE     WITH 0, ;
            PRICE_R   WITH 0, ;
            QNT_DAY   WITH 0, ;
            QNT_MIN   WITH 0, ;
            ADDRESS   WITH "",;
            MEMO_ST   WITH ""
    IF dist_cash
      IF SEEK(STR(dist_c_c,5)+STR(CODE,7), "RYN_L")
        REPLACE QNT_START WITH RYN_L.QNT, ;
                QNT_S_REP WITH RYN_L.QNT, ;
                QNT       WITH RYN_L.QNT, ;
                PRICE_B   WITH RYN_L.PRICE_D, ;
                PRICE_B_R WITH RYN_L.PRICE_R, ;
                QNT_REP   WITH RYN_L.QNT
      ENDIF
    ENDIF
    SELECT ST000
  ENDSCAN
  SELECT STOCK
  APPEND BLANK
  REPLACE CODE WITH 9000000, NAME WITH "==========", DATE_OFF WITH DATE()
  SELECT RYN_L
  USE
ENDCASE

SELECT STOCK
USE

SELECT ST000
USE

IF dist_sto > 1
  USE (scr_path+"\BASE\DIVISION")
  SELECT 0
  USE (base_path+"\DIVISION") ALIAS DIV000
  SCAN
    DO C_B_Name WITH "DIV000", "DIVISION"
    SELECT DIV000
  ENDSCAN
  USE (scr_path+"\BASE\DEVELOP")
  SELECT DIVISION
  USE (base_path+"\DEVELOP") ALIAS DEV000
  SCAN FOR CODE # 1
    DO C_B_Name WITH "DEV000", "DEVELOP"
    SELECT DEV000
  ENDSCAN
  USE
  SELECT DEVELOP
  USE
ENDIF

DO Wt_Mess
DO Wt_Mess WITH "��������"
d_sav = SET("DEFAULT")+CURDIR()
s = "! "+LEFT(scr_path,2)
&s
s = "! CD "+SUBSTR(scr_path,3)+"\"
&s
s = "! "+SET("DEFAULT")+CURDIR()+"ARJ A -R -Y -V1400000 "+scr_path+"\STO_DIST > NUL"
&s
s = "! "+LEFT(d_sav,2)
&s
s = "! CD "+SUBSTR(d_sav,3)
&s
DO Wt_Mess
IF .NOT. File_O(base_path+"DIST_LOG.DBF")
  CREATE DBF (base_path+"DIST_LOG.DBF") ;
    ( SYS_NAME  C(10), ;
      CUS_CODE  N( 8), ;
      CUS_NAME  C(60), ;
      DIST_DAT  D,     ;
      WHO       N( 3), ;
      WHERE     C( 3), ;
      VERSION   C( 8), ;
      REG_NOM   C( 8), ;
      NOTE      C(60), ;
      MODE_STO  N( 1), ;
      MODE_CASH L,     ;
      MODE_VAL  C( 1)  )
ELSE
  USE (base_path+"DIST_LOG.DBF")
ENDIF

APPEND BLANK
REPLACE SYS_NAME  WITH "�����",  ;
        CUS_CODE  WITH dist_c_c, ;
        CUS_NAME  WITH dist_c_n, ;
        DIST_DAT  WITH DATE(),   ;
        WHO       WITH user,     ;
        WHERE     WITH arm,      ;
        VERSION   WITH dist_ver, ;
        REG_NOM   WITH dist_reg, ;
        NOTE      WITH dist_note,;
        MODE_STO  WITH dist_sto, ;
        MODE_CASH WITH dist_cash,;
        MODE_VAL  WITH IIF(dist_val=1, "�", "$")
USE
dist_path = "C:\"
dist_path = Int_dir(dist_path, "�㤠 �������� १����?")
dist_path = ALLTRIM(dist_path)
IF .NOT. EMPTY(dist_path)
  s = "! COPY "+scr_path+"\STO_DIST.A* "+dist_path+"STORE.* > NUL"
  &s
ENDIF
s = "! DELTREE /Y "+scr_path+" > NUL"
&s
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Fil_Blank    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ���������� ������.                           �
*�                                                                        �
*����������������������������������������������������������� 05/19/1999 ���
PROCEDURE Fil_Blank
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

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()
DO Use_Dummy
SELECT 0
DO Use_Link
SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
SELECT 0
USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*������������������������������������������������������������������Ŀ
*� ��[ ] ���� � ��室�� ⥪�⠬���������������������������������Ŀ �
*� � ....:....!....:....!....:....!....:....!....:....!....:....! � �
*� ���������������������������������������������������������������� �
*� ��[ ] ������ �� ���� �����������������������������������������Ŀ �
*� � ....:....!....:....!....:....!....:....!....:....!....:....! � �
*� ���������������������������������������������������������������� �
*� �č����������������������������������������������������������Ŀ �
*� � ( ) �����  ( ) ���쪮 ������  ( ) ���     [ ] ���. ����稥 � �
*� ���������������������������������������������������������������� �
*� �ďਬ�砭����������������������������������������������������Ŀ �
*� � ....:....!....:....!....:....!....:....!....:....!....:....! � �
*� ���������������������������������������������������������������� �
*�                 �Ă��������������������������Ŀ                 �
*�                 �      ( ) �㡫�  ( ) USD      �                 �
*�                 ��������������������������������                 �
*�                 < OK Ctrl-W > < �⪠������ Esc >                 �
*��������������������������������������������������������������������

PRIVATE ex, sw_path, sw_cus
ex = 1
sw_path = .F.
sw_cus  = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 19, 70, "�������� �����"
DO Sun_Bord WITH  2,  3,  4, 66, ""
DO Sun_Bord WITH  5,  3,  7, 66, ""
DO Sun_Bord WITH  8,  3, 10, 66, "�����������"
DO Sun_Bord WITH 11,  3, 13, 66, "�ਬ�砭��"
DO Sun_Bord WITH 14, 19, 16, 50, "�����"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@  2,  5 GET sw_path PICTURE "@*C ���� � ��室�� ⥪�⠬" VALID Ch_Path()
@  3,  5 GET dist_path WHEN .F.
@  5,  5 GET sw_cus PICTURE "@*C ������ �� ���� ������" VALID Ch_Cust()
@  6,  5 GET dist_c_n WHEN dist_c_c = 0
@  9,  5 GET dist_sto PICTURE "@*RH ����� ;���쪮 ������ ;���" VALID T_Cash()
@  9, 49 GET dist_cash PICTURE "@*C ���. ����稥" WHEN dist_c_c # 0 .AND. dist_sto > 1
@ 12,  5 GET dist_note
@ 15, 26 GET dist_val PICTURE "@*RH �㡫� ;USD" WHEN .NOT. FOUND("RYN_L_T") .OR. dist_c_c = 0
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1
  *
  * ��ࠡ��뢠�� �����
  
ENDIF
*--------------------------------------------------------------------------

CLOSE DATABASES
POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Ch_Path      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ���ࠪ⨢�� �롮� ���.                        �
*�                                                                        �
*����������������������������������������������������������� 19.05.1999 ���
PROCEDURE Ch_Path

PRIVATE w_path

w_path = ALLTRIM(dist_path)
w_path = Int_dir(w_path, "������ ����!")
IF .NOT. EMPTY(w_path)
  dist_path = PADR(w_path,60)
  SHOW GET dist_path
ENDIF
sw_path = .F.
SHOW GET sw_path

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Ch_Cust      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ���ࠪ⨢�� �롮� ������.                      �
*�                                                                        �
*����������������������������������������������������������� 19.05.1999 ���
PROCEDURE Ch_Cust

IF sw_cus
  SELECT ACCOUNT
  GO TOP
  IF .NOT. Custs("", .f.,.f.,.t.,"A")
    dist_c_c  = 0
    dist_c_n  = SPACE(60)
    sw_cus = .F.
    SHOW GET sw_cus
  ELSE
    dist_c_c  = ACCOUNT.CUS_CODE
    dist_c_n  = PADR(ACCOUNT.CUS_NAME, 60)
    IF SEEK(dist_c_c, "RYN_L_T")
      dist_val = IIF(RYN_L_T.VALUTA = "$", 2, 1)
      SHOW GET dist_val
    ENDIF
  ENDIF
ELSE
  dist_c_c  = 0
  dist_c_n  = SPACE(60)
ENDIF
SHOW GET dist_c_n
IF dist_c_c = 0
  dist_cash = .F.
  SHOW GET dist_cash
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� T_Cash      ���ࠡ��稪 ���३ ��ᨭ                       �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ���४�� ������.                                �
*�                                                                        �
*����������������������������������������������������������� 19.05.1999 ���
PROCEDURE T_Cash

IF dist_sto < 2
  dist_cash = .F.
  SHOW GET dist_cash
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� C_B_Name     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                  ����஢���� ����� �� ������ �����.                   �
*�                                                                        �
*����������������������������������������������������������� 21.05.1999 ���
PROCEDURE C_B_Name
PARAMETERS s, d
PRIVATE ALL
SELECT (s)
SCATTER MEMVAR
SELECT (d)
APPEND BLANK
GATHER MEMVAR
RETURN