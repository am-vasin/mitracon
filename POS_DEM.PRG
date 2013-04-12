*������������������������������������������������������������������������ͻ
*� ���� Pos_Dem      ���ࠡ��稪 ���३ ��ᨭ           19.04.97 12:07:40 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                     ��ࠡ�⪠ ��� ��� ����樨.                      �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Pos_Dem
PRIVATE mss

*
*    �����⮢�� 䠩���.
*
IF .NOT. FILE_O(tmpo_path+"L_DEM.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ����⮢�� ���..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ELSE
  PRIVATE c000
  = ADIR(c000,tmpo_path+"L_DEM.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ����⮢�� ��� �� "+DTOC(c000(1,3))+", ࠡ�⠥�? [ �� ] [ ��� ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 2
    RETURN
  ENDIF
ENDIF

DO Use_Dummy

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"STOCK")

DELETE FILE (tmpo_path+"L_DEM0.DBF")
DELETE FILE (tmpo_path+"L_DEM0.CDX")
DELETE FILE (tmpo_path+"T_DEM0.DBF")
DELETE FILE (tmpo_path+"T_DEM0.CDX")

SET EXCLUSIVE ON

CREATE DBF (tmpo_path+"T_DEM0.DBF")  ;
   ( CODE     N(FSIZE("CODE","STOCK")),       ;
     PREFIX   C(FSIZE("PREFIX","STOCK")),     ;
     NAME     C(FSIZE("NAME","STOCK")),       ;
     PRO_CODE N(FSIZE("PRO_CODE","STOCK")),   ;
     PRODUCER C(FSIZE("PRODUCER","STOCK")),   ;
     CUS_QNT  N(5),                           ;
     QNT      N( 6),                          ;
     PRICE_R  N( 8,2),                        ;
     SUM_D    N(12,2)   )
  INDEX ON SUM_D TAG SUM_D DESCENDING

SELECT 0
CREATE DBF (tmpo_path+"L_DEM0.DBF")  ;
   ( CUS_CODE N(FSIZE("CUS_CODE","ACCOUNT")), ;
     CODE     N(FSIZE("CODE","STOCK")),       ;
     PREFIX   C(FSIZE("PREFIX","STOCK")),     ;
     NAME     C(FSIZE("NAME","STOCK")),       ;
     PRO_CODE N(FSIZE("PRO_CODE","STOCK")),   ;
     PRODUCER C(FSIZE("PRODUCER","STOCK")),   ;
     QNT      N( 6),                          ;
     PRICE_R  N( 8,2),                        ;
     SUM_D    N(12,2)   )
  INDEX ON STR(CODE, 7)+STR(SUM_D,12,2) TAG CODE DESCENDING

SET EXCLUSIVE OFF

APPEND FROM (tmpo_path+"L_DEM.DBF")
REPLACE ALL SUM_D WITH QNT*PRICE_R
GO TOP

PRIVATE cd_sav, q_sav, qc_sav, prf_sav, nm_sav, prc_sav, prn_sav, price_sv
cd_sav   = CODE
prf_sav  = PREFIX
nm_sav   = NAME
prc_sav  = PRO_CODE
prn_sav  = PRODUCER
price_sv = PRICE_R
qc_sav   = 0
q_sav    = 0
IF RECCOUNT() # 0
  SCAN
    IF cd_sav # CODE
      SELECT T_DEM0
      APPEND BLANK
      REPLACE CODE     WITH cd_sav,   ;
              PREFIX   WITH prf_sav,  ;
              NAME     WITH nm_sav,   ;
              PRO_CODE WITH prc_sav,  ;
              PRODUCER WITH prn_sav,  ;
              CUS_QNT  WITH qc_sav,   ;
              QNT      WITH q_sav,    ;
              PRICE_R  WITH price_sv, ;
              SUM_D    WITH q_sav*price_sv
      SELECT L_DEM0
      cd_sav   = CODE
      prf_sav  = PREFIX
      nm_sav   = NAME
      prc_sav  = PRO_CODE
      prn_sav  = PRODUCER
      price_sv = PRICE_R
      qc_sav   = 0
      q_sav    = 0
    ENDIF
    qc_sav = qc_sav+1
    q_sav  = q_sav+QNT

  ENDSCAN

  * ... � ��᫥���� ������...
  SELECT T_DEM0
  APPEND BLANK
  REPLACE CODE     WITH cd_sav,   ;
          PREFIX   WITH prf_sav,  ;
          NAME     WITH nm_sav,   ;
          PRO_CODE WITH prc_sav,  ;
          PRODUCER WITH prn_sav,  ;
          CUS_QNT  WITH qc_sav,   ;
          QNT      WITH q_sav,    ;
          PRICE_R  WITH price_sv, ;
          SUM_D    WITH q_sav*price_sv

  DO View_Dem

ELSE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���� ����..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

CLOSE DATABASES
*DELETE FILE (tmpo_path+"L_DEM0.DBF")
*DELETE FILE (tmpo_path+"L_DEM0.CDX")
*DELETE FILE (tmpo_path+"T_DEM0.DBF")
*DELETE FILE (tmpo_path+"T_DEM0.CDX")

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                        ��ᬮ�� ᯨ᪠ ����権.                        �
*�                                                                        �
*����������������������������������������������������� 19.04.97 12:54:45 ͼ
PROCEDURE View_Dem
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN && COLOR SCHEME 4

DEFINE PAD C_View OF (menu_name) PROMPT "�������" KEY Ctrl-A, "Enter"
ON SELECTION PAD C_View OF (menu_name) Do Sw_Mode WITH "C_View"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ����� ࠧ��஢ ����
*
SELECT T_DEM0
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("PREFIX")+FSIZE("NAME")+FSIZE("PRODUCER")+  ;
     FSIZE("CUS_QNT")+FSIZE("QNT")+FSIZE("SUM_D")+5

DO D_Wins WITH ln, wd, "���� �� ������", 0, 0

@ 1, 2 SAY PADC("���������",FSIZE("PREFIX")+FSIZE("NAME")+FSIZE("PRODUCER")+3,"�")
@ ROW(), COL() SAY PADC("��-����",FSIZE("CUS_QNT")+1,"�")
@ ROW(), COL() SAY PADC("���-���",FSIZE("QNT")+1,"�")
@ ROW(), COL() SAY PADC("��đ㬬�",FSIZE("SUM_D"),"�")
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="",  ;
           CUS_QNT:H="", QNT, SUM_D:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "exit", "C_View")
    ENDIF

  CASE what_do = "C_View"    && ��ᬮ�� ᯨ� �����⮢

    DO View_Cus
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
RELEASE MENU (menu_name) EXTENDED

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                        ��ᬮ�� ᯨ᪠ �����⮢.                       �
*�                                                                        �
*����������������������������������������������������� 19.04.97 12:54:45 ͼ
PROCEDURE View_Cus
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN && COLOR SCHEME 4

DEFINE PAD Report OF (menu_name) PROMPT "��ࠢ��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Report OF (menu_name) Do Sw_Mode WITH "Report"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ����� ࠧ��஢ ����
*
SELECT L_DEM0
SET RELATION TO CUS_CODE INTO ACCOUNT
PRIVATE ln, wd
ln = MIN(15, WROWS(""))   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("CUS_NAME","ACCOUNT")+FSIZE("QNT")+FSIZE("SUM_D")+2

DO D_Wins WITH ln, wd, ALLTRIM(T_DEM0.PREFIX)+" "+  ;
                    ALLTRIM(T_DEM0.NAME)+" "+ALLTRIM(T_DEM0.PRODUCER), 0, 0

@ 1, 2 SAY PADC("�������������������",FSIZE("CUS_NAME","ACCOUNT")+1,"�")
@ ROW(), COL() SAY PADC("���-���",FSIZE("QNT")+1,"�")
@ ROW(), COL() SAY PADC("��đ㬬�",FSIZE("SUM_D"),"�")
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE KEY STR(T_DEM0.CODE,7)   ;
           FIELDS ACCOUNT.CUS_NAME:H="",  ;
           QNT, SUM_D:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Report")
    ENDIF

  CASE what_do = "Report"    && ��ᬮ�� ᯨ᪠
    DO Move_S WITH L_DEM0.CODE
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

SELECT T_DEM0

RETURN