*������������������������������������������������������������������������ͻ
*� ���� Inc          ���ࠡ��稪 ���३ ��ᨭ           06.05.97 14:58:44 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                     �ணࠬ�� ��室������ ⮢��.                     �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Inc
PARAMETER nds_main

PRIVATE supl_c, supl_n, mss, d_names, d_codes, n, w, i000
PRIVATE nds_flt
nds_flt = nds_main

*
*   ���᮪ ࠧ����� ������������
*
USE (base_path+"DIVISION")
n = 0
SCAN
  n = n+1
  DIMENSION w(n,2)
  w(n,1) = B_NAME
  w(n,2) = CODE
ENDSCAN
= ASORT(w,1)

DIMENSION d_names(n), d_codes(n)
FOR i000 = 1 TO n
  d_codes(i000) = w(i000,2)
  d_names(i000) = w(i000,1)
ENDFOR
RELEASE w, i000

*
*   ���᮪ ���⠢騪��
*
USE (base_path+"CUSTOMER") ORDER TAG NAME
n = 0
SCAN FOR SUP # " "
  n = n+1
  DIMENSION supl_c(n), supl_n(n)
  supl_c(n) = CODE
  supl_n(n) = B_NAME
ENDSCAN

IF .NOT. File_O(log_path+"DUE_LOG.DBF")
  DELETE FILE (log_path+"DUE_LOG.CDX")
  CREATE DBF (log_path+"DUE_LOG") ;
   ( DATE_DUE D,    ;
     TIME_DUE C(8), ;
     STO_CODE N(7), ;
     GOT      L,    ;
     WHO      N(3), ;
     WHERE    C(3)  )
  FOR n = 1 TO 1000
    APPEND BLANK
  ENDFOR
  INDEX ON DTOS(DATE_DUE)+TIME_DUE TAG HRON
ENDIF

USE
DO Use_Dummy

IF File_O(base_path+"INC_LOG.DBF")
  SELECT 0
  USE (base_path+"INC_LOG") ALIAS LOG
ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE

SELECT 0
USE (base_path+"INC_TIT")

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG DUE_CODE

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG DUE_NAM ALIAS DUE_NAM AGAIN

SELECT 0
USE (base_path+"INC_LIST") ORDER TAG L_CODE
DELETE FILE (tmpo_path+"TMP_INC.DBF")
COPY STRUCTURE TO (tmpo_path+"TMP_INC")
SELECT 0
USE (tmpo_path+"TMP_INC") EXCLUSIVE

SELECT 0
USE (base_path+"INC_DUE") ORDER TAG L_CODE

SELECT 0
USE (base_path+"INC_NEW") ORDER TAG L_NAME
DELETE FILE (tmpo_path+"TMP_NEW.DBF")
COPY STRUCTURE TO (tmpo_path+"TMP_NEW")
SELECT 0
USE (tmpo_path+"TMP_NEW") EXCLUSIVE
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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Title_Pad OF (menu_name) PROMPT "���������"
ON PAD Title_Pad OF (menu_name) ACTIVATE POPUP Title_Pad

DEFINE POPUP Title_Pad MARGIN SHADOW COLOR SCHEME 4

DEFINE BAR 1 OF Title_Pad PROMPT "���४��" KEY Ctrl-A, "Enter"
ON SELECTION BAR 1 OF Title_Pad Do Sw_Mode WITH "Corr"

DEFINE BAR 2 OF Title_Pad PROMPT "��������" KEY Ctrl-A, "Ins"
ON SELECTION BAR 2 OF Title_Pad Do Sw_Mode WITH "Add"

DEFINE BAR 3 OF Title_Pad PROMPT "�������" KEY Ctrl-A, "Del"
ON SELECTION BAR 3 OF Title_Pad Do Sw_Mode WITH "Del"

DEFINE BAR 4 OF Title_Pad PROMPT "����" KEY Ctrl-A, "F2"
ON SELECTION BAR 4 OF Title_Pad Do Sw_Mode WITH "Price"

DEFINE BAR 5 OF Title_Pad PROMPT "�������� ���" KEY Ctrl-A, "Shift-Tab"
ON SELECTION BAR 5 OF Title_Pad Do Sw_Mode WITH "GTD"

DEFINE PAD Note OF (menu_name) PROMPT "�ਬ." KEY Ctrl-A, "F4"
ON SELECTION PAD Note OF (menu_name) Do Sw_Mode WITH "Note"

DEFINE PAD File OF (menu_name) PROMPT "����" KEY Ctrl-A, "F3"
ON SELECTION PAD File OF (menu_name) Do Sw_Mode WITH "File"

DEFINE PAD Act OF (menu_name) PROMPT "���." KEY Ctrl-A, "Tab"
ON SELECTION PAD Act OF (menu_name) Do Sw_Mode WITH "Act"

DEFINE PAD Exit OF (menu_name) PROMPT "���." KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-10   && ������⢮ ������� ��ப BROWSE
wd = 55

DO D_Wins WITH ln, wd, "����⮢�� �ਥ���� ��⮢", 0, 0
what_do = "List"
@ 1,  2 SAY "��"
@ 1,  7 SAY " ���⠢騪 "
@ 1, 24 SAY " ����. N "
@ 1, 35 SAY " ��� "
@ 1, 45 SAY "����"
@ 1, 51 SAY "%���"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    SELECT INC_TIT
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter   KEYBOARD CHR(23)
    ON KEY LABEL Ins     Do Sw_Mode WITH "Add"
    ON KEY LABEL Del     Do Sw_Mode WITH "Del"
    ON KEY LABEL F2      Do Sw_Mode WITH "Price"
    ON KEY LABEL F4      Do Sw_Mode WITH "Note"
    ON KEY LABEL F3      Do Sw_Mode WITH "File"
    ON KEY LABEL Tab     Do Sw_Mode WITH "Act"
    ON KEY LABEL BackTab Do Sw_Mode WITH "GTD"

*
*    BROWSE - ����
*
    BROWSE FOR SUP_CODE # 0   ;
           FIELDS INC_TIT.FIRM:H="":3,        ;
                  INC_TIT.SUP_NAME:H="":16,   ;
                  INC_TIT.INC_DOC:H="":10,    ;
                  INC_TIT.INC_DATE:H="",      ;
                  F000 = PADC(IS_PRICE,4),    ;
                  NDS_:H="",  ;
                  F001 = IIF(BUY=1, "�","�"), ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND REST NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
*           WHEN Draw_Foot() ;
*
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && ���४�� �ਥ����� ���

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      nds_flt = NDS_
      DO Inc_List
    ENDIF
    what_do = "List"

  CASE what_do = "Add"     && ���������� ����⮢��

    DO Inc_Tit
    DO Set_Ctry WITH STR(RECNO(), 5)
    what_do = "List"

  CASE what_do = "GTD"     && ����⠮��� ���

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO List_GTD
    ENDIF
    what_do = "List"

  CASE what_do = "Del"     && �������� ����⮢��

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"  ����⢨⥫쭮 㤠����? [ �� ] [ ��� ]"
      mss(3) = ""
      IF Out_Mess(7, "mss") = 1
        DO Del_Tit
      ENDIF
    ENDIF

    what_do = "List"

  CASE what_do = "Note"    && �ਬ�砭��

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO ActNote
    ENDIF
    what_do = "List"

  CASE what_do = "Price"   && ��⠭���� ०��� 業

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO Prices
    ENDIF
    what_do = "List"

  CASE what_do = "File"   && ���� ������ �� 䠩��

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO Ext_Inc WITH RECNO()
    ENDIF
    what_do = "List"

  CASE what_do = "Act"     && ��室

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      nds_flt = NDS_
      DO Prep_Act
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
DELETE FILE (tmpo_path+"TMP_INC.DBF")
DELETE FILE (tmpo_path+"TMP_NEW.DBF")

RELEASE MENU (menu_name) EXTENDED

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Draw_Foot    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE Draw_Foot
ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
@ WROWS()-2, 2 SAY "����� ��� "+GTD_NUM
@ WROWS()-2, COL()+1 SAY "��࠭� "+LEFT(COUNTRY,13)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                           ��⠭���� ०���.                            �
*�                                                                        �
*����������������������������������������������������� 13.05.97 21:43:41 ͼ
PROCEDURE Prices
*
*  ��६���� ���ﭨ� ��� ������樨
*

IF .NOT. Get_L()
  RETURN
ENDIF
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

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*���������������������������������������Ŀ
*� �� ���� ���������������������������Ŀ �
*� � ( ) ���   ( ) �㡫�   ( ) ������� � �
*� ������������������������������������� �
*� [ ] ����.           ��業� ��� 999.9 �
*�   < OK Ctrl-W > < �⪠������ Esc >    �
*�����������������������������������������

PRIVATE ex, v_tmp, nds_w, tmpEnab
ex    = 1
nds_w = NDS_
m.tmpEnab = ENAB_PRICE
IF IS_PRICE = " "
  v_tmp = 1
ELSE
  v_tmp = IIF(IS_PRICE = "$", 3, 2)
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 43, "�����"
DO Sun_Bord WITH 2, 3, 4, 39, " ���� "
*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 5 GET v_tmp PICTURE "@*RH ���  ;�㡫�  ;�������" VALID Set_NDS()
@ 5, 3 GET tmpEnab PICTURE "@*C ����. 業"
@ 5,23 SAY "��業�" GET nds_w PICTURE "@Z 999.9" WHEN v_tmp = 2
@ 6, 5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1
  SELECT INC_LIST
  nds_flt = nds_w
  DO CASE
  CASE v_tmp = 1
    SEEK STR(RECNO("INC_TIT"),5)
    REPLACE REST WHILE LIST_NO = STR(RECNO("INC_TIT"),5) PRICE WITH 0
    SELECT INC_TIT
    REPLACE IS_PRICE WITH " ", NDS_ WITH nds_w, ENAB_PRICE WITH m.tmpEnab
  CASE v_tmp = 2
    IF INC_TIT.IS_PRICE = "$"
      SEEK STR(RECNO("INC_TIT"),5)
      REPLACE REST WHILE LIST_NO = STR(RECNO("INC_TIT"),5) PRICE WITH Clc_Rub(PRICE, kurs_b)
    ENDIF
    IF INC_TIT.IS_PRICE = "�"
      SEEK STR(RECNO("INC_TIT"),5)
      REPLACE REST WHILE LIST_NO = STR(RECNO("INC_TIT"),5) PRICE WITH ROUND_6(PRICE)
    ENDIF
    SELECT INC_TIT
    REPLACE IS_PRICE WITH "�", NDS_ WITH nds_w, ENAB_PRICE WITH m.tmpEnab
  CASE v_tmp = 3
    IF INC_TIT.IS_PRICE = "�"
      SEEK STR(RECNO("INC_TIT"),5)
      REPLACE REST WHILE LIST_NO = STR(RECNO("INC_TIT"),5) PRICE WITH ROUND(PRICE/kurs_b,2)
    ENDIF
    SELECT INC_TIT
    REPLACE IS_PRICE WITH "$", NDS_ WITH nds_w, ENAB_PRICE WITH m.tmpEnab
  ENDCASE
ENDIF
SELECT INC_TIT
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

REPLACE INC_TIT.LOCKED WITH "***"

RETURN
*������������������������������������������������������������������������ͻ
*�                                                                        �
*�            ��⠭���� ��� � ᮮ⢥��⢨� � ⨯�� 業.                   �
*�                                                                        �
*����������������������������������������������������� 07.05.97 11:22:51 ͼ
PROCEDURE Set_NDS

DO CASE
CASE v_tmp = 1
  nds_w = 0
CASE v_tmp = 2
  nds_w = nds_flt
CASE v_tmp = 3
  nds_w = 0
ENDCASE
SHOW GET nds_w
RETURN .T.

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                           ��墠��� ᯨ᮪!                            �
*�                                                                        �
*����������������������������������������������������� 07.05.97 11:22:51 ͼ
PROCEDURE Get_L

PRIVATE mss, sel_sav, arm_name

IF INC_TIT.LOCKED # "***"
  sel_sav = SELECT()
  SELECT 0
  USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS ARM000 AGAIN
  IF SEEK(INC_TIT.LOCKED)
    arm_name = ALLTRIM(ARM_N)
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"��������! ��� ᯨ᮪ � ࠡ��, ��� "+'"'+arm_name+'"!'
    mss(3) = CHR(0)+"  ��� ����㯨�? [ ����� ࠡ���� ] [ ���������! ]"
    mss(4) = ""
    IF Out_Mess (7, "mss") = 1
      USE
      SELECT (sel_sav)
      RETURN .F.
    ELSE
      IF INC_TIT.LOCKED # arm
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"����� � �㦨� ᯨ᪮� ������㯭� !"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        USE
        SELECT (sel_sav)
        RETURN .F.
      ENDIF
    ENDIF
  ENDIF
  USE
  SELECT (sel_sav)
ENDIF

REPLACE INC_TIT.LOCKED WITH arm

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� ActNote      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      �ਬ�砭�� � �ਥ����� ����                       �
*�                                                                        �
*����������������������������������������������������������� 10/26/2006 ���
PROCEDURE ActNote
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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*������������������������������������������������������������������������Ŀ
*�....:....!....:....!....:....!....:....!....:....!....:....!....:....!..�
*�                    < OK Ctrl-W > < �⪠������ Esc >                    �
*��������������������������������������������������������������������������

PRIVATE ex, tmpNote
m.ex = 1
m.tmpNote = NOTE
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 5, 76, "�ਬ�砭�� � �ਥ����� ����"

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 2, 2 GET m.tmpNote
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF m.ex = 1
  *
  * ��ࠡ��뢠�� �����
  REPLACE NOTE WITH m.tmpNote
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN