*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                  ��ࠢ��: ���죨 �� ࠡ��� �����.                   �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Mon_Arms

PRIVATE ln, wd, rc_sav, arm_sav, sw_flt

HIDE MENU Main_M
SELECT 0

DEFINE MENU List_A IN SCREEN COLOR SCHEME 4
DEFINE PAD List_C OF List_A PROMPT "��ᬮ�� ��." KEY Ctrl-A, "F1"
DEFINE PAD List_D OF List_A PROMPT "��ᬮ�� ����." KEY Ctrl-A, "F2"
DEFINE PAD Filter OF List_A PROMPT "������" KEY Ctrl-A, "F6"
DEFINE PAD Total OF List_A PROMPT "�㬬�" KEY Ctrl-A, "F3"
DEFINE PAD Exit   OF List_A PROMPT "��室" KEY Ctrl-A, "Esc"

ON SELECTION PAD List_C OF List_A  DO Ch_Mode WITH "List_A","show_mr"
ON SELECTION PAD List_D OF List_A  DO Ch_Mode WITH "List_A","show_md"
ON SELECTION PAD Filter OF List_A  DO Ch_Mode WITH "List_A","Filter"
ON SELECTION PAD Total OF List_A  DO Ch_Mode WITH "List_A","Total"
ON SELECTION PAD Exit   OF List_A  DO Ch_Mode WITH "List_A","���!"

USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM

SELECT 0    
USE (base_path+"KASSA") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG NAME
wd = FSIZE("ARM_N","ARM_LST")+FSIZE("KASSA","ARM_LST")+FSIZE("KASSA_USD","ARM_LST")+2
ln = WROWS("")-7
PRIVATE tt, ft, tt_m
tt_m = "���᮪ ࠡ. ����"
DIMENSION tt(1), ft(1)
tt(1) = ""
ft(1) = ""

DO Def_Win WITH "Arm_M", "Arm_E", "Arm_I", ln, wd, tt_m, tt, ft

@ 1, 2 SAY "Đ��. ����������Ď��⮪ (�)�Ď��⮪ ($)"

PRIVATE what_do

what_do = "arm_lst"

rc_sav = RECNO()
rst = .F.
sw_flt = .F.
DO WHILE .T.
  DO CASE
  CASE what_do = "arm_lst"  && ���᮪ ࠡ��� ����
    DO Use_Dummy
    IF USED("ARM_LST")
      SELECT ARM_LST
    ELSE
      SELECT 0
      USE (base_path+"ARM_LST") ORDER TAG NAME
    ENDIF
    GO rc_sav
    IF sw_flt
      SET FILTER TO KASSA # 0 .OR. KASSA_USD # 0
    ENDIF
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("���� F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU List_A
*    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F10 ACTIVATE MENU List_A
    ON KEY LABEL F6  DO Ch_Mode WITH "List_A","Filter"
    ON KEY LABEL F1  DO Ch_Mode WITH "List_A","show_mr"
    ON KEY LABEL F2  DO Ch_Mode WITH "List_A","show_md"
    ON KEY LABEL F3  DO Ch_Mode WITH "List_A","Total"
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "List_A", "Arm_"
    ACTIVATE WINDOW Arm_E
    BROWSE FIELDS ARM_N:H="",   ;
                  KASSA:H="":P="@Z ", ;
                  KASSA_USD:H="":P="@Z ", ;
                  DUMMY.F:H=""  ;
           FREEZE DUMMY.F NOMODIFY NOCLEAR WINDOW Arm_I IN WINDOW Arm_E
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "arm_lst", "Filter")
    ENDIF

    HIDE MENU List_A
    arm_sav = ARM_C
    rc_sav = RECNO()
    CLOSE DATABASES
*    IF READKEY() % 256 # 12 .AND. what_do = "arm_lst"
*      what_do = "show_mr"
*    ENDIF
    IF what_do = "arm_lst"
      what_do = "���!"
    ENDIF
  CASE what_do = "show_md"   && ������� �� ࠡ�祬 ����
    DO Mon_Hist WITH arm_sav, "$"
    what_do = "arm_lst"
  CASE what_do = "show_mr"   && �㡫� �� ࠡ�祬 ����
    DO Mon_Hist WITH arm_sav, " �"
    what_do = "arm_lst"
  CASE what_do = "Filter"   && ������
    sw_flt = .NOT. sw_flt
    what_do = "arm_lst"
  CASE what_do = "Total"    && �㬬� ����筮��
    DO Sum_Tot
    what_do = "arm_lst"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

CLOSE DATA
RELEASE WINDOWS Arm_M, Arm_E, Arm_I
RELEASE MENU List_A

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�        �ணࠬ�� - ��ࠡ��稪 ��誨 ��� ᯨ᪮�.                      �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE M_Proc_1
PARAMETER menu_nm, wind_nm
PRIVATE mc, mr, cr

*
*  �ᨭ�஭��� ����
*
IF MENU() == UPPER(menu_nm)
  RETURN
ENDIF

IF MROW("") = 0
  IF .NOT. MENU() == UPPER(menu_nm)
    ACTIVATE MENU (menu_nm)
  ENDIF
  RETURN
ENDIF

*
*  ���� BROWSE
*
mc = MCOL(wind_nm+"E")
mr = MROW(wind_nm+"E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  �᭮���� ����
*
mc = MCOL(wind_nm+"M")
mr = MROW("")
IF mc >= 0
  IF mr = WLROW(wind_nm+"E")-1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WLROW(wind_nm+"E")+WROWS(wind_nm+"E")
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Sum_Tot      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         �㬬�ୠ� ����筮���.                          �
*�                                                                        �
*����������������������������������������������������������� 04/28/2006 ���
PROCEDURE Sum_Tot
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
PRIVATE s_sav, s_RUR, s_USD

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()
m.s_sav = SELECT()
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS ARM_6428
SUM KASSA, KASSA_USD TO m.s_RUR, m.s_USD
USE


*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*�������������������������Ŀ
*� �㬬� ��. 999999999.99 �
*�                         �
*� �㬬� USD  9999999.9999 �
*�                         �
*�        < OK >           �
*���������������������������

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 29, "�㬬�ୠ� ����筮���"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 2, 3 SAY "�㬬� ��." GET m.s_RUR PICTURE "999999999.99"
@ 4, 3 SAY "�㬬� USD " GET m.s_USD PICTURE "9999999.9999"
CLEAR GETS
@ WROWS()-2, FLOOR(WCOLS()/2-3) GET m.ex PICTURE "@*HT \ OK "

READ CYCLE

IF m.ex = 1
  *
  * ��ࠡ��뢠�� �����
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN