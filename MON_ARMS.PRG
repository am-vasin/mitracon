*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  ����→�: ぅ�譯� �� ��｀腮� �メ���.                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Mon_Arms

PRIVATE ln, wd, rc_sav, arm_sav, sw_flt

HIDE MENU Main_M
SELECT 0

DEFINE MENU List_A IN SCREEN COLOR SCHEME 4
DEFINE PAD List_C OF List_A PROMPT "蹍甃�矗 珮�." KEY Ctrl-A, "F1"
DEFINE PAD List_D OF List_A PROMPT "蹍甃�矗 ぎ��." KEY Ctrl-A, "F2"
DEFINE PAD Filter OF List_A PROMPT "�┼赳�" KEY Ctrl-A, "F6"
DEFINE PAD Total OF List_A PROMPT "�祠��" KEY Ctrl-A, "F3"
DEFINE PAD Exit   OF List_A PROMPT "�諷��" KEY Ctrl-A, "Esc"

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
tt_m = "���甌� ���. �メ�"
DIMENSION tt(1), ft(1)
tt(1) = ""
ft(1) = ""

DO Def_Win WITH "Arm_M", "Arm_E", "Arm_I", ln, wd, tt_m, tt, ft

@ 1, 2 SAY "����. �メ皰陳陳陳��痰�皰� (�)陳�痰�皰� ($)"

PRIVATE what_do

what_do = "arm_lst"

rc_sav = RECNO()
rst = .F.
sw_flt = .F.
DO WHILE .T.
  DO CASE
  CASE what_do = "arm_lst"  && ���甌� ��｀腮� �メ�
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
    @ 0, 0 SAY PADL("�キ� F10 ",WCOLS()) COLOR SCHEME 13
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
  CASE what_do = "show_md"   && �����琺 �� ��｀腑� �メ皀
    DO Mon_Hist WITH arm_sav, "$"
    what_do = "arm_lst"
  CASE what_do = "show_mr"   && �磧�� �� ��｀腑� �メ皀
    DO Mon_Hist WITH arm_sav, " �"
    what_do = "arm_lst"
  CASE what_do = "Filter"   && �┼赳�
    sw_flt = .NOT. sw_flt
    what_do = "arm_lst"
  CASE what_do = "Total"    && �祠�� ����膈�痰�
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

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�        蹍������ - �÷�｀砒┴ �謌�� か� 甎�瓷��.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE M_Proc_1
PARAMETER menu_nm, wind_nm
PRIVATE mc, mr, cr

*
*  �瓱�縲����� �キ�
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
*  ｪ�� BROWSE
*
mc = MCOL(wind_nm+"E")
mr = MROW(wind_nm+"E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  �甅�↓�� ����
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Sum_Tot      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         �祠��爿�� ����膈�痰�.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04/28/2006 陳�
PROCEDURE Sum_Tot
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
PRIVATE s_sav, s_RUR, s_USD

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
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
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳�
*� �祠�� 珮�. 999999999.99 �
*�                         �
*� �祠�� USD  9999999.9999 �
*�                         �
*�        < OK >           �
*青陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 29, "�祠��爿�� ����膈�痰�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 3 SAY "�祠�� 珮�." GET m.s_RUR PICTURE "999999999.99"
@ 4, 3 SAY "�祠�� USD " GET m.s_USD PICTURE "9999999.9999"
CLEAR GETS
@ WROWS()-2, FLOOR(WCOLS()/2-3) GET m.ex PICTURE "@*HT \ OK "

READ CYCLE

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
