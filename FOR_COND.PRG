*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� For_Cond     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ��玻オ罔� 甎�瓷� 痰����珥�諷 籥��.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 26.03.2000 様�
PROCEDURE For_Cond
PARAMETERS cd, nm
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
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

s_sav = SELECT()
SELECT 0
DO Use_Dummy
SELECT 0
IF EMPTY(cd)
  USE (base_path+"FOR_COND") ORDER TAG NAME ALIAS CND_0326 AGAIN
ELSE
  USE (base_path+"FOR_COND") ORDER TAG CODE ALIAS CND_0326 AGAIN
  IF SEEK(cd)
    SET ORDER TO TAG NAME
  ELSE
    SET ORDER TO TAG NAME
    GO TOP
  ENDIF
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Modify OF (menu_name) PROMPT "��玻オ皋牀��碎" KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "�襦ム�皀 皀�痰", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Modify"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && ��玻オ罔�

    IF .NOT. BOF() .AND. .NOT. EOF()
      cd = CODE
      IF TYPE("nm") = "C"
        nm = NAME
      ENDIF
      EXIT
    ELSE
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"��� �� ├ 腑�� �襦���碎!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      what_do = "List"
    ENDIF

  CASE what_do = "Modify"    && ��玻オ罔�

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Modi_Item
    ENDIF
    what_do = "List"

  CASE what_do = "Add"       && ����←キ┘

    DO Modi_Item WITH .T.
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
USE
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Modi_Item    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��玻オ罔�/ぎ��←キ┘ ����瓱.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.03.2000 陳�
PROCEDURE Modi_Item
PARAMETERS new

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

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                     �
*�       �オ痰 ....:....!....:.        �
*�                                     �
*�  < OK Ctrl-W > < �皖���碎瘴 Esc >   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, nm_w
ex = 1
nm_w = IIF(new, SPACE(16), NAME)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 41, IIF(new, "����←キ┘", "��玻オ罔�")+" 皀�痰�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 9 SAY "�オ痰" GET nm_w
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  IF new
    SET ORDER TO TAG CODE
    GO BOTT
    IF EOF() .OR. BOF()
      ex = "    1"
    ELSE
      ex = VAL(ALLTRIM(CODE))
      ex = STR(ex+1,5)
    ENDIF
    APPEND BLANK
    REPLACE CODE WITH ex
    SET ORDER TO TAG NAME
  ENDIF
  REPLACE NAME WITH nm_w
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    ���※�讚�痰� ������キ�� ゛����.                     �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.03.2000 陳�
PROCEDURE Tst_Blank

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�オ痰 �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
