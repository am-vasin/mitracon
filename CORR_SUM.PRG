*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Corr_Sum     ��о�｀砒┴ ��むォ ��瓱�           06.04.98 15:59:07 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        �皰�� �� 甎ユ. あ└キ��.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Corr_Sum

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
DO Use_Dummy
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
PRIVATE cnt
COUNT REST WHILE CUS_CODE < 0 FOR EMPTY(DATE_OFF) TO cnt

IF cnt = 0
  CLOSE DATABASES
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� 甎ユ��┘���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
GO TOP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�ョ�碎 ぎ�祠キ��"
ON PAD Choose OF (menu_name) ACTIVATE POPUP Choose

  DEFINE POPUP Choose MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Choose PROMPT "� 珮゛閻" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 1 OF Choose DO Sw_Mode WITH "Choose_R"

  DEFINE BAR  2 OF Choose PROMPT "� ぎ������" KEY Ctrl-A, "Ctrl+Enter"
  ON SELECTION BAR 2 OF Choose DO Sw_Mode WITH "Choose$"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, cnt)   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 24

DO D_Wins WITH ln, wd, "��ユ.��┘�硅", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "Choose$"

*
*    BROWSE - �キ�
*
    BROWSE KEY -9999999, -1 ;
           FIELDS F000 = LEFT(CUS_NAME,24):24:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose_R")
    ENDIF

  CASE what_do = "Choose_R"    && �ョ�碎 甎�瓷� � 珮゛閻

    DO P_Sp_Sum WITH CUS_CODE, "�"
    what_do = "List"

  CASE what_do = "Choose$"     && �ョ�碎 甎�瓷� � ぎ������

    DO P_Sp_Sum WITH CUS_CODE, "$"
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN
