*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Ord_List     ��о�｀砒┴ ��むォ ��瓱�           02.02.98 09:34:56 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�            �ョ�碎 � ����瘡 � ���� ��錢�� �� ��痰�→� 皰����.           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Ord_List
PARAMETER manCode
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
USE (base_path+"CUSTOMER") ORDER TAG CODE
SELECT 0
USE (base_path+"ORD_TIT")
SET RELATION TO SUP_CODE INTO CUSTOMER
IF TYPE("m.manCode") ="N"
  SET FILTER TO WHO_ORD = m.manCode
ENDIF
GO BOTT
IF EOF() .OR. BOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き�� ��錢��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT CUSTOMER
  USE
  SELECT DUMMY
  USE
  RETURN
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Send OF (menu_name) PROMPT "�ムイ���" KEY Ctrl-A, "Enter"
ON SELECTION PAD Send OF (menu_name) DO Sw_Mode WITH "Send"

DEFINE PAD URM OF (menu_name) PROMPT "���" KEY Ctrl-A, "Home"
ON SELECTION PAD URM OF (menu_name) DO Sw_Mode WITH "URM"

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD View OF (menu_name) PROMPT "蹍甃�矗" KEY Ctrl-A, "Tab"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("DATE") = "ON", 75, 73)

DO D_Wins WITH ln, wd, "��錢��", 0, 0

IF SET("DATE") = "ON"
  @ 1, 2 SAY "�爼����ム ぎ�.陳陳�����陳陳���痰�∫┴陳陳陳陳踸�ョ��┘"
ELSE
  @ 1, 2 SAY "�爼����ム ぎ�.陳�����陳陳��痰�∫┴陳陳陳陳踸�ョ��┘"
ENDIF

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+P DO Sw_Mode WITH "Print"
    ON KEY LABEL Tab    DO Sw_Mode WITH "View"
    ON KEY LABEL Home   DO Sw_Mode WITH "URM"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS FIRM:H="", ORD_NUM:H="", ORD_DATE:H="",     ;
                  CUSTOMER.B_NAME:H="", NOTE:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Send")
    ENDIF

  CASE what_do = "Send"    && ���� か� �ムイ�腮

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO S_Ord WITH FIRM, ORD_NUM, ORD_DATE, ""
    ENDIF
    what_do = "List"

  CASE what_do = "Print"   && �ョ�碎

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Ord WITH FIRM, ORD_NUM, ORD_DATE
    ENDIF
    what_do = "List"

  CASE what_do = "View"    && 蹍甃�矗

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO V_Order WITH FIRM, ORD_DATE, ORD_NUM
    ENDIF
    what_do = "List"

  CASE what_do = "URM"    && ���

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO To_URM_O WITH FIRM, ORD_DATE, ORD_NUM
    ENDIF
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
