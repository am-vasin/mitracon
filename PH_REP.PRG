*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Ph_Rep       ��о�｀砒┴ ��むォ ��瓱�           30.01.97 00:32:26 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   �カヤ���覃 甎��〓膈┴ �爛く爬閧��.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Ph_Rep

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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

*DEFINE PAD Seek1 OF (menu_name) PROMPT "���瓷 �� ���. 瓱��." KEY Ctrl-A, "F7"
*ON SELECTION PAD Seek1 OF (menu_name) Do Sw_Mode WITH "Seek1"

*DEFINE PAD Seek2 OF (menu_name) PROMPT "����. ���瓷" KEY Ctrl-A, "Ctrl-F7"
*ON SELECTION PAD Seek2 OF (menu_name) Do Sw_Mode WITH "Seek2"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

USE DEPS ORDER TAG CODE

DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") ORDER TAG NAME
SET RELATION TO DEP_NO INTO DEPS

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, pic
ln = 100   && ����腑痰〓 ※え�諷 痰牀� BROWSE
*
*敖� ��矗磴�┴ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳� ���. 皀�. 陳陳陳 ��.�.�
*外旭葦旭旭屋旭葦旭旭屋旭� 旭旭碓旭芦旭旭碓 旭旭碓旭芦旭旭碓 旭旭碓�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖陳陳 �發カ 陳陳陳陳陳陳 ç��お� 陳陳陳陳� ������� 陳陳陳陳陳陳朕
*�     旭旭碓旭芦旭旭碓   旭旭碓旭芦旭旭碓   旭旭碓旭芦旭旭碓      �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

pic = FSIZE("NAME")+FSIZE("FAMILY")+1
wd = pic+FSIZE("W_PHONE1")+FSIZE("W_PHONE2")+2
pic = "@S"+ALLTRIM(STR(pic))

DO D_Wins WITH ln, wd, "���甌� 甌矗磴�┴��", 0, 1
@ 1, 4 SAY " ��矗磴�┴ "
@ 1, 44 SAY " ���. 皀�. "
@ 1, 61 SAY " ��.�."
@ WROWS()-3,  7 SAY " �發カ "
@ WROWS()-3, 26 SAY " ç��お� "
@ WROWS()-3, 45 SAY " ������� "

what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    DO Key_Seek
*    ON KEY LABEL F7      Do Sw_Mode WITH "Seek1"
*    ON KEY LABEL Ctrl+F7 Do Sw_Mode WITH "Seek2"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS F001 = ALLTRIM(FAMILY)+" "+ALLTRIM(NAME):H="":P=pic,  ;
                  W_PHONE1:H="",              ;
                  W_PHONE2:H="",              ;
                  DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_Lin()                    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = "exit"
    ENDIF

  CASE what_do = "Seek1"    && ���瓷 �� ���. 瓱�〓���

    what_do = "list"

  CASE what_do = "Seek2"    && ���皀�痰�覃 ���瓷

    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      �襤�� ┃筮爼�罔����� 痰牀��                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 30.01.97 01:02:08 夕
PROCEDURE Show_Lin

ACTIVATE WINDOW (win_name+"_M") BOTTOM

@ WROWS()-2,  7 SAY DEPS.B_NAME COLOR SCHEME 1
@ WROWS()-2, 26 SAY PLACE COLOR SCHEME 1
@ WROWS()-2, 45 SAY ROOM COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
