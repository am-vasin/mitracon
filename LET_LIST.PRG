*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Let_List     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�         ��玻オ罔� 甎�瓷� ぎ�竕キ�諷 � ������� ��｀腑�� �メ��.          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 15.09.2006 様�
PROCEDURE Let_List
PARAMETERS prmArm	&& ��� ��｀腑�� �メ��, メ�� �竅皰, 皰 皀�竕ゥ.

PRIVATE tmpArm
m.tmpArm = IIF(EMPTY(m.prmArm), m.arm, m.prmArm)

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
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS ARM_6914 ORDER TAG ARM_C
SEEK m.tmpArm
SELECT 0
USE (m.base_path+"LET_LIST") SHARED AGAIN ALIAS LL_6914 ORDER TAG USER_CODE
SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ALIAS PR_6914 ORDER TAG NAME
SET RELATION TO STR(CODE)+m.tmpArm INTO LL_6914


*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "��оヨ�碎/���爛皋碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-5   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 46

DO D_Wins WITH m.ln, m.wd, "���甌� ぎ�竕キ�諷 � ������� ��｀腑�� �メ��", 1, 0
m.what_do = "List"
@ 1, 2 SAY "��｀腑� �メ皰 "+ARM_6914.ARM_N
@ 2, 2 SAY ;
"���矗磴�┴陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳���痰."
*....:....!....:....!....:....!....:....! 

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS ;
    	   tmpFIO = ALLTRIM(PR_6914.FAMILY)+" "+LEFT(PR_6914.NAME, 1)+" "+LEFT(PR_6914.S_NAME, 1):40:H="", ;
    	   tmpMrk = IIF(FOUND("LL_6914"), "  �  ", "     "):5:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    IF .NOT. EOF() .AND. .NOT. BOF()
      SELECT LL_6914
      IF FOUND("LL_6914")
        REPLACE USER_CODE WITH 0, ARM_CODE WITH ""
      ELSE
        GO TOP
        IF .NOT. EMPTY(USER_CODE) .OR. EOF()
          APPEND BLANK
        ENDIF
        REPLACE USER_CODE WITH PR_6914.CODE, ARM_CODE WITH ARM_6914.ARM_C
      ENDIF
      SELECT PR_6914
    ENDIF
    m.what_do = "List"

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
USE IN LL_6914
USE IN PR_6914
USE IN ARM_6914
SELECT (m.s_sav)

RETURN
