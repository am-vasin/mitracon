*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Pers_Acc     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 ��ユ���讚襯 ����罔� か� ���讌���皀�ォ.                 �
*�                            ���甌� ����罔�.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 17.10.2006 様�
PROCEDURE Pers_Acc

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
DO Tmp_Acc

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "���甌� ���讌���皀�ォ" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"


DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("ACC_NAME")

DO D_Wins WITH m.ln, m.wd, "��ユ���讚襯 ����罔� か� ���讌���皀�ォ", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS ACC_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    DO Acc_Pers WITH RECNO("TMP_ACC"), ALLTRIM(TMP_ACC.ACC_NAME)
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
PRIVATE tmpName
m.tmpName = DBF()
USE
DELETE FILE (m.tmpName)
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tmp_Acc      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             ���甌� ����罔�                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 17.10.2006 陳�
PROCEDURE Tmp_Acc

PRIVATE tmpName

m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	ACC_NAME	C(50)	)
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_ACC
APPEND BLANK
REPLACE ACC_NAME WITH "��痰祚 � ���祚�膈覓 罐��� �爬 �牀��Ε"
GO TOP

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Acc_Pers     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 ��ユ���讚襯 ����罔� か� ���讌���皀�ォ.                 �
*�                         ���甌� ���讌���皀�ォ.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 17.10.2006 様�
PROCEDURE Acc_Pers
PARAMETERS tmpCode, tmpName

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
USE (m.base_path+"ACC_PERS") SHARED AGAIN ORDER TAG PERS_CODE ALIAS ACC_61017
SELECT 0
USE (m.base_path+"USERS") SHARED AGAIN ORDER TAG USER_CODE ALIAS USER_61017
SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG NAME ALIAS PERS_61017
SET RELATION TO CODE INTO ACC_61017, CODE INTO USER_61017
SET FILTER TO FOUND("USER_61017")

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "��痰祚" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("FAMILY", "PERS_61017")+ ;
       FSIZE("NAME", "PERS_61017")+   ;
       FSIZE("S_NAME", "PERS_61017")+9

DO D_Wins WITH m.ln, m.wd, m.tmpName, 0, 0
@ 1, 2 SAY ;
       PADR("���┼��", FSIZE("FAMILY", "PERS_61017")+1, "�")+ ;
       PADR("���", FSIZE("NAME", "PERS_61017")+1, "�")+   ;
       PADR("�砒メ癶�", FSIZE("S_NAME", "PERS_61017")+1, "�")+"��痰祚"

m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PERS_61017.FAMILY:H="",  ;
                  PERS_61017.NAME:H="",    ;
                  PERS_61017.S_NAME:H="",  ;
                  TMP_ACC=SUBSTR(ACC_61017.ACCESS, m.tmpCode, 1):6, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    SELECT ACC_61017
    IF .NOT. FOUND("ACC_61017")
      APPEND BLANK
      REPLACE PERS_CODE WITH PERS_61017.CODE
    ENDIF
    IF EMPTY(SUBSTR(ACCESS, m.tmpCode, 1))
      REPLACE ACCESS WITH STUFF(ACCESS, m.tmpCode, 1, "�")
    ELSE
      REPLACE ACCESS WITH STUFF(ACCESS, m.tmpCode, 1, " ")
    ENDIF
    SELECT PERS_61017
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
USE IN ACC_61017
USE IN USER_61017
USE IN PERS_61017
SELECT (m.s_sav)

RETURN
