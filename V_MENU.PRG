*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� V_Menu       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             蹍甃�矗 �キ�.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 19.02.2002 様�
PROCEDURE V_Menu

PRIVATE tmpLevel, tmpName, tmpKey, tmpTitle

USE (base_path+"S_RIGHTS")
SET FILTER TO .NOT. EMPTY(NAME)

SELECT 0
USE (base_path+"MENU_LST")

tmpTitle = "�甅�↓�� �キ�"
tmpLevel = ""
tmpName = SYS(3)
SELECT 0
CREATE DBF (tmpo_path+tmpName) (LINK N(10))
USE (tmpo_path+tmpName) ALIAS TMP_MENU EXCLUSIVE

DO Fil_Tmp

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
SELECT TMP_MENU

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�ム�竄赱�" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("P_PROMPT", "MENU_LST")+2

DO D_Wins WITH ln, wd, "�キ� 瓱痰ガ�", 1, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 0 SAY PADC(tmpTitle, WCOLS())
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    

*
*    BROWSE - �キ�
*
    BROWSE FIELDS MENU_LST.P_PROMPT:H="",  ;
                  P000 = IIF(EMPTY(MENU_LST.COMM_STR), "", " "), ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    IF EMPTY(MENU_LST.COMM_STR)
      tmpLevel = STR(MENU_LST.LEVEL1, 2)
      IF .NOT. EMPTY(MENU_LST.LEVEL2)
        tmpLevel = tmpLevel+STR(MENU_LST.LEVEL2, 2)
      ENDIF
      DO Fil_Tmp
    ELSE
      DO V_Rights
    ENDIF
    what_do = "List"

  OTHERWISE

    IF LEN(tmpLevel) = 0
      EXIT
    ELSE
      tmpLevel = LEFT(tmpLevel, LEN(tmpLevel)-2)
      DO Fil_Tmp
    ENDIF
    what_do = "List"

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT TMP_MENU
tmpName = DBF()
USE
DELETE FILE (tmpName)
SELECT MENU_LST
USE
SELECT S_RIGHTS
USE

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fil_Tmp      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ������キ┘ ∇ガキ���� �����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 19.02.2002 陳�
PROCEDURE Fil_Tmp

SELECT TMP_MENU
SET RELATION TO
ZAP

SELECT MENU_LST
SET ORDER TO TAG LEVELS
SEEK tmpLevel
IF LEN(tmpLevel) # 0
  tmpTitle = ALLTRIM(P_PROMPT)
  SKIP
ELSE
  tmpTitle = "�甅�↓�� �キ�"
ENDIF

SCAN REST WHILE LEFT(STR(MENU_LST.LEVEL1,2)+STR(MENU_LST.LEVEL2,2)+  ;
                     STR(MENU_LST.LEVEL3,2), LEN(tmpLevel)) = tmpLevel
  IF LEN(tmpLevel) = 0 .AND. MENU_LST.LEVEL2 # 0
    LOOP
  ENDIF
  IF LEN(tmpLevel)  = 2 .AND. MENU_LST.LEVEL3 # 0
    LOOP
  ENDIF
  SELECT TMP_MENU
  APPEND BLANK
  REPLACE LINK WITH RECNO("MENU_LST")
  SELECT MENU_LST
ENDSCAN
SET ORDER TO

SELECT TMP_MENU
SET RELATION TO LINK INTO MENU_LST
GO TOP

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� V_Rights     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            ����罔� ぎ痰祚�.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.02.2002 陳�
PROCEDURE V_Rights

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
SELECT S_RIGHTS
GO TOP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "��痰祚" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Users OF (menu_name) PROMPT "���讌���皀��" KEY Ctrl-A, "Tab"
ON SELECTION PAD Users OF (menu_name) DO Sw_Mode WITH "Users"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("NAME")+2

DO D_Wins WITH ln, wd, ALLTRIM(MENU_LST.P_PROMPT), 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Users"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS S000 = IIF(EMPTY(SUBSTR(MENU_LST.ACC_LABELS, RIGHT, 1)), "�", " "):1:H="", ;
                  NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && ��キ� ぎ痰祚�
    
    SELECT MENU_LST
    IF EMPTY(SUBSTR(MENU_LST.ACC_LABELS, S_RIGHTS.RIGHT, 1))
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS, S_RIGHTS.RIGHT, 1, "*")
    ELSE
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS, S_RIGHTS.RIGHT, 1, " ")
    ENDIF
    SELECT S_RIGHTS

    what_do = "List"

  CASE what_do = "Users"    && ���讌���皀��
    
    DO R_Users WITH S_RIGHTS.RIGHT
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
SELECT (s_sav)

RETURN
