*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Sto_Men      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ���甌� 甌矗磴�┴�� 瓷����.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 30.12.1999 様�
PROCEDURE Sto_Men

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
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS M_9C30 AGAIN
SELECT 0
USE (base_path+"STO_MEN") ORDER TAG ALL_NAMES ALIAS S_M_9C30 AGAIN
SET RELATION TO CODE INTO M_9C30
REPLACE ALL NAME   WITH M_9C30.NAME,   ;
            S_NAME WITH M_9C30.S_NAME, ;
            FAMILY WITH M_9C30.FAMILY
SELECT M_9C30
USE
SELECT S_M_9C30

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Work OF (menu_name) PROMPT "�� ��｀皀" KEY Ctrl-A, "Enter"
ON SELECTION PAD Work OF (menu_name) DO Sw_Mode WITH "Work"

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�....:....!....:....!....:....!....:....!....:....!....:....!.. .�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 64

DO D_Wins WITH ln, wd, "��矗磴�┴� 瓷����", 0, 0
what_do = "List"

PRIVATE tmpF, tmpN, tmpS, tmpC, www

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS S000 = FAMILY-(" "+NAME)-(" "+S_NAME):H="",  ;
                  S001 = IIF(WORK, "�", " "):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Work")
    ENDIF

  CASE what_do = "Work"    && �� ��｀皀

    REPLACE WORK WITH .NOT. WORK
    what_do = "List"

  CASE what_do = "Del"     && �����碎

    REPLACE CODE WITH 0, WORK WITH .F.
    what_do = "List"

  CASE what_do = "Add"     && ����※碎

    STORE "" TO tmpF, tmpN, tmpS, www
    tmpC = 0
    DO Ch_Pers WITH tmpC, www, tmpF, tmpN, tmpS
    IF tmpC # 0
      www = RECNO()
      LOCATE FOR tmpC = CODE
      IF FOUND()
        what_do = "List"
        LOOP
      ENDIF
      SET ORDER TO
      LOCATE FOR CODE = 0
      IF .NOT. FOUND()
        APPEND BLANK
      ENDIF
      REPLACE CODE   WITH tmpC, ;
              FAMILY WITH tmpF, ;
              NAME   WITH tmpN, ;
              S_NAME WITH tmpS, ;
              WORK   WITH .F.
      SET ORDER TO ALL_NAMES
    ENDIF
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
SELECT S_M_9C30
USE
SELECT (s_sav)

RETURN
