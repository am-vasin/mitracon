*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ch_Lett      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �襦�� ����皰→� ��瘡��                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 10.04.2006 様�
PROCEDURE Ch_Lett
PARAMETERS prmSubj, prmText

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
USE (base_path+"MAILTXT") SHARED AGAIN ALIAS ML_6410 ORDER TAG WHO

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Modify OF (menu_name) PROMPT "��玻." KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Delete OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, tmpRec, blnk0
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("SUBJECT")
SCATTER TO blnk0 BLANK

DO D_Wins WITH m.ln, m.wd, "����皰→� ��瓮�", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Modify"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Delete"

*
*    BROWSE - �キ�
*
    BROWSE KEY m.user REST ;
           FIELDS SUBJECT,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && �襦��

    IF TYPE("m.prmSubj") = "C"
      m.prmSubj = ALLTRIM(SUBJECT)
    ENDIF
    IF TYPE("m.prmText") = "C"
      SET ORDER TO TAG LETTID
      m.ln = LETTID
      SCAN REST WHILE m.ln = LETTID
        m.prmText = m.prmText+TEXT
      ENDSCAN
      m.prmText = ALLTRIM(m.prmText)
    ENDIF
    EXIT

  CASE m.what_do = "Modify"    && ��玻オ罔�

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO ModiLett WITH LETTID
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Add"       && ����←キ┘

    m.tmpRec = ModiLett()
    IF .NOT. EMPTY(m.tmpRec)
      GO m.tmpRec
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Delete"    && ����キ┘

    IF .NOT. BOF() .AND. .NOT. EOF()
      m.tmpRec = LETTID
      SET ORDER TO TAG LETTID
      DO WHILE SEEK(STR(m.tmpRec))
        GATHER FROM blnk0
      ENDDO
      SET ORDER TO TAG WHO
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

USE

SELECT (m.s_sav)

RETURN
