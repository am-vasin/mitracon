*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ch_O_T       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           �襦�� 皋�� ������                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 16.11.2007 様�
PROCEDURE Ch_O_T
PARAMETERS srcType

PRIVATE tmpType, s_sav, tmpName, rc

m.tmpType = PADL(ALLTRIM(m.srcType), 10)
m.s_sav = SELECT()
SELECT 0
USE m.base_path+"ORD_TYPE" SHARED AGAIN ALIAS TMP_OT
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(CODE C(10), NAME C(FSIZE("ORD_NAME", "TMP_OT")))
USE (m.tmpo_path+m.tmpName) ALIAS TMP_LIST
m.rc = 1
APPEND BLANK
REPLACE NAME WITH "�≒���皋腑瓷┤ �襦�� 皋�� ������"
APPEND BLANK
REPLACE CODE WITH "         0", ;
        NAME WITH "�┓ ������ �� 祠�������"
IF m.tmpType = CODE
  m.rc = RECNO()
ENDIF
SELECT TMP_OT
SCAN
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE CODE WITH STR(TMP_OT.CODE), ;
          NAME WITH TMP_OT.ORD_NAME
  IF m.tmpType = CODE
    m.rc = RECNO()
  ENDIF
  SELECT TMP_OT
ENDSCAN

USE

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
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
SELECT 0
DO Use_Dummy
SELECT 0
SELECT TMP_LIST
GO m.rc

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("NAME")

DO D_Wins WITH m.ln, m.wd, "���Θ皀 皋� ������", 0, 0
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
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    m.tmpType = CODE
    EXIT
*    m.what_do = "List"

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
m.tmpName = DBF()
USE
DELETE FILE (m.tmpName)
SELECT (m.s_sav)

RETURN m.tmpType
