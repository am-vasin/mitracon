*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Lst_Arm      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 �ェ��赳ム��皋↓覃 甎�甌� ��｀腮� �メ�                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 21.10.2005 様�
PROCEDURE Lst_Arm
PARAMETERS tmpAlias


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
USE base_path+"ARM_LST" SHARED AGAIN ORDER TAG NAME ALIAS ARM_5A21
SET RELATION TO ARM_C INTO (m.tmpAlias)

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎/�皖���碎瘴" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD All OF (menu_name) PROMPT "�襦��碎 ≡�" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD All OF (menu_name) DO Sw_Mode WITH "All"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, rc_sav
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("ARM_N")+2

DO D_Wins WITH m.ln, m.wd, "���甌� ��｀腮� �メ�", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "All"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS &tmpAlias..MARK:H="", ;
                  ARM_N:H="",           ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && 蹍甃�矗 甎�瓷�

   SELECT (m.tmpAlias)
   IF .NOT. SEEK(ARM_5A21.ARM_C)
     APPEND BLANK
     REPLACE CODE WITH ARM_5A21.ARM_C
     IF FSIZE("NAME") # 0
       REPLACE NAME WITH ALLTRIM(ARM_5A21.ARM_N)
     ENDIF
   ENDIF
   REPLACE MARK WITH IIF(EMPTY(MARK), "�", " ")
   SELECT ARM_5A21
   what_do = "List"

  CASE what_do = "All"    && �皙モ�碎 ≡�

    m.rc_sav = RECNO()
    SCAN
      SELECT (m.tmpAlias)
      IF .NOT. SEEK(ARM_5A21.ARM_C)
        APPEND BLANK
        REPLACE CODE WITH ARM_5A21.ARM_C
        IF FSIZE("NAME") # 0
          REPLACE NAME WITH ARM_5A21.ARM_N
        ENDIF
      ENDIF
      REPLACE MARK WITH "�"
      SELECT ARM_5A21
    ENDSCAN
    IF BETWEEN(m.rc_sav, 1, RECCOUNT())
      GO m.rc_sav
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
USE IN ARM_5A21
SELECT (m.s_sav)

RETURN

