*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Lst_Pers     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�            �ェ��赳ム��皋↓覃 �襦�� 甌矗磴�┴� �爛く爬閧��.             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 01.09.1999 様�
PROCEDURE Lst_Pers
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
USE (base_path+"DEPS") ORDER TAG CODE AGAIN ALIAS DP_9901

SELECT 0
USE (base_path+"PERSONS") ORDER TAG NAME AGAIN ALIAS PR_9901
SET RELATION TO DEP_NO INTO DP_9901, CODE INTO (m.tmpAlias)

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�皙モ��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD All OF (menu_name) PROMPT "�皙モ�碎 ≡�" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD All OF (menu_name) DO Sw_Mode WITH "All"

DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳�"
ON PAD Filter OF (menu_name) ACTIVATE POPUP Filter

  DEFINE POPUP Filter MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Filter PROMPT "�痰���※碎" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Filter DO Sw_Mode WITH "Dep"

  DEFINE BAR  2 OF Filter PROMPT "�腮痰�碎" KEY Ctrl-A, "Shift-Tab"
  ON SELECTION BAR 2 OF Filter DO Sw_Mode WITH "Clr"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, d_name, rc_sav
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("FAMILY")+FSIZE("NAME")+FSIZE("S_NAME")+2
d_name = PADR("�瓮 �發カ�", FSIZE("B_NAME","DP_9901"))

DO D_Wins WITH ln, wd, "��矗磴�┴� �爛く爬閧��", 1, 1
@ 1, 3 SAY "�┼赳�"
@ WROWS()-3, 3 SAY " �發カ "
what_do = "List"
ln = 0

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 10 SAY d_name
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "All"
    ON KEY LABEL Tab DO Sw_Mode WITH "Dep"
    ON KEY LABEL BackTab DO Sw_Mode WITH "Clr"
    DO Set_Key

*
*    BROWSE - �キ�
*
    IF "DEP" $ TAG()
      BROWSE KEY STR(m.ln,3) ;
             FIELDS &tmpAlias..MARK:H="", ;
                    F000 = FAMILY-(" "+NAME)-(" "+S_NAME):H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Foot()    ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS &tmpAlias..MARK:H="", ;
                    F000 = FAMILY-(" "+NAME)-(" "+S_NAME):H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Foot()    ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �皙モ�碎

    IF .NOT. EOF() .AND. .NOT. BOF()
      SELECT (m.tmpAlias)
      IF .NOT. SEEK(PR_9901.CODE)
        APPEND BLANK
        REPLACE CODE WITH PR_9901.CODE
        IF FSIZE("NAME") # 0
          REPLACE NAME WITH ALLTRIM(PR_9901.FAMILY)+" "+LEFT(PR_9901.NAME, 1)+" "+LEFT(PR_9901.S_NAME, 1)
        ENDIF
      ENDIF
      REPLACE MARK WITH IIF(EMPTY(MARK), "�", " ")
      SELECT PR_9901
    ENDIF

    what_do = "List"

  CASE what_do = "All"    && �皙モ�碎 ≡�

    m.rc_sav = RECNO()
    IF "DEP" $ TAG()
      SEEK STR(m.ln,3)
      SCAN REST WHILE DEP_NO = m.ln
        SELECT (m.tmpAlias)
        IF .NOT. SEEK(PR_9901.CODE)
          APPEND BLANK
          REPLACE CODE WITH PR_9901.CODE
          IF FSIZE("NAME") # 0
            REPLACE NAME WITH ALLTRIM(PR_9901.FAMILY)+" "+LEFT(PR_9901.NAME, 1)+" "+LEFT(PR_9901.S_NAME, 1)
          ENDIF
        ENDIF
        REPLACE MARK WITH "�"
        SELECT PR_9901
      ENDSCAN
    ELSE
      SCAN
        SELECT (m.tmpAlias)
        IF .NOT. SEEK(PR_9901.CODE)
          APPEND BLANK
          REPLACE CODE WITH PR_9901.CODE
          IF FSIZE("NAME") # 0
            REPLACE NAME WITH ALLTRIM(PR_9901.FAMILY)+" "+LEFT(PR_9901.NAME, 1)+" "+LEFT(PR_9901.S_NAME, 1)
          ENDIF
        ENDIF
        REPLACE MARK WITH "�"
        SELECT PR_9901
      ENDSCAN
    ENDIF
    IF BETWEEN(m.rc_sav, 1, RECCOUNT())
      GO m.rc_sav
    ENDIF

    what_do = "List"

  CASE what_do = "Clr"        && �腮痰�碎 筥�赳�

    ln = 0
    SET ORDER TO TAG NAME
    d_name = PADR("�瓮 �發カ�", FSIZE("B_NAME","DP_9901"))
    what_do = "List"

  CASE what_do = "Dep"        && �痰���※碎 筥�赳�

    DO Ch_Dep WITH ln, wd, d_name
    IF ln # 0
      SET ORDER TO TAG DEP_NAME
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
USE
SELECT DP_9901
USE
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*

@ WROWS()-2, 3 SAY DP_9901.B_NAME

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Key      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    �痰���→� �÷�｀砒┴�� か� ＜��.                    �
*�                                                                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01.09.1999 陳�
PROCEDURE Set_Key

PRIVATE shb_l, shb_u, s, s1, i
shb_u = "ABCDEFGHIJKLMNOPQASTUVWXYZ��������������������������������"
shb_l = SYS(15, lwr, shb_u)

FOR i = 1 TO LEN(shb_l)
  s =  SUBSTR(shb_l,i,1)
  s1 = '"'+s+'"'
  ON KEY LABEL (s) DO Seek_ WITH &s1
ENDFOR
FOR i = 1 TO LEN(shb_u)
  s =  SUBSTR(shb_u,i,1)
  s1 = '"'+SUBSTR(shb_l,i,1)+'"'
  ON KEY LABEL (s) DO Seek_ WITH &s1
ENDFOR
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         ���瓷 �� �ム〓� ＜�▲.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01.09.1999 陳�
PROCEDURE Seek_
PARAMETERS lett

PRIVATE r_sv

r_sv = RECNO()

IF "DEP" $ TAG()
  SEEK STR(ln,3)+lett
ELSE
  SEEK lett
ENDIF
IF .NOT. FOUND()
  ?? CHR(7)
  IF BETWEEN(r_sv,1,RECCOUNT())
    GO r_sv
  ENDIF
ENDIF

RETURN
