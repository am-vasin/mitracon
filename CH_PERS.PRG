*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ch_Pers      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     �襦�� 甌矗磴�┴� �爛く爬閧��.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 01.09.1999 様�
PROCEDURE Ch_Pers
PARAMETERS cd, fio, fm, nm, s_nm

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
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS PR_9901
SET RELATION TO DEP_NO INTO DP_9901
IF cd # 0 .AND. SEEK(cd)
  SET ORDER TO TAG NAME
ELSE
  SET ORDER TO TAG NAME
  GO TOP
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳�"
ON PAD Filter OF (menu_name) ACTIVATE POPUP Filter

  DEFINE POPUP Filter MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Filter PROMPT "�痰���※碎" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Filter DO Sw_Mode WITH "Dep"

  DEFINE BAR  2 OF Filter PROMPT "�腮痰�碎" KEY Ctrl-A, "Shift-Tab"
  ON SELECTION BAR 2 OF Filter DO Sw_Mode WITH "Clr"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, d_name
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("FAMILY")+FSIZE("NAME")+FSIZE("S_NAME")
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
    ON KEY LABEL Tab DO Sw_Mode WITH "Dep"
    ON KEY LABEL BackTab DO Sw_Mode WITH "Clr"
    DO Set_Key

*
*    BROWSE - �キ�
*
    IF "DEP" $ TAG()
      BROWSE KEY STR(ln,3) ;
             FIELDS F000 = FAMILY-(" "+NAME)-(" "+S_NAME):H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Foot()    ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS F000 = FAMILY-(" "+NAME)-(" "+S_NAME):H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Foot()    ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    cd = CODE
    IF TYPE("fio") = "C"
      fio = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
    ENDIF
    IF TYPE("fio") = "C"
      fio = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
    ENDIF
    IF TYPE("fm") = "C"
      fm = FAMILY
    ENDIF
    IF TYPE("nm") = "C"
      nm = NAME
    ENDIF
    IF TYPE("s_nm") = "C"
      s_nm = S_NAME
    ENDIF
    EXIT

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
