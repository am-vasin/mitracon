*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ch_Oper      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        �襦�� ��瘁�〓� ��ム�罔�.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 10/23/1998 様�
PROCEDURE Ch_Oper
PARAMETERS o_code, o_name

PRIVATE w_code, w_name
PRIVATE fl_level, s_sav, what_do, titles
fl_level = 0
DIMENSION titles(1)   && ��� 轤�� ��瘁│ - �� 艪� ┃��, ��� 痰オ ������→��
titles = "�甅�↓襯 ��Гカ�"

s_sav = SELECT()
DO Use_Dummy

SELECT 0
USE (base_path+"OPERSTRU") ORDER TAG CODE ALIAS C8A22 AGAIN   && �痰�膈┴ ��ぎ�

SELECT 0
USE (base_path+"OPERSTRU") ORDER TAG PREV_LINE ALIAS V8A22 AGAIN && ��＝癶キ�� ��玻オ罔�

SELECT 0
USE (base_path+"MM_TYPES") ORDER TAG CODE ALIAS O8A22 AGAIN  && ｯム�罔�

what_do = "Level"
IF SEEK(o_code)               && �矗�━ 痰オ ��む�Гカ��
  PRIVATE tmp_arr, ok, nxt_w
  DIMENSION tmp_arr(1)
  nxt_w = GROUP
  ok = nxt_w # 0
  DO WHILE nxt_w # 0
    IF .NOT. SEEK(nxt_w, "C8A22")
      ok = .F.
      EXIT
    ENDIF
    DIMENSION tmp_arr(ALEN(tmp_arr)+1)
    tmp_arr(ALEN(tmp_arr)) = ALLTRIM(C8A22.NAME)
    nxt_w = C8A22.PREV_LINE
  ENDDO
  IF ok
    o_name = O8A22.NAME
    DIMENSION titles(ALEN(tmp_arr))
    FOR ok = 2 TO ALEN(tmp_arr)
      titles(ok) = tmp_arr(ALEN(tmp_arr)-ok+2)
    ENDFOR
    what_do = "TermList"
    fl_level = O8A22.GROUP
  ENDIF
ENDIF
SET ORDER TO TAG GROUP
SELECT V8A22

DO WHILE .T.
  DO CASE
  CASE what_do = "Level"
    what_do = Level()
  CASE what_do = "TermList"
    what_do = TermList()
    IF what_do = "Exit"
      o_code = O8A22.CODE
      o_name = O8A22.NAME
    ENDIF
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

SELECT V8A22
USE
SELECT C8A22
USE
SELECT O8A22
USE
SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Level        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�               �襦�� ├ 甎�瓷� ��Гカ�� 皀�竕ィ� 窶�↓�.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10/22/1998 陳�
PROCEDURE Level

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
PRIVATE ret_v
*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Next OF (menu_name) PROMPT "��イ. 窶�▲��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Next OF (menu_name) DO Sw_Mode WITH "Next"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, mss, rc_sav, w
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, titles(ALEN(titles)), 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE KEY fl_level FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Next")
    ENDIF

  CASE what_do = "Next"    && �� 甄イ竡薑� 窶�▲��

    IF .NOT. BOF() .AND. .NOT. EOF()
      fl_level = CODE
      DIMENSION titles(ALEN(titles)+1)
      titles(ALEN(titles)) = ALLTRIM(NAME)
      ret_v =IIF(IS_TERM, "TermList", "Level")
      EXIT
    ELSE
      what_do = "List"
    ENDIF

  OTHERWISE

    IF fl_level = 0
      ret_v = "Exit"
    ELSE
      = SEEK(fl_level,"C8A22")
      fl_level = C8A22.PREV_LINE
      DIMENSION titles(ALEN(titles)-1)
      ret_v = RECNO("C8A22")
      GO ret_v
      ret_v = "Level"
    ENDIF
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

RETURN ret_v

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TermList     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ���甌� ��ム�罔� � ��Гカ�.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.10.1998 陳�
PROCEDURE TermList
PRIVATE ret_v
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

SELECT O8A22

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "��メ皋 � ��Гカ" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 27

DO D_Wins WITH ln, wd, ALLTRIM(V8A22.NAME), 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE KEY STR(fl_level) ;
           FIELDS NAME:H="", ;
                  F001 = IIF(M_RUB, "�磧", "� �"):3:H="", ;
                  F002 = IIF(M_DIR = 0, "踸絎�", "��痳��"):H="":6, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")

  CASE what_do = "Choose"       && �襦��!

    ret_v = "Exit"
    EXIT

  OTHERWISE

    ret_v = "Level"
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
SELECT V8A22
fl_level = PREV_LINE
DIMENSION titles(ALEN(titles)-1)

RETURN ret_v
