*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� OperStru     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�           ��爼�牀���┘ 痰珮�矣琺 ��Гカ�� ��瘁��諷 ��ム�罔�.           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 10/22/1998 様�
PROCEDURE OperStru

PRIVATE fl_level, s_sav, what_do, titles
fl_level = 0
DIMENSION titles(1)   && ��� 轤�� ��瘁│ - �� 艪� ┃��, ��� 痰オ ������→��
titles = "�甅�↓襯 ��Гカ�"

s_sav = SELECT()
DO Use_Dummy
SELECT 0
USE (base_path+"MM_TYPES") ORDER TAG GROUP ALIAS O8A22 AGAIN  && ｯム�罔�
SELECT 0

USE (base_path+"OPERSTRU") ORDER TAG CODE ALIAS C8A22 AGAIN   && �痰�膈┴ ��ぎ�
SELECT 0
USE (base_path+"OPERSTRU") ORDER TAG PREV_LINE ALIAS V8A22 AGAIN && ��＝癶キ�� ��玻オ罔�

what_do = "Level"
DO WHILE .T.
  DO CASE
  CASE what_do = "Level"
    what_do = Level()
  CASE what_do = "TermList"
    DO TermList
    what_do = "Level"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

USE
SELECT C8A22
USE
SELECT O8A22
USE

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Level        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�               ��玻オ罔� 甎�瓷� �祗�皰� 皀�竕ィ� 窶�↓�.                �
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

DEFINE PAD Corr OF (menu_name) PROMPT "��玻." KEY Ctrl-A, "Tab"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "���." KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "����." KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

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
    ON KEY LABEL Tab   DO Sw_Mode WITH "Corr"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"

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

  CASE what_do = "Corr"    && ��玻オ皋牀��碎

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Modi_Line
    ENDIF
    what_do = "List"

  CASE what_do = "Add"     && ����※碎

    DO Modi_Line WITH .T.
    what_do = "List"

  CASE what_do = "Del"     && �����碎

    IF EOF() .OR. BOF()
      what_do = "List"
      LOOP
    ENDIF

    IF IS_TERM
      IF SEEK(STR(CODE), "O8A22")
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"�皰� ��Гカ 甌ぅ爨�� ��ム�罔�!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        LOOP
      ENDIF
    ELSE
      w = CODE
      rc_sav = RECNO()
      IF SEEK(w)
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"�皰� ��Гカ 甌ぅ爨�� ��む�Гカ�!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        GO rc_sav
        LOOP
      ENDIF
      GO rc_sav
    ENDIF
    REPLACE CODE WITH 0
    SEEK fl_level
    what_do = "List"

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
*�   蹍罐ゃ�� Modi_Line    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     ��玻オ罔�/ぎ��←キ┘ ��Гカ�.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10/22/1998 陳�
PROCEDURE Modi_Line
PARAMETERS new

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ������〓� ��Гカ� 陳陳陳陳陳� �
*� �   ....:....!....:....!....   � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�[ ] ���跏� �� 甌ぅ爨�� ��む�Гカ���
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, nm, sw
IF new
  nm = SPACE(24)
  sw = .F.
ELSE
  nm = NAME
  sw = IS_TERM
ENDIF
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 38, IIF(new, "��Г��┘", "��玻オ罔�")+" ��Гカ�"
DO Sun_Bord WITH  2,  3,  4, 34, " ������〓� ��Гカ� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 7 GET nm
@ 5, 2 GET sw PICTURE "@*C ���跏� �� 甌ぅ爨�� ��む�Гカ��"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_B()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  IF new
    GO TOP IN C8A22
    IF C8A22.CODE = 0 .AND. .NOT. BOF("C8A22") .AND. .NOT. EOF("C8A22")
      GO RECNO("C8A22")
    ELSE
      APPEND BLANK
    ENDIF
    GO BOTTOM IN C8A22
    REPLACE CODE WITH MAX(C8A22.CODE+1, 1)
  ENDIF
  REPLACE NAME      WITH nm,  ;
          PREV_LINE WITH fl_level, ;
          IS_TERM   WITH sw

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_B        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                蹍▲爲� ���※�讚�痰� ������キ�� ゛����.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10/22/1998 陳�
PROCEDURE Tst_B

PRIVATE r_sav, mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��Б��┘ ��Гカ� �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF .NOT. new .AND. IS_TERM # sw
  IF .NOT. sw   && ���� �� 皀爼!
    IF SEEK(STR(CODE), "O8A22")
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"��Гカ 甌ぅ爨�� ��ム�罔� � ィ� 痰�矣� �� ��Ε� °碎 ├�キキ!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      RETURN .F.
    ENDIF
  ELSE
    rc_sav = RECNO()
    IF SEEK(CODE)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"��Гカ 甌ぅ爨�� ��む�Гカ� � ィ� 痰�矣� �� ��Ε� °碎 ├�キキ!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      GO rc_sav
      RETURN .F.
    ENDIF
    GO rc_sav
  ENDIF
ENDIF

RETURN .T.


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TermList     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ���甌� ��ム�罔� � ��Гカ�.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.10.1998 陳�
PROCEDURE TermList
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
SET RELATION TO GROUP INTO C8A22
SET ORDER TO NAME

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Inc OF (menu_name) PROMPT "��メ皋 � ��Гカ" KEY Ctrl-A, "Enter"
ON SELECTION PAD Inc OF (menu_name) DO Sw_Mode WITH "Inc"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 54

DO D_Wins WITH ln, wd, ALLTRIM(V8A22.NAME), 0, 0
@ 1, 4 SAY " ｯム�罔� 陳陳陳陳陳陳陳陳陳 ��Гカ "
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter DO Inc_Oper

*
*    BROWSE - �キ�
*
    BROWSE FIELDS F000 = IIF(GROUP = V8A22.CODE, "�", " "):1:H="",   ;
           NAME:H="", ;
           F001 = IIF(M_RUB, "�磧", "� �"):H="":3,  ;
           F002 = IIF(M_DIR=0, "踸絎�" ,"��痳��"):6:H="", ;
           F003 = IIF(GROUP = 0 .OR. .NOT. FOUND("C8A22"), "", C8A22.NAME):24:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"

  CASE what_do = "Inc"       && �痰�※碎!

    DO Inc_Oper
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
SET RELATION TO
SET ORDER TO GROUP
SELECT V8A22
fl_level = PREV_LINE
DIMENSION titles(ALEN(titles)-1)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Inc_Oper     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �痰�→� ��ム�罔� � ��Гカ.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.10.1998 陳�
PROCEDURE Inc_Oper

REPLACE GROUP WITH V8A22.CODE
RETURN
