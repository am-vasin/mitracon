*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Mss_List     ��о�｀砒┴ ��むォ ��瓱�           04.09.98 13:32:12 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                ���甌� ��甄��┤ ��о�｀砒┴� �� 皀��...                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Mss_List
PARAMETER topic_c, topic_n, p_who

PRIVATE s_sav, p_w

p_w = ""
IF TYPE("p_who") = "N"
  p_w = IIF(p_who > 0, STR(p_who), "")
ENDIF

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Add OF (menu_name) PROMPT "����※碎 甌�♂キ┘" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD View OF (menu_name) PROMPT "蹍甃�矗"
ON PAD View OF (menu_name) ACTIVATE POPUP View

  DEFINE POPUP View MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF View PROMPT "���♂キ��" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 1 OF View DO Sw_Mode WITH "Mess"

  DEFINE BAR  2 OF View PROMPT "�癶モ�" KEY Ctrl-A, "Ctrl+Enter"
  ON SELECTION BAR 2 OF View DO Sw_Mode WITH "Answer"

  IF EMPTY(p_w)
    DEFINE BAR  3 OF View PROMPT "�〓� �癶モ�" KEY Ctrl-A, "Tab"
    ON SELECTION BAR 3 OF View DO Sw_Mode WITH "Reply"
  ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "�ム�竄赱�" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
s_sav = SELECT()

DO Use_Dummy
SELECT 0
USE (base_path+"MSS_LIST") ORDER TAG TOPIC

*
*   ��瘍モ ��Кム�� ����
*

PRIVATE ln, wd
ln = 12   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("MSS_TITLE")+IIF(SET("CENTURY") = "ON", 32, 28)

DO D_Wins WITH ln, wd, ALLTRIM(topic_n), 0, IIF(EMPTY(p_w), 1, 0)
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "Answer"
    ON KEY LABEL Ins        DO Sw_Mode WITH "Add"
    IF EMPTY(p_w)
      ON KEY LABEL Tab DO Sw_Mode WITH "Reply"
    ENDIF

*
*    BROWSE - �キ�
*
    BROWSE KEY p_w+topic_c ;
           FIELDS MSS_DATE:H="",   ;
                  MSS_TIME:H="",   ;
                  MSS_TITLE:H="",  ;
                  ANSW_DATE:H="",  ;
                  ANSW_TIME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Mess")
    ENDIF

  CASE what_do = "Mess"    && 蹍甃�矗 甌�♂キ��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO V_Mess
    ENDIF
    what_do = "List"

  CASE what_do = "Add"     && �〓� 甌�♂キ��

    DO A_Mess WITH topic_c, topic_n
    what_do = "List"

  CASE what_do = "Answer"  && 蹍甃�矗 �癶モ�

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO V_Answer
    ENDIF
    what_do = "List"

  CASE what_do = "Reply"   && �〓� �癶モ�

    DO A_Answer
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
SELECT MSS_LIST
USE
RELEASE MENU (menu_name) EXTENDED
SELECT (s_sav)

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                       蹍������ □��� 甌�♂キ��.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 04.09.98 17:32:16 夕
PROCEDURE A_Mess
PARAMETERS topic_c, topic_n

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�          ������〓� 甌�♂キ�� ....:....!....:....!....:....!..          �
*� 敖 �オ痰 甌�♂キ�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� �                                                                    � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�                    < OK Ctrl-W > < �皖���碎瘴 Esc >                    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, nm_mess, t_mess
ex   = 1
t_mess = ""
nm_mess = SPACE(32)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 21, 76, ALLTRIM(topic_n)
DO Sun_Bord WITH 3, 3, 18, 72, " �オ痰 甌�♂キ�� "
@ 18, (WCOLS()-50)/2 SAY " �諷�� ├ ���� 爛���皋牀����� - Tab ┼� Shift-Tab "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  2, 12 SAY "������〓� 甌�♂キ��" GET nm_mess
@  4,  4 EDIT t_mess SIZE 14, 68
@ 19, 22 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID T_Blank()

IF ex = 1
  DO SetPrice WITH "�イ▲れ! �窶｀�! ���痰�!!!", t_mess
  APPEND BLANK
  REPLACE TOPIC     WITH topic_c,  ;
          WHO       WITH p_who,    ;
          WHERE     WITH arm,      ;
          MSS_DATE  WITH DATE(),   ;
          MSS_TIME  WITH TIME(),   ;
          MSS_TITLE WITH nm_mess,  ;
          MSS_TEXT  WITH t_mess
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     蹍▲爲� ��玻オ皚�痰� ゛����.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 04.09.98 18:31:41 夕
PROCEDURE T_Blank
PRIVATE mss

IF ex = 1
  IF EMPTY(nm_mess)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"������〓� 甌�♂キ�� �� ��Ε� °碎 �竅硅�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF EMPTY(t_mess)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"���♂キ┘ �� ��Ε� °碎 �竅硅�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ォ痰※皀�讚�, 甌縲��錺�? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 2
    RETURN .F.
  ENDIF
ELSE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ォ痰※皀�讚�, �皖�щ��ガ瘴 甌縲��閧�? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 2
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.
