*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                      �
*�                蹍������ ��｀硅 甌 甎�瓷�� 皰��牀�.                  �
*�                                                                      �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Goods

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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
*
*  ���甌� �發カ�� �爛く爬閧��
*
PRIVATE mss, sel_sav
sel_sav = SELECT()
SELECT 0
USE GOODS ORDER TAG NAME
GO TOP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "add"

DEFINE PAD Edit OF (menu_name) PROMPT "��玻オ皋牀��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Edit OF (menu_name) Do Sw_Mode WITH "edit"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

SELECT GOODS

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, pic
ln = 100   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "���甌� 皰��牀�", 0, 0

IF EOF() .OR. BOF()
  what_do = "add"
ELSE
  what_do = "list"
ENDIF

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "add"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E") COLOR SCHEME 13
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "edit")
    ENDIF

  CASE what_do = "add"    && ����※碎

    DO Corr_God WITH "A"
    IF EOF() .OR. BOF()
      what_do = "exit"
    ELSE
      what_do = "list"
    ENDIF

  CASE what_do = "edit"   && ��玻オ皋牀��碎

    DO Corr_God WITH "C"
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
SELECT GOODS
USE
SELECT (sel_sav)
RELEASE MENU (menu_name) EXTENDED

RETURN


*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ ��玻オ罔� 皰����.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Corr_God
PARAMETER mode

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

PRIVATE ex, b_n, l_n, w
                                    &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1                            &&

IF mode = "A"
  l_n  = SPACE(FSIZE("NAME"))
ELSE
  l_n  = NAME
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 52, "�������皀 ��珥�腦�"

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                �
*�  ��━キ����┘ 皰���� 臼臼憶臼越臼臼憶臼越臼臼  �
*�                                                �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >        �
*�                                                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3,  4 SAY "��━キ����┘ 皰����" GET l_n
@ 5, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  IF mode = "A"
    SET ORDER TO TAG CODE
    GO BOTTOM
    IF EOF() .OR. BOF()
      w = 1
    ELSE
      w = CODE+1
    ENDIF
    APPEND BLANK
    REPLACE CODE  WITH w
    SET ORDER TO TAG NAME
  ENDIF
  REPLACE NAME WITH l_n
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ �牀▲爲� ��玻オ皚�痰� ���ォ.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tst_Blank

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF EMPTY(l_n)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ, ���-皰 �� (皰���) ≡�-���� ぎ�Ε� ��щ��碎瘴!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
