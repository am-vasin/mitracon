*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� V_0001       ��о�｀砒┴ ��むォ ��瓱�           15.06.99          �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�             ��メキ┘ ��Ж罔� ��痰�∫┴� � 瓣�� ���キ���矣珮.           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE V_0001

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

DEFINE PAD Mark OF (menu_name) PROMPT "�皙モ��"
ON PAD Mark OF (menu_name) ACTIVATE POPUP Mark

  DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Mark PROMPT "�痰./��閧�" KEY Ctrl-A, "蹍．�"
  ON SELECTION BAR 1 OF Mark DO Sw_Mode WITH "Mark"

  DEFINE BAR  2 OF Mark PROMPT "��▲珥�牀��碎 ≡�" KEY Ctrl-A, "*"
  ON SELECTION BAR 2 OF Mark DO Sw_Mode WITH "Not"

  DEFINE BAR  3 OF Mark PROMPT "�腮痰�碎 ≡�" KEY Ctrl-A, "-"
  ON SELECTION BAR 3 OF Mark DO Sw_Mode WITH "Clear"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
PRIVATE r_sav, o_sav
r_sav = RECNO()
o_sav = TAG()
SET ORDER TO
SELECT LINK
SET RELATION TO LNK INTO IN_STOCK

*
*   ��瘍モ ��Кム�� ����
*
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 71

*
*��皙陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳���� 甌Г.陳���.朕
*�MMM PPPP NNNNNNNNNNNNNNNNNNNNNNNNN PPPPPPPP DDDDDDDDDD 999999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖 �キ� ��痰�∫┴� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 99999.99珮�   99999.9999$                                   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Wins WITH ln, wd, ALLTRIM(s_name), 0, 1

@ 1, 2 SAY "�皙陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳���� 甌Г.陳���."

@ WROWS()-3,  3 SAY " �キ� ��痰�∫┴� "
@ WROWS()-2, 11 SAY "珮�           $"
what_do = "List"

DO WHILE .T.
 
  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL SpaceBar DO Mrk
    ON KEY LABEL "*"      DO Inv_Mrk
    ON KEY LABEL "-"      DO Clr_Mrk
*
*    BROWSE - �キ�
*
    BROWSE FIELDS IN_STOCK.MRK:H="", ;
                  IN_STOCK.PREFIX:H="", ;
                  IN_STOCK.NAME:H="", ;
                  IN_STOCK.PRODUCER:H="", ;
                  IN_STOCK.DATE_ON:H="", ;
                  F000 = IIF(IN_STOCK.SUP_QNT < 0, " �痰� ", TRANSFORM(IN_STOCK.SUP_QNT, "@Z 999999")):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_F() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Mark"     && �痰���※碎/��閧� �皙モ��

    DO Mrk
    what_do = "List"

  CASE what_do = "Clear"    && �腮痰�碎 ≡� �皙モ��

    DO Clr_Mrk
    what_do = "List"

  CASE what_do = "Not"      && �▲珥�牀��碎 ≡� �皙モ��

    DO Inv_Mrk
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
SET RELATION TO
SELECT IN_STOCK
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SET ORDER TO (o_sav)

RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      �襤�� ┃筮爼�罔����� 痰牀��.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 14.06.97 14:49:18 夕
PROCEDURE Show_F

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-2,  3 SAY IN_STOCK.PRICE_R PICTURE "@Z 99999.99" COLOR SCHEME 1
@ WROWS()-2, 17 SAY IN_STOCK.PRICE_D PICTURE "@Z 99999.9999" COLOR SCHEME 1

RETURN .T.
