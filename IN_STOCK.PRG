*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� In_Stock     ��о�｀砒┴ ��むォ ��瓱�           15.06.99          �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�             ��メキ┘ ��Ж罔� ��痰�∫┴� � 瓣�� ���キ���矣珮.           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE In_Stock

PRIVATE s_code, s_name, i0, i1
s_code = 0
s_name = ""
IF .NOT. Prp_Tmp()
  RETURN
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

DEFINE PAD Mark OF (menu_name) PROMPT "�皙モ��"
ON PAD Mark OF (menu_name) ACTIVATE POPUP Mark

  DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Mark PROMPT "�痰./��閧�" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Mark DO Sw_Mode WITH "Mark"

  DEFINE BAR  2 OF Mark PROMPT "��▲珥�牀��碎 ≡�" KEY Ctrl-A, "*"
  ON SELECTION BAR 2 OF Mark DO Sw_Mode WITH "Not"

  DEFINE BAR  3 OF Mark PROMPT "�腮痰�碎 ≡�" KEY Ctrl-A, "-"
  ON SELECTION BAR 3 OF Mark DO Sw_Mode WITH "Clear"

DEFINE PAD Move OF (menu_name) PROMPT "���讓� �皙." KEY Ctrl-A, "F2"
ON SELECTION PAD Move OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Seek_C OF (menu_name) PROMPT "���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek_C OF (menu_name) Do Sw_Mode WITH "Seek_C"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
SELECT 0
DO Use_Dummy
SELECT 0
DO Use_Link
SELECT IN_STOCK

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, interr1, interr2
DIMENSION interr1(5), interr2(5)
interr1(1) = 9
interr2(1) = "{Tab}"
interr1(2) = 42
interr2(2) = "*"
interr1(3) = 45
interr2(3) = "-"
interr1(4) = -1
interr2(4) = "{F2}"
interr1(5) = -6
interr2(5) = "{F7}"
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 71

*
*��皙陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳���� 甌Г.陳���.朕
*�MMM PPPP NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN PPPPPPPP DDDDDDDDDD 999999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖 �キ� ��痰�∫┴� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 99999.99珮�   99999.9999$                                   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Wins WITH ln, wd, ALLTRIM(s_name), 1, 1
@ 1, 9 SAY "���瓷"
@ 1, 15 SAY PADR("Enter",25) COLOR (SCHEME(14,2))

@ 2, 2 SAY "�皙陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳���� 甌Г.陳���."

@ WROWS()-3,  3 SAY " �キ� ��痰�∫┴� "
@ WROWS()-2, 11 SAY "珮�           $"
what_do = "List"

DO WHILE .T.
 
  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Tab      DO Mrk
    ON KEY LABEL "*"      DO Inv_Mrk
    ON KEY LABEL "-"      DO Clr_Mrk
    ON KEY LABEL F2       DO Sw_Mode WITH "Filter"
    ON KEY LABEL F7       DO Sw_Mode WITH "Seek_C"
    ON KEY LABEL Enter    DO Seek_P

*
*    BROWSE - �キ�
*
    BROWSE FIELDS MRK:H="", ;
                  PREFIX:H="", ;
                  NAME:H="", ;
                  PRODUCER:H="", ;
                  DATE_ON:H="", ;
                  F000 = IIF(SUP_QNT < 0, " �痰� ", TRANSFORM(SUP_QNT, "@Z 999999")):H="", ;
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

  CASE what_do = "Seek_C"   && ���皀�痰�覃 ���瓷

    DO Seek_C
    what_do = "List"

  CASE what_do = "Clear"    && �腮痰�碎 ≡� �皙モ��

    DO Clr_Mrk
    what_do = "List"

  CASE what_do = "Not"      && �▲珥�牀��碎 ≡� �皙モ��

    DO Inv_Mrk
    what_do = "List"

  CASE what_do = "Filter"   && ���甌�/���キ���矣��

    DO Filter
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

SET ORDER TO TAG NAME1
GO TOP
IF EOF() .OR. BOF()
  s_code = 0
ELSE
  s_code = 1
ENDIF

RELEASE MENU (menu_name) EXTENDED

CLEAR WINDOWS
CLOSE DATABASES

IF s_code # 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ����※� �皙ョキ�襯 ��Ж罔� � ���キ���矣珮? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 1
    USE (base_path+"STOCK")
    SELECT 0
    USE (tmpo_path+"IN_STOCK") ORDER TAG NAME1
    SCAN
      i0 = 1
      DO WHILE i0 <= 25
        IF ISDIGIT(SUBSTR(NAME, i0, 1))
          EXIT
        ENDIF
        i0 = i0+1
      ENDDO
      IF i0 <= 25
        i1 = 25
        DO WHILE i1 >= i0
          IF ISDIGIT(SUBSTR(NAME, i1, 1))
            EXIT
          ENDIF
          i1 = i1-1
        ENDDO
      ENDIF
      IF i0 <= 25
        i1 = SUBSTR(NAME, i0, i1-i0+1)
      ELSE
        i1 = ""
      ENDIF
      SELECT STOCK
      APPEND BLANK
      REPLACE CODE      WITH IN_STOCK.CODE,     ;
              PREFIX    WITH IN_STOCK.PREFIX,   ;
              NAME      WITH IN_STOCK.NAME,     ;
              PRODUCER  WITH IN_STOCK.PRODUCER, ;
              PREFIX1   WITH SeekName(IN_STOCK.PREFIX),   ;
              NAME1     WITH SeekName(IN_STOCK.NAME),     ;
              PRODUCER1 WITH SeekName(IN_STOCK.PRODUCER), ;
              PRO_CODE  WITH IN_STOCK.PRO_CODE, ;
              NUM_PART  WITH i1, ;
              DATE_ON   WITH DATE(), ;
              WHO_ADD   WITH user, ;
              WHERE_ADD WITH arm
      SELECT IN_STOCK
    ENDSCAN
  ENDIF
ENDIF
CLOSE DATABASES
DELETE FILE (tmpo_path+"IN_STOCK.DBF")
DELETE FILE (tmpo_path+"IN_STOCK.CDX")

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      ���瓷 �� �����讚覓 瓱�〓���.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 10.06.97 10:43:18 夕
PROCEDURE Seek_P
PRIVATE prf, s, ln, rc_sav, i

ACTIVATE WINDOW (win_name+"_M") SAME

prf = ""
ln = 25
DO WHILE .T.
  ACTIVATE WINDOW (win_name+"_M") SAME
  @ 1, 15 SAY REPLICATE("�",25) COLOR SCHEME 1
  @ 1, 15 SAY prf COLOR SCHEME 1
  s = INKEY(0)
  *
  *   �瘁�イ礇� �� �爛がモ ����腮� �÷�｀砒┴�
  *
  i = ASCAN(interr1, s)
  IF i # 0
    KEYBOARD interr2(i)
    EXIT
  ENDIF
  IF s < 0       && �祗�罔����讚�� ���※��
    IF s = -9
      KEYBOARD "{F10}"
      EXIT
    ENDIF
    LOOP
  ENDIF
  IF BETWEEN(s,  32,126) .OR.   ;
     BETWEEN(s, 128,175) .OR.   ;
     BETWEEN(s, 224,241)     && �━〓�
    IF LEN(prf) >= 25        && �瓮! ���跏� �オ磴�!
      LOOP
    ENDIF
    rc_sav = RECNO()
    IF SEEK(prf+CHR(s))
      prf = prf+CHR(s)
      SHOW WINDOW (ALIAS()) REFRESH
    ELSE
      ?? CHR(7)
      GO rc_sav
    ENDIF
  ENDIF
  IF s = 127       && Back Space
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      = SEEK(prf)
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
  ENDIF
  IF s = 13        && Enter
    EXIT
  ENDIF
  IF s = 24        && Down Arrow
    KEYBOARD "{DnArrow}"
    EXIT
  ENDIF
  IF s = 5         && Up Arrow
    KEYBOARD "{UpArrow}"
    EXIT
  ENDIF
  IF s = 3         && Page Down
    KEYBOARD "{PgDn}"
    EXIT
  ENDIF
  IF s = 18        && Page Up
    KEYBOARD "{PgUp}"
    EXIT
  ENDIF
  IF s = 27        && Escape
    KEYBOARD "{Esc}"
    EXIT
  ENDIF
ENDDO

@ 1, 15 SAY PADR("Enter",25) COLOR (SCHEME(14,2))
  
ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Tmp      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�            �襦�� ��痰�∫┴� � ��ぃ�皰→� ∇ガキ���� �����.             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 15.06.1999 陳�
PROCEDURE Prp_Tmp
PRIVATE c_names, c_codes, n_c

n_c = 0
USE (base_path+"CUSTOMER") ORDER TAG NAME
SCAN FOR .NOT. EMPTY(SUP)
  n_c = n_c+1
  DIMENSION c_names(n_c), c_codes(n_c)
  c_names(n_c) = B_NAME
  c_codes(n_c) = CODE
ENDSCAN
USE
IF n_c = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 甌Г��� �� �き��� ��痰�∫┴�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*


PRIVATE ln, point
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
ln = MIN(WROWS("")-5, n_c+3)
DO D_Win_N WITH ln, LEN(c_names(1))+4, "��痰�∫┴�"
point = 1
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO WHILE .T.
  point = MIN(point, n_c)
  point = MAX(point, 1)
  @ 1, 1 MENU c_names, n_c, WROWS()-3
  READ MENU TO point
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    s_code = c_codes(point)
    s_name = c_names(point)
    EXIT
  ENDIF
ENDDO
POP KEY
RELEASE WINDOW (win_name)

IF EMPTY(s_name)
  RETURN .F.
ENDIF
DO Wt_Mess WITH "��ぃ�皰→� ����諷"
DELETE FILE (tmpo_path+"IN_STOCK.DBF")
DELETE FILE (tmpo_path+"IN_STOCK.CDX")
CREATE DBF (tmpo_path+"IN_STOCK") ;
     ( MRK      C( 3), ;
       CODE     N( 7), ;
       PREFIX   C( 4), ;
       NAME     C(35), ;
       PRODUCER C( 8), ;
       PRO_CODE N( 5), ;
       PRICE_R  N(8,2), ;
       PRICE_D  N(8,4), ;
       SUP_QNT  N( 6), ;
       DATE_ON  D  )

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"OFFER") ORDER TAG SUP_CODE

SEEK s_code
SCAN REST WHILE s_code = SUP_CODE
  IF EMPTY(CODE)
    LOOP
  ENDIF
  IF SEEK(CODE,"STOCK")
    LOOP
  ENDIF
  SELECT IN_STOCK
  APPEND BLANK
  REPLACE CODE     WITH OFFER.CODE,    ;
          PREFIX   WITH OFFER.PREFIX,  ;
          NAME     WITH OFFER.NAME,    ;
          PRODUCER WITH OFFER.PRODUCER,;
          PRO_CODE WITH OFFER.PRO_CODE,;
          PRICE_R  WITH OFFER.PRICE_R, ;
          PRICE_D  WITH OFFER.PRICE_D, ;
          SUP_QNT  WITH OFFER.SUP_QNT, ;
          DATE_ON  WITH OFFER.DATE_ON
  SELECT OFFER
ENDSCAN
DO Wt_Mess

USE
SELECT STOCK
USE
SELECT IN_STOCK
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� �爛か�Ε�┬ ��痰�∫┴� �モ �� �き�� ��〓� ��Ж罔�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN .F.
ENDIF

INDEX ON NAME+PREFIX+PRODUCER TAG NAME1 FOR .NOT. EMPTY(MRK)
INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE
GO TOP

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      �襤�� ┃筮爼�罔����� 痰牀��.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 14.06.97 14:49:18 夕
PROCEDURE Show_F

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-2,  3 SAY PRICE_R PICTURE "@Z 99999.99" COLOR SCHEME 1
@ WROWS()-2, 17 SAY PRICE_D PICTURE "@Z 99999.9999" COLOR SCHEME 1

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Mrk          ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �痰���※碎/��閧� �皙モ��.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 15.06.1999 陳�
PROCEDURE Mrk

REPLACE IN_STOCK.MRK WITH IIF(EMPTY(IN_STOCK.MRK), " � ", "")

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clr_Mrk      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         �腮痰�碎 ≡� �皙モ��.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 15.06.1999 陳�
PROCEDURE Clr_Mrk
PRIVATE r_sav, o_sav
o_sav = TAG()
r_sav = RECNO()
SCAN
  REPLACE IN_STOCK.MRK WITH ""
ENDSCAN

SET ORDER TO (o_sav)

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SHOW WINDOW (ALIAS()) REFRESH SAME

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Inv_Mrk      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ��▲珥�牀��碎 ≡� �皙モ��.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 15.06.1999 陳�
PROCEDURE Inv_Mrk
PRIVATE r_sav, o_sav
o_sav = TAG()
r_sav = RECNO()
SET ORDER TO
SCAN
  REPLACE IN_STOCK.MRK WITH IIF(EMPTY(IN_STOCK.MRK), " � ", "")
ENDSCAN

SET ORDER TO (o_sav)

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SHOW WINDOW (ALIAS()) REFRESH SAME
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Filter       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �痰���→�/��閧┘ 筥�赳��.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 15.06.1999 陳�
PROCEDURE Filter

PRIVATE o_sav
o_sav = TAG()

IF "1" $ o_sav
  SET ORDER TO TAG NAME
  DEFINE PAD Move OF (menu_name) PROMPT "���讓� �皙." KEY Ctrl-A, "F2" BEFORE Seek_C
ELSE
  SET ORDER TO TAG NAME1
  DEFINE PAD Move OF (menu_name) PROMPT "�瓮 ��Ж罔�" KEY Ctrl-A, "F2" BEFORE Seek_C
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_C       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���皀�痰�覃 ���瓷.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 15.06.1999 陳�
PROCEDURE Seek_C
PRIVATE shb, r_sav, o_sav

shb = G_Shab()
IF EMPTY(shb)
  RETURN
ENDIF
r_sav = RECNO()
DO Wt_Mess WITH "���瓷..."
SELECT LINK
ZAP
SELECT IN_STOCK
SCAN
  IF shb $ NAME
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("IN_STOCK")
    SELECT IN_STOCK
  ENDIF
ENDSCAN

IF RECCOUNT("LINK") = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� ���ぅ��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  IF BETWEEN(r_sav,1,RECCOUNT())
    GO r_sav
  ENDIF
  RETURN
ENDIF

DO V_0001

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_Shab       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �〓� ��゛��� か� ���瓷�.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 15.06.1999 陳�
PROCEDURE G_Shab
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
*�    ....:....!....:....!....:     �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE s, ex
s = ""
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 5, 38, "�����皀 ��゛�� か� ���瓷�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2, 6 EDIT s SIZE 1, 25 COLOR SCHEME 1
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex # 1
  s = ""  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN s

