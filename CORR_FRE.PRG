*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Corr_Fre     ��о�｀砒┴ ��むォ ��瓱�           21.06.97 15:25:14 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�         蹍������ ��玻オ罔� � 筮爼�牀����� 瓣�｀き��� 甎�瓷�.          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Corr_Fre

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

DEFINE PAD Sw_Mode OF (menu_name) PROMPT "��/���." KEY Ctrl-A, "F2"
ON SELECTION PAD Sw_Mode OF (menu_name) Do Sw_Mode WITH "Sw_Mode"

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) Do Sw_Mode WITH "Seek1"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, h_list, chr_shab, int_1, int_2
DIMENSION int_1(3), int_2(3)   && ｯ����┘ �祗�皰� �キ� か� �牀������ ���瓷�
int_1(1) = 27      && Esc
int_2(1) = "{Esc}"
int_1(2) = -6      && F7
int_2(2) = "{F7}"
int_1(3) = -1      && F2
int_2(3) = "{F2}"

chr_shab = ' !"'+    ;
           "#$%&'()*+,-./0123456789:;<=>?@"+  ;
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"+         ;
           "[\]^_`"+                             ;
           "abcdefghijklmnopqrstuvwxyz"+         ;
           "{|}~"+                               ;
           "��������������������������������"+   ;
           "�、�ぅΗ┤������珀矣粤肄蓍裨跋鈿"
ln = 100   && ����腑痰〓 ※え�諷 痰牀� BROWSE
DO CASE
CASE WK_TIT.IS_QNT .AND. WK_TIT.IS_PRICE
  wd = 65
CASE WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
  wd = 56
CASE .NOT. WK_TIT.IS_QNT .AND. WK_TIT.IS_PRICE
  wd = 58
CASE .NOT. WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
  wd = 51
ENDCASE

h_list = .T.

DO D_Wins WITH ln, wd, ALLTRIM(WK_TIT.L_NAME), 1, 0

@ 1, 3 SAY "���瓷:"
@ 1, 10 SAY PADR("Tab",FSIZE("NAME","STOCK")) COLOR (SCHEME(14,2))
IF .NOT. WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
  @ 1, 17 SAY "�皙モ��: "
  @ 1, COL() SAY PADR("蹍．�",WCOLS()-COL()-1) COLOR (SCHEME(14,2))
ENDIF
@ 2,  4 SAY " ��Ж罔� "

what_do = "List"
SELECT STOCK
SET ORDER TO TAG NAME
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Tab DO Seek_
    ON KEY LABEL F2 DO Sw_Mode WITH "Sw_Mode"
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek1"
    ON KEY LABEL F5 DO Sw_Mode WITH "Addr"
    ON KEY LABEL F3 DO Sw_Mode WITH "New_pos"
    ON KEY LABEL F4 DO Sw_Mode WITH "Due"
    IF .NOT. WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
      ON KEY LABEL SpaceBar DO Mov_Rec
    ENDIF

*
*    BROWSE - �キ�
*
    IF h_list           && �瘴 ���キ���矣��
      SELECT TMP_WK
      SET FILTER TO
      SET ORDER TO TAG CODE
      SELECT STOCK
      SET ORDER TO TAG NAME
      SET RELATION TO IIF(SEEK(CODE,"TMP_WK"), CODE, 0) INTO TMP_WK
    ELSE
      SELECT STOCK
      SET ORDER TO TAG CODE
      SELECT TMP_WK
      SET FILTER TO CODE # 0
      SET ORDER TO TAG NAME
      SET RELATION TO CODE INTO STOCK
    ENDIF
    DO CASE
    CASE WK_TIT.IS_QNT .AND. WK_TIT.IS_PRICE
      BROWSE FIELDS ;
             PREFIX:H="":4:W=.F.,    ;
             NAME:H="":35:W=.F.,     ;
             PRODUCER:H="":8:W=.F.,  ;
             TMP_WK.QNT:H="":P="@Z 999999":V=Mov_Rec(), ;
             TMP_WK.PRICE:H="":P="@Z 99999.99":W=TMP_WK.QNT#0 ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    CASE WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
      BROWSE FIELDS ;
             PREFIX:H="":4:W=.F.,    ;
             NAME:H="":35:W=.F.,     ;
             PRODUCER:H="":8:W=.F.,  ;
             TMP_WK.QNT:H="":P="@Z 999999":V=Mov_Rec() ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    CASE .NOT. WK_TIT.IS_QNT .AND. WK_TIT.IS_PRICE
      BROWSE FIELDS ;
             PREFIX:H="":4:W=.F.,    ;
             NAME:H="":35:W=.F.,     ;
             PRODUCER:H="":8:W=.F.,  ;
             TMP_WK.PRICE:H="":P="@Z 99999.99":V=Mov_Rec() ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    CASE .NOT. WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
      BROWSE FIELDS ;
             PREFIX:H="":4:W=.F.,    ;
             NAME:H="":35:W=.F.,     ;
             PRODUCER:H="":8:W=.F.,  ;
             F000 = IIF(TMP_WK.QNT # 0, "�", " "):H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDCASE
    SET RELATION TO
    IF what_do = "List"
      what_do = "Exit"
    ENDIF
    ON KEY

  CASE what_do = "Sw_Mode"    && 蹍甃�矗 甎�瓷�

    h_list = .NOT. h_list
    what_do = "List"

  CASE what_do = "Seek1"      && ���皀�痰�覃 ���瓷

    DO Seek1
    what_do = "List"

  OTHERWISE

    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ��縲��━ ├�キキ��? [ �� ] [ �モ ]"
    mss(3) = ""
    ln = Out_Mess(5, "mss")
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN ln = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                    ����←キ┘ ┼� 祀��キ┘ ����瓱.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 21.06.97 16:11:21 夕
PROCEDURE Mov_Rec
PRIVATE q_sav, p_sav, nv

nv = 0
q_sav = 0
p_sav = 0

IF WK_TIT.IS_QNT      && ����腑痰〓 Θ▲� � ぎ�┃�珮モ!
  IF TMP_WK.QNT = 0 
    IF TMP_WK.CODE # 0
      nv = -1
    ENDIF
  ELSE
    IF TMP_WK.CODE = 0
      q_sav = QNT
      p_sav = PRICE
      nv = 1
    ENDIF
  ENDIF
ELSE                  && ����腑痰〓 �� Θ▲�:
  IF .NOT. WK_TIT.IS_PRICE   &&   皰�讓� ���モ��
    IF TMP_WK.QNT # 0 
      IF TMP_WK.CODE # 0
        nv = -1
      ENDIF
    ELSE
      IF TMP_WK.CODE = 0
        q_sav = 1
        nv = 1
      ENDIF
    ENDIF
  ELSE                       &&   皰�讓� 罐��
    IF TMP_WK.PRICE = 0 
      IF TMP_WK.CODE # 0
        nv = -1
      ENDIF
    ELSE
      IF TMP_WK.CODE = 0
        q_sav = QNT
        p_sav = PRICE
        nv = 1
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF nv = 1
  SELECT TMP_WK
  REPLACE QNT WITH 0, PRICE WITH 0
  APPEND BLANK
  REPLACE LIST_NO  WITH RECNO("WK_TIT"),;
          CODE     WITH STOCK.CODE,     ;
          PREFIX   WITH STOCK.PREFIX,   ;
          NAME     WITH STOCK.NAME,     ;
          PRODUCER WITH STOCK.PRODUCER, ;
          PRICE    WITH p_sav,          ;
          QNT      WITH q_sav
  SELECT STOCK
  SHOW WINDOW (ALIAS()) SAME REFRESH
ENDIF
IF nv = -1
  SELECT TMP_WK
  DELETE
  IF h_list
    SELECT STOCK
  ENDIF
  SHOW WINDOW (ALIAS()) SAME REFRESH
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 蹍������ ���瓷� �� �����讚覓 ＜����.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.05.97 22:05:53 夕
PROCEDURE Seek_

PRIVATE s, prf, ln, bg, rc_s, i

ln = FSIZE("NAME", "STOCK")
ACTIVATE WINDOW (win_name+"_M") SAME
prf = ""
@ 1, 3 SAY "���瓷:"
@ 1, 10 SAY SPACE(WCOLS()-9)
bg = REPLICATE("�", ln)
DO WHILE .T.
  @ 1, 10 SAY bg
  @ 1, 10 SAY prf
  s = INKEY(0)
  i = ASCAN(int_1, s)
  IF i # 0
    KEYBOARD int_2(i)
    EXIT
  ENDIF
  IF s < 0     && �祗�罔����讚�� ���※��
    IF s = -9  && F10 - ���竅��ガ �キ�
      KEYBOARD "{F10}"
      EXIT
    ENDIF
    LOOP
  ENDIF
  s = CHR(s)
  IF s $ chr_shab .AND. LEN(prf) < ln   && �━〓�
    rc_s = RECNO()
    IF SEEK(prf+s)
      prf = prf+s
    ELSE
      IF rc_s < RECCOUNT() .AND. rc_s > 0
        GO rc_s
      ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH
    LOOP
  ENDIF
  IF s = CHR(13)       && Enter
    EXIT
  ENDIF
  IF s = CHR(3)        && PgDn
    KEYBOARD "{PgDn}"
    EXIT
  ENDIF
  IF s = CHR(18)       && PgUp
    KEYBOARD "{PgUp}"
    EXIT
  ENDIF
  IF s = CHR(5)        && UpArrow
    KEYBOARD "{UpArrow}"
    EXIT
  ENDIF
  IF s = CHR(24)       && DnArrow
    KEYBOARD "{DnArrow}"
    EXIT
  ENDIF
  IF s = CHR(127)      && BackSpace
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      SEEK prf
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
    LOOP
  ENDIF
ENDDO

@ 1, 10 SAY PADR("Tab",ln) COLOR (SCHEME(14,2))
IF .NOT. WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
  @ 1,17 SAY "�皙モ��: "
  @ 1, COL() SAY PADR("蹍．�",WCOLS()-COL()-1) COLOR (SCHEME(14,2))
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     蹍罐ゃ�� ���皀�痰���� ���瓷�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.05.97 11:59:42 夕
PROCEDURE Seek1

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

PRIVATE ex, nm_w     &&
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
nm_w = SPACE(25)     &&
*------------------------------------------------------------------------
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                                     �
*� �����皀 ��゛�� か� ���瓷� 臼臼憶臼越臼臼憶臼越臼臼� �
*�                                                     �
*�          < OK Ctrl-W > < �皖���碎瘴 Esc >           �
*�                                                     �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 57, "���瓷 � "+IIF(h_list, "���キ���矣爛.", "甎�瓷�.")

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  3 SAY "�����皀 ��゛�� か� ���瓷�" GET nm_w
@ 5, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE
nm_w = ALLTRIM(nm_w)

IF ex = 1 .AND. .NOT. EMPTY(nm_w)
  DO Choose_P WITH nm_w, h_list
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 ��ぁ�爲� �� ���皀�痰� � �襦�� ��Ж罔�.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.05.97 12:17:29 夕
PROCEDURE Choose_P
PARAMETERS shab, is_stock
PRIVATE sel_sav

sel_sav = SELECT()

DO Wt_Mess WITH "��ぁ�爲�..."
IF is_stock
  SELECT PREFIX, NAME, PRODUCER, RECNO() AS REC_N FROM STOCK  ;
         INTO CURSOR TMP_C   ;
         WHERE (shab $ NAME .OR. shab $ PREFIX .OR. shab $ PRODUCER)  ;
               .AND. EMPTY(DATE_OFF)
ELSE
  SELECT PREFIX, NAME, PRODUCER, RECNO() AS REC_N FROM INC_LIST  ;
         INTO CURSOR TMP_C   ;
         WHERE (shab $ NAME .OR. shab $ PREFIX .OR. shab $ PRODUCER)  ;
               .AND. LIST_NO = key_v .AND. (QNT # 0 .OR. QNT_INV # 0)
ENDIF
DO Wt_Mess
SELECT TMP_C
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� 磴���瘡 ���皋..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 49
DO D_Wins WITH ln, wd, "�襦ム�皀 ��Ж罔�", 0, 0
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS F000 = PREFIX+" "+NAME+" "+PRODUCER:H="":49,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    IF is_stock
      GO TMP_C.REC_N IN STOCK
    ELSE
      GO TMP_C.REC_N IN INC_LIST
    ENDIF
    what_do = "exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

SELECT TMP_C
USE
SELECT (sel_sav)
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN
