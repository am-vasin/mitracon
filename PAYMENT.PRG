*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Payment      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             �〓� ����硅.                               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 20.01.1999 様�
PROCEDURE Payment
PARAMETER t_list, allDocs

PRIVATE max_ord, pl_code, show_p, show_a, show_f, show_b, tpList, pay_bill
PRIVATE ll

pay_bill = .T.
max_ord = 0
tpList = ""
IF TYPE("t_list") = "C"
  tpList = t_list
ENDIF
STORE .T. TO show_p, show_a
IF enab_firm
  show_f = ""
ELSE
  show_f = def_firm
ENDIF
IF enab_bank
  show_b = ""
ELSE
  show_b = def_bank
ENDIF
IF File_O(base_path+"MAX_ORD.MEM")
  RESTORE FROM (base_path+"MAX_ORD.MEM") ADDITIVE
ENDIF

*pl_code = G_Pl()
pl_code = "B"
IF EMPTY(pl_code)
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
DO Use_Link
*ZAP

SELECT 0
USE (base_path+"CUS_NAME") ORDER TAG CUS_CODE ALIAS LNM_9120 AGAIN

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_9120 AGAIN
SET RELATION TO CUS_CODE  INTO LNM_9120

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9120 AGAIN

SELECT 0
USE (base_path+"BANKS") ORDER TAG BANK ALIAS B_9120 AGAIN

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS WHE_9120 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO_9120 AGAIN

SELECT 0
USE (base_path+"BILLS") ORDER TAG HRON ALIAS BLS_9120 AGAIN
IF .NOT. m.allDocs
  SET FILTER TO WHERE = m.arm
ENDIF
SET RELATION TO CUS_CODE  INTO CUS_9120, ;
                FIRM      INTO F_9120,   ;
                FIRM+BANK INTO B_9120,   ;
                WHO       INTO WHO_9120, ;
                WHERE     INTO WHE_9120
                

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4
DEFINE PAD Pay OF (menu_name) PROMPT "ｯ����"
ON PAD Pay OF (menu_name) ACTIVATE POPUP Pay
  DEFINE POPUP Pay MARGIN RELATIVE SHADOW COLOR SCHEME 4
  DEFINE BAR  1 OF Pay PROMPT "踳あ�爬皀�讚襯 ����襯 " KEY Ctrl-A, "F3"
  ON SELECTION BAR 1 OF Pay DO Sw_Mode WITH "prv_data"
  DEFINE BAR  2 OF Pay PROMPT "�� 瘍モ� " KEY Ctrl-A, "Enter"
  ON SELECTION BAR 2 OF Pay DO Sw_Mode WITH "int_pay"
  DEFINE BAR  3 OF Pay PROMPT "�ェ 瘍モ� " KEY Ctrl-A, "Ins"
  ON SELECTION BAR 3 OF Pay DO Sw_Mode WITH "new_pay"
  DEFINE BAR  4 OF Pay PROMPT "����甌�襯 瘍モ�-���矣琺 "
  ON SELECTION BAR 4 OF Pay DO Sw_Mode WITH "stopDoc"

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek_B
  DEFINE POPUP Seek_B MARGIN RELATIVE SHADOW COLOR SCHEME 4
  DEFINE BAR  1 OF Seek_B PROMPT "�� ��┘�矣 " KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek_B DO Sw_Mode WITH "seek_c"
  DEFINE BAR  2 OF Seek_B PROMPT "�� 痺��� " KEY Ctrl-A, "F8"
  ON SELECTION BAR 2 OF Seek_B DO Sw_Mode WITH "seek_s"
  DEFINE BAR  3 OF Seek_B PROMPT "��甄イ�┘ ���ぅ��襯" KEY Ctrl-A, "F9"
  ON SELECTION BAR 3 OF Seek_B DO Sw_Mode WITH "seek_l"
  DEFINE BAR  4 OF Seek_B PROMPT "���瓷 �� 瘍モ�" && KEY Ctrl-A, "F9"
  ON SELECTION BAR 4 OF Seek_B DO Sw_Mode WITH "seek_b"

DEFINE PAD Cust OF (menu_name) PROMPT "��┘��"
ON PAD Cust OF (menu_name) ACTIVATE POPUP Cust
  DEFINE POPUP Cust MARGIN RELATIVE SHADOW COLOR SCHEME 4
  DEFINE BAR  4 OF Cust PROMPT "��┃��� ━� "
  ON SELECTION BAR 4 OF Cust DO Sw_Mode WITH "LName"
  DEFINE BAR  1 OF Cust PROMPT "�痰�爬� ��┘��� " KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1OF Cust DO Sw_Mode WITH "CHist"
  DEFINE BAR  2 OF Cust PROMPT "�矗─竄� ��┘���" KEY Ctrl-A, "Home"
  ON SELECTION BAR 2 OF Cust DO Sw_Mode WITH "CusBil"
  DEFINE BAR  3 OF Cust PROMPT "��爲モ┃������ ┃筮爼." KEY Ctrl-A, "Ctrl-Home"
  ON SELECTION BAR 3 OF Cust DO Sw_Mode WITH "CusInfo"

DEFINE PAD Print OF (menu_name) PROMPT "�腑�" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "print"

DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳�" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "filter"

DEFINE PAD Delete OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "delete"

DEFINE PAD Exit   OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit   OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 72
*��爼陳N ぎ�-��陳陳����陳陳陳陳陳陳��祠��陳���┘�眥陳陳陳陳陳陳陳陳陳陳陳朕
*���� ����������� ��.��.���� 999_999_999.99 �������������������������������
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�       踳く爬閧┘ 9999999999999999         ���� 9999999999999999       �
*� �え��肓           ��痰�鎰┘                  ���祀�牀���!              �
*� �皰 �覩���� 瘍モ 999999999999999999999999999999   ��� 9999999999999999 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
DO D_Wins WITH ln, wd, "�腑��", 0, 3
@ 1, 2 SAY "�爼陳N ぎ�-��陳陳����陳陳陳陳陳陳��祠��陳���┘��"
@ WROWS()-4, 9 SAY "踳く爬閧┘                          ����"
@ WROWS()-3, 3 SAY "�え��肓           ��痰�鎰┘"
@ WROWS()-2, 3 SAY "�皰 �覩���� 瘍モ                                  ���"
what_do = "List"

GO BOTTOM

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F3     DO Sw_Mode WITH "prv_data"
    ON KEY LABEL Ins    DO Sw_Mode WITH "new_pay"
    ON KEY LABEL F7     DO Sw_Mode WITH "seek_c"
    ON KEY LABEL F8     DO Sw_Mode WITH "seek_s"
    ON KEY LABEL F9     DO Sw_Mode WITH "seek_l"
    ON KEY LABEL Ctrl+P DO Sw_Mode WITH "print"
    ON KEY LABEL F6     DO Sw_Mode WITH "filter"
    ON KEY LABEL Del    DO Sw_Mode WITH "delete"
    ON KEY LABEL Tab    DO Sw_Mode WITH "CHist"
    ON KEY LABEL Home   DO Sw_Mode WITH "CusBil"
    ON KEY LABEL Ctrl-Home DO Sw_Mode WITH "CusInfo"

*
*    BROWSE - �キ�
*
    BROWSE FOR Flt_Val() REST ;
           FIELDS FIRM:3:H="",  ;
                  F000 = V_Doc(DOC_NUM):11:H="", ;
                  DOC_DATE:H="",                 ;
                  BILL_SUM:H="":P="999 999 999.99",;
                  F001 = Left_Cus(30):30:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "int_pay")
    ENDIF

  CASE what_do = "int_pay"    && ���皀� �� 瘍モ�

    IF .NOT. EOF() .AND. .NOT. BOF()
      *
      *    蹍▲爲�
      *
      IF ALLTRIM(STATUS) = "���琺�"
        PRIVATE mss_000
        DIMENSION mss_000(3)
        mss_000(1) = ""
        mss_000(2) = CHR(0)+"�皰� 瘍モ �イ�痰祚キ か� □��� ���皀��!"
        mss_000(3) = ""
        DO Out_Mess WITH 7, "mss_000"
        what_do = "List"
        LOOP
      ENDIF
      IF ALLTRIM(STATUS) = "ｯ��腑��"
        PRIVATE mss_000
        DIMENSION mss_000(3)
        mss_000(1) = ""
        mss_000(2) = CHR(0)+"�皰� 瘍モ 礒� ����腑�. �磴ガ □�え碎 ���皀�? [ �モ ] [ �� ]"
        mss_000(3) = ""
        IF Out_Mess(7, "mss_000") = 1
          what_do = "List"
          LOOP
        ENDIF
      ENDIF
      IF OLD
        PRIVATE mss_000
        DIMENSION mss_000(3)
        mss_000(1) = ""
        mss_000(2) = CHR(0)+"�皰� 瘍モ ���祀�牀���. �磴ガ □�え碎 ���皀�? [ �モ ] [ �� ]"
        mss_000(3) = ""
        IF Out_Mess(7, "mss_000") = 1
          what_do = "List"
          LOOP
        ENDIF
      ENDIF
      IF Tst1_INN(CUS_CODE)
        DO IncMoney WITH FIRM, DOC_NUM, DOC_DATE, tpList
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "new_pay"    && ���皀� ．� 瘍モ�

    DO IncMoney WITH "", "", "", tpList
    what_do = "List"

  CASE what_do = "stopDoc"    && ����甌�襯 瘍モ�-���矣琺

    m.pay_bill = Set_PB(m.pay_bill)
    ACTIVATE WINDOW (win_name+"_M") SAME
    ll = FLOOR(WCOLS()-36)/2
    IF m.pay_bill
      @ WROWS()-5, ll SAY REPLICATE("�", 36) && COLOR (SCHEME(13,6))
    ELSE
      @ WROWS()-5, ll SAY "����甌�襯 瘍モ� ���矣琺 �� 甌Г�鈞瘴" COLOR SCHEME 7
    ENDIF
    what_do = "List"

  CASE what_do = "CHist"      && �痰�爬� ��┘���

    IF .NOT. EMPTY(CUS_CODE)
      DO Acc_Hist WITH CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "LName"      && ��┃��� ━�

    IF .NOT. EMPTY(CUS_CODE)
      DO Cus_Name WITH CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "CusBil"     && �矗─竄� ��┘���

    IF .NOT. EMPTY(CUS_CODE)
      DO Cus_Bil WITH CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "CusInfo"    && ��珥�腦� ��爲モ┃��

    IF .NOT. EMPTY(CUS_CODE)
      DO Cli_Info WITH "", .F., CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "seek_c"     && ���瓷 �� ��┘�矣

    IF Seek_Cus()
      DO Ch_Last
    ENDIF
    what_do = "List"

  CASE what_do = "seek_b"     && ���瓷 �� 瘍モ�

    m.rec = Seek_Bl()
    IF m.rec # 0
      GO m.rec IN BLS_9120
    ENDIF
    what_do = "List"

  CASE what_do = "prv_data"   && 踳あ�爬皀�讚襯 ����襯 �� ����皀

    IF BLS_9120.STATUS = "ｯ��腑��  "
      PRIVATE mss000
      DIMENSION mss000(3)
      mss000(1) = ""
      mss000(2) = CHR(0)+"� �爛あ�爬皀�讚覓� ����覓� �� ���Г���: 瘍モ ����腑�!"
      mss000(3) = ""
      DO Out_Mess WITH 7, "mss000"
    ELSE
      REPLACE BLS_9120.STATUS  WITH "踳あ.", ;
              BLS_9120.WHO_INC WITH user,     ;
              BLS_9120.DAT_INC WITH DATE()
    ENDIF
    what_do = "List"

  CASE what_do = "seek_s"     && ���瓷 �� 痺���

    IF Seek_Sum()
      DO Ch_Last
    ENDIF
    what_do = "List"

  CASE what_do = "seek_l"     && ��甄イ�ゥ ���ぅ���

    DO Ch_Last
    what_do = "List"

  CASE what_do = "print"      && �ョ�碎 瘍モ�

    DO P_Bill WITH FIRM, DOC_NUM, DOC_DATE
    what_do = "List"

  CASE what_do = "filter"     && �痰���→� 筥�赳��

    DO Set_Flt
    what_do = "List"

  CASE what_do = "delete"     && ����キ┘ 瘍モ�

    DO Del_Bill
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
SELECT BLS_9120
USE

SELECT CUS_9120
USE

SELECT F_9120
USE

SELECT B_9120
USE

SELECT LNM_9120
USE

SELECT WHE_9120
USE

SELECT WHO_9120
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
PRIVATE w1, w2

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-4, 21 SAY F_9120.B_NAME COLOR (SCHEME(13,6))
@ WROWS()-4, 51 SAY B_9120.B_NAME COLOR (SCHEME(13,6))
@ WROWS()-3, 11 SAY IIF(BLS_9120.VALUTA = "$", "� �", "�磧") COLOR (SCHEME(13,6))
@ WROWS()-3, 31 SAY BLS_9120.STATUS COLOR (SCHEME(13,6))
@ WROWS()-3, 48 SAY IIF(BLS_9120.OLD, "���祀�牀���!", "            ") COLOR (SCHEME(13,6))
IF FOUND("LNM_9120")
  @ WROWS()-3, COL()+1 SAY "��. ━� メ碎"
ELSE
  @ WROWS()-3, COL()+1 SAY "            "
ENDIF
@ WROWS()-3, 48 SAY IIF(BLS_9120.OLD, "���祀�牀���!", "            ") COLOR (SCHEME(13,6))
DO CASE
CASE BLS_9120.WHO = 0
  w1 = "���!"
CASE .NOT. FOUND("WHO_9120")
  w1 = "�エБメ皚覃!"
OTHERWISE
  w1 = ALLTRIM(WHO_9120.FAMILY)+" "+LEFT(WHO_9120.NAME,1)+". "+LEFT(WHO_9120.S_NAME,1)+"."
ENDCASE
w1 = PADR(w1,30)

DO CASE
CASE .NOT. FOUND("WHE_9120")
  w2 = "               "
OTHERWISE
  w2 = WHE_9120.ARM_N
ENDCASE

@ WROWS()-2, 20 SAY w1 COLOR (SCHEME(13,6))
@ WROWS()-2, 57 SAY w2 COLOR (SCHEME(13,6))
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Flt_Val      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���腑�┘ 筥�赳��.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.01.1999 陳�
PROCEDURE Flt_Val
IF pl_code # LEFT(BLS_9120.DOC_NUM,1)
  RETURN .F.
ENDIF
IF .NOT. show_p .AND. BLS_9120.STATUS = "ｯ��腑��  "
  RETURN .F.
ENDIF
IF .NOT. show_a .AND. BLS_9120.OLD
  RETURN .F.
ENDIF
IF .NOT. BLS_9120.FIRM = show_f
  RETURN .F.
ENDIF
IF .NOT. BLS_9120.BANK = show_b
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� V_Doc        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      �├���讚覃 ���ム ぎ�祠キ��.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.01.1999 陳�
PROCEDURE V_Doc
PARAMETER doc_n

RETURN LEFT(doc_n,4)+"-"+ALLTRIM(RIGHT(doc_n,6))

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Left_Cus     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        ｡爛������ ━� ��┘���.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.01.1999 陳�
PROCEDURE Left_Cus
PARAMETERS len_nam

PRIVATE nm_w
IF EMPTY(BLS_9120.CUS_CODE) .OR. .NOT. FOUND("CUS_9120")
  nm_w = ALLTRIM(BLS_9120.CUS_NAME)
ELSE
  nm_w = ALLTRIM(CUS_9120.CUS_NAME)
ENDIF
IF LEN(nm_w) > len_nam
  nm_w = LEFT(nm_w, len_nam-1)+""
ELSE
  nm_w = PADR(nm_w, len_nam)
ENDIF
RETURN nm_w

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      蹍������ �襦��� �����お�.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE G_Pl

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

PRIVATE ex, pl_w, ln, wd, c1, c2
                       &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1               &&
pl_w = 1               &&
USE (base_path+"PLACES")
ln = RECCOUNT()+5
wd = MAX(38, FSIZE("P_NAME")+12)
c1 = FLOOR((wd-FSIZE("P_NAME")-4)/2)
c2 = FLOOR((wd-32)/2)

*    �襦ム�皀 �����お�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�  ( ) ç��お� 1                  �
*�  ( ) ç��お� 2                  �
*�  ..............                  �
*�  ( ) ç��お� N                  �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, "�襦ム�皀 �����お�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO G_Place WITH 2, c1, pl_w
@ WROWS()-2, c2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  GO pl_w
  pl_w = P_CHAR
ELSE
  pl_w = ""
ENDIF
USE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN pl_w

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Flt      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �痰���→� 筥�赳��.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.01.1999 陳�
PROCEDURE Set_Flt
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�  [ ] ｯ��腑��襯   [ ] ���祀�牀����襯  �
*�                                       �
*� 踳く爬閧┘ 999 9999999999999999 (F1) �
*�                                       �
*�       [ ] ���� 9999999999999999       �
*�                                       �
*�   < OK Ctrl-W > < �皖���碎瘴 Esc >    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, w_p, w_a
PRIVATE f_c000, f_n000, b_c000, b_n000, b_sw000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 43, "��┘��"
w_p = show_p
w_a = show_a
f_c000 = PADR(show_f,3)
f_n000 = "                "
b_c000 = PADR(show_b,3)
b_n000 = "                "
b_sw000= .F.
f_c_old= f_c000
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2,  4 GET w_p PICTURE "@*C ｯ��腑��襯"
@ 2, 21 GET w_a PICTURE "@*C ���祀�牀����襯"
DO Get_Firm WITH 4, 3, enab_firm
DO Get_Bank WITH 6, 9, enab_bank
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  show_p = w_p
  show_a = w_a
  show_f = IIF(EMPTY(f_c000), "", f_c000)
  show_b = IIF(EMPTY(b_c000), "", b_c000)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Del_Bill     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            ����キ┘ 瘍モ�.                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.01.1999 陳�
PROCEDURE Del_Bill

PRIVATE k00, mss, s_sav, arr_w

IF EMPTY(INC_DOC) .AND. .NOT. OLD
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" �皰� 瘍モ �� ����腑� � �� ���祀�牀���! ����錺�? [ �モ ] [ �� ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 1
    RETURN
  ENDIF
ENDIF

k00 = FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM
SCATTER TO arr_w BLANK
GATHER FROM arr_w

s_sav = SELECT()
SELECT 0
USE (base_path+"BILLS_D") ORDER TAG DOC_NUM ALIAS B_D_9120 AGAIN
SCATTER TO arr_w BLANK
DO WHILE SEEK(k00)
  GATHER FROM arr_w
ENDDO
USE
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_Cus     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ���瓷 �� ��┘�矣.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.01.1999 陳�
PROCEDURE Seek_Cus
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��゛�� か� ���瓷� 陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�       < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, seek_sh, nm_w, ret
seek_sh = SPACE(40)
ex = 1
ret = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 50, "���瓷 瘍モ� �� ��┘�矣"
DO Sun_Bord WITH  2,  3,  4, 46, " ��゛�� か� ���瓷� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3, 5 GET seek_sh
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(seek_sh)
  *
  * �矗���硅��ガ ゛���
  seek_sh = ALLTRIM(SYS(15,lwr,seek_sh))
  PRIVATE r_sav
  r_sav = RECNO()
  DO Wt_Mess WITH "�薀�..."
  SELECT LINK
  ZAP
  SELECT BLS_9120
  SCAN
    IF EMPTY(BLS_9120.CUS_CODE) .OR. .NOT. FOUND("CUS_9120")
      nm_w = ALLTRIM(BLS_9120.CUS_NAME)
    ELSE
      nm_w = ALLTRIM(CUS_9120.CUS_NAME)
    ENDIF
    IF seek_sh $ SYS(15, lwr, nm_w)
      SELECT LINK
      APPEND BLANK
      REPLACE LNK WITH RECNO("BLS_9120")
      SELECT BLS_9120
    ENDIF
  ENDSCAN
  ret = RECCOUNT("LINK") # 0
  IF BETWEEN(r_sav,1,RECCOUNT())
    GO r_sav
  ENDIF
  DO Wt_Mess
  IF .NOT. ret
    PRIVATE mss33
    DIMENSION mss33(3)
    mss33(1) = ""
    mss33(2) = CHR(0)+"��腑�� �� ���ぅ��..."
    mss33(3) = ""
    DO Out_Mess WITH 7, "mss33"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ret

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Last      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍甃�矗 ���ぅ����� � �襦��.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.01.1999 陳�
PROCEDURE Ch_Last
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

IF RECCOUNT("LINK") = 0
  PRIVATE mss22
  DIMENSION mss22(3)
  mss22(1) = ""
  mss22(2) = CHR(0)+"��腑�� �� °�� ���ぅ��"
  mss22(3) = ""
  DO Out_Mess WITH 7, "mss22"
  RETURN
ENDIF
  
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

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit   OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit   OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, r_sav
ln = MIN(WROWS("")-11, RECCOUNT("LINK"))   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 72
*��爼陳N ぎ�-��陳陳����陳陳陳陳陳��祠��陳���┘�眥陳陳陳陳陳陳陳陳陳陳陳陳朕
*���� ����������� ��.��.���� 9_999_999.99 ���������������������������������
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�       踳く爬閧┘ 9999999999999999         ���� 9999999999999999       �
*�       �え��肓           ��痰�鎰┘                  ���祀�牀���!        �
*� �皰 �覩���� 瘍モ 999999999999999999999999999999   ��� 9999999999999999 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
DO D_Wins WITH ln, wd, "�ェ祀赳�� ���瓷�...", 0, 3
@ 1, 2 SAY "�爼陳N ぎ�-��陳陳����陳陳陳陳陳��祠��陳���┘��"
@ WROWS()-4, 9 SAY "踳く爬閧┘                          ����"
@ WROWS()-3, 9 SAY "�え��肓           ��痰�鎰┘"
@ WROWS()-2, 3 SAY "�皰 �覩���� 瘍モ                                  ���"

what_do = "List"
r_sav = RECNO()
SET ORDER TO
SELECT LINK
SET RELATION TO LNK INTO BLS_9120
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS BLS_9120.FIRM:3:H="",  ;
                  F000 = V_Doc(BLS_9120.DOC_NUM):11:H="", ;
                  BLS_9120.DOC_DATE:H="",                 ;
                  BLS_9120.BILL_SUM:H="":P="9 999 999.99",;
                  F001 = Left_Cus(32):32:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    IF .NOT. Flt_Val()
      PRIVATE mss22
      DIMENSION mss22(4)
      mss22(1) = ""
      mss22(2) = CHR(0)+"�襦���覃 ���� 瘍モ �ウ�� �� �爛ぅ���� 筥�赳��"
      mss22(3) = CHR(0)+"┼� 甌Г�� �� む磽�� �����お�!"
      mss22(4) = ""
      DO Out_Mess WITH 7, "mss22"
      what_do = "List"
    ELSE
      SET RELATION TO
      EXIT
    ENDIF

  OTHERWISE

    SET RELATION TO
    IF BETWEEN(r_sav, 1, RECCOUNT("BLS_9120"))
      GO r_sav IN BLS_9120
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
SELECT BLS_9120
SET ORDER TO HRON

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_Sum     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            ���瓷 �� 痺���.                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.01.1999 陳�
PROCEDURE Seek_Sum
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�   �祠�� か� ���瓷� 9 999 999.99   �
*�                                   �
*� < OK Ctrl-W > < �皖���碎瘴 Esc >  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, seek_sum, ret
seek_sum = 0
ex = 1
ret = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 39, "���瓷 瘍モ� �� 痺���"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2, 5 SAY "�祠�� か� ���瓷�" GET seek_sum PICTURE "@Z 9 999 999.99"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(seek_sum)
  *
  * �矗���硅��ガ ゛���
  PRIVATE r_sav
  r_sav = RECNO()
  DO Wt_Mess WITH "�薀�..."
  SELECT LINK
  ZAP
  SELECT BLS_9120
  SCAN
    IF seek_sum = BILL_SUM
      SELECT LINK
      APPEND BLANK
      REPLACE LNK WITH RECNO("BLS_9120")
      SELECT BLS_9120
    ENDIF
  ENDSCAN
  ret = RECCOUNT("LINK") # 0
  IF BETWEEN(r_sav,1,RECCOUNT())
    GO r_sav
  ENDIF
  DO Wt_Mess
  IF .NOT. ret
    PRIVATE mss33
    DIMENSION mss33(3)
    mss33(1) = ""
    mss33(2) = CHR(0)+"��腑�� �� ���ぅ��..."
    mss33(3) = ""
    DO Out_Mess WITH 7, "mss33"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ret
