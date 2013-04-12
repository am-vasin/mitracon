*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Payment      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                             ���� ������.                               �
*�                                                                        �
*����������������������������������������������������������� 20.01.1999 �ͼ
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
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
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
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4
DEFINE PAD Pay OF (menu_name) PROMPT "�����"
ON PAD Pay OF (menu_name) ACTIVATE POPUP Pay
  DEFINE POPUP Pay MARGIN RELATIVE SHADOW COLOR SCHEME 4
  DEFINE BAR  1 OF Pay PROMPT "�।���⥫�� ����� " KEY Ctrl-A, "F3"
  ON SELECTION BAR 1 OF Pay DO Sw_Mode WITH "prv_data"
  DEFINE BAR  2 OF Pay PROMPT "�� ���� " KEY Ctrl-A, "Enter"
  ON SELECTION BAR 2 OF Pay DO Sw_Mode WITH "int_pay"
  DEFINE BAR  3 OF Pay PROMPT "��� ��� " KEY Ctrl-A, "Ins"
  ON SELECTION BAR 3 OF Pay DO Sw_Mode WITH "new_pay"
  DEFINE BAR  4 OF Pay PROMPT "����ᮢ� ���-䠪���� "
  ON SELECTION BAR 4 OF Pay DO Sw_Mode WITH "stopDoc"

DEFINE PAD Seek OF (menu_name) PROMPT "����"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek_B
  DEFINE POPUP Seek_B MARGIN RELATIVE SHADOW COLOR SCHEME 4
  DEFINE BAR  1 OF Seek_B PROMPT "�� ������� " KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek_B DO Sw_Mode WITH "seek_c"
  DEFINE BAR  2 OF Seek_B PROMPT "�� �㬬� " KEY Ctrl-A, "F8"
  ON SELECTION BAR 2 OF Seek_B DO Sw_Mode WITH "seek_s"
  DEFINE BAR  3 OF Seek_B PROMPT "��᫥���� ��������" KEY Ctrl-A, "F9"
  ON SELECTION BAR 3 OF Seek_B DO Sw_Mode WITH "seek_l"
  DEFINE BAR  4 OF Seek_B PROMPT "���� �� ����" && KEY Ctrl-A, "F9"
  ON SELECTION BAR 4 OF Seek_B DO Sw_Mode WITH "seek_b"

DEFINE PAD Cust OF (menu_name) PROMPT "������"
ON PAD Cust OF (menu_name) ACTIVATE POPUP Cust
  DEFINE POPUP Cust MARGIN RELATIVE SHADOW COLOR SCHEME 4
  DEFINE BAR  4 OF Cust PROMPT "������� ��� "
  ON SELECTION BAR 4 OF Cust DO Sw_Mode WITH "LName"
  DEFINE BAR  1 OF Cust PROMPT "����� ������ " KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1OF Cust DO Sw_Mode WITH "CHist"
  DEFINE BAR  2 OF Cust PROMPT "��ਡ��� ������" KEY Ctrl-A, "Home"
  ON SELECTION BAR 2 OF Cust DO Sw_Mode WITH "CusBil"
  DEFINE BAR  3 OF Cust PROMPT "��થ⨭������ ����." KEY Ctrl-A, "Ctrl-Home"
  ON SELECTION BAR 3 OF Cust DO Sw_Mode WITH "CusInfo"

DEFINE PAD Print OF (menu_name) PROMPT "���" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "print"

DEFINE PAD Filter OF (menu_name) PROMPT "������" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "filter"

DEFINE PAD Delete OF (menu_name) PROMPT "�������" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "delete"

DEFINE PAD Exit   OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit   OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 72
*ڔ���N ���-����Ą��������������đ㬬���Ċ����������������������������Ŀ
*���� ����������� ��.��.���� 999_999_999.99 �������������������������������
*��������������������������������������������������������������������������
*������������������������������������������������������������������������Ŀ
*�       �।���⨥ 9999999999999999         ���� 9999999999999999       �
*� �������           ����ﭨ�                  ���㫨஢��!              �
*� �� �믨ᠫ ��� 999999999999999999999999999999   ��� 9999999999999999 �
*��������������������������������������������������������������������������
DO D_Wins WITH ln, wd, "���", 0, 3
@ 1, 2 SAY "����N ���-����Ą��������������đ㬬���Ċ�����"
@ WROWS()-4, 9 SAY "�।���⨥                          ����"
@ WROWS()-3, 3 SAY "�������           ����ﭨ�"
@ WROWS()-2, 3 SAY "�� �믨ᠫ ���                                  ���"
what_do = "List"

GO BOTTOM

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

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
*    BROWSE - ����
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

  CASE what_do = "int_pay"    && ���⥦ �� ����

    IF .NOT. EOF() .AND. .NOT. BOF()
      *
      *    �஢�ન
      *
      IF ALLTRIM(STATUS) = "������"
        PRIVATE mss_000
        DIMENSION mss_000(3)
        mss_000(1) = ""
        mss_000(2) = CHR(0)+"��� ��� ������㯥� ��� ����� ���⥦�!"
        mss_000(3) = ""
        DO Out_Mess WITH 7, "mss_000"
        what_do = "List"
        LOOP
      ENDIF
      IF ALLTRIM(STATUS) = "����祭�"
        PRIVATE mss_000
        DIMENSION mss_000(3)
        mss_000(1) = ""
        mss_000(2) = CHR(0)+"��� ��� 㦥 ����祭. �㤥� ������� ���⥦? [ ��� ] [ �� ]"
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
        mss_000(2) = CHR(0)+"��� ��� ���㫨஢��. �㤥� ������� ���⥦? [ ��� ] [ �� ]"
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

  CASE what_do = "new_pay"    && ���⥦ ��� ���

    DO IncMoney WITH "", "", "", tpList
    what_do = "List"

  CASE what_do = "stopDoc"    && ����ᮢ� ���-䠪����

    m.pay_bill = Set_PB(m.pay_bill)
    ACTIVATE WINDOW (win_name+"_M") SAME
    ll = FLOOR(WCOLS()-36)/2
    IF m.pay_bill
      @ WROWS()-5, ll SAY REPLICATE("�", 36) && COLOR (SCHEME(13,6))
    ELSE
      @ WROWS()-5, ll SAY "����ᮢ� ��� 䠪���� �� ᮧ������" COLOR SCHEME 7
    ENDIF
    what_do = "List"

  CASE what_do = "CHist"      && ����� ������

    IF .NOT. EMPTY(CUS_CODE)
      DO Acc_Hist WITH CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "LName"      && ������� ���

    IF .NOT. EMPTY(CUS_CODE)
      DO Cus_Name WITH CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "CusBil"     && ��ਡ��� ������

    IF .NOT. EMPTY(CUS_CODE)
      DO Cus_Bil WITH CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "CusInfo"    && ����窠 ��થ⨭��

    IF .NOT. EMPTY(CUS_CODE)
      DO Cli_Info WITH "", .F., CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "seek_c"     && ���� �� �������

    IF Seek_Cus()
      DO Ch_Last
    ENDIF
    what_do = "List"

  CASE what_do = "seek_b"     && ���� �� ����

    m.rec = Seek_Bl()
    IF m.rec # 0
      GO m.rec IN BLS_9120
    ENDIF
    what_do = "List"

  CASE what_do = "prv_data"   && �।���⥫�� ����� �� �����

    IF BLS_9120.STATUS = "����祭�  "
      PRIVATE mss000
      DIMENSION mss000(3)
      mss000(1) = ""
      mss000(2) = CHR(0)+"� �।���⥫�묨 ����묨 �� ��������: ��� ����祭!"
      mss000(3) = ""
      DO Out_Mess WITH 7, "mss000"
    ELSE
      REPLACE BLS_9120.STATUS  WITH "�।�.", ;
              BLS_9120.WHO_INC WITH user,     ;
              BLS_9120.DAT_INC WITH DATE()
    ENDIF
    what_do = "List"

  CASE what_do = "seek_s"     && ���� �� �㬬�

    IF Seek_Sum()
      DO Ch_Last
    ENDIF
    what_do = "List"

  CASE what_do = "seek_l"     && ��᫥���� ��������

    DO Ch_Last
    what_do = "List"

  CASE what_do = "print"      && ����� ���

    DO P_Bill WITH FIRM, DOC_NUM, DOC_DATE
    what_do = "List"

  CASE what_do = "filter"     && ��⠭���� 䨫���

    DO Set_Flt
    what_do = "List"

  CASE what_do = "delete"     && �������� ���

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
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Draw_Foot    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE Draw_Foot
PRIVATE w1, w2

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
@ WROWS()-4, 21 SAY F_9120.B_NAME COLOR (SCHEME(13,6))
@ WROWS()-4, 51 SAY B_9120.B_NAME COLOR (SCHEME(13,6))
@ WROWS()-3, 11 SAY IIF(BLS_9120.VALUTA = "$", "� �", "��") COLOR (SCHEME(13,6))
@ WROWS()-3, 31 SAY BLS_9120.STATUS COLOR (SCHEME(13,6))
@ WROWS()-3, 48 SAY IIF(BLS_9120.OLD, "���㫨஢��!", "            ") COLOR (SCHEME(13,6))
IF FOUND("LNM_9120")
  @ WROWS()-3, COL()+1 SAY "��. ��� ����"
ELSE
  @ WROWS()-3, COL()+1 SAY "            "
ENDIF
@ WROWS()-3, 48 SAY IIF(BLS_9120.OLD, "���㫨஢��!", "            ") COLOR (SCHEME(13,6))
DO CASE
CASE BLS_9120.WHO = 0
  w1 = "���!"
CASE .NOT. FOUND("WHO_9120")
  w1 = "���������!"
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Flt_Val      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ���祭�� 䨫���.                            �
*�                                                                        �
*����������������������������������������������������������� 20.01.1999 ���
PROCEDURE Flt_Val
IF pl_code # LEFT(BLS_9120.DOC_NUM,1)
  RETURN .F.
ENDIF
IF .NOT. show_p .AND. BLS_9120.STATUS = "����祭�  "
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_Doc        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ���㠫�� ����� ���㬥��.                       �
*�                                                                        �
*����������������������������������������������������������� 20.01.1999 ���
PROCEDURE V_Doc
PARAMETER doc_n

RETURN LEFT(doc_n,4)+"-"+ALLTRIM(RIGHT(doc_n,6))

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Left_Cus     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                        ��१����� ��� ������.                         �
*�                                                                        �
*����������������������������������������������������������� 20.01.1999 ���
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

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                      �ணࠬ�� �롮� ���頤��.                       �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE G_Pl

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

PRIVATE ex, pl_w, ln, wd, c1, c2
                       &&   ���塞 � ������塞 ���� ������
ex   = 1               &&
pl_w = 1               &&
USE (base_path+"PLACES")
ln = RECCOUNT()+5
wd = MAX(38, FSIZE("P_NAME")+12)
c1 = FLOOR((wd-FSIZE("P_NAME")-4)/2)
c2 = FLOOR((wd-32)/2)

*    �롥�� ���頤��
*����������������������������������Ŀ
*�  ( ) ���頤�� 1                  �
*�  ( ) ���頤�� 2                  �
*�  ..............                  �
*�  ( ) ���頤�� N                  �
*�                                  �
*� < OK Ctrl-W > < �⪠������ Esc > �
*������������������������������������

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, "�롥�� ���頤��"

*------------------------------------------------------------------------
*      ���� ����� ������
*
DO G_Place WITH 2, c1, pl_w
@ WROWS()-2, c2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Set_Flt      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ��⠭���� 䨫���.                           �
*�                                                                        �
*����������������������������������������������������������� 20.01.1999 ���
PROCEDURE Set_Flt
*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*���������������������������������������Ŀ
*�  [ ] ����祭��   [ ] ���㫨஢����  �
*�                                       �
*� �।���⨥ 999 9999999999999999 (F1) �
*�                                       �
*�       [ ] ���� 9999999999999999       �
*�                                       �
*�   < OK Ctrl-W > < �⪠������ Esc >    �
*�����������������������������������������

PRIVATE ex, w_p, w_a
PRIVATE f_c000, f_n000, b_c000, b_n000, b_sw000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyz������񦧨�����������������������"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 10, 43, "������"
w_p = show_p
w_a = show_a
f_c000 = PADR(show_f,3)
f_n000 = "                "
b_c000 = PADR(show_b,3)
b_n000 = "                "
b_sw000= .F.
f_c_old= f_c000
*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 2,  4 GET w_p PICTURE "@*C ����祭��"
@ 2, 21 GET w_a PICTURE "@*C ���㫨஢����"
DO Get_Firm WITH 4, 3, enab_firm
DO Get_Bank WITH 6, 9, enab_bank
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1
  *
  * ��ࠡ��뢠�� �����
  show_p = w_p
  show_a = w_a
  show_f = IIF(EMPTY(f_c000), "", f_c000)
  show_b = IIF(EMPTY(b_c000), "", b_c000)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Del_Bill     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            �������� ���.                             �
*�                                                                        �
*����������������������������������������������������������� 20.01.1999 ���
PROCEDURE Del_Bill

PRIVATE k00, mss, s_sav, arr_w

IF EMPTY(INC_DOC) .AND. .NOT. OLD
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ��� ��� �� ����祭 � �� ���㫨஢��! ����塞? [ ��� ] [ �� ]"
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_Cus     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ���� �� �������.                            �
*�                                                                        �
*����������������������������������������������������������� 20.01.1999 ���
PROCEDURE Seek_Cus
*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*����������������������������������������������Ŀ
*� �� ������ ��� ���᪠ ���������������������Ŀ �
*� � ....:....!....:....!....:....!....:....! � �
*� �������������������������������������������� �
*�       < OK Ctrl-W > < �⪠������ Esc >       �
*������������������������������������������������

PRIVATE ex, seek_sh, nm_w, ret
seek_sh = SPACE(40)
ex = 1
ret = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 50, "���� ��� �� �������"
DO Sun_Bord WITH  2,  3,  4, 46, " ������ ��� ���᪠ "

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 3, 5 GET seek_sh
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(seek_sh)
  *
  * ��ࠡ��뢠�� �����
  seek_sh = ALLTRIM(SYS(15,lwr,seek_sh))
  PRIVATE r_sav
  r_sav = RECNO()
  DO Wt_Mess WITH "�饬..."
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
    mss33(2) = CHR(0)+"��祣� �� �������..."
    mss33(3) = ""
    DO Out_Mess WITH 7, "mss33"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ret

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Ch_Last      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ��ᬮ�� ���������� � �롮�.                      �
*�                                                                        �
*����������������������������������������������������������� 20.01.1999 ���
PROCEDURE Ch_Last
*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

IF RECCOUNT("LINK") = 0
  PRIVATE mss22
  DIMENSION mss22(3)
  mss22(1) = ""
  mss22(2) = CHR(0)+"��祣� �� �뫮 �������"
  mss22(3) = ""
  DO Out_Mess WITH 7, "mss22"
  RETURN
ENDIF
  
*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit   OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit   OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, r_sav
ln = MIN(WROWS("")-11, RECCOUNT("LINK"))   && ������⢮ ������� ��ப BROWSE
wd = 72
*ڔ���N ���-����Ą������������đ㬬���Ċ������������������������������Ŀ
*���� ����������� ��.��.���� 9_999_999.99 ���������������������������������
*��������������������������������������������������������������������������
*������������������������������������������������������������������������Ŀ
*�       �।���⨥ 9999999999999999         ���� 9999999999999999       �
*�       �������           ����ﭨ�                  ���㫨஢��!        �
*� �� �믨ᠫ ��� 999999999999999999999999999999   ��� 9999999999999999 �
*��������������������������������������������������������������������������
DO D_Wins WITH ln, wd, "������� ���᪠...", 0, 3
@ 1, 2 SAY "����N ���-����Ą������������đ㬬���Ċ�����"
@ WROWS()-4, 9 SAY "�।���⨥                          ����"
@ WROWS()-3, 9 SAY "�������           ����ﭨ�"
@ WROWS()-2, 3 SAY "�� �믨ᠫ ���                                  ���"

what_do = "List"
r_sav = RECNO()
SET ORDER TO
SELECT LINK
SET RELATION TO LNK INTO BLS_9120
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
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

  CASE what_do = "Choose"    && �롮�

    IF .NOT. Flt_Val()
      PRIVATE mss22
      DIMENSION mss22(4)
      mss22(1) = ""
      mss22(2) = CHR(0)+"��࠭� ���� ��� ����� �� �।����� 䨫���"
      mss22(3) = CHR(0)+"��� ᮧ��� �� ��㣮� ���頤��!"
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
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT BLS_9120
SET ORDER TO HRON

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_Sum     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            ���� �� �㬬�.                             �
*�                                                                        �
*����������������������������������������������������������� 20.01.1999 ���
PROCEDURE Seek_Sum
*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*�����������������������������������Ŀ
*�   �㬬� ��� ���᪠ 9 999 999.99   �
*�                                   �
*� < OK Ctrl-W > < �⪠������ Esc >  �
*�������������������������������������

PRIVATE ex, seek_sum, ret
seek_sum = 0
ex = 1
ret = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 6, 39, "���� ��� �� �㬬�"

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 2, 5 SAY "�㬬� ��� ���᪠" GET seek_sum PICTURE "@Z 9 999 999.99"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(seek_sum)
  *
  * ��ࠡ��뢠�� �����
  PRIVATE r_sav
  r_sav = RECNO()
  DO Wt_Mess WITH "�饬..."
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
    mss33(2) = CHR(0)+"��祣� �� �������..."
    mss33(3) = ""
    DO Out_Mess WITH 7, "mss33"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ret