*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ ��｀硅 甌 瘍モ���.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Bills

PRIVATE c_dat, c_stat, flt1, flt2, flt3, rc, pl_code, max_ord

max_ord = 0
IF File_O(base_path+"MAX_ORD.MEM")
  RESTORE FROM (base_path+"MAX_ORD.MEM") ADDITIVE
ENDIF

pl_code = G_Pl()
IF EMPTY(pl_code)
  RETURN
ENDIF

*
*    PAD - �キ� �ム〓�� 窶�↓�
*
DEFINE MENU B_Sale IN SCREEN COLOR SCHEME 4
DEFINE PAD Pay    OF B_Sale PROMPT "ｯ����"
DEFINE PAD Seek   OF B_Sale PROMPT "���瓷"
DEFINE PAD Print  OF B_Sale PROMPT "�ョ�碎 瘍モ�"
DEFINE PAD Filter OF B_Sale PROMPT "�┼赳�" KEY Ctrl-A, "F6"
DEFINE PAD Delete OF B_Sale PROMPT "�����碎" KEY Ctrl-A, "Del"
DEFINE PAD Exit   OF B_Sale PROMPT "�諷��" KEY Ctrl-A, "Esc"

ON PAD Pay  OF B_Sale ACTIVATE POPUP Pay
ON PAD Seek OF B_Sale ACTIVATE POPUP Seek_B
ON SELECTION PAD Print  OF B_Sale DO Ch_Mode WITH "B_Sale","print"
ON SELECTION PAD Filter OF B_Sale DO Ch_Mode WITH "B_Sale","filter"
ON SELECTION PAD Delete OF B_Sale DO Del_Bill
ON SELECTION PAD Exit   OF B_Sale DO Ex_Proc WITH "B_Sale"

*
*    POPUP - �キ� ≒�牀�� 窶�↓�
*
DEFINE POPUP Pay MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Pay PROMPT "踳あ�爬皀�讚襯 ����襯 " KEY Ctrl-A, "F3"
DEFINE BAR  2 OF Pay PROMPT "�� 瘍モ� " KEY Ctrl-A, "Enter"
DEFINE BAR  3 OF Pay PROMPT "�ェ 瘍モ� " KEY Ctrl-A, "Ins"
ON SELECTION BAR 1 OF Pay DO Set_Prev
ON SELECTION BAR 2 OF Pay DO Ch_Mode WITH "B_Sale","int_pay"
ON SELECTION BAR 3 OF Pay DO Ch_Mode WITH "B_Sale","new_pay"

DEFINE POPUP Seek_B MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Seek_B PROMPT "�� ��┘�矣 " KEY Ctrl-A, "F7"
DEFINE BAR  2 OF Seek_B PROMPT "�� 痺��� " KEY Ctrl-A, "F8"
ON SELECTION BAR 1 OF Seek_B DO Ch_Mode WITH "B_Sale","seek_c"
ON SELECTION BAR 2 OF Seek_B DO Ch_Mode WITH "B_Sale","seek_s"

HIDE MENU Main_M
DO Entry_P WITH "", "B_Sale"

USE ACCOUNT ORDER TAG CUS_CODE

DO Use_Dummy
DO Use_Link

SELECT 0
USE BILLS ORDER TAG HRON
SET FILTER TO LEFT(DOC_NUM,1) = pl_code
SET RELATION TO CUS_CODE INTO ACCOUNT
GO BOTTOM
*IF BOF() .OR. EOF()
*  PRIVATE mss
*  DIMENSION mss(3)
*  mss(1) = ""
*  mss(2) = CHR(0)+"�� 磴���瘡 ���皋 �� �き��� 瘍モ�!"
*  mss(3) = ""
*  DO Out_Mess WITH 7, "mss"
*  
*  CLOSE DATABASES
*
*  DO Exit_P WITH "", "B_Sale"
*  RETURN
*ENDIF

c_dat  = 0
c_stat = 0
flt1   = .T.
flt2   = .T.
flt3   = .T.
DO Prep_W

what_do = "b_list"
DO WHILE .T.

  DO CASE
  CASE what_do = "b_list"     && ���甌� 瘍モ��
    ON KEY LABEL F10 DO F10_Proc WITH "B_Sale"
    ON KEY LABEL F3    DO Set_Prev
    ON KEY LABEL Del   DO Del_Bill
    ON KEY LABEL Enter DO Ch_Mode WITH "B_Sale","int_pay"
    ON KEY LABEL Ins   DO Ch_Mode WITH "B_Sale","new_pay"
    ON KEY LABEL F6    DO Ch_Mode WITH "B_Sale","filter"
    ON KEY LABEL F7    DO Ch_Mode WITH "B_Sale","seek_c"
    ON KEY LABEL F8    DO Ch_Mode WITH "B_Sale","seek_s"
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "B_Sale"
    ACTIVATE WINDOW Bills_E
    BROWSE FOR Fltr(flt1, flt2, flt3)   ;
           FIELDS f001 = Doc_N():11:H="" , ;
                  BILL_SUM:H="",  ;
                  f002 = IIF(FOUND("ACCOUNT"), ACCOUNT.CUS_NAME ,  ;
                  BILLS.CUS_NAME):50:H= "", DUMMY.F:H=""   ;
           FREEZE DUMMY.F  ;
           WHEN Show_D()   ;
           NOMENU NORGRID NOAPPEND NODELETE NOMODIFY REST NOCLEAR ;
           WINDOW Bills_I IN WINDOW Bills_E

    ON KEY
    IF what_do = "b_list"
      what_do = "exit"
    ENDIF
    ON KEY
  CASE what_do = "seek_c"     && �〓� ���皀�痰� か� ���瓷� ��┘���
    what_do = IIF(Prep_C(), "choose", "b_list")
  CASE what_do = "seek_s"     && �〓� 痺�� か� ���瓷� �� 痺���
    what_do = IIF(Prep_S(), "choose", "b_list")
  CASE what_do = "choose"     && �襦�� ├ ���ぅ�����
    DO Choose
    what_do = "b_list"
  CASE what_do = "filter"     && �痰���→� 筥�赳��
    DO Set_Flt
    what_do = "b_list"
  CASE what_do = "int_pay"    && �〓� ���皀Κ� �� 瘍モ�
    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Inp_Pay WITH "B_Sale", .T.
    ENDIF
    what_do = "b_list"
  CASE what_do = "new_pay"    && �〓� ���皀Κ� ．� 瘍モ�
    DO Inp_Pay WITH "B_Sale", .F.
    what_do = "b_list"
  CASE what_do = "print"    && �ョ�碎 瘍モ�
    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Bill
    ENDIF
    what_do = "b_list"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW Bills_M
RELEASE WINDOW Bills_E
RELEASE WINDOW Bills_I
CLOSE DATABASES

DO Exit_P WITH "", "B_Sale"
RETURN

*************************************************************************
PROCEDURE Doc_N
RETURN LEFT(BILLS.DOC_NUM,4)+"-"+ALLTRIM(RIGHT(BILLS.DOC_NUM,6))

*************************************************************************
PROCEDURE Show_D

IF FOUND("ACCOUNT")
  IF .NOT. EMPTY(ACCOUNT.DATE_OFF)
    WAIT " ��������! ��┘�� 磴��キ! " WINDOW NOWAIT
  ENDIF
ENDIF
ACTIVATE WINDOW Bills_M BOTTOM
@ WROWS()-1, c_dat  SAY TRANSFORM(BILLS.DOC_DATE,"@D") COLOR SCHEME 1
@ WROWS()-1, c_stat SAY BILLS.STATUS COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   蹍������ ��ぃ�皰→� ����.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prep_W

PRIVATE l, c, ln, wd, tit1, tit2, foot
DIMENSION tit2(1)
*
*   �甅�↓�� �ムョキ� ���ォ か� BROWSE
*
tit2(1) = PADC("�N �腑��",FSIZE("DOC_NUM" ,"BILLS")+1,"�")+"�"+ ;
          PADC("陳�祠��",FSIZE("BILL_SUM" ,"BILLS"),"�")+"�"+ ;
          PADC("��┘�眥陳陳陳�",FSIZE("CUS_NAME" ,"BILLS"),"�")
ln = WROWS("")-7
tit1 = "���甌� 瘍モ��"
DIMENSION foot(2)
foot(1) = ""
foot(2) = "����                     ��痰�鎰┘ 0123456789"

DO Def_Win WITH "Bills_M", "Bills_E", "Bills_I", ln, LEN(tit2(1)), tit1, tit2, foot
c = PADC(foot(2),WCOLS())
@ WROWS()-1, 0 SAY c
c_dat  = AT("����", c)+4
c_stat = AT("0123456789", c)-1

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�        蹍������ - �÷�｀砒┴ �謌�� か� 甎�瓷��.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE M_Proc_1
PARAMETER menu_nm
PRIVATE mc, mr

*
*  �瓱�縲����� �キ�
*
IF MENU() == UPPER(menu_nm)
  RETURN
ENDIF

IF MROW("") = 0
  ACTIVATE MENU (menu_nm)
  RETURN
ENDIF

*
*  ｪ�� BROWSE
*
mc = MCOL("Bills_E")
mr = MROW("Bills_E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  �甅�↓�� ����
*
mc = MCOL("Bills_M")
mr = MROW("Bills_M")
IF mc >= 0 .AND. mr >= 0
  IF mr = 1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WROWS("Bills_M")-2
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ 竅����→� 筥�赳��.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Set_Flt

PRIVATE f1, f2, f3, ex, rc_sav

DO Entry_P WITH "B_Sale", ""

f1 = flt1
f2 = flt2
f3 = flt3
ex = 1

rc_sav = RECNO()

ON KEY LABEL LeftMouse DO Ms_Lock
DO D_W WITH "Filter", 8, 67, "�┼赳�", 1

@ 3, 5 GET f1 PICTURE "@*C �ギ���腑��襯"
@ 3,24 GET f2 PICTURE "@*C 踳あ. ����襯"
@ 3,44 GET f3 PICTURE "@*C ｯ��腑��襯 瘍モ�"
@ 5,15 GET ex PICTURE "@*HT \! ��縲���碎 Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Empt_List()

RELEASE WINDOW Filter

DO Exit_P WITH "B_Sale", ""

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ ����←キ�� �謌�� ↓� ����.                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Ms_Lock

IF MROW() < 0 .OR. MCOL() < 0
  = INKEY("M")
ENDIF
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                         �┼赳� 瘍モ��.                                �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Fltr
PARAMETERS f01, f02, f03

DO CASE
CASE EMPTY(BILLS.STATUS)
  RETURN f01
CASE ALLTRIM(BILLS.STATUS) = "ｯ��腑��"
  RETURN f03
OTHERWISE
  RETURN f02
ENDCASE

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                     � �� �竅� �� 甎�甌�?                              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Empt_List

IF ex = 2
  RETURN .T.
ENDIF

PRIVATE err_m
IF .NOT. f1 .AND. .NOT. f2 .AND. .NOT. f3
  DIMENSION err_m(4)
  err_m(1) = ""
  err_m(2) = CHR(0)+"��茱 Ε���┘ ��腑�� �� ※ぅ碎 ���閧�� � ≡� Ε..."
  err_m(3) = CHR(0)+"�襦ム�皀 絎碎 艪�-�─磴�."
  err_m(4) = ""
  DO Out_Mess WITH 7, "err_m"
  RETURN .F.
ENDIF
IF .NOT. Fltr(f1, f2, f3)
  LOCATE FOR Fltr(f1, f2, f3)
  IF .NOT. FOUND()
    GO rc_sav
    DIMENSION err_m(3)
    err_m(1) = ""
    err_m(2) = CHR(0)+"�モ 瘍モ��, 甌�癶モ痰≪鉗�� ��茱�� 竅��※�."
    err_m(3) = ""
    DO Out_Mess WITH 7, "err_m"
    RETURN .F.
  ENDIF
ENDIF
flt1 = f1
flt2 = f2
flt3 = f3

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                          ����キ┘ 瘍モ�.                              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Del_Bill

DELETE
DEACTIVATE MENU B_Sale
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          �痰���→�/甅閧┘ ���モ�� � �爛あ. 瓣イキ�閻.                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Set_Prev

DO CASE
CASE EMPTY(BILLS.STATUS)
  REPLACE BILLS.STATUS WITH "踳あ."
  = Show_D()
CASE ALLTRIM(BILLS.STATUS) = "ｯ��腑��"
  PRIVATE err_m
  DIMENSION err_m(3)
  err_m(1) = ""
  err_m(2) = CHR(0)+"���┘ �爛あ�爬皀�讚襯 ����襯? �腑� 礒� ����腑�!"
  err_m(3) = ""
OTHERWISE
  REPLACE BILLS.STATUS WITH "   "
  = Show_D()
ENDCASE

DEACTIVATE MENU B_Sale

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ ��ぃ�皰→� ����� LINK か� ��┘���.             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prep_C

PRIVATE shab, ex, rc_sav, nm

DO Entry_P WITH "B_Sale", ""

ex = 1
rc_sav = RECNO()
shab = SPACE(50)

ON KEY LABEL LeftMouse DO Ms_Lock
DO D_W WITH "Seek_W", 8, 76, "���瓷 瘍モ� �� ��┘�矣", 1

@ 3, 4 SAY "��゛�� か� ���瓷�" GET shab
@ 5,20 GET ex PICTURE "@*HT \! �瓷�碎 Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

RELEASE WINDOW Seek_W

IF EMPTY(shab) .OR. READKEY()%256 = 12
  ex = 2
ENDIF

IF ex = 1
  SELECT LINK
  ZAP
  DO Wt_Mess WITH "�薀�..."
  shab = ALLTRIM(shab)
  shab = SYS(15,lwr,shab)
  SELECT BILLS
  SCAN 
    IF .NOT. Fltr(flt1, flt2, flt3)
      LOOP
    ENDIF
    nm = IIF(FOUND("ACCOUNT"), ACCOUNT.CUS_NAME , BILLS.CUS_NAME)
    nm = SYS(15,lwr,nm)
    IF shab $ nm
      SELECT LINK
      APPEND BLANK
      REPLACE LNK WITH RECNO("BILLS")
      SELECT BILLS
    ENDIF
  ENDSCAN
  GO rc_sav
  DO Wt_Mess
  IF RECCOUNT("LINK") = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��腑�� �� ���ぅ��!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    ex = 2
  ENDIF
ENDIF

DO Exit_P WITH "B_Sale", ""

RETURN ex = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�           蹍������ ��ぃ�皰→� ����� LINK か� 痺��.                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prep_S

PRIVATE sum0, sum1, ex, rc_sav, pic

DO Entry_P WITH "B_Sale", ""

ex = 1
rc_sav = RECNO()
sum0 = 0
sum1 = 0
pic = "@Z 9999999.99"

ON KEY LABEL LeftMouse DO Ms_Lock
DO D_W WITH "Seek_W", 8, 42, "���瓷 瘍モ� �� 痺���", 1

@ 3, 5 SAY "�祠�� �" GET sum0 PICTURE pic
@ 3,24 SAY "��"      GET sum1 PICTURE pic

@ 5,3 GET ex PICTURE "@*HT \! �瓷�碎 Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID V_Sum()

RELEASE WINDOW Seek_W

IF ex = 2 .OR. READKEY()%256 = 12
  ex = 2
ENDIF

IF ex = 1
  SELECT LINK
  ZAP
  DO Wt_Mess WITH "�薀�..."
  SELECT BILLS
  SCAN
    IF .NOT. Fltr(flt1, flt2, flt3) .OR. .NOT. BETWEEN(BILL_SUM, sum0, sum1)
      LOOP
    ENDIF
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("BILLS")
    SELECT BILLS
  ENDSCAN
  GO rc_sav
  DO Wt_Mess
  IF RECCOUNT("LINK") = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��腑�� �� ���ぅ��!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    ex = 2
  ENDIF
ENDIF

DO Exit_P WITH "B_Sale", ""

RETURN ex = 1

***************************************************************************
PROCEDURE V_Sum
PRIVATE mss

IF ex = 2 .OR. READKEY()%256 = 12
  ex = 2
  RETURN .T.
ENDIF

IF sum0 < 0 .OR. sum1 < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�矗���皀�讚�� 痺���? ��� �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF sum0 > sum1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�����讚�� 痺��� ｀�跏� ���ョ���? �皰-皰 Гメ� �� ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�            蹍������ �襦��� ├ 痲�爼�牀������� 甎�瓷�.                �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Choose

PRIVATE rc

rc = RECNO()

DEFINE MENU Choose IN SCREEN COLOR SCHEME 4
DEFINE PAD Select OF Choose PROMPT "�襦��" KEY Ctrl-A, "Enter"
DEFINE PAD Abort  OF Choose PROMPT "�皖��" KEY Ctrl-A, "Esc"

*ON SELECTION PAD Select OF Choose KEYBOARD CHR(27)+CHR(13)
*ON SELECTION PAD Abort  OF Choose KEYBOARD CHR(27)+CHR(27)
ON SELECTION PAD Select OF Choose DO Out_Menu WITH CHR(13)
ON SELECTION PAD Abort  OF Choose DO Out_Menu WITH CHR(27)


SET ORDER TO 0

SELECT LINK
SET RELATION TO LNK INTO BILLS
DO Entry_P WITH "B_Sale", "Choose"

ON KEY LABEL Enter KEYBOARD CHR(23)
ON KEY LABEL Home GO TOP
ON KEY LABEL End  GO BOTTOM
ON KEY LABEL LeftMouse DO M_Proc_1 WITH "Choose"
ACTIVATE WINDOW Bills_E
BROWSE FIELDS f001 = Doc_N():11:H="" , ;
              BILLS.BILL_SUM:H="",  ;
              f002 = IIF(FOUND("ACCOUNT"), ACCOUNT.CUS_NAME ,  ;
              BILLS.CUS_NAME):50:H= "", DUMMY.F:H=""   ;
       FREEZE DUMMY.F  ;
       WHEN Show_D()   ;
       NOMENU NORGRID NOAPPEND NODELETE NOMODIFY REST NOCLEAR ;
       WINDOW Bills_I IN WINDOW Bills_E
ON KEY LABEL Enter
ON KEY LABEL Home
ON KEY LABEL End
ON KEY LABEL LeftMouse
SET RELATION TO
SELECT BILLS
SET ORDER TO TAG HRON
IF READKEY() % 256 = 12
  GO rc
ENDIF
DO Exit_P WITH "B_Sale", "Choose"
RETURN

*********************************************************************
PROCEDURE Out_Menu
PARAMETER c

KEYBOARD c
DEACTIVATE MENU Choose
RETURN

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
