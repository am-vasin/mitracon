*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   蹍甃�矗 ����膈諷 ���皀Ε�.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE View_Nal
PRIVATE p_type, p_list, p_numb
*
*    PAD - �キ� �ム〓�� 窶�↓�
*
DEFINE MENU W_Pay IN SCREEN && COLOR SCHEME 4
DEFINE PAD P_Type OF W_Pay PROMPT "�┓ ���皀��"
ON PAD P_Type OF W_Pay ACTIVATE POPUP P_Type

  DEFINE POPUP P_Type MARGIN RELATIVE SHADOW && COLOR SCHEME 4

  DEFINE BAR  1 OF P_Type PROMPT "������� � �/���." KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF P_Type DO Ch_Mode WITH "W_Pay","Type_1"

  DEFINE BAR  2 OF P_Type PROMPT "���." KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF P_Type DO Ch_Mode WITH "W_Pay","Type_2"

  DEFINE BAR  3 OF P_Type PROMPT "��ぅ��." KEY Ctrl-A, "F3"
  ON SELECTION BAR 3 OF P_Type DO Ch_Mode WITH "W_Pay","Type_3"

DEFINE PAD Place OF W_Pay PROMPT "ç��お�" KEY Ctrl-A, "F8"
DEFINE PAD Seek  OF W_Pay PROMPT "��┘��" KEY Ctrl-A, "F7"
DEFINE PAD Print OF W_Pay PROMPT "�ョ�碎" KEY Ctrl-A, "Enter"
DEFINE PAD Exit  OF W_Pay PROMPT "�諷��" KEY Ctrl-A, "Esc"

ON SELECTION PAD Place OF W_Pay DO Ch_Mode WITH "W_Pay","place"
ON SELECTION PAD Seek  OF W_Pay DO Ch_Mode WITH "W_Pay","seek"
ON SELECTION PAD Print OF W_Pay DO Ch_Mode WITH "W_Pay","print"
ON SELECTION PAD Exit  OF W_Pay DO Ex_Proc WITH "W_Pay"

HIDE MENU Main_M
DO Entry_P WITH "", "W_Pay"

DO Use_Dummy
DO Use_Link

SELECT 0
USE INCMONEY ORDER TAG HRON

SELECT 0
p_list = ""
USE PLACES
p_numb = RECCOUNT()
p_list = LEFT("ABCDEFGHIJKLMNOPQRSTUVWXYZ", p_numb)

USE ACCOUNT ORDER TAG CUS_CODE

SELECT INCMONEY
SET RELATION TO CUS_CODE INTO ACCOUNT

DO Prep_W

PRIVATE what_do, fl, cus_
what_do = "b_list"
cus = 0
fl = .F.
p_type = " "
DO WHILE .T.

  DO CASE
  CASE what_do = "b_list"     && ���甌� ぎ�祠キ皰�
    ON KEY LABEL Enter DO Ch_Mode WITH "W_Pay","print"
    ON KEY LABEL F7    DO Ch_Mode WITH "W_Pay","seek"
    ON KEY LABEL F8    DO Ch_Mode WITH "W_Pay","place"
    ON KEY LABEL F1    DO Ch_Mode WITH "W_Pay","Type_1"
    ON KEY LABEL F2    DO Ch_Mode WITH "W_Pay","Type_2"
    ON KEY LABEL F3    DO Ch_Mode WITH "W_Pay","Type_3"
    ON KEY LABEL F10   ACTIVATE MENU W_Pay
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "W_Pay"
    ACTIVATE WINDOW Pay_M BOTTOM
    DO CASE
    CASE p_type = " "
      @ 0, 0 SAY PADC("�����硅 � �/�", WCOLS()) COLOR SCHEME 5
    CASE p_type = "2"
      @ 0, 0 SAY PADC("踸絎き襯 �爐ム� ( ��� )", WCOLS()) COLOR SCHEME 5
    CASE p_type = "3"
      @ 0, 0 SAY PADC("��ぅ�瓱珮ガ襯 ���皀Θ", WCOLS()) COLOR SCHEME 5
    ENDCASE
    ACTIVATE WINDOW Pay_E
    IF fl
      SET ORDER TO TAG CUS_CODE
    ELSE
      SET ORDER TO TAG HRON
    ENDIF
    BROWSE FOR DOC_TYPE = p_type .AND. LEFT(DOC_NUM,1) $ p_list ;
           FIELDS f001 = Doc_N(INCMONEY.DOC_NUM):11:H="" , ;
                  INCMONEY.DOC_DATE:H="" , ;
                  INCMONEY.MONEY_R:H="" , ;
                  INCMONEY.MONEY_D:H="" , ;
                  DUMMY.F:H=""   ;
           FREEZE DUMMY.F  ;
           WHEN Show_Cus() ;
           KEY IIF(fl, cus_, "")         ;
           NOMENU NORGRID NOAPPEND NODELETE NOMODIFY REST NOCLEAR ;
           WINDOW Pay_I IN WINDOW Pay_E
    ON KEY
    IF what_do = "b_list"
      what_do = "exit"
    ENDIF
  CASE what_do = "seek"       && �痰���→� 筥�赳�� �� ��┘�矣
    fl = Custs("W_Pay", .F., .F.)
    cus_ = ACCOUNT.CUS_CODE
    what_do = "b_list"
  CASE what_do = "print"      && �ョ�碎 ���皀Κ�
    DO P_Inc_O WITH DOC_NUM, DOC_DATE, DOC_TYPE
    what_do = "b_list"
  CASE what_do = "place"     && ��玻オ罔� 甎�瓷� �����ぎ�
    DO Get_Pl
    what_do = "b_list"
  CASE what_do = "Type_1"     && ��キ� 皋�� ���皀��
    p_type = " "
    what_do = "b_list"
  CASE what_do = "Type_2"     && ��キ� 皋�� ���皀��
    p_type = "2"
    what_do = "b_list"
  CASE what_do = "Type_3"     && ��キ� 皋�� ���皀��
    p_type = "3"
    what_do = "b_list"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

CLOSE DATABASES
CLEAR WIND
DO Exit_P WITH "", "W_Pay"
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   蹍������ ��ぃ�皰→� ����.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prep_W

PRIVATE l, c, ln, wd, tit1, tit2, foot, dat_ln
DIMENSION tit2(1)
*
*   �甅�↓�� �ムョキ� ���ォ か� BROWSE
*
dat_ln = IIF(SET("CENTURY") = "ON", 10, 8)
tit2(1) = PADC("�N ���_��",FSIZE("DOC_NUM" ,"INCMONEY")+1,"�")+"�"+ ;
          PADC("����",dat_ln,"�")+"�"+ ;
          PADC("�祠��(珮�)",FSIZE("MONEY_R" ,"INCMONEY"),"�")+"�"+ ;
          PADC("�祠��($)",FSIZE("MONEY_D" ,"INCMONEY"),"�")
ln = WROWS("")-7
tit1 = ""
DIMENSION foot(2)
foot(1) = ""
foot(2) = ""

DO Def_Win WITH "Pay_M", "Pay_E", "Pay_I", ln, LEN(tit2(1)), tit1, tit2, foot

RETURN

****************************************************************************
PROCEDURE Doc_N
PARAMETER s
RETURN LEFT(s,1)+"-"+ALLTRIM(SUBSTR(s,2))

****************************************************************************
PROCEDURE Show_Cus
PRIVATE nm, l

l = WCOLS()
ACTIVATE WINDOW Pay_M BOTTOM
nm = ALLTRIM(ACCOUNT.CUS_NAME)
IF LEN(nm) < l
  nm = nm+SPACE(l/2)
ELSE
  nm = LEFT(nm,l)
ENDIF
nm = PADC(nm, l)
@ WROWS()-1, 0 SAY nm
ACTIVATE WINDOW (ALIAS())
RETURN .T.

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
mc = MCOL("Pay_E")
mr = MROW("Pay_E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  �甅�↓�� ����
*
mc = MCOL("Pay_M")
mr = MROW("Pay_M")
IF mc >= 0 .AND. mr >= 0
  IF mr = 1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WROWS("Pay_M")-2
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

********************************************************************
PROCEDURE Ms_Lock
IF MROW() < 0 .OR. MCOL() <= 0
  = INKEY("M")
ENDIF
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ �襦��� ��痰�鎰���� ��┘���.                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Ch_Cust

IF new_cli
  RETURN .T.
ENDIF

IF Custs("", .F., .F.)
  cli_nm = ACCOUNT.CUS_NAME
  SHOW GET cli_nm
ELSE
  new_cli = .T.
  SHOW GET new_cli
ENDIF
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            �襦�� �����ぎ�.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 02.04.97 12:36:46 夕
PROCEDURE Get_Pl
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
PRIVATE wk_arr, ln, i
DIMENSION wk_arr(p_numb)
FOR i = 1 TO p_numb
  wk_arr(i) = CHR(ASC("A")+i-1) $ p_list
ENDFOR

ex   = 1
*------------------------------------------------------------------------
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�   ( ) ....:....!....:....!....  �
*�   ( ) ....:....!....:....!....  �
*�   ( ) ....:....!....:....!....  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc> �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO D_Win_N WITH p_numb+5, 38, "ç��お�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO G_Place WITH 2, 5, wk_arr
@ WROWS()-2, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Pl()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 蹍▲爲� ��玻オ皚�痰� 甎�瓷� �����ぎ�.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 02.04.97 12:56:12 夕
PROCEDURE Tst_Pl

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF ASCAN(wk_arr, .T.) = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 絎硼 ° �き�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

p_list = ""

FOR i = 1 TO p_numb
  IF wk_arr(i)
    p_list = p_list+CHR(ASC("A")+i-1)
  ENDIF
ENDFOR

RETURN .T.
