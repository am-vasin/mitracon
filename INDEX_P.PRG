*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                踸絎� ┃ぅ�瓱珮ガ��� ぅ��Ж��.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Index_P
PARAMETERS hide_add    && �甄� メ碎 - 皰�讓� �牀甃�矗!
PRIVATE sw_add
sw_add = TYPE("hide_add") = "L"

*
*    PAD - �キ� �ム〓�� 窶�↓�
*
DEFINE MENU W_Pay IN SCREEN COLOR SCHEME 4
DEFINE PAD Seek  OF W_Pay PROMPT "��┘��" KEY Ctrl-A, "F7"
DEFINE PAD Print OF W_Pay PROMPT "�ョ�碎" KEY Ctrl-A, "Enter"
IF sw_add
  DEFINE PAD Ins   OF W_Pay PROMPT "踸絎�" KEY Ctrl-A, "Ins"
ENDIF
DEFINE PAD Exit  OF W_Pay PROMPT "�諷��" KEY Ctrl-A, "Esc"

ON SELECTION PAD Seek  OF W_Pay DO Ch_Mode WITH "W_Pay","seek"
ON SELECTION PAD Print OF W_Pay DO Ch_Mode WITH "W_Pay","print"
IF sw_add
  ON SELECTION PAD Ins   OF W_Pay DO Ch_Mode WITH "W_Pay","insert"
ENDIF
ON SELECTION PAD Exit  OF W_Pay DO Ex_Proc WITH "W_Pay"

HIDE MENU Main_M
DO Entry_P WITH "", "W_Pay"

DO Use_Dummy
DO Use_Link

SELECT 0
USE INCMONEY ORDER TAG HRON

SELECT 0
USE ACCOUNT ORDER TAG CUS_CODE

SELECT INCMONEY
SET RELATION TO CUS_CODE INTO ACCOUNT

DO Prep_W

PRIVATE what_do, fl, cus_
what_do = "b_list"
cus = 0
fl = .F.
DO WHILE .T.

  DO CASE
  CASE what_do = "b_list"     && ���甌� 瘍モ��
    ON KEY LABEL Enter DO Ch_Mode WITH "W_Pay","print"
    ON KEY LABEL F7    DO Ch_Mode WITH "W_Pay","seek"
    IF sw_add
      ON KEY LABEL Ins   DO Ch_Mode WITH "W_Pay","insert"
    ENDIF
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "W_Pay"
    ACTIVATE WINDOW Pay_E
    IF fl
      SET ORDER TO TAG CUS_CODE
    ELSE
      SET ORDER TO TAG HRON
    ENDIF
    BROWSE FOR DOC_TYPE ="2"  ;
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

    ON KEY LABEL F7
    ON KEY LABEL Enter
    ON KEY LABEL Ins
    ON KEY LABEL Home
    ON KEY LABEL End
    ON KEY LABEL LeftMouse
    IF what_do = "b_list"
      what_do = "exit"
    ENDIF
  CASE what_do = "insert"     && 踸絎� ぅ�ィ
    DO Add_Ind WITH "W_Pay"
    what_do = "b_list"
  CASE what_do = "seek"       && �痰���→� 筥�赳�� �� ��┘�矣
    fl = Custs("W_Pay", .F., .F., 2)
    cus_ = ACCOUNT.CUS_CODE
    what_do = "b_list"
  CASE what_do = "print"      && �ョ�碎 ���皀Κ�
    DO Prn_Ind WITH DOC_NUM, DOC_DATE
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
tit1 = "���甌� ┃ぅ�瓱珮ガ諷 ���皀Ε�"
DIMENSION foot(2)
foot(1) = ""
foot(2) = ""

DO Def_Win WITH "Pay_M", "Pay_E", "Pay_I", ln, LEN(tit2(1)), tit1, tit2, foot

RETURN

****************************************************************************
PROCEDURE Doc_N
PARAMETER s
RETURN LEFT(s,4)+"-"+ALLTRIM(SUBSTR(s,5))

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

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�               蹍������ □��� ┃ぅ�瓱珮ガ��� ���皀��.                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Add_Ind
PARAMETER ext_menu

PRIVATE new_cli, cli_nm, cli_cd, pay_note, pay_dat, pay_sumr, pay_sumd, ex, p_doc
PRIVATE ln, wd, v_rem, w_kurs

DO Entry_P WITH ext_menu, ""

ON KEY LABEL LeftMouse DO Ms_Lock

new_cli  = .NOT. fl
cli_nm   = IIF(fl, ACCOUNT.CUS_NAME , SPACE(FSIZE("CUS_NAME","ACCOUNT")))
cli_cd   = IIF(fl, ACCOUNT.CUS_NAME , 0)
pay_sumr = 0
pay_sumd = 0
w_kurs   = kurs_b

p_doc = .T.
ex = 1

ln = 12
wd = 66
v_rem = 0
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�  敖 ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕  �
*�  �      [ ] ���覃                                         �  �
*�  � ��� ....:....!....:....!....:....!....:....!....:....! �  �
*�  青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰  �
*� 敖陳 �祠�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �  �窶� USD 99.999(F1)  �磧�� 9999999.99   USD 99999.9999  � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�                     [ ] �ョ�碎 ぎ�祠キ��                     �
*�        < ｯ爬絎ぎ��碎 Ctrl-W > < �皖���碎瘴 Esc >            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰


DO D_W WITH "Inp_Pay", ln, wd, "踸絎� ぅ�ィ �� ┃ぅ�瓱珮ガ覃 ぅ��Ж�", 1

@  2, 4 TO 5, WCOLS()-5
@  2, 6 SAY " ��┘�� "
@  6, 3 TO 8, WCOLS()-4
@  6, 5 SAY " �祠�� "

@  3,11 GET new_cli PICTURE "@*C ���覃" VALID Ch_Cust()
@  4, 6 SAY "���" GET cli_nm WHEN new_cli
@  7, 6 SAY "�窶� USD" GET w_kurs PICTURE "99.999" WHEN Set_F1() VALID Tst_Kurs()
@  7,27 SAY "�磧��"  GET pay_sumr PICTURE "@Z 9999999.99"  ;
                             WHEN Rm_(pay_sumr) VALID Tuda()
@  7,46 SAY "USD" GET pay_sumd PICTURE "@Z 99999.9999"     ;
                             WHEN Rm_(pay_sumd) VALID Obratno()
@  9,23 GET p_doc PICTURE "@*C �ョ�碎 ぎ�祠キ��"
@ 10,10 GET ex PICTURE "@*HT \! ｯ爬絎ぎ��碎 Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID Tst_Blank()

RELEASE WINDOW Inp_Pay

DO Exit_P WITH ext_menu, ""

RETURN

********************************************************************
PROCEDURE Set_F1

ON KEY LABEL F1 DO Set_Kurs

RETURN .T.

********************************************************************
PROCEDURE Tst_Kurs

IF READKEY() % 256 # 12
  IF w_kurs <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"����� �窶甌� ぎ����� �� °��モ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF
ON KEY LABEL F1

RETURN .T.

********************************************************************
PROCEDURE Set_Kurs
PRIVATE ln, wd, n, st

PUSH KEY CLEAR
st = " �窶� 2 ("+ALLTRIM(STR(kurs_b,10,3))+") ; �窶� 3 ("+ALLTRIM(STR(kurs_3,10,3))+") ;\? �痰�※碎 ��� °�� Esc "
wd = LEN(st)+10
st = "@*HT \"+st
ln = 6
n  = IIF(w_kurs = kurs_3, 2, 1)
DO D_W WITH "Set_Kurs", ln, wd, "�襦ム�皀 �窶�", 1
@ 3, 3 GET n PICTURE st
READ CYCLE
RELEASE WINDOW Set_Kurs
IF n = 3
  RETURN
ENDIF

w_kurs = IIF(n = 1, kurs_b, kurs_3)
SHOW GET w_kurs

RETURN

********************************************************************
PROCEDURE Ms_Lock
IF MROW() < 0 .OR. MCOL() <= 0
  = INKEY("M")
ENDIF
RETURN
********************************************************************
PROCEDURE Rm_
PARAMETER v
v_rem = v
RETURN .T. 
********************************************************************
PROCEDURE Tuda
IF pay_sumr = v_rem
  RETURN .T.
ENDIF

pay_sumd = ROUND(pay_sumr/w_kurs,4)
SHOW GET pay_sumd
RETURN .T.

********************************************************************
PROCEDURE Obratno
IF pay_sumd = v_rem
  RETURN .T.
ENDIF

pay_sumr = ROUND(pay_sumd*w_kurs,2)
SHOW GET pay_sumr
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ �襦��� ��痰�鎰���� ��┘���.                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Ch_Cust

IF new_cli
  RETURN .T.
ENDIF

IF Custs("", .F., .F., 2)
  cli_nm = ACCOUNT.CUS_NAME
  SHOW GET cli_nm
ELSE
  new_cli = .T.
  SHOW GET new_cli
ENDIF
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ �諷��� ├ 蹣������ 筮爼�.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tst_Blank

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF new_cli .AND. EMPTY(cli_nm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��┘�� � �竅硅� ━キガ? �� ��Ε� °碎!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

*IF pay_sumr <= 0
*  DIMENSION mss(3)
*  mss(1) = ""
*  mss(2) = CHR(0)+"�皰膈�皀, 瓷��讓� Ε �� ����皋�."
*  mss(3) = ""
*  DO Out_Mess WITH 7, "mss"
*  RETURN .F.
*ENDIF
PRIVATE sel_sv

sel_sv = SELECT()
IF new_cli
  SELECT 0
  USE PARMS
  cli_cd = LAST_CUS+1
  REPLACE LAST_CUS WITH cli_cd
  USE
  SELECT ACCOUNT
  APPEND BLANK
  REPLACE SUBSYS   WITH sys_char, ;
          CUS_CODE WITH cli_cd, ;
          CUS_NAME WITH cli_nm, ;
          ACC_TYPE WITH 2,      ;
          DATE_ON  WITH DATE(), ;
          WHO      WITH user,   ;
          WHERE    WITH arm
ENDIF  
SELECT ACCOUNT

REPLACE ACC_SUM  WITH ACC_SUM+pay_sumr, ;
        ACC_SUMD WITH ACC_SUMD+pay_sumd
cli_cd  = CUS_CODE
cli_nam = CUS_NAME

SELECT 0
USE ARM_LST
GO arm_rec
ln = LAST_IND+1
REPLACE LAST_IND WITH ln, ;
        KASSA    WITH KASSA+pay_sumr        
USE
SELECT INCMONEY
APPEND BLANK
REPLACE DOC_TYPE  WITH "2",         ;  
        DOC_NUM   WITH sys_char+CHRTRAN(arm," ","0")+STR(ln,6), ;
        DOC_DATE  WITH DATE(),      ;
        DOC_TIME  WITH SECONDS(),   ;
        ENTER_DAT WITH DATE(),      ;
        CUS_CODE  WITH cli_cd,      ;
        CUS_NAME  WITH cli_nm,      ;
        MONEY_R   WITH pay_sumr,    ;
        MONEY_D   WITH pay_sumd,    ;
        WHO       WITH user,   ;
        WHERE     WITH arm

IF p_doc
  DO Prn_Ind WITH DOC_NUM, DOC_DATE
ENDIF
SELECT (sel_sv)

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    �ョ�碎 ����諷 � ���皀Κ�.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prn_Ind
PARAMETERS d_n, d_d

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, stw, t_p, p_drctry
PRIVATE sel_rem, sb, ffeed
DIMENSION sb(7,2)

sel_rem = SELECT()

SELECT 0

USE INCMONEY ORDER TAG DOC_NUM AGAIN ALIAS TMP000
SEEK "2"+LEFT(DTOS(d_d),4)+d_n
IF .NOT. FOUND()
  USE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���祠キ� �� ���ぅ� "+LEFT(d_n,4)+"-"+ALLTRIM(SUBSTR(d_n,5))+  ;
                  " "+TRANSFORM(d_d,"@D")
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_rem)
  RETURN
ENDIF
sb(1,1) = "{Doc1    }"
sb(1,2) = LEFT(d_n,4)+"-"+ALLTRIM(RIGHT(d_n,6))

sb(2,1) = "{Date1   }"
sb(2,2) = TRANSFORM(d_d,"@D")

sb(3,1) = "{Client}"
sb(3,2) = CUS_CODE

sb(4,1) = "{Sum_R   }"
sb(4,2) = STR(MONEY_R,10,2)

sb(5,1) = "{Sum_D   }"
sb(5,2) = STR(MONEY_D,10,4)

sb(6,1) = "{Who}"
sb(6,2) = WHO

sb(7,1) = "{Cource}"
IF MONEY_D # 0
  sb(7,2) = STR(ROUND(ABS(MONEY_R/MONEY_D),4),8,4)
ELSE
  sb(7,2) = "        "
ENDIF

USE ACCOUNT ORDER TAG CUS_CODE AGAIN ALIAS TMP000
SEEK sb(3,2)
IF FOUND()
  sb(3,2) = CUS_NAME
ELSE
  sb(3,2) = ""
ENDIF

IF sb(6,2) = 0
  sb(6,2) = "��� □カ!"
ELSE
  USE USERS ORDER TAG USER_CODE AGAIN ALIAS TMP000
  SEEK sb(6,2)
  IF FOUND()
    sb(6,2) = ALLTRIM(USER_NAME)
  ELSE
    sb(6,2) = ""
  ENDIF
ENDIF

USE DOC_FORM
LOCATE FOR "PAY2" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(DOC_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(DOC_H,i)
ENDFOR

USE

DO Ini_Prn WITH "踸絎� �� ┃�. ぅ��Ж� N "+sb(1,2), p_ln, lft, n_cp, fnt, ffeed, p_drctry

FOR i = 1 TO ALEN(t_p)
  stw = t_p(i)
  FOR j = 1 TO 7
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  @ PROW()+1, 0 SAY stw
ENDFOR
EJECT
DO Term_Prn WITH ""

SELECT (sel_rem)
RETURN
