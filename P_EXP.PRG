*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                 �
*�       蹍������ �ョ�皋 �����き諷 � ���皀Ν諷 ぎ�祠キ皰�.        �
*�                                                                 �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE P_Exp

PRIVATE ln, wd, c1, c2, c3, dat_0, dat_1, dat_v0, dat_v1, ex
IF .NOT. EMPTY(MENU())
  HIDE MENU (MENU())
ENDIF

USE PARMS
dat_v0 = DAT_START   && ���珥���� ���� 縲��キ�� ����諷
dat_v1 = DATE()      && �オ竕�� ����
USE

dat_0 = dat_v0
dat_1 = dat_v1
ex    = 1

*                          �ム┏�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                            �
*�   �����讚�� ���� ��.��.����     ���ョ��� ���� ��.��.����   �
*�                                                            �
*�              < OK Ctrl-W > < �皖���碎瘴 Esc >              �
*�                                                            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*
ln = 8
IF SET("CENTURY") = "ON"
  wd = 64
  c1 = 6
  c2 = 35
ELSE
  wd = 60
  c1 = 6
  c2 = 33
ENDIF
c3 = FLOOR((wd-32)/2)

DEFINE WINDOW W_H_Bnds FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW W_H_Bnds

IF .NOT. S_Bounds()
  RELEASE WINDOW W_H_Bnds
  RETURN
ENDIF

RELEASE WINDOW W_H_Bnds

*
*    ｯ爛ぅ�錺� �キ�
*
DEFINE MENU P_Exp IN SCREEN COLOR SCHEME 4
DEFINE PAD P_Inv OF P_Exp PROMPT "�����き��" KEY Ctrl-A, "Enter"
DEFINE PAD P_Pay OF P_Exp PROMPT "���皀Ν覃 ぎ�祠キ�" KEY Ctrl-A, "Ctrl-Enter"
DEFINE PAD Exit  OF P_Exp PROMPT "�諷��" KEY Ctrl-A, "Esc"

ON SELECTION PAD P_Inv OF P_Exp DO Ch_Mode WITH "P_Exp","p_inv"
ON SELECTION PAD P_Pay OF P_Exp DO Ch_Mode WITH "P_Exp","p_doc"
ON SELECTION PAD Exit  OF P_Exp DO Ch_Mode WITH "P_Exp","�瓮!"

DO Use_Dummy

SELECT 0
USE ACCOUNT ORDER TAG CUS_CODE
SELECT 0

USE SALE_TIT ORDER TAG DOC_DATE
SET RELATION TO CUS_CODE INTO ACCOUNT

PRIVATE tit_m, tit, foot, what_do, pic
pic = "@Z 9999999.99"

tit_m = "�����き襯 �� �ム┏� � "+TRANSFORM(dat_0,"@D")+" �� "+TRANSFORM(dat_1,"@D")
DIMENSION tit(1), foot(2)
tit(1) = PADC("����",IIF(SET("DATE") = "ON", 11, 9),"�")+  ;
         PADC("N ����.",FSIZE("DOC_NUM")+2,"�")+  ;
         PADC("陳�祠��",FSIZE("TOTAL_R")+1,"�")+  ;
         PADC("陳��瘁�",FSIZE("KASSA")+1,"�")+    ;
         PADC("��. ���祠.",11,"�")+              ;
         PADC("N ��. ���.",FSIZE("PAY_DOC")+1,"�")
foot(1) = ""
foot(2) = ""

ln = WROWS("")-7
wd = LEN(tit(1))

DO Def_Win WITH "P_Exp_M", "P_Exp_E", "P_Exp_I", ln, wd, tit_m, tit, foot

what_do = "list"
GO BOTTOM

DO WHILE .T.
  DO CASE
  CASE what_do = "list"
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Ch_Mode WITH "P_Exp","p_doc"
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "P_Exp", "P_Exp_"
    ON KEY LABEL F10 ACTIVATE MENU P_Exp
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("�キ� F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU P_Exp
    ACTIVATE WINDOW P_Exp_E

    BROWSE FIELDS DOC_DATE:H="":P="@D",   ;
                  f0001 = LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5)):11:H="", ;
                  TOTAL_R:P=pic:H="",   ;
                  KASSA:P=pic:H="",  ;
                  f0002 = Doc_T(PAY_TYPE, PAY_DOC):10:H="",  ;
                  f0003 = IIF( EMPTY(PAY_DOC), PAY_DOC,      ;
                          LEFT(PAY_DOC,4)+"-"+ALLTRIM(SUBSTR(PAY_DOC,5))):11:H="", ;
                  DUMMY.F:H=""  ;
           FREEZE DUMMY.F       ;
           WHEN Sh_Cus()        ;
           KEY DTOS(dat_0), DTOS(dat_1)  ;
           NOMODIFY NOCLEAR WINDOW P_Exp_I IN WINDOW P_Exp_E

    ON KEY LABEL Enter
    ON KEY LABEL F10
    ON KEY LABEL Ctrl+Enter
    ON KEY LABEL Home
    ON KEY LABEL End
    ON KEY LABEL LeftMouse
    IF what_do = "list"
      what_do = IIF(READKEY()%256 = 12, "�瓮!", "p_inv")
    ENDIF
  CASE what_do = "p_inv"
    DO Prn_Inv WITH DOC_NUM, DOC_DATE
    what_do = "list"
  CASE what_do = "p_doc"
    DO P_Doc WITH PAY_TYPE, PAY_DOC, PAY_DATE
    what_do = "list"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO


CLOSE DATABASES
RELEASE WINDOW P_Exp_M
RELEASE WINDOW P_Exp_E
RELEASE WINDOW P_Exp_I
RELEASE MENU P_Exp
RETURN

*********************************************************************
PROCEDURE Doc_T
PARAMETERS p_t, p_d
IF EMPTY(p_d)
  RETURN " "
ENDIF
DO CASE
CASE p_t = " "
  RETURN "��. �爐ム"
CASE p_t = "1"
  RETURN "���皀Κ�"
CASE p_t = "2"
  RETURN "���. ぅ�."
CASE p_t = "3"
  RETURN "����牀�"
ENDCASE

RETURN " "  

*********************************************************************
PROCEDURE Sh_Cus
PRIVATE s

IF .NOT. FOUND("ACCOUNT")
  s = SALE_TIT.CUS_NAME
ELSE
  s = ACCOUNT.CUS_NAME
ENDIF

ACTIVATE WINDOW P_Exp_M SAME
s = PADR(s,WCOLS()-4)
@ WROWS()-1, 2 SAY s
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    ��皀���皋↓�� 竅����→� ���.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE S_Bounds
PRIVATE mss

DO D_W WITH "S_Bounds", ln, wd, "�ム┏�", 1

@ 3, c1 SAY "�����讚�� ����" GET dat_0 PICTURE "@D"
@ 3, c2 SAY "���ョ��� ����"  GET dat_1 PICTURE "@D"
@ 5, c3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID V_Dat()

RELEASE WINDOW S_Bounds
RETURN ex = 1

*********************************************************************
PROCEDURE V_Dat

IF ex = 2
  RETURN .T.
ENDIF

IF dat_0 < dat_v0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��ゥ� ����襯 皰�讓� � "+TRANSFORM(dat_v0,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_0 = dat_v0
  SHOW GETS
  RETURN .F.
ENDIF

IF dat_1 > dat_v1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ィ�き� 皰�讓� "+TRANSFORM(dat_v1,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_1 = dat_v1
  SHOW GETS
  RETURN .F.
ENDIF

RETURN .T.


*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ �ムオ�鈑キ�� 爛Θ��.                            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Ch_Mode
PARAMETERS menu_nm, mode_

KEYBOARD CHR(27)
what_do = mode_

IF .NOT. EMPTY(menu_nm)
  DEACTIVATE MENU (menu_nm)
ENDIF
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�        蹍������ - �÷�｀砒┴ �謌�� か� 甎�瓷��.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE M_Proc_1
PARAMETER menu_nm, wind_nm
PRIVATE mc, mr, cr

*
*  �瓱�縲����� �キ�
*
IF MENU() == UPPER(menu_nm)
  RETURN
ENDIF

IF MROW("") = 0
  IF .NOT. MENU() == UPPER(menu_nm)
    ACTIVATE MENU (menu_nm)
  ENDIF
  RETURN
ENDIF

*
*  ｪ�� BROWSE
*
mc = MCOL(wind_nm+"E")
mr = MROW(wind_nm+"E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  �甅�↓�� ����
*
mc = MCOL(wind_nm+"M")
mr = MROW("")
IF mc >= 0
  IF mr = WLROW(wind_nm+"E")-1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WLROW(wind_nm+"E")+WROWS(wind_nm+"E")
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                     蹍������ �ョ�皋 ぎ�祠キ皰�.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE P_Doc

PARAMETERS tp, d_n, dt
PRIVATE mss

IF EMPTY(d_n)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"｡��皋皀 ↓━��┘: �モ �爬�錻�� � ���皀Ν��� ぎ�祠キ矣!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

ACTIVATE SCREEN
HIDE MENU P_Exp
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

DO CASE
CASE TP = " "
  DO P_Inc_O WITH d_n, DT
CASE TP = "1"
  DO Prn_Pay WITH d_n, DT
CASE TP = "2"
  DO Prn_Pay1 WITH d_n, DT
ENDCASE

RETURN
