*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�           蹍甃�矗 � 竄▲爨ぅ�┘ 瓩�腮 �砒モ�� � ��｀腮� �メ�.        � 
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE View_K
PARAMETER let_m

PRIVATE see, cl0, what_do

cl0 = SCHEME(14,2)

USE USERS ORDER TAG USER_CODE

DO Use_Dummy

SELECT 0
USE ARM_LST ORDER TAG ARM_C

SELECT 0
USE KASSA ORDER TAG HRON
SET RELATION TO WHERE INTO ARM_LST
GO BOTTOM

see = .T.
*
*  ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU View_K IN SCREEN COLOR SCHEME 4
IF let_m
  DEFINE PAD Agree OF View_K PROMPT "�癶ムえ碎" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Agree OF View_K DO Ch_Mode WITH "View_K", "Agree"
ENDIF

DEFINE PAD Filtr OF View_K PROMPT "�┼赳�" KEY Ctrl-A, "Tab"
ON SELECTION PAD Filtr OF View_K DO Ch_Mode WITH "View_K", "Sw_Filtr"

DEFINE PAD Exit  OF View_K PROMPT "�諷��"  KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF View_K DO Ch_Mode WITH "View_K", "Ex"

DO Entry_P WITH "", "View_K"

@ 0, 0 SAY PADL("�キ� F10 ",WCOLS()) COLOR SCHEME 13
SHOW MENU View_K
PRIVATE ln, wd, ft, tt

DIMENSION tt(1), ft(2)
tt(1) = "�陳N ���-��陳�"+  ;
        PADC("����",IIF(SET("DATE")="ON", 10, 8),"�")+  ;
        "陳陳�祠��陳陳���. �メ皰陳陳陳�踸�ョ��┘陳陳陳陳陳"
ft(1) = REPLICATE("�", LEN(tt(1)))
ft(2) = PADR("  ���� ....!....0....!.  踸�鍮 ....!....0....!.", ;
             LEN(tt(1)))
ln = WROWS("")-9
wd = LEN(tt(1))

DO Def_Win WITH "W_M", "W_E", "W_I", ln, wd, "�砒モ� � ��｀腮� �メ�", ;
                tt, ft

ACTIVATE WINDOW W_E

IF let_m
  ON KEY LABEL Enter DO Ch_Mode WITH "View_K", "Agree"
ENDIF

ON KEY LABEL Tab DO Ch_Mode WITH "View_K", "Sw_Filtr"
ON KEY LABEL F10 DO F10_Proc WITH "View_K"

what_do = "List"
DO WHILE .T.
  DO CASE
  CASE what_do = "List"
    BROWSE FOR see .OR. EMPTY(WHO_GET)  REST ;
           FIELDS f000 = IIF(EMPTY(WHO_GET), " ", "�"):H="", ;
                  f001 = LEFT(DOC_NUM,4)+"-"+ALLTRIM(RIGHT(DOC_NUM,6)):10:H="", ;
                  DOC_DATE:H="", ;
                  R_TOTAL:H="",  ;
                  ARM_LST.ARM_N:H="", ;
                  NOTE:H="",     ;
                  DUMMY.F:H=""   ;
                  WHEN Sh_Users()  ;
                  FREEZE DUMMY.F NOCLEAR NOMENU NOLGRID NOMODIFY  ;
                  WINDOW W_I IN WINDOW W_E
    IF what_do = "List"
      what_do = "Ex"
    ENDIF
  CASE what_do = "Agree"
    DO Agree
    what_do = "List"
  CASE what_do = "Sw_Filtr"
    DO Sw_Filtr
    what_do = "List"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW W_M
RELEASE WINDOW W_E
RELEASE WINDOW W_I

CLOSE DATABASES

DO Exit_P WITH "", "View_K"
RETURN

***************************************************************************
PROCEDURE Sw_Filtr
see = .NOT. see
RETURN

***************************************************************************
PROCEDURE Sh_Users
PRIVATE w

ACTIVATE WINDOW W_M BOTTOM
IF WHO_REP = 0
  w = PADR("��� □カ!", 16)
ELSE
  = SEEK(WHO_REP,"USERS")
  w = PADR(USERS.USER_NAME, 16)
ENDIF
@ WROWS()-1, 9 SAY w COLOR (cl0)
IF WHO_GET = 0
  w = SPACE( 16)
ELSE
  = SEEK(WHO_GET,"USERS")
  w = PADR(USERS.USER_NAME, 16)
ENDIF
@ WROWS()-1, 34 SAY w COLOR (cl0)

RETURN .T.

***************************************************************************
PROCEDURE Agree
PRIVATE mss

IF .NOT. EMPTY(WHO_GET)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��甃�矗�皀 ↓━�皀�讚�, 轤�� �砒モ 礒� 竄▲爨ぅ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  REPLACE WHO_GET WITH user
  ACTIVATE WINDOW W_M BOTTOM
  = SEEK(WHO_GET,"USERS")
  w = PADR(USERS.USER_NAME, 16)
  @ WROWS()-1, 34 SAY w COLOR (cl0)
ENDIF

RETURN
