*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�               蹍������ �襪�腮 ��牆��硅 ├ ��瘁�.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Salary
PRIVATE ln, wd, ex, s_usd1, s_usd2, s_rub, kurs_w, n_d, note1
DIMENSION oper_lst(oper_num, f_num)

DIMENSION note1(3)

note1(1) = ""
note1(2) = CHR(0)+"�襪��� ��牆��硅 瓮���� �痺薀痰←錺矚� � む磽�� �メ皀!"
note1(3) = ""
DO Out_Mess WITH 7, "note1"

RETURN

s_rub  = 0
kurs_w = kurs_b
s_usd1  = 0
s_usd2  = 0
note1  = SPACE(32)

ln = 14
wd = 46
ex =  1
DO D_W WITH "Input", ln, wd, "�襪��� ��牆��硅", 1

PUSH KEY CLEAR
@ 1, 0 SAY ""
TEXT
 敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
 �                                          �
 �          �襪��� 999999.999 USD           �
 �                                          �
 �      �� ��� � 珮゛閻 999999.999 USD      �
 �                                          �
 � �� �窶痺 99.999 艪� 甌痰�←錺�           �
 �                                          �
 �  敖 ���� ( �爬�ョ��┘ ) 陳陳陳陳陳陳陳�  �
 �  �  ....:....1....:....2....:....3..  �  �
 �  青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰  �
 �     < OK Ctrl-W > < �皖���碎瘴 Esc >     �
 青陳陳陳陳陳 F1 - 瘍モ腮� �祚釵 陳陳陳陳陳潰
ENDTEXT
@ 0, 0 SAY PADC("�襪��� ��牆��硅",WCOLS()) COLOR SCHEME 5

@ 3, 19 GET s_usd1 PICTURE "@Z 999999.999"
@ 5, 24 GET s_usd2 PICTURE "@Z 999999.999" VALID Draw()
@ 7, 12 GET kurs_w PICTURE "99.999" VALID Draw()
@ 10, 7 GET note1
@ 12, 7 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

ON KEY LABEL F1 DO Clc_C
READ CYCLE
ON KEY LABEL F1
RELEASE WINDOW Input
IF ex = 2
  RETURN
ENDIF

*
*    ��爼�珮ガ �爬ガ �砒モ�

SELECT KASSA_S
n_d = Doc_Num("KASSA_M", "", arm, DATE())
REPLACE SUM_D WITH SUM_D-s_usd1

SELECT VIEW
APPEND BLANK
REPLACE DOC_NUM  WITH sys_char+STR(n_d,6),     ;
        DOC_DATE WITH DATE(),                  ;
        DOC_TIME WITH SECONDS(),               ;
        CODE     WITH oper_lst(oper_n, i_cod), ;
        C_BILL   WITH oper_lst(oper_n, i_bill),;
        SUM_D    WITH -s_usd1,                 ;
        NOTE     WITH note1,                   ;
        WHO      WITH user,                    ;
        WHERE    WITH arm

IF s_usd2 # 0    && � ���▲珥�罔ォ!
  SELECT KASSA_S
  n_d = Doc_Num("KASSA_M", "", arm, DATE())
  REPLACE SUM_R    WITH SUM_R-s_rub,   ;
          SUM_D    WITH SUM_D+s_usd2
  UNLOCK

  SELECT VIEW
  APPEND BLANK
  REPLACE DOC_NUM  WITH sys_char+STR(n_d,6),     ;
          DOC_DATE WITH DATE(),                  ;
          DOC_TIME WITH SECONDS(),               ;
          CODE     WITH 2,                       ;
          SUM_R    WITH -s_rub,                  ;
          SUM_D    WITH s_usd2,                  ;
          KURS_FL  WITH kurs_w,                  ;
          WHO      WITH user,                    ;
          WHERE    WITH arm
ENDIF
SELECT VIEW

POP KEY
RETURN

************************************************************************
PROCEDURE Draw
PRIVATE mss

IF VARREAD() = "S_USD2"
  IF s_usd2 > s_usd1
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"�� �謔�モメ� ���▲珥�牀��碎 ｀�跏�, 腑� �襪���."
    mss(3) = CHR(0)+"�皰 ���※�讚�? [ �� ] [ �モ ]"
    mss(4) = ""
    IF Out_Mess(7,"mss") = 2
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
s_rub = ROUND(s_usd2*kurs_w,2)
@ 7, 33 SAY s_rub PICTURE "@Z 9999999.99"

RETURN .T.

**********************************************************
PROCEDURE Clc_C
PRIVATE v

v = VARREAD()

IF v = "S_USD1"
  s_usd1 = Cupury("$", q_usd, 0, 23)
ELSE
  = Cupury(" ", q_rub, 0, -23)
ENDIF

RETURN
