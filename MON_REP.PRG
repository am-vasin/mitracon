*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ �ムイ�腮 ぅ�ィ � ＜紕��皀爬�.                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Mon_Rep
PARAMETER vlt_p

PRIVATE cq, ln, wd, rest, rest0, ex, k000, s001, s000
PRIVATE d_n, d_d, nt, vlt_w
IF TYPE("vlt_p") = "C"
  vlt_w = IIF(vlt_p = "$", "$", "�")
ELSE
  vlt_w = "�"
ENDIF

ACTIVATE SCREEN
HIDE MENU Main_M
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

USE (base_path+"KASSA")
SUM FOR EMPTY(WHO_GET) .AND. WHERE = arm .AND. VALUTA = vlt_w TOTAL TO rest

USE (base_path+"ARM_LST")
GO arm_rec
DIMENSION cq(1)
cq = 0
*          < ��｀腑� �メ皰 >
* 敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
* � 敖 �キ譯� 陳陳陳陳陳陳陳陳陳陳陳陳朕 �
* � �        ��瘁� 999 999 999.99      � �
* � �  �ムュ. �痰. 999 999 999.99      � �
* � �      � 瓩�腑 999 999 999.99      � �
* � �        �祠�� 999 999 999.99 (F1) � �
* � �      �痰�皰� 999 999 999.99      � �
* � 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
* �     敖 踸�ョ��┘ 陳陳陳陳陳陳朕     �
* �     �  ....!....0....!....0..  �     �
* �     青陳陳陳陳陳陳陳陳陳陳陳陳潰     �
* �   < OK Ctrl-W > < �皖���碎瘴 Esc >   �
* 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
ln   = 14
wd   = 42
rest = IIF(vlt_w = "$", KASSA_USD, KASSA) - rest
rest0 = 0
s001 = rest
k000 = rest
ex   =  1
s000 =  0
d_n  = ""
d_d  = DATE()
nt   = SPACE(22)
DO D_W WITH "Mon_Rep", ln, wd, ALLTRIM(ARM_N), 1
@ 2, 3 TO 8, WCOLS()-4
@ 2, 5 SAY IIF(vlt_w = "$"," �キ譯� (USD)"," �キ譯� ")

@ 9, 7 TO 11, WCOLS()-8
@ 9, 9 SAY " 踸�ョ��┘ "

@ 3, 13 SAY "��瘁� "+ IIF(vlt_w = "$",TRANSFORM(k000,"@Z 9 999 999.9999"),TRANSFORM(k000,"@Z 999 999 999.99"))
@ 4,  7 SAY "�ムュ. �痰." GET rest0  PICTURE IIF(vlt_w = "$","@Z 9 999 999.9999","@Z 999 999 999.99") VALID Rest0()
@ 5, 11 SAY "� 瓩�腑 "+TRANSFORM(s001, IIF(vlt_w = "$","@Z 9 999 999.9999","@Z 999 999 999.99"))
@ 6, 34 SAY "(F1)"
@ 6, 13 SAY "�祠��" GET s000 PICTURE IIF(vlt_w = "$","@Z 9 999 999.9999","@Z 999 999 999.99") WHEN Entr() VALID Exi()
@ 7, 11 SAY "�痰�皰� "+TRANSFORM(rest, IIF(vlt_w = "$","@Z 9 999 999.9999","@Z 999 999 999.99"))
@ 10,10 GET nt
@ 12, 5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
@ 1, 1 FILL TO WROWS()-1, WCOLS()-1 COLOR SCHEME 13

READ CYCLE VALID Rep()

RELEASE WINDOW Mon_Rep

IF .NOT. EMPTY(d_n)
  DO P_Rep WITH d_n, d_d
ENDIF

USE

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 蹍������ √��� � ���� 痺���.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Entr

ON KEY LABEL F1 DO Modi_S
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 蹍������ �諷��� ├ ���� 痺���.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Exi
PRIVATE mss, i, s_w
IF READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF k000-s000 < 0
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �謔�モメ� 瓩�碎 ぅ�ィ ｀�跏�, 腑� メ碎 � ��瘁�."
  mss(3) = CHR(0)+"  �皰 ���※�讚�? [ �� ] [ �モ ]"
  mss(4) = ""
  IF Out_Mess(7,"mss") = 2
    RETURN .F.
  ENDIF
ENDIF
s_w = 0
IF TYPE("CupRubW") # "U"
  EXTERNAL ARRAY CupRubW
  FOR i = 1 TO ALEN(cq)
    s_w = s_w+CupRubW(i)*cq(i)
  ENDFOR
ENDIF
IF s_w # 0
  IF s_w # s000
    s000 = s_w
    SHOW GET s000
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� �甎��讌����� ���讓祀閧��: 痺��� �キ閧� �カ讌�!"
    mss(3) = ""
    DO Out_Mess WITH 7,"mss"
  ENDIF
ENDIF

rest = k000-s000
@ 7, 11 SAY "�痰�皰� "+IIF(vlt_w = "$",TRANSFORM(rest,"@Z 9 999 999.9999"),TRANSFORM(rest, "@Z 999 999 999.99"))

ON KEY LABEL F1
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�            蹍������ �諷��� ├ ���� �ムュ�わ薀�� �痰�皖�.             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Rest0
PRIVATE mss
IF READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF k000-rest0 < 0
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�ムュ�わ薑� �痰�皰� ｀�跏� ����膈�痰�!"
  mss(3) = CHR(0)+"  �皰 ���※�讚�? [ �� ] [ �モ ]"
  mss(4) = ""
  IF Out_Mess(7,"mss") = 2
    RETURN .F.
  ENDIF
ENDIF
s001 = k000-rest0
@ 5, 11 SAY "� 瓩�腑 "+IIF(vlt_w = "$",TRANSFORM(s001,"@Z 9 999 999.9999"),TRANSFORM(s001,"@Z 999 999 999.99"))

ON KEY LABEL F1
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 蹍������ 筮爼�牀����� 痺���.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Rep
PRIVATE d_w, i, w

IF ex = 2 .OR. s000 = 0
  RETURN .T.
ENDIF
d_w = ""
EXTERNAL ARRAY CupRubC
FOR i = 1 TO ALEN(cq)
  IF cq(i) = 0
    LOOP
  ENDIF
  d_w = d_w+CupRubC(i)+ALLTRIM(STR(cq(i),10))+"!"
ENDFOR

d_n = Doc_Num("KASSA", def_firm, arm, DATE())

&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
&&REPLACE LAST_REP WITH d_n,    ;
&&        KASSA    WITH KASSA-s000      
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

d_n = sys_char+CHRTRAN(arm," ","0")+STR(d_n,6)
USE (base_path+"KASSA")
APPEND BLANK
REPLACE DOC_NUM    WITH d_n,    ;
        DOC_DATE   WITH d_d,    ;
        DOC_TIME   WITH SECONDS(), ;
        DETAIL     WITH d_w,    ;
        VALUTA     WITH vlt_w,  ;
        TOTAL      WITH s000,   ;
        MONEY_REST WITH rest0,  ;  
        KASSA_REST WITH rest,   ;
        NOTE       WITH nt,     ;
        WHO_REP    WITH user,   ;
        WHERE      WITH arm
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   蹍������ ��め腑�� �祚釵.                           �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Modi_S

s000 = Cupury(" ", cq, 0, -23)
SHOW GET s000
RETURN
