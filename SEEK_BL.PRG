*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                         ���瓷 瘍モ� �� ���ム�.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Seek_Bl
*
*  ��Б����ガ ���ム ����瓱 � SALE_TIT - 0 �ャ���� ┼� �皖��.
*

PRIVATE sel_rem, ye, t_doc, d_n, let_year, ex, mss, ln, wd, ret_v
PRIVATE f_c000, f_n000, f_c_old, s_sav
PRIVATE shab1, shab2
m.s_sav = SELECT()
SELECT 0
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"

DEFINE WINDOW H_W_123 FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW   WINDOW H_W_123

m.f_c000 = "   "
m.f_c_old = "   "
m.f_n000 = ""
sel_rem = SELECT()
SELECT 0
PUSH KEY CLEAR

ret_v = 0   && ��Б����ガ�� Л�腑�┘,
            &&    ������皀�讚� 竅����←│�モ瘴 � �牀������ Test_Form
t_doc = 1   && �薀� �����き竡 ( 2 - 甎�甌� )
ex    = 1
d_n   = SPACE(11)
ye    = YEAR(DATE())
let_year = .F.

*            ���祠キ�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                       �
*�     ��� 9999    ���ム ....:....!.     �
*�                                       �
*� 踳く爬閧┘ AAA AAAAAAAAAAAAAAAA (F1) �
*�                                       �
*�   < OK Ctrl-W > < �皖���碎瘴 Esc >    �
*�                                       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*
ln = 10
wd = 43
ex = 1

DO D_W WITH "Get_NN", ln, wd, "�腑�", 1

@ 3, 7 SAY "���" GET ye PICTURE "@Z 9999" VALID Test_Ye()
@ 3,19 SAY "���ム" GET d_n VALID Test_Num(.T.)
DO Get_Firm WITH 5, 3, enab_firm
@ 7, 5 GET ex    PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID Test_Form()

RELEASE WINDOW Get_NN
RELEASE WINDOW H_W_123
SELECT (m.s_sav)

RETURN ret_v

*********************************************************************
PROCEDURE Test_Ye   && ��� □イキ ���? ���※�讚� ��?
IF READKEY() % 256 = 12 .OR. ex = 2 .OR. t_doc = 2
  RETURN .T.
ENDIF
DO CASE
CASE ye < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+'���祠キ皰� 甌 ∇ガキ "ぎ �. �." �� 甌縲��┼�瘡!'
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
CASE ye < 30
  ye = 2000+ye
CASE ye < 100
  ye = 1900+ye
CASE ye < 1900
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+STR(ye,4)+' ���?! �皰, �牀痰�皀, ぎ ┼� ��甄� ��Δキ�� �爬痰�?'
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDCASE
SHOW GET ye
IF ye < YEAR(DATE())-20
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+STR(ye,4)+' ���?! �� 轤� ����� ラ� °��!'
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF  
IF ye > YEAR(DATE())
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+STR(ye,4)+' ���?! �� 轤� ����� ラ� ＜ぅ�!'
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF  

RETURN .T.

*********************************************************************
PROCEDURE Test_Num   && ��� ���ム ぎ�祠キ��? ���※�讚� ��?
PARAMETERS fld

PRIVATE v, res

IF READKEY() % 256 = 12 .OR. ex = 2 && User �皰 ≡ィ� �皖����瘴!
  RETURN .T.
ENDIF

IF fld .AND. EMPTY(d_n) && �薀 �� �諷�� - ��Ν� �痰�※碎 �竅硅�
  RETURN .T.
ENDIF

res = .T.

IF SUBSTR(d_n,5,1) # "-"
  res = .F.
ENDIF
IF LEFT(d_n,1) # sys_char
  res = .F.
ENDIF
v = SUBSTR(d_n,2,3)
IF .NOT. v $ RIGHT(STR(1000+VAL(v),4),3)+"***"
  res = .F.
ENDIF
v = ALLTRIM(SUBSTR(d_n,6))
IF ALLTRIM(STR(VAL(v))) # v
  res = .F.
ENDIF
IF .NOT. res
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ���ム�� 瘍モ�� �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

RETURN res

*********************************************************************
PROCEDURE Test_Form   && �薀� ぎ�祠キ�...
PRIVATE sal_n, sal_d, s_sav

IF READKEY() % 256 = 12 .OR. ex = 2 && User �皰 ≡ィ� �皖����瘴!
  RETURN .T.
ENDIF

IF .NOT. Test_Ye()
  RETURN .F.
ENDIF

IF .NOT. Test_Num()
  RETURN .F.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� �爛く爬閧┘!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

m.s_sav = SELECT()
SELECT 0
USE (base_path+"BILLS") ORDER TAG DOC_NUM ALIAS BILL_000 AGAIN
sal_n = LEFT(d_n,4)+PADL(ALLTRIM(SUBSTR(d_n,6)),6)
sal_d = ye
IF .NOT. SEEK(f_c000+STR(sal_d,4)+sal_n)
  USE
  sal_n = LEFT(sal_n,4)+"-"+ALLTRIM(SUBSTR(sal_n,5))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�腑� "+sal_n+" �� ���ぅ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
ret_v = RECNO()
USE

SELECT (m.s_sav)

RETURN .T.
