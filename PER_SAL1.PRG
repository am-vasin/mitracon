*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Per_sal1     ��о�｀砒┴ ��むォ ��瓱�           22.01.98 09:53:40 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ｪ��る 甌矗磴�┴�� �� �メ閹��.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Per_Sal1
PRIVATE m0, y0, m1, y1, w

w = DATE()
y1 = YEAR(w)
m1 = MONTH(w)
w = GOMONTH(w,-11)
y0 = YEAR(w)
m0 = MONTH(w)

IF .NOT. Period()
  RETURN
ENDIF

DO Prep_Tmp

PRIVATE new_f, f_sz, a_w
= ADIR(a_w, tmpo_path+"SLR_000.DBF")
f_sz = a_w(1,2)
new_f = New_File("", "DBF", "�磴� ��茱� 爛с�赳��?", f_sz) 

IF .NOT. EMPTY(new_f)
  DO Wt_Mess WITH "����瘡..."
  new_f = ALLTRIM(new_f)
  COPY FILE (tmpo_path+"SLR_000.DBF") TO (new_f)
  DO Wt_Mess
ENDIF

DELETE FILE (tmpo_path+"SLR_000.DBF")

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                    ��皀���皋↓�� 竅����→� �ム┏��.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 22.01.98 09:58:22 夕
PROCEDURE Period
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

*     �����皀 �ム┏�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�    敖陳陳陳陳朕          敖陳陳陳陳朕       �
*�  � �          � 199N  �� �          � 199N  �
*�    青陳陳陳陳潰          青陳陳陳陳潰       �
*�      < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, m_list
DIMENSION m_list(12)
FOR ex = 1 TO 12
  m_list(ex) = Month_R(ex)
ENDFOR
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 49, "�����皀 �ム┏�"
@ 3,  4 SAY "�"
@ 3, 25 SAY "��"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2,  6 GET m0 PICTURE "@^ " FROM m_list SIZE 1, 12
@ 3, 19 GET y0 PICTURE "9999"
@ 2, 28 GET m1 PICTURE "@^ " FROM m_list SIZE 1, 12
@ 3, 41 GET y1 PICTURE "9999"
@ 5,  8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID T_Period()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     蹍▲爲 ��玻オ皚�痰� �ム┏��.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 22.01.98 10:07:47 夕
PROCEDURE T_Period
IF ex = 2
  RETURN .T.
ENDIF

IF 12*y1+m1 < 12*y0+m0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�����讚覃 �メ閹 ｀�跏� ���ョ����? ��� �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 22.01.98 10:22:35 夕
PROCEDURE Prep_Tmp
PRIVATE cr_lin, i, n, y_w, m_w, lin0, lin1, ln, w, dep_w, ln1, wd1
PRIVATE progress, win_name

*         ��爼�珮ガ ����襯
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                          �
*� �發カ     ....:....!....:....!....:....! �
*�                                          �
*� ��矗磴�┴ ....:....!....:....!....:....! �
*�                                          �
*� �メ閹�    ....:....!....:....!....:....! �
*�                                          �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰



USE (base_path+"KARJERA") ORDER TAG CODE DESCENDING

SELECT 0
USE (base_path+"DEPS") ORDER CODE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG DEP_NAME
CALCULATE MAX(LEN(ALLTRIM(FAMILY))) TO ln
ln = MAX(ln+8,20)

wd1 = y1*12+m1-y0*12-m0+1
ln1 = 10
win_name = "Progress"
DO D_Win_N WITH ln1, MAX(wd1, ln)+16, "��爼�珮ガ ����襯"
@ 3, 3 SAY "�發カ"
@ 5, 3 SAY "��矗磴�┴"
@ 7, 3 SAY "�メ閹�"

DELETE FILE (tmpo_path+"SLR_000.DBF")
cr_lin = ""
FOR i = y0*12+m0-1 TO y1*12+m1-1
  m_w = i%12 + 1
  y_w = (i - m_w + 1)/12
  cr_lin = cr_lin+", S"+RIGHT(STR(10000+y_w,5),4)+RIGHT(STR(100+m_w,3),2)+" C(8)"
ENDFOR

cr_lin = "NAME C("+ALLTRIM(STR(ln))+"), DATE_IN C(10)"+cr_lin
SELECT 0
CREATE DBF (tmpo_path+"SLR_000.DBF") ( &cr_lin )


SCATTER TO lin0 BLANK
SCATTER TO lin1 BLANK
lin0(1) = ""
lin1(1) = " ��矗磴�┴� "
lin0(2) = "        "
lin1(2) = "��｀��モ �"
n = 3
FOR i = y0*12+m0-1 TO y1*12+m1-1
  m_w = i%12 + 1
  y_w = (i - m_w + 1)/12
  lin0(n) = PADC(Month_R(m_w),8)
  lin1(n) = STR(y_w,6)
  n = n+1
ENDFOR
APPEND FROM ARRAY lin0
APPEND FROM ARRAY lin1

SELECT PERSONS
dep_w = -1
lin1 = ""
SCAN
  IF dep_w # DEP_NO
    dep_w = DEP_NO
    lin1(1) = IIF(SEEK(dep_w,"DEPS"), "    "+DEPS.B_NAME, "  �ギ��Л���覃 �發カ")
    SELECT SLR_000
    APPEND FROM ARRAY lin1
    SELECT PERSONS
    @ 3, 13 SAY lin1(1)
  ENDIF
  lin0(1) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+". "+LEFT(S_NAME,1)+"."
  lin0(2) = DTOC(DATE_IN)
  @ 5, 13 SAY lin0(1)
  n = 3
  @ 5, 13 SAY lin0(1)
  progress = ""
  FOR i = y0*12+m0-1 TO y1*12+m1-1
    progress = progress+"�"
    @ 7, 13 SAY PADR(progress, wd1)
    m_w = i%12 + 1
    y_w = (i - m_w + 1)/12
    w   = 0
    SELECT KARJERA
    IF SEEK(STR(PERSONS.CODE,5)+STR(y_w,4)+STR(m_w,2))
      w = Crpt_2(SALARY)
    ELSE
      ln = RECNO(0)
      IF BETWEEN(ln, 1, RECCOUNT())
        GO ln
        IF CODE = PERSONS.CODE
          w = Crpt_2(SALARY)
        ENDIF
      ENDIF
    ENDIF
    lin0(n) = TRANSFORM(w,"@Z 99999999")
    n = n+1
  ENDFOR
  SELECT SLR_000
  APPEND FROM ARRAY lin0
  SELECT PERSONS
ENDSCAN

USE

RELEASE WINDOW (win_name)

SELECT KARJERA
USE
SELECT SLR_000
USE

RETURN
