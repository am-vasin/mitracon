*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Release      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                                �������.                                �
*�                                                                        �
*����������������������������������������������������������� 09.04.2001 �ͼ
PROCEDURE Release
PARAMETERS tmpDate, tmpPers

**tmpMONTH - N(2),����� �����
**tmpYEAR  - N(4),���
	
PRIVATE mss, sl, nm, tit_1, pic, s_date, days_sh, was_pay, days_w
PRIVATE ex, tmp_arr, i, l0, c0, n_week, y_on, m_on, d_on, y_off, m_off, d_off
PRIVATE tmpMonth, tmpYear
PRIVATE k_days		&& ���� �� ���������
PRIVATE t_days		&& ���� �� ⠡���
PRIVATE i_days		&& ���쭨���
PRIVATE ip_days		&& ����稢����� ���쭨��
PRIVATE d_price		&& �⮨����� ���
PRIVATE tmpRest		&& ���⮪
PRIVATE slr_w1		&& �����
PRIVATE slr_w2		&& ����쭠� ����
PRIVATE slr_w		&& �㬬�ୠ� ��௫��
PRIVATE otp_prc		&& ���� ���
PRIVATE n_days		&& ���� � ��᫥����� ���᫥���
PRIVATE n_otp		&& ���� ���᪠
PRIVATE n_adm		&& ���� ���. ���᪠

STORE 0 TO slr_w1, slr_w, slr_w2, otp_prc, n_days, n_otp, n_adm

STORE 0 TO k_days, t_days, i_days, ip_days, d_price

tmpMonth = MONTH(tmpDate)
tmpYear  = YEAR(tmpDate)

pic = "@M "
sl = SELECT()
SELECT 0
USE (base_path+"MOULD") ORDER TAG MONTH ALIAS ML_TMP AGAIN
SEEK STR(tmpYEAR,4)+STR(tmpMONTH,2)
IF .NOT. FOUND() .OR. RECNO() = RECCOUNT()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��������� �� ��� ����� �� �⢥ত��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sl)
  RETURN
ENDIF
was_pay = ML_TMP.FLAG

days_sh = ALLTRIM(ML_TMP.DAYS)
s_date  = "01."+STR(tmpMONTH,2)+"." +STR(tmpYEAR,4)
s_date	= CTOD(s_date)

USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PS_TMP AGAIN
SEEK tmpPers
nm = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+". "+LEFT(S_NAME,1)+"."
y_on = YEAR(DATE_IN)
m_on = MONTH(DATE_IN)
d_on = DAY(DATE_IN)
y_off = YEAR(DATE_OFF)
m_off = MONTH(DATE_OFF)
d_off = DAY(DATE_OFF)
tit_1 = "������ �� "
DO CASE
CASE tmpMONTH = 1
  tit_1 = tit_1+"ﭢ���"
CASE tmpMONTH = 2
  tit_1 = tit_1+"䥢ࠫ�"
CASE tmpMONTH = 3
  tit_1 = tit_1+"����"
CASE tmpMONTH = 4
  tit_1 = tit_1+"��५�"
CASE tmpMONTH = 5
  tit_1 = tit_1+"���"
CASE tmpMONTH = 6
  tit_1 = tit_1+"���"
CASE tmpMONTH = 7
  tit_1 = tit_1+"���"
CASE tmpMONTH = 8
  tit_1 = tit_1+"������"
CASE tmpMONTH = 9
  tit_1 = tit_1+"ᥭ����"
CASE tmpMONTH = 10
  tit_1 = tit_1+"������"
CASE tmpMONTH = 11
  tit_1 = tit_1+"�����"
CASE tmpMONTH = 12
  tit_1 = tit_1+"�������"
ENDCASE
tit_1 = tit_1+STR(tmpYEAR,5)

USE (base_path+"LS_TABLE") ORDER TAG PS_CODE ALIAS LS_TMP AGAIN
***�஢��塞,��� �� 㦥 ⠪�� �����
IF .NOT. SEEK(STR(tmpPers,5)+STR(tmpYEAR,4)+STR(tmpMONTH,2))
  IF was_pay
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"������ ��� ���㤭��� ���������!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (sl)
    RETURN
  ENDIF
  days_w = days_sh

  DO CASE
  CASE tmpMONTH+12*tmpYEAR < m_on+12*y_on && �� �� �뫮
    days_w = REPLICATE("�",LEN(days_w))
  CASE tmpMONTH+12*tmpYEAR = m_on+12*y_on && �� ���.
    IF d_on > 1
      days_w = REPLICATE("�", d_on-1)+SUBSTR(days_w, d_on)
    ENDIF
  ENDCASE

  IF m_off # 0
    DO CASE
    CASE tmpMONTH+12*tmpYEAR > m_off+12*y_off && ��� �� �뫮
      days_w = REPLICATE(" ",LEN(days_w))
    CASE tmpMONTH+12*tmpYEAR = m_off+12*y_off && �� ���.
      IF d_off < LEN(days_w)
        days_w = LEFT(days_w, d_off)+REPLICATE(" ",LEN(days_w)-d_off+1)
      ENDIF
    ENDCASE
  ENDIF
  APPEND BLANK
  REPLACE MONTH   WITH tmpMONTH, ;
          YEAR    WITH tmpYEAR,  ;
          DAYS    WITH days_w,   ;
          PS_CODE WITH tmpPERS
ENDIF
IF .NOT. EMPTY(WHO) .OR. was_pay
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���죨 �� ⠡��� ���᫥��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sl)
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

ex   = 1
DIMENSION tmp_arr(LEN(ALLTRIM(DAYS)))
FOR i = 1 TO ALEN(tmp_arr)
  tmp_arr(i) = SUBSTR(DAYS,i,1)
  IF i >= DAY(tmpDate)
    tmp_arr(i) = " "
  ENDIF
ENDFOR

*------------------------------------------------------------------------

*          ������� �. �.
*������������������������������������Ŀ
*�      ������ �� <�����> <���>       �
*�    ���������������������������Ŀ   �
*�    ����    � 5 ��12 ��19 ��26 ��   �
*�    ���    � 6 ��13 ��20 ��27 ��   �
*�    ���    � 7 ��14 ��21 ��28 ��   �
*�    ��� 1 �� 8 ��15 ��22 ��29 ��   �
*�    ��� 2 �� 9 ��16 ��23 ��30 ��   �
*�    ���� 3 ��10 ��17 ��24 ��31 ��   �
*�    ��� 4 ��11 ��18 ��25 ��    �   �
*�    �����������������������������   �
*��Ē�������������������������������Ŀ�
*��                                  ��
*��                                  ��
*��                                  ��
*��                                  ��
*������������������������������������ٳ
*�      ���⮪   99999.99            �
*�      �� ⠡��� 99999.99 [ ]        �
*�      ���᪭� 99999.99 [ ]        �
*�      �⮣�     99999.99            �
*�  < OK Ctrl-W > < �⪠������ Esc >  �
*��������������������������������������

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
SELECT 0
USE (base_path+"DAYS_T")
l0 = CEILING(RECCOUNT()/2)
DO D_Win_N WITH 20+l0, 42, nm
DO Sun_Bord WITH 12, 2, 13+l0, WCOLS()-3, "����"
SCAN
  pic = pic+CODE+","
  IF RECNO() <= l0
    @ 12+RECNO(), 4 SAY CODE+"-"+NAME
  ELSE
    @ 12+RECNO()-l0, 22 SAY CODE+"-"+NAME
  ENDIF
ENDSCAN
pic = LEFT(pic,LEN(pic)-1)
USE
SELECT LS_TMP

@ 2, 2 SAY PADC(tit_1, WCOLS()-4)
*
*  ���㥬 ���
n_week = DOW(s_date)
n_week = IIF(n_week = 1, 7, n_week-1)
n_week = CEILING((ALEN(tmp_arr)+n_week-1)/7)
l0     = 3
c0     = FLOOR((WCOLS()-4-5*n_week)/2)
@ l0,   c0 SAY+"���"+REPLICATE("�����",n_week)+"�"
@ l0+1, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+2, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+3, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+4, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+5, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+6, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+6, c0+1 FILL TO l0+6, c0+2 COLOR (SCHEME(14,2))
@ l0+7, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+7, c0+1 FILL TO l0+7, c0+2 COLOR (SCHEME(14,2))
@ l0+8, c0 SAY+"���"+REPLICATE("�����",n_week)+"�"

*------------------------------------------------------------------------
*      ���� ����� ������
*

c0 = c0+4
n_week = DOW(s_date)
n_week = IIF(n_week = 1, 7, n_week-1)
PRIVATE mac

FOR i = 1 TO ALEN(tmp_arr)
  @ l0+n_week, c0 SAY STR(i,2) COLOR (IIF(SUBSTR(days_sh,i,1)="�", ;
                                        SCHEME(13,1), SCHEME(14,2)) )
  mac = STR(i)
  @ l0+n_week, c0+3 SAY tmp_arr(i) && PICTURE pic COLOR SCHEME 1
  n_week = n_week+1
  IF n_week = 8
    n_week = 1
    c0 = c0+5
  ENDIF
ENDFOR

l0 = FLOOR((WCOLS()-18)/2)
PRIVATE tmpRest, tmpTab, tmpOtp, tmpTotal, showTab, showOtp
STORE 0 TO tmpRest, tmpTab, tmpOtp, tmpTotal
STORE .F. TO showTab, showOtp

DO ClcSlr
DO ClcOtp
tmpTotal = tmpRest+tmpTab+tmpOtp
@ WROWS()-6, l0 SAY "���⮪   "+STR(tmpRest, 8, 2)
@ WROWS()-5, l0 SAY "�� ⠡��� "+STR(tmpTab, 8, 2)
@ ROW(), COL()+1 GET showTab PICTURE "@*C " VALID View(1)
@ WROWS()-4, l0 SAY "���᪭� "+STR(tmpOtp, 8, 2)
@ ROW(), COL()+1 GET showOtp PICTURE "@*C " VALID View(2)
@ WROWS()-3, l0 SAY "�⮣�     "+STR(tmpTotal, 8, 2)

l0 = FLOOR((WCOLS()-32)/2)
@ WROWS()-2, l0 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF ex = 1 .AND. EMPTY(WHO) .AND. .NOT. was_pay
  *
  * ����塞 �� ⠡���
  pic = ""
  FOR i = 1 TO ALEN(tmp_arr)
    pic = pic+tmp_arr(i)
  ENDFOR
  SELECT LS_TMP
  REPLACE DAYS WITH pic, ;
          WHO  WITH user,;
          WHERE WITH arm
*  DO Tbl_Pay WITH tmpPers, tmpMONTH, tmpYEAR

  PRIVATE d_num, d_dat
  d_dat = DATE()
  IF tmpTab # 0
    SELECT 0
    USE (base_path+"SLR_MOVE") ORDER TAG DOC_NUM ALIAS TMP_MV AGAIN
    GO BOTTOM
    IF BOF() .OR. EOF()
      d_num = 1
    ELSE
      IF YEAR(d_dat) # YEAR(DOC_DATE)
        d_num = 1
      ELSE
        d_num = VAL(ALLTRIM(DOC_NUM))+1
      ENDIF
    ENDIF
    d_num = STR(d_num,7)
    APPEND BLANK
    REPLACE DOC_NUM  WITH d_num,               ;
            DOC_DATE WITH d_dat,               ;
            DOC_TIME WITH TIME(),              ;
            CODE     WITH -1,                  ;
            PS_CODE  WITH tmpPers,             ;
            TOTAL    WITH Crpt_1(tmpTab,10,2), ;
            YEAR     WITH YEAR(tmpDate),       ;
            MONTH    WITH MONTH(tmpDate),      ;
            WHO      WITH user,                ;
            WHERE    WITH arm
    USE (base_path+"PS_STAT") ORDER TAG CODE AGAIN ALIAS PSS_1411
    SEEK tmpPers
    REPLACE REST WITH Crpt_1(Crpt_2(REST)+tmpTab,10,2)
    USE
    SELECT LS_TMP
  ENDIF
  *
  *  ����塞 ���᪭�
  IF tmpOtp # 0
    SELECT 0
    USE (base_path+"PS_STAT") ORDER TAG CODE AGAIN ALIAS PSS_1411
    SEEK tmpPers
    REPLACE S_DATE WITH tmpDate
    USE
    SELECT LS_TMP
    DO To_Inc WITH -5, tmpPers, 0, 0, tmpOtp,   ;
                   "", YEAR(tmpDate), 0, ""
  ENDIF

  IF .NOT. EMPTY(tmpTotal)
    ex = IIF(Pay_Slr(tmpPers, "1", YEAR(tmpDate), MONTH(tmpDate), tmpTotal), 1, 2)
  ENDIF
  IF ex = 1
    SELECT 0
    USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS PSS_1411
    SEEK tmpPers
    REPLACE DATE_OFF WITH tmpDate
    USE
    SELECT LS_TMP
  ENDIF
ENDIF
USE
SELECT(sl)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� ClcSlr       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            ����� ��௫���.                            �
*�                                                                        �
*����������������������������������������������������������� 09.04.2001 ���
PROCEDURE ClcSlr

PRIVATE s_sav, i, tmpValue

k_days = OCCURS("�", days_sh)
s_sav = SELECT()
SELECT 0
USE (base_path+"PS_STAT") ORDER TAG CODE AGAIN ALIAS PS_1409
SEEK tmpPers
d_price = ROUND(Crpt_2(SALARY)/k_days, 2)
tmpRest = Crpt_2(REST)

t_days = 0
i_days = 0
FOR i = 1 TO ALEN(tmp_arr)
  IF tmp_arr(i) == "�"
    t_days = t_days+1
  ENDIF
  IF tmp_arr(i) == "�"
    i_days = i_days+1
  ENDIF
ENDFOR
ip_days = MIN(i_days, REST_ILL)
ip_days = MAX(ip_days, 0)
tmpTab  = (t_days+ip_days)*d_price
USE

SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� ClcOtp       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ����� ���᪭��.                            �
*�                                                                        �
*����������������������������������������������������������� 09.04.2001 ���
PROCEDURE ClcOtp

PRIVATE s_sav, i, tmpValue, m_st, m_en, str_w, dt_sav

s_sav = SELECT()
SELECT 0
USE (base_path+"PS_STAT") ORDER TAG CODE AGAIN ALIAS PS_1409
SEEK tmpPers
m_st = S_DATE
dt_sav = m_st
slr_w1 = Crpt_2(SALARY)           && �����
slr_w2 = Salary2(tmpPers, 3)      && ����쭠� ����
slr_w  = slr_w1+slr_w2
otp_prc = ROUND(slr_w/330,2)

n_days = MAX(tmpDate-PS_1409.S_DATE, 0)
n_adm  = 0
n_otp  = 0

tmpValue = MONTH(tmpDate-1)+12*YEAR(tmpDate-1)
IF DTOS(tmpDate-1) < DTOS(PS_1409.S_DATE)
  USE
  SELECT(s_sav)
  RETURN
ENDIF

FOR i = 1 TO ALEN(tmp_arr)
  IF tmpValue = MONTH(PS_1409.S_DATE)+12*YEAR(PS_1409.S_DATE)
    IF i < DAY(PS_1409.S_DATE)
      LOOP
    ENDIF
  ENDIF
  IF tmp_arr(i) == "�"
    n_otp = n_otp+1
  ENDIF
  IF tmp_arr(i) == "�"
    n_adm = n_adm+1
  ENDIF
ENDFOR

IF tmpValue > MONTH(PS_1409.S_DATE)+12*YEAR(PS_1409.S_DATE)
  USE (base_path+"LS_TABLE") ORDER TAG PS_CODE AGAIN ALIAS PS_1409
  SEEK STR(tmpPers,5)
  m_st = MONTH(m_st)+12*YEAR(m_st)
  m_en = tmpValue-1
  SCAN REST WHILE PS_CODE = tmpPers FOR BETWEEN(YEAR*12+MONTH, m_st, m_en)
    str_w = DAYS
    IF YEAR*12+MONTH = m_st
      str_w = SUBSTR(str_w, DAY(dt_sav))
    ENDIF
    n_otp = n_otp + OCCURS("�", str_w)
    n_adm = n_adm + OCCURS("�", str_w)
  ENDSCAN
ENDIF

tmpOtp = otp_prc*(n_days-n_adm-n_otp)

USE
SELECT (s_sav)

RETURN


*������������������������������������������������������������������������Ŀ
*�   ��楤�� View         ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                                ��ᬮ��                                �
*�                                                                        �
*����������������������������������������������������������� 09.04.2001 ���
PROCEDURE View
PARAMETERS mode

IF mode = 1
  showTab = .F.
  SHOW GET showTab
  DO V_Tab
ELSE
  showOtp = .F.
  SHOW GET showOtp
  DO V_Otp
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_Tab        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         ���᫥��� �� ⠡���.                          �
*�                                                                        �
*����������������������������������������������������������� 09.04.2001 ���
PROCEDURE V_Tab
*
*  ��६���� ���ﭨ� ��� ������樨
*
PRIVATE s_sav, tmpName

s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PS_1409 AGAIN
SEEK tmpPers
tmpName = ALLTRIM(FAMILY)+" "+ALLTRIM(NAME)+" "+ALLTRIM(S_NAME)
USE
SELECT (s_sav)

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

*....:....!....:....!....:....!....:....!....:....!....:....!....
*������������������������������������������������������������������Ŀ
*�             ����塞 ��௫��� �� ���������� 199?�.              �
*��� ����稥 ��� �������������������������������������������������Ŀ�
*�� �� ��������� 99 �� ⠡��� 99  ��業�� 99999.99 �⮣� 99999.99 ��
*������������������������������������������������������������������ٳ
*��� ���쭨�� ��������������������������������������������������Ŀ�
*�� �ᥣ� ���� 99 ����稢����� 99 ��業�� 99999.99 �⮣� 99999.99 ��
*������������������������������������������������������������������ٳ
*�                                                  ����� 99999.99  �
*�                 < OK Ctrl-W > < �⪠������ Esc >                 �
*��������������������������������������������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 12, 70, tmpName
DO Sun_Bord WITH  3,  2,  5, 67, " ����稥 ��� "
DO Sun_Bord WITH  6,  2,  8, 67, " ���쭨�� "
@ 2, 15 SAY "����塞 ��௫��� �� "+Month_R(MONTH(tmpDate))+" "+STR(YEAR(tmpDate),4)+"�."
@ 4, 4 SAY "�� ���������"+STR(k_days,3)+" �� ⠡���"+STR(t_days,3)+"  ��業�� "+STR(d_price, 8,2)+" �⮣� "+STR(t_days*d_price, 8,2)
@ 7, 4 SAY "�ᥣ� ����"+STR(i_days,3)+" ����稢�����"+STR(ip_days,3)+" ��業�� "+STR(d_price, 8,2)+" �⮣� "+STR(ip_days*d_price, 8,2)
@ 9,52 SAY "����� "+STR((t_days+ip_days)*d_price, 8,2)
*------------------------------------------------------------------------
*      ���� ����� ������
*

@ WROWS()-2, FLOOR(WCOLS()/2-6) GET ex PICTURE "@*HT \ OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_Otp        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         ��������� �� ����                          �
*�                                                                        �
*����������������������������������������������������������� 10.04.2001 ���
PROCEDURE V_Otp

PRIVATE s_sav, tmpName, tmpD

s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PS_1409 AGAIN
SEEK tmpPers
tmpName = ALLTRIM(FAMILY)+" "+ALLTRIM(NAME)+" "+ALLTRIM(S_NAME)
USE (base_path+"PS_STAT") ORDER TAG CODE AGAIN ALIAS PS_1409
SEEK tmpPers
tmpD = S_DATE
USE
SELECT (s_sav)

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

*                          < ��� ���᫥��� >
*��������������������������������������������������������������������Ŀ
*�                                                                    �
*�   ����� 99999 ��. ���� 999999.99 �⮣� 999999.99 �� ���� 9999.99  �
*�                                                                    �
*�   ���᫨�� �� 9999 ��� � ��.��.���� �� ��.��.���� �� 9999 ����    �
*�                                                                    �
*�   ���� 9999 ����  ���. ���� 9999 ����  �⮣� 9999 ����         �
*�                                                                    �
*�                  �⮣� �� 9999 ���� 9999999.99 $                   �
*�                                                                    �
*�                             < OK >                                 �
*����������������������������������������������������������������������


PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 13, 72, tmpName
*------------------------------------------------------------------------
*      ���� ����� ������
*
@  3,  5 SAY "�����       ��. ����           �⮣�           �� ����"
@  3, 11 SAY TRANSFORM(slr_w1, "99999")
@  3, 27 SAY TRANSFORM(slr_w2, "999999.99")
@  3, 43 SAY TRANSFORM(slr_w1+slr_w2, "999999.99")
@  3, 61 SAY TRANSFORM(otp_prc, "9999.99")
@  5,  5 SAY "���᫨�� ��      ��� �            ��            ��      ����"
@  5, 18 SAY TRANSFORM(YEAR(tmpDate), "9999")
@  5, 29 SAY TRANSFORM(tmpD, "@D")
@  5, 43 SAY TRANSFORM(tmpDate-1, "@D")
@  5, 57 SAY TRANSFORM(n_days, "9999")
@  7,  5 SAY "����      ����  ���. ����      ����  �⮣�      ����"
@  7, 12 SAY TRANSFORM(n_otp, "9999")
@  7, 35 SAY TRANSFORM(n_adm, "9999")
@  7, 52 SAY TRANSFORM(n_otp+n_adm, "9999")
@  9, 20 SAY "�⮣� ��      ����            $"
@  9, 29 SAY TRANSFORM(n_days-n_otp-n_adm, "9999")
@  9, 39 SAY TRANSFORM((n_days-n_otp-n_adm)*otp_prc, "9999999.99")

@ WROWS()-2, FLOOR(WCOLS()/2-6) GET ex PICTURE "@*HT \ OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN