*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Release      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                                �〓��碎.                                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 09.04.2001 様�
PROCEDURE Release
PARAMETERS tmpDate, tmpPers

**tmpMONTH - N(2),���ム �メ閹�
**tmpYEAR  - N(4),���
	
PRIVATE mss, sl, nm, tit_1, pic, s_date, days_sh, was_pay, days_w
PRIVATE ex, tmp_arr, i, l0, c0, n_week, y_on, m_on, d_on, y_off, m_off, d_off
PRIVATE tmpMonth, tmpYear
PRIVATE k_days		&& ��ォ �� ���キ��瑟
PRIVATE t_days		&& ��ォ �� ��．��
PRIVATE i_days		&& ���讚�膈諷
PRIVATE ip_days		&& ｯ��腮��ガ諷 ｀�讚�膈覃
PRIVATE d_price		&& �皰━�痰� き�
PRIVATE tmpRest		&& �痰�皰�
PRIVATE slr_w1		&& ｪ���
PRIVATE slr_w2		&& �ぅ�讚�� ��痰�
PRIVATE slr_w		&& �祠��爿�� ��牆����
PRIVATE otp_prc		&& �キ� き�
PRIVATE n_days		&& ��ォ � ��甄イ�ィ� ��腮甄キ��
PRIVATE n_otp		&& ��ォ �皴竅��
PRIVATE n_adm		&& ��ォ �が. �皴竅��

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
  mss(2) = CHR(0)+"���キ��瑕 �� 轤�� �メ閹 �� 竄▲爨ぅ�!"
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
tit_1 = "��．�� �� "
DO CASE
CASE tmpMONTH = 1
  tit_1 = tit_1+"鎰��瑕"
CASE tmpMONTH = 2
  tit_1 = tit_1+"筌∇���"
CASE tmpMONTH = 3
  tit_1 = tit_1+"��珥"
CASE tmpMONTH = 4
  tit_1 = tit_1+"��爛��"
CASE tmpMONTH = 5
  tit_1 = tit_1+"���"
CASE tmpMONTH = 6
  tit_1 = tit_1+"�遶�"
CASE tmpMONTH = 7
  tit_1 = tit_1+"�遨�"
CASE tmpMONTH = 8
  tit_1 = tit_1+"�■竅�"
CASE tmpMONTH = 9
  tit_1 = tit_1+"瓮�硼÷�"
CASE tmpMONTH = 10
  tit_1 = tit_1+"��硼÷�"
CASE tmpMONTH = 11
  tit_1 = tit_1+"��錙瑕"
CASE tmpMONTH = 12
  tit_1 = tit_1+"ぅ��÷�"
ENDCASE
tit_1 = tit_1+STR(tmpYEAR,5)

USE (base_path+"LS_TABLE") ORDER TAG PS_CODE ALIAS LS_TMP AGAIN
***蹍▲瑙ガ,�モ �� 礒� ����� ����瓱
IF .NOT. SEEK(STR(tmpPers,5)+STR(tmpYEAR,4)+STR(tmpMONTH,2))
  IF was_pay
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��．�� か� 甌矗磴�┴� �矚竄痰≪モ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (sl)
    RETURN
  ENDIF
  days_w = days_sh

  DO CASE
  CASE tmpMONTH+12*tmpYEAR < m_on+12*y_on && �薀 �� °��
    days_w = REPLICATE("�",LEN(days_w))
  CASE tmpMONTH+12*tmpYEAR = m_on+12*y_on && �覊 ��痰.
    IF d_on > 1
      days_w = REPLICATE("�", d_on-1)+SUBSTR(days_w, d_on)
    ENDIF
  ENDCASE

  IF m_off # 0
    DO CASE
    CASE tmpMONTH+12*tmpYEAR > m_off+12*y_off && �Ε �� °��
      days_w = REPLICATE(" ",LEN(days_w))
    CASE tmpMONTH+12*tmpYEAR = m_off+12*y_off && �覊 ��痰.
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
  mss(2) = CHR(0)+"�キ譯� �� ��．�� ��腮甄キ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sl)
  RETURN
ENDIF

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

ex   = 1
DIMENSION tmp_arr(LEN(ALLTRIM(DAYS)))
FOR i = 1 TO ALEN(tmp_arr)
  tmp_arr(i) = SUBSTR(DAYS,i,1)
  IF i >= DAY(tmpDate)
    tmp_arr(i) = " "
  ENDIF
ENDFOR

*------------------------------------------------------------------------

*          ���┼�� �. �.
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�      ��．�� �� <�メ閹> <���>       �
*�    敖賃陳陳堕陳賃陳陳堕陳賃陳陳�   �
*�    ����    � 5 嘘12 嘘19 嘘26 嘘   �
*�    ��盂    � 6 嘘13 嘘20 嘘27 嘘   �
*�    ��牾    � 7 嘘14 嘘21 嘘28 嘘   �
*�    ��盂 1 嘘 8 嘘15 嘘22 嘘29 嘘   �
*�    �郾 2 嘘 9 嘘16 嘘23 嘘30 嘘   �
*�    ��ヽ 3 嘘10 嘘17 嘘24 嘘31 嘘   �
*�    ��甞 4 嘘11 嘘18 嘘25 嘘    �   �
*�    青珍陳陳祖陳珍陳陳祖陳珍陳陳�   �
*楽��┓訥陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖                                  崖
*崖                                  崖
*崖                                  崖
*崖                                  崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�      �痰�皰�   99999.99            �
*�      �� ��．�� 99999.99 [ ]        �
*�      �皴竅��襯 99999.99 [ ]        �
*�      �皰��     99999.99            �
*�  < OK Ctrl-W > < �皖���碎瘴 Esc >  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
SELECT 0
USE (base_path+"DAYS_T")
l0 = CEILING(RECCOUNT()/2)
DO D_Win_N WITH 20+l0, 42, nm
DO Sun_Bord WITH 12, 2, 13+l0, WCOLS()-3, "�┓�"
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
*  ��痺ガ 瓮皖�
n_week = DOW(s_date)
n_week = IIF(n_week = 1, 7, n_week-1)
n_week = CEILING((ALEN(tmp_arr)+n_week-1)/7)
l0     = 3
c0     = FLOOR((WCOLS()-4-5*n_week)/2)
@ l0,   c0 SAY+"敖�"+REPLICATE("堕陳�",n_week)+"�"
@ l0+1, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+2, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+3, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+4, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+5, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+6, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+6, c0+1 FILL TO l0+6, c0+2 COLOR (SCHEME(14,2))
@ l0+7, c0 SAY+"���"+REPLICATE("�    ",n_week)+"�"
@ l0+7, c0+1 FILL TO l0+7, c0+2 COLOR (SCHEME(14,2))
@ l0+8, c0 SAY+"青�"+REPLICATE("祖陳�",n_week)+"�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
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
@ WROWS()-6, l0 SAY "�痰�皰�   "+STR(tmpRest, 8, 2)
@ WROWS()-5, l0 SAY "�� ��．�� "+STR(tmpTab, 8, 2)
@ ROW(), COL()+1 GET showTab PICTURE "@*C " VALID View(1)
@ WROWS()-4, l0 SAY "�皴竅��襯 "+STR(tmpOtp, 8, 2)
@ ROW(), COL()+1 GET showOtp PICTURE "@*C " VALID View(2)
@ WROWS()-3, l0 SAY "�皰��     "+STR(tmpTotal, 8, 2)

l0 = FLOOR((WCOLS()-32)/2)
@ WROWS()-2, l0 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF ex = 1 .AND. EMPTY(WHO) .AND. .NOT. was_pay
  *
  * ��腮甄錺� �� ��．��
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
  *  ��腮甄錺� �皴竅��襯
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ClcSlr       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            ��瘍モ ��牆��硅.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.04.2001 陳�
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ClcOtp       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��瘍モ �皴竅��諷.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.04.2001 陳�
PROCEDURE ClcOtp

PRIVATE s_sav, i, tmpValue, m_st, m_en, str_w, dt_sav

s_sav = SELECT()
SELECT 0
USE (base_path+"PS_STAT") ORDER TAG CODE AGAIN ALIAS PS_1409
SEEK tmpPers
m_st = S_DATE
dt_sav = m_st
slr_w1 = Crpt_2(SALARY)           && ｪ���
slr_w2 = Salary2(tmpPers, 3)      && �ぅ�讚�� ��痰�
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


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View         ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                                蹍甃�矗                                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.04.2001 陳�
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� V_Tab        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         ��腮甄キ�� �� ��．��.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.04.2001 陳�
PROCEDURE V_Tab
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*
PRIVATE s_sav, tmpName

s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PS_1409 AGAIN
SEEK tmpPers
tmpName = ALLTRIM(FAMILY)+" "+ALLTRIM(NAME)+" "+ALLTRIM(S_NAME)
USE
SELECT (s_sav)

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

*....:....!....:....!....:....!....:....!....:....!....:....!....
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�             ��腮甄錺� ��牆��矣 �� ���������� 199?�.              �
*楽� ��｀腮� き� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖 �� ���キ��瑟 99 �� ��．�� 99  ��瘋キ�� 99999.99 �皰�� 99999.99 崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*楽� ���讚�膈覃 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖 �瓮�� きォ 99 ����腮��ガ諷 99 ��瘋キ�� 99999.99 �皰�� 99999.99 崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�                                                  ����� 99999.99  �
*�                 < OK Ctrl-W > < �皖���碎瘴 Esc >                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 70, tmpName
DO Sun_Bord WITH  3,  2,  5, 67, " ��｀腮� き� "
DO Sun_Bord WITH  6,  2,  8, 67, " ���讚�膈覃 "
@ 2, 15 SAY "��腮甄錺� ��牆��矣 �� "+Month_R(MONTH(tmpDate))+" "+STR(YEAR(tmpDate),4)+"�."
@ 4, 4 SAY "�� ���キ��瑟"+STR(k_days,3)+" �� ��．��"+STR(t_days,3)+"  ��瘋キ�� "+STR(d_price, 8,2)+" �皰�� "+STR(t_days*d_price, 8,2)
@ 7, 4 SAY "�瓮�� きォ"+STR(i_days,3)+" ����腮��ガ諷"+STR(ip_days,3)+" ��瘋キ�� "+STR(d_price, 8,2)+" �皰�� "+STR(ip_days*d_price, 8,2)
@ 9,52 SAY "����� "+STR((t_days+ip_days)*d_price, 8,2)
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ WROWS()-2, FLOOR(WCOLS()/2-6) GET ex PICTURE "@*HT \ OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� V_Otp        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         ����キ��罔� �� �皴竅�                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10.04.2001 陳�
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

*                          < �┓ ��腮甄キ�� >
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                                    �
*�   ｪ��� 99999 ��. ��痰� 999999.99 �皰�� 999999.99 �� ぅ�� 9999.99  �
*�                                                                    �
*�   ��腮甄�碎 �� 9999 ��� � ��.��.���� �� ��.��.���� �� 9999 きォ    �
*�                                                                    �
*�   �皴竅� 9999 きォ  �が. �皴竅� 9999 きォ  �皰�� 9999 きォ         �
*�                                                                    �
*�                  �皰�� �� 9999 きォ 9999999.99 $                   �
*�                                                                    �
*�                             < OK >                                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰


PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 13, 72, tmpName
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  5 SAY "ｪ���       ��. ��痰�           �皰��           �� ぅ��"
@  3, 11 SAY TRANSFORM(slr_w1, "99999")
@  3, 27 SAY TRANSFORM(slr_w2, "999999.99")
@  3, 43 SAY TRANSFORM(slr_w1+slr_w2, "999999.99")
@  3, 61 SAY TRANSFORM(otp_prc, "9999.99")
@  5,  5 SAY "��腮甄�碎 ��      ��� �            ��            ��      きォ"
@  5, 18 SAY TRANSFORM(YEAR(tmpDate), "9999")
@  5, 29 SAY TRANSFORM(tmpD, "@D")
@  5, 43 SAY TRANSFORM(tmpDate-1, "@D")
@  5, 57 SAY TRANSFORM(n_days, "9999")
@  7,  5 SAY "�皴竅�      きォ  �が. �皴竅�      きォ  �皰��      きォ"
@  7, 12 SAY TRANSFORM(n_otp, "9999")
@  7, 35 SAY TRANSFORM(n_adm, "9999")
@  7, 52 SAY TRANSFORM(n_otp+n_adm, "9999")
@  9, 20 SAY "�皰�� ��      きォ            $"
@  9, 29 SAY TRANSFORM(n_days-n_otp-n_adm, "9999")
@  9, 39 SAY TRANSFORM((n_days-n_otp-n_adm)*otp_prc, "9999999.99")

@ WROWS()-2, FLOOR(WCOLS()/2-6) GET ex PICTURE "@*HT \ OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
