*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ ��腮甄キ�� ぅ�ィ �� ��．��.                �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tbl_Pay
PARAMETERS ps_c, mn, ye
PRIVATE ex, sel_sav, nm, clndr, mss, slr, slr_d, w1, w2
PRIVATE days_w, d_on, m_on, y_on, d_off, m_off, y_off, d_in
PRIVATE cur_year

IF user = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��む鈬�, �� ぅ��� 轤���, ＜ゃ� �ク爬閧��痰�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
sel_sav = SELECT()
SELECT 0
USE (base_path+"PARMS") ALIAS TBL_PAY AGAIN
cur_year = SLR_YEAR
IF cur_year # ye
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�オ竕┤ ��� "+STR(cur_year,4)+", ��腮甄キ�� �� "+STR(cur_year,4)+" ���爛薀��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN .F.
ENDIF

USE (base_path+"MOULD") ALIAS TBL_PAY ORDER TAG MONTH AGAIN
IF .NOT. SEEK(STR(ye,4)+STR(mn,2))
  nm = .T.
ELSE
  clndr = ALLTRIM(DAYS)
  nm = RECNO() = RECCOUNT()
ENDIF
USE
IF nm
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���キ��瑕 �� "+ALLTRIM(Month_R(mn))+" "+STR(ye,4)+ ;
                  "�. �� 竄▲爨ぅ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_sav)
  RETURN .F.
ENDIF

USE (base_path+"PERSONS") ALIAS TBL_PAY ORDER TAG CODE AGAIN
SEEK ps_c
nm = ALLTRIM(FAMILY)+" "+ALLTRIM(NAME)+" "+ALLTRIM(S_NAME)
slr = SALARY
y_on = YEAR(DATE_IN)
m_on = MONTH(DATE_IN)
d_on = DAY(DATE_IN)
y_off = YEAR(DATE_OFF)
m_off = MONTH(DATE_OFF)
d_off = DAY(DATE_OFF)
d_in  = DATE_IN

USE (base_path+"PS_STAT") ALIAS TBL_PAY ORDER TAG CODE AGAIN
IF .NOT. SEEK(ps_c)
  w1 = Crpt_1(0,5)
  w2 = Crpt_1(0,10)
  APPEND BLANK
  REPLACE CODE      WITH ps_c, ;
          START     WITH w2,   ;
          REST      WITH w2,   ;
          SALARY    WITH slr,  ;
          PREV_SUM  WITH w1,   ;
          PREV_SUMD WITH w1,   ;
          PAY_SUM   WITH w1,   ;
          PAY_VAL   WITH "�",  ;
          S_DATE    WITH d_in
ENDIF
slr = Crpt_2(SALARY)
slr_d = ROUND(slr/OCCURS("�",clndr), 2)

SELECT 0
USE (base_path+"LS_TABLE") ALIAS LS_PAY ORDER TAG PS_CODE AGAIN
IF .NOT. SEEK(STR(ps_c,5)+STR(ye,4)+STR(mn,2))
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+nm+"!"
  mss(3) = CHR(0)+"��．�� �� "+ALLTRIM(Month_R(mn))+" "+STR(ye,4)+ ;
                  "�. �� ������キ! �������碎 �� ���キ��瑟? [ �� ] [ �モ ]"
  mss(4) = ""
  IF Out_Mess(7, "mss") = 2
    USE
    SELECT TBL_PAY
    USE
    SELECT (sel_sav)
    RETURN .F.
  ELSE
    days_w = clndr

    DO CASE
    CASE mn+12*ye < m_on+12*y_on && �薀 �� °��
      days_w = REPLICATE("�",LEN(days_w))
    CASE mn+12*ye = m_on+12*y_on && �覊 ��痰.
      IF d_on > 1
        days_w = REPLICATE("�", d_on-1)+SUBSTR(days_w, d_on)
      ENDIF
    ENDCASE

    IF m_off # 0
      DO CASE
      CASE mn+12*ye > m_off+12*y_off && �Ε �� °��
        days_w = REPLICATE("�",LEN(days_w))
      CASE mn+12*ye = m_off+12*y_off && �覊 ��痰.
        IF d_off < LEN(days_w)
          days_w = LEFT(days_w, d_off)+REPLICATE("�",LEN(days_w)-d_off+1)
        ENDIF
      ENDCASE
    ENDIF
    APPEND BLANK
    REPLACE PS_CODE WITH ps_c, ;
            MONTH   WITH mn,   ;
            YEAR    WITH ye,   ;
            DAYS    WITH days_w
  ENDIF
ENDIF

IF ALLTRIM(DAYS) = clndr
  ex = View_1()
ELSE
  ex = View_2()
ENDIF

SELECT TBL_PAY
USE

SELECT LS_PAY
USE

SELECT (sel_sav)
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 ��腮甄キ┘ ��牆��硅 �� ����覃 �メ閹.                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE View_1

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

*....:....!....:....!....:....!....:....!....:....!....:....!....
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�          ��腮甄錺� ��牆��矣 �� ���������� 199?�.           �
*�                                                            �
*�        ��．�� 甌�����モ � ���キ��爛�, �皰�� 999999         �
*�                                                            �
*�              < OK Ctrl-W > < �皖���碎瘴 Esc >              �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, nm_w
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 64, nm

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
nm_w = "��腮甄錺� ��牆��矣 �� "+ALLTRIM(Month_R(mn))+STR(ye,6)+"�."
@ 2, 2 SAY PADC(nm_w, WCOLS()-4)
nm_w = "��．�� 甌�����モ � ���キ��爛�, �皰�� "+ALLTRIM(STR(slr))+"."
@ 4, 2 SAY PADC(nm_w, WCOLS()-4)
@ 6, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  PRIVATE d_num, d_dat
  d_dat = DATE()
  IF slr # 0
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
    REPLACE DOC_NUM  WITH d_num, ;
            DOC_DATE WITH d_dat, ;
            DOC_TIME WITH TIME(),;
            CODE     WITH -1,    ;
            PS_CODE  WITH ps_c,  ;
            TOTAL    WITH Crpt_1(slr,10,2), ;
            YEAR     WITH ye,    ;
            MONTH    WITH mn,    ;
            WHO      WITH user,  ;
            WHERE    WITH arm
    USE
  ENDIF
  SELECT LS_PAY
  REPLACE WHO   WITH user,  ;
          WHERE WITH arm
  SELECT TBL_PAY
  REPLACE REST WITH Crpt_1(Crpt_2(REST)+slr,10,2)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN


*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                ��腮甄キ┘ ��牆��硅 �� �ク���覃 �メ閹.                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE View_2

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

PRIVATE ex, nm_w, h_days, w_days, sum_w, h_ill, p_ill, sum_i, sum_h, n_rest
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 70, nm
DO Sun_Bord WITH 3, 2, 5, 67, " ��｀腮� き� "
DO Sun_Bord WITH 6, 2, 8, 67, " ���讚�膈覃 "
h_days = OCCURS("�", clndr)
w_days = OCCURS("�", LS_PAY.DAYS)
sum_w  = slr_d*w_days
h_ill  = OCCURS("�", LS_PAY.DAYS)
n_rest = OCCURS("�", LS_PAY.DAYS)
p_ill  = MIN(h_ill, TBL_PAY.REST_ILL)
p_ill  = MAX(p_ill, 0)
sum_i  = slr_d*p_ill
sum_h  = sum_w+sum_i
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
nm_w = "��腮甄錺� ��牆��矣 �� "+ALLTRIM(Month_R(mn))+STR(ye,6)+"�."
@ 2, 2 SAY PADC(nm_w, WCOLS()-4)
nm_w = "�� ���キ��瑟 "+STR(h_days,2)+" �� ��．�� "+STR(w_days,2)+  ;
       "  ��瘋キ�� "+STR(slr_d,8,2)+" �皰�� "+STR(sum_w,8,2)
@ 4, 3 SAY PADC(nm_w, WCOLS()-6)
nm_w = "�瓮�� きォ "+STR(h_ill,2)+" ����腮��ガ諷 "+STR(p_ill,2)+ ;
        " ��瘋キ�� "+STR(slr_d,8,2)+" �皰�� "+STR(sum_i,8,2)
@ 7, 3 SAY PADC(nm_w, WCOLS()-6)
@ 9,52 SAY "����� "+STR(sum_h,8,2)
@10,19 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  PRIVATE d_num, d_dat
  d_dat = DATE()
  IF sum_i # 0 .OR. sum_w # 0
    SELECT 0
    USE (base_path+"SLR_MOVE") ORDER TAG DOC_NUM ALIAS TMP_MV AGAIN
    GO BOTTOM
    IF sum_w # 0
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
      REPLACE DOC_NUM  WITH d_num, ;
              DOC_DATE WITH d_dat, ;
              DOC_TIME WITH TIME(),;
              CODE     WITH -2,    ;
              PS_CODE  WITH ps_c,  ;
              TOTAL    WITH Crpt_1(sum_w,10,2), ;
              QNT      WITH w_days,;
              PRICE    WITH Crpt_1(slr_d,10,2), ;
              UNIT     WITH "���. きォ", ;
              YEAR     WITH ye,    ;
              MONTH    WITH mn,    ;
              WHO      WITH user,  ;
              WHERE    WITH arm
    ENDIF
    GO BOTTOM
    IF sum_i # 0
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
      REPLACE DOC_NUM  WITH d_num, ;
              DOC_DATE WITH d_dat, ;
              DOC_TIME WITH TIME(),;
              CODE     WITH -3,    ;
              PS_CODE  WITH ps_c,  ;
              TOTAL    WITH Crpt_1(sum_i,10,2), ;
              QNT      WITH p_ill, ;
              PRICE    WITH Crpt_1(slr_d,10,2), ;
              UNIT     WITH "きォ",;
              YEAR     WITH ye,    ;
              MONTH    WITH mn,    ;
              WHO      WITH user,  ;
              WHERE    WITH arm
    ENDIF
    USE
  ENDIF
  SELECT LS_PAY
  REPLACE WHO   WITH user,  ;
          WHERE WITH arm
  SELECT TBL_PAY
  REPLACE REST WITH Crpt_1(Crpt_2(REST)+sum_w+sum_i,10,2), ;
          REST_ILL WITH REST_ILL-h_ill,      ;
          REST_OTP WITH REST_OTP-n_rest
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
