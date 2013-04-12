*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                  �ணࠬ�� ���᫥��� ����� �� ⠡���.                �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Tbl_Pay
PARAMETERS ps_c, mn, ye
PRIVATE ex, sel_sav, nm, clndr, mss, slr, slr_d, w1, w2
PRIVATE days_w, d_on, m_on, y_on, d_off, m_off, y_off, d_in
PRIVATE cur_year

IF user = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������, �� ����� �⮣�, ���� �����⭮��!"
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
  mss(2) = CHR(0)+"����騩 ��� "+STR(cur_year,4)+", ���᫥��� �� "+STR(cur_year,4)+" ����饭�!"
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
  mss(2) = CHR(0)+"��������� �� "+ALLTRIM(Month_R(mn))+" "+STR(ye,4)+ ;
                  "�. �� �⢥ত��!"
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
  mss(3) = CHR(0)+"������ �� "+ALLTRIM(Month_R(mn))+" "+STR(ye,4)+ ;
                  "�. �� ��������! ��������� �� ���������? [ �� ] [ ��� ]"
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
    CASE mn+12*ye < m_on+12*y_on && �� �� �뫮
      days_w = REPLICATE("�",LEN(days_w))
    CASE mn+12*ye = m_on+12*y_on && �� ���.
      IF d_on > 1
        days_w = REPLICATE("�", d_on-1)+SUBSTR(days_w, d_on)
      ENDIF
    ENDCASE

    IF m_off # 0
      DO CASE
      CASE mn+12*ye > m_off+12*y_off && ��� �� �뫮
        days_w = REPLICATE("�",LEN(days_w))
      CASE mn+12*ye = m_off+12*y_off && �� ���.
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

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                 ���᫥��� ��௫��� �� ����� �����.                  �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE View_1

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

*....:....!....:....!....:....!....:....!....:....!....:....!....
*������������������������������������������������������������Ŀ
*�          ����塞 ��௫��� �� ���������� 199?�.           �
*�                                                            �
*�        ������ ᮢ������ � �������६, �⮣� 999999         �
*�                                                            �
*�              < OK Ctrl-W > < �⪠������ Esc >              �
*��������������������������������������������������������������

PRIVATE ex, nm_w
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 64, nm

*------------------------------------------------------------------------
*      ���� ����� ������
*
nm_w = "����塞 ��௫��� �� "+ALLTRIM(Month_R(mn))+STR(ye,6)+"�."
@ 2, 2 SAY PADC(nm_w, WCOLS()-4)
nm_w = "������ ᮢ������ � �������६, �⮣� "+ALLTRIM(STR(slr))+"."
@ 4, 2 SAY PADC(nm_w, WCOLS()-4)
@ 6, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

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


*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                ���᫥��� ��௫��� �� ������� �����.                 �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE View_2

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

PRIVATE ex, nm_w, h_days, w_days, sum_w, h_ill, p_ill, sum_i, sum_h, n_rest
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 12, 70, nm
DO Sun_Bord WITH 3, 2, 5, 67, " ����稥 ��� "
DO Sun_Bord WITH 6, 2, 8, 67, " ���쭨�� "
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
*      ���� ����� ������
*
nm_w = "����塞 ��௫��� �� "+ALLTRIM(Month_R(mn))+STR(ye,6)+"�."
@ 2, 2 SAY PADC(nm_w, WCOLS()-4)
nm_w = "�� ��������� "+STR(h_days,2)+" �� ⠡��� "+STR(w_days,2)+  ;
       "  ��業�� "+STR(slr_d,8,2)+" �⮣� "+STR(sum_w,8,2)
@ 4, 3 SAY PADC(nm_w, WCOLS()-6)
nm_w = "�ᥣ� ���� "+STR(h_ill,2)+" ����稢����� "+STR(p_ill,2)+ ;
        " ��業�� "+STR(slr_d,8,2)+" �⮣� "+STR(sum_i,8,2)
@ 7, 3 SAY PADC(nm_w, WCOLS()-6)
@ 9,52 SAY "����� "+STR(sum_h,8,2)
@10,19 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

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
              UNIT     WITH "ࠡ. ����", ;
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
              UNIT     WITH "����",;
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