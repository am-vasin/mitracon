*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Pay_Slr      ��о�｀砒┴ ��むォ ��瓱�           31.10.97 11:52:09 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           �覩���� ��牆��硅.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Pay_Slr
PARAMETERS ps_code, pay_type, ye_p, mn_p, total_pay

PRIVATE sel_sav, ye, mn, mss, h_usd, r_usd, r_rub, kurs_w
PRIVATE tmpPay
PRIVATE q_usd, q_rub
DIMENSION q_usd(1), q_rub(1)
tmpPay = .F.

q_rub = 0
q_usd = 0

h_usd  = 0
r_usd  = 0
r_rub  = 0
kurs_w = kurs_b

sel_sav = SELECT()

SELECT 0
USE (base_path+"MOULD") ORDER TAG MONTH ALIAS MLD_PAY AGAIN
  
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PS_PAY AGAIN
SEEK ps_code
  
SELECT 0
USE (base_path+"PS_STAT") ORDER TAG CODE ALIAS STT_PAY AGAIN
IF .NOT. SEEK(ps_code)
  h_usd = Crpt_1(0,10)
  r_usd = Crpt_1(0,5)
  APPEND BLANK
  REPLACE CODE      WITH ps_code,   ;
          START     WITH h_usd,     ;
          REST      WITH h_usd,     ;
          SALARY    WITH PS_PAY.SALARY, ;
          PREV_SUM  WITH r_usd,     ;
          PREV_SUMD WITH r_usd,     ;
          PAY_SUM   WITH r_usd,     ;
          PAY_VAL   WITH "�",       ;
          S_DATE    WITH PS_PAY.DATE_IN
  h_usd = 0
  r_usd = 0
ENDIF

IF TYPE("ye_p") = "N"
  ye = ye_p
ELSE
  ye = YEAR(DATE())
ENDIF

IF TYPE("mn_p") = "N"
  mn = mn_p
ELSE
  mn = MONTH(DATE())
ENDIF

DO CASE
CASE pay_type = "1"
  DO S_Pay_1 WITH ps_code
CASE pay_type = "2"
  DO S_Pay_2 WITH ps_code, ye, mn
ENDCASE

SELECT PS_PAY
USE
SELECT STT_PAY
USE
SELECT MLD_PAY
USE
SELECT (sel_sav)
RETURN tmpPay

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           �覩���� ������.                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 29.10.97 19:03:23 夕
PROCEDURE S_Pay_1
PARAMETERS ps_code

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
*                   ��┘��
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                           �
*�         �祠�� � ����皀 99999.99$          �
*�                                           �
*� �� ��� � 珮゛閻 99999.99$ �� �窶痺 99.999 �
*�                                           �
*�     �皰�� 99999.99$   9999999.99 珮�.     �
*�                                           �
*� �祚釵�:  USD:  999999   �磧��: 9999999.99 �
*�                                           �
*�    踸�ョ��┘ ....:....!....:....!....    �
*� 敖[ ] �ョ�碎 �痰�爬� ��腮甄キ┤陳陳陳陳朕 �
*� �      � ��.��.���� �� ��.��.����       � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�     < OK Ctrl-W > < �皖���碎瘴 Esc >      �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                < OK >                     �
PRIVATE ex, nt, w_usd, inv, to_print, dt0, dt1, tmpBuff
nt = SPACE(24)
ex   = 1
inv  = 1
IF TYPE("total_pay") # "N"
  h_usd  = Crpt_2(STT_PAY.PREV_SUM)
ELSE
  h_usd  = total_pay
ENDIF
r_usd  = Crpt_2(STT_PAY.PREV_SUMD)
r_rub  = ROUND(r_usd*kurs_w,2)
w_usd  = h_usd-r_usd
to_print = .F.
dt0    = DATE()-60
dt1    = DATE()
tmpBuff = Get_Sys("SlrDate_1")
IF .NOT. EMPTY(tmpBuff)
  dt0 = CTOD(ALLTRIM(LEFT(tmpBuff,10)))
  dt1 = CTOD(ALLTRIM(SUBSTR(tmpBuff,11)))
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 17, 47, ALLTRIM(PS_PAY.FAMILY)+" "+  ;
              LEFT(PS_PAY.NAME,1)+". "+LEFT(PS_PAY.S_NAME,1)+"."
@ 7, 35 SAY "珮�"
@ 9, 3 SAY "�祚釵�:  USD:           �磧��:"
DO Sun_Bord WITH WROWS()-5, 3, WROWS()-3, 43
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  9, 11 GET inv PICTURE "@*IH ;" SIZE 1, 6, 10 VALID Clc_C(9,34,18)
@ 11,  6 SAY "踸�ョ��┘" GET nt
@ WROWS()-5, 5 GET to_print PICTURE "@*C �ョ�碎 �痰�爬� ��腮甄キ┤"
@ WROWS()-4, 10 SAY "�" GET dt0 PICTURE "@D" WHEN to_print
@ WROWS()-4, 23 SAY "��" GET dt1 PICTURE "@D" WHEN to_print
IF TYPE("total_pay") # "N"
  @ WROWS()-2,  7 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
ELSE
  @ WROWS()-2,  FLOOR(WCOLS()/2)-3 GET ex PICTURE "@*HT \ OK "
ENDIF
@  3, 11 SAY "�祠�� � ����皀" GET h_usd PICTURE "@Z 99999.99" VALID Clc_E() ;
                                            WHEN TYPE("total_pay") # "N"
@  5,  3 SAY "�� ��� � 珮゛閻" GET r_usd PICTURE "@Z 99999.99" VALID Clc_E()
@  5, 27 SAY "$ �� �窶痺" GET kurs_w PICTURE "@Z 99.999" VALID Clc_E()
@  7,  7 SAY "�皰��" GET w_usd PICTURE "@Z 99999.99" WHEN .F.
@  7, 21 SAY "$  " GET r_rub PICTURE "@Z 9999999.99" WHEN .F.

READ CYCLE

IF ex = 1 .AND. (h_usd # 0 .OR. r_usd # 0)
  DO Bd_Pay WITH STT_PAY.CODE, h_usd, r_usd, kurs_w, nt, r_usd, kurs_w
  tmpPay = .T.
ENDIF
IF to_print .AND. ex = 1
  DO Per1_Sal WITH STT_PAY.CODE, dt0, dt1
ENDIF
DO Put_Sys WITH "SlrDate_1", PADR(DTOC(dt0),10)+DTOC(dt1)

*--------------------------------------------------------------------------

ON KEY
POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           �ムメ腑� Л�腑�┤.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 31.10.97 17:14:01 夕
PROCEDURE Clc_E

r_rub  = ROUND(r_usd*kurs_w,2)
w_usd  = h_usd-r_usd

SHOW GET r_rub
SHOW GET w_usd

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                             ��め腑� �祚釵.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 31.10.97 18:04:49 夕
PROCEDURE Clc_C
PARAMETERS rw, cl_r, cl_d
PRIVATE v

IF inv = 1
  v = Cupury("$", q_usd, 0, 23)
  @ rw, cl_d SAY TRANSFORM(v, "@Z 999999")
ELSE
  v = Cupury(" ", q_rub, 0, -23)
  @ rw, cl_r SAY TRANSFORM(v, "@Z 9999999.99")
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           �覩���� ��牆��硅.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 29.10.97 19:03:23 夕
PROCEDURE S_Pay_2
PARAMETERS ps_code, ye, mn

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

*             < �┓ ��腮甄キ�� >
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*楽� ���鈑�碎 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖               敖陳陳陳陳朕敖陳陳朕               崖
*崖 臼臼� きォ �� � ....:... 崖 1234 � �� 臼臼臼�.臼 崖
*崖               青陳陳陳陳潰青陳陳潰               崖
*崖                                 �瓮�� 臼臼臼�.臼 崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*� �痰. 999999.99  �ムュ. 999999.99 �皰�� 9999999.99  �
*�                        �皰�� �覩��皋碎 臼臼臼�.臼  �
*�                                                    �
*�    �� ��� � 珮゛閻 臼臼臼�.臼 $ �� �窶痺 臼臼臼    �
*�                                                    �
*�        �皰�� 9999999.99 $   9999999.99 珮�.        �
*�                                                    �
*�     �祚釵�:  USD:  999999   �磧��: 9999999.99      �
*�                                                    �
*�        踸�ョ��┘ ....:....!....:....!....         �
*�     敖[ ] �ョ�碎 �痰�爬� ��腮甄キ┤陳陳陳陳朕      �
*�     �      � ��.��.���� �� ��.��.����       �      �
*�     青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�      �
*�          < OK Ctrl-W > < �皖���碎瘴 Esc >          �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ex, nt, w_usd, inv, rst
PRIVATE ex, mn_list, ye_list, i, ye_numb, ye_w, mn_w, total, prc, q
PRIVATE rst_0, rst_sv, to_print, dt0, dt1, tmpBuff

to_print = .F.
dt0    = DATE()-60
dt1    = DATE()
tmpBuff = Get_Sys("SlrDate_2")
IF .NOT. EMPTY(tmpBuff)
  dt0 = CTOD(ALLTRIM(LEFT(tmpBuff,10)))
  dt1 = CTOD(ALLTRIM(SUBSTR(tmpBuff,11)))
ENDIF
ex   = 1
inv  = 1
rst_0 = Crpt_2(STT_PAY.REST)
rst_sv = Crpt_2(STT_PAY.SAV_REST)
h_usd  = rst_0-rst_sv
rst    = h_usd
IF STT_PAY.PAY_VAL = "$"
  w_usd = Crpt_2(STT_PAY.PAY_SUM)
  r_usd = h_usd-w_usd
ELSE
  r_usd = Crpt_2(STT_PAY.PAY_SUM)
  w_usd = h_usd-r_usd
ENDIF
r_rub  = ROUND(r_usd*kurs_w,2)
ye_numb = MIN(YEAR(DATE())-2, ye)
ex      = MAX(YEAR(DATE())+1, ye)
DIMENSION ye_list(ex-ye_numb+1)
FOR i = 1 TO ex-ye_numb+1
  ye_list(i) = STR(ye_numb+i-1,4)
ENDFOR
nt = SPACE(24)
total = 0
q     = 0
DIMENSION mn_list(12)
FOR i = 1 TO 12
  mn_list(i) = Month_R(i)
ENDFOR
mn_w = mn
ye_w = ASCAN(ye_list, STR(ye,4))

prc   = Set_Pr0()
total= 0
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 23, 56, ALLTRIM(PS_PAY.FAMILY)+" "+  ;
              LEFT(PS_PAY.NAME,1)+". "+LEFT(PS_PAY.S_NAME,1)+"."
DO Sun_Bord WITH 2, 2, 7, 53, " ���鈑�碎 "
DO Sun_Bord WITH WROWS()-5, 3, WROWS()-3, 52
@  4, 10 SAY "きォ ��"
@  8,  3 SAY "�痰. "+TRANSFORM(rst_0, "@Z 999999.99")
@  8, 19 SAY "�ムュ."+TRANSFORM(rst_sv, "@Z 999999.99")
@  8, 36 SAY "�皰�� "+TRANSFORM(rst, "@Z 9999999.99")
@ 15,  7 SAY "�祚釵�:  USD:           �磧��:"
@ 13, 42 SAY "珮�."

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 15, 15 GET inv PICTURE "@*IH ;" SIZE 1, 6, 10 VALID Clc_C(15,38,22)
@ 17, 10 SAY "踸�ョ��┘" GET nt
@ WROWS()-5, 5 GET to_print PICTURE "@*C �ョ�碎 �痰�爬� ��腮甄キ┤"
@ WROWS()-4, 14 SAY "�" GET dt0 PICTURE "@D" WHEN to_print
@ WROWS()-4, 27 SAY "��" GET dt1 PICTURE "@D" WHEN to_print
@ WROWS()-2, 12 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
@  4,  4 GET q PICTURE "@Z 99999" VALID Set_Val(1)
@  3, 18 GET mn_w PICTURE "@^ " FROM mn_list SIZE 1, 12 VALID Set_Val(2)
@  3, 30 GET ye_w PICTURE "@^ " FROM ye_list SIZE 1, 8 VALID Set_Val(2)
@  4, 39 SAY "��" GET prc PICTURE "@Z 9999999.99" VALID Set_Val(3)
@  6, 36 SAY "�瓮��" GET total PICTURE "@Z 9999999.99" VALID Set_Val(4)
@  9, 26 SAY "�皰�� �覩��皋碎" GET h_usd PICTURE "@Z 9999999.99" VALID Set_Val(5)
@ 11,  6 SAY "�� ��� � 珮゛閻" GET r_usd PICTURE "@Z 9999999.99" VALID Set_Val(6)
@ 11, 33 SAY "$ �� �窶痺" GET kurs_w PICTURE "@Z 99.999" VALID Set_Val(7)
@ 13, 10 SAY "�皰��" GET w_usd PICTURE "@Z 9999999.99" WHEN .F.
@ 13, 27 SAY "$  " GET r_rub PICTURE "@Z 9999999.99" WHEN .F.

READ CYCLE

IF ex = 1 .AND. h_usd # 0
  DO Bd_Pay WITH STT_PAY.CODE, h_usd, r_usd, kurs_w, nt, r_usd, kurs_w
  tmpPay = .T.
ENDIF
IF to_print .AND. ex = 1
  DO Per1_Sal WITH STT_PAY.CODE, dt0, dt1
ENDIF
DO Put_Sys WITH "SlrDate_2", PADR(DTOC(dt0),10)+DTOC(dt1)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                  ��瘍モ 罐�� �� �甅����┬ ���キ��瑙.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 30.10.97 09:30:02 夕
PROCEDURE Set_Pr0

PRIVATE sw, wg, d

d = 0
sw = .T.
IF .NOT. SEEK(ye_list(ye_w)+STR(mn_w,2), "MLD_PAY")
  sw = .F.
ELSE
  IF RECNO("MLD_PAY") = RECCOUNT("MLD_PAY")
    sw = .F.
  ELSE
    d = OCCURS("�", MLD_PAY.DAYS)
  ENDIF
ENDIF

IF sw
  wg = ROUND(Crpt_2(STT_PAY.SALARY)/d,2)
ELSE
  wg = 0
  WAIT WINDOW "��������! ���キ��瑕 �� "+Month_R(mn_w)+" "+ye_list(ye_w)+ ;
              " �� 竄▲爨ぅ� ┼� �� 痺薀痰≪モ!"
ENDIF

RETURN wg

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           �ムメ腑� Л�腑�┤.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 03.11.97 16:06:05 夕
PROCEDURE Set_Val
PARAMETER mode

IF mode = 2
  prc = Set_Pr0()
  SHOW GET prc
ENDIF

IF mode < 4
  total = q*prc
  SHOW GET total
ENDIF
  
IF mode < 5
  h_usd = total+rst
  SHOW GET h_usd
ENDIF

IF mode < 6
  IF STT_PAY.PAY_VAL = "$"
    w_usd = Crpt_2(STT_PAY.PAY_SUM)
    r_usd = h_usd-w_usd
  ELSE
    r_usd = Crpt_2(STT_PAY.PAY_SUM)
    w_usd = h_usd-r_usd
  ENDIF
  SHOW GET w_usd
  SHOW GET r_usd
ENDIF

IF mode < 7
  w_usd = h_usd-r_usd
  SHOW GET w_usd
ENDIF

r_rub = ROUND(r_usd*kurs_w,2)
SHOW GET r_rub

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                       �矗�｀皖� � ����� ����諷.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 04.11.97 11:46:15 夕
PROCEDURE Bd_Pay
PARAMETERS ps_c, h_pay, r_pay, kurs_w, nt, r_usd, kurs_w

PRIVATE s_sav, d_d, d_ts, d_tt, dn_k1, dn_k2, dn_s, nm

s_sav = SELECT()

nm = ALLTRIM(PS_PAY.FAMILY)+" "+LEFT(PS_PAY.NAME,1)+" "+LEFT(PS_PAY.S_NAME,1)
SELECT 0
USE (base_path+"SLR_MOVE") ORDER TAG DOC_NUM DESCENDING ALIAS S_MV AGAIN

SELECT 0
USE (base_path+"KASSA_M") ALIAS K_MV AGAIN

SELECT 0
USE (base_path+"KASSA_S") ALIAS K_ST AGAIN

d_d  = DATE()
d_ts = SECONDS()
d_tt = TIME()

dn_k1 = Doc_Num("KASSA_M", "", arm, DATE())
* ����� 紲矗�� ���痰珮�罔� �� 甄竍�� ����腮� ┼� �矚竄痰※� ���▲珥�罔�.
REPLACE SUM_R    WITH SUM_R-ROUND(r_pay*kurs_w,2), ;
        SUM_D    WITH SUM_D-h_pay+r_pay
UNLOCK
USE
dn_k2 = 0
IF r_pay # 0 .AND. kurs_w # 0
  dn_k2 = Doc_Num("KASSA_M", "", arm, DATE())
ENDIF
dn_k1 = sys_char+STR(dn_k1,6)
dn_k2 = sys_char+STR(dn_k2,6)

SELECT S_MV
IF BOF() .OR. EOF()
  dn_s = 1
ELSE
  IF YEAR(DOC_DATE) = YEAR(d_d)
    dn_s = VAL(ALLTRIM(DOC_NUM))+1
  ELSE
    dn_s = 1
  ENDIF
ENDIF
dn_s = STR(dn_s,7)
APPEND BLANK
REPLACE DOC_NUM  WITH dn_s,   ;
        DOC_DATE WITH d_d,    ;
        DOC_TIME WITH d_tt,   ;
        CODE     WITH -4,     ;
        PS_CODE  WITH ps_c,   ;
        TOTAL    WITH Crpt_1(-h_pay,10,2), ;
        R_PART   WITH Crpt_1(r_usd,10,2),  ;
        COURCE   WITH Crpt_1(kurs_w,7,3),  ;
        K_DOC    WITH dn_k1,  ;
        K_DATE   WITH d_d,    ;
        NOTE     WITH nt,     ;
        WHO      WITH user,   ;
        WHERE    WITH arm
USE
SELECT K_MV
APPEND BLANK
REPLACE DOC_NUM  WITH dn_k1, ;
        DOC_DATE WITH d_d,   ;
        DOC_TIME WITH d_ts,  ;
        CODE     WITH 3,     ;
        REF_NUM  WITH dn_s,  ;
        REF_DATE WITH d_d,   ;
        SUM_D    WITH -h_pay,;
        NOTE     WITH nm,    ;
        WHO      WITH user,  ;
        WHERE    WITH arm
IF r_pay # 0 .AND. kurs_w # 0
  APPEND BLANK
  REPLACE DOC_NUM  WITH dn_k2, ;
          DOC_DATE WITH d_d,   ;
          DOC_TIME WITH d_ts,  ;
          CODE     WITH 2,     ;
          SUM_D    WITH r_pay, ;
          SUM_R    WITH -ROUND(r_pay*kurs_w,2),;
          KURS_FL  WITH kurs_w,;
          WHO      WITH user,  ;
          WHERE    WITH arm
ENDIF

USE

SELECT STT_PAY
d_d = Crpt_2(REST)-h_pay
REPLACE REST WITH Crpt_1(d_d,10,2)

SELECT (s_sav)
RETURN
