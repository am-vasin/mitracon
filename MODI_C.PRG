*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�               �ணࠬ�� ���४樨 ����窨 ���㤭���.                �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Modi_C
PARAMETER ps_code      && ��� ���㤭���

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

*   ���뢠�� 䠩�� ��
PRIVATE ex, slr, slr_sav, f_pay, f_pay_r, s_pay, s_pay_v, s_dat
PRIVATE rst, r_days, o_days, sel_sav, tt, nt1, nt2, sv_rst
sel_sav = SELECT()

SELECT 0
USE (base_path+"KARJERA") ORDER TAG CODE ALIAS K_MODI_C AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_MODI_C AGAIN
SEEK ps_code
tt = ALLTRIM(FAMILY)+" "+ALLTRIM(NAME)+" "+ALLTRIM(S_NAME)

SELECT 0
USE (base_path+"PS_STAT") ORDER TAG CODE ALIAS C_MODI_C AGAIN
IF .NOT. SEEK(ps_code)
  IF FSIZE("SALARY","P_MODI_C") # 0
    slr = P_MODI_C.SALARY
  ELSE
    slr = "    0"
    DO SetPrice WITH "�������!", slr
  ENDIF
  f_pay = "    0"
  ex = "         0"
  DO SetPrice WITH "�������!", f_pay
  DO SetPrice WITH "�������!", ex
  APPEND BLANK
  REPLACE CODE      WITH P_MODI_C.CODE,;
          START     WITH ex,         ;
          REST      WITH ex,         ;
          SAV_REST  WITH ex,         ;
          SALARY    WITH slr,        ;
          PREV_SUM  WITH f_pay,      ;
          PREV_SUMD WITH f_pay,      ;
          PAY_SUM   WITH f_pay,      ;
          PAY_VAL   WITH "�",        ;
          S_DATE    WITH P_MODI_C.DATE_IN
ENDIF

slr     = Crpt_2(SALARY)
slr_sav = slr
f_pay   = Crpt_2(PREV_SUM)
f_pay_r = Crpt_2(PREV_SUMD)
s_pay   = Crpt_2(PAY_SUM)
rst     = Crpt_2(REST)
s_pay_v = IIF(PAY_VAL = "$", "USD", "��")
s_dat   = S_DATE
r_days  = REST_ILL
o_days  = REST_OTP
nt1     = NOTE_1
nt2     = NOTE_2
sv_rst  = Crpt_2(SAV_REST)

*                        �.�.�. ���㤭���
*��������������������������������������������������������������Ŀ
*�               ����� 99999   ���⮪ 9999999.99               �
*� �� ����� ������������������������������������������������Ŀ �
*� �  ����� 99999 �� ��� � �㡫�� 99999  ��௫�� � ??? 99999 � �
*� ������������������������������������������������������������ �
*�                ���室�騩 ���⮪ 9999999.99                �
*�             �� �ਬ�砭�� ��������������������Ŀ             �
*�             �  ....:....!....:....!....:....!  �             �
*�             �  ....:....!....:....!....:....!  �             �
*�             ������������������������������������             �
*� ���. ���᫥�� ��.��.����  ���. ����. 9999  ���. ��. 9999  �
*�               < OK Ctrl-W > < �⪠������ Esc >               �
*����������������������������������������������������������������

ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 14, 66, tt
DO Sun_Bord WITH 3,  3,  5, 62, " ����� "
DO Sun_Bord WITH 7, 15, 10, 50, " �ਬ�砭�� "
*------------------------------------------------------------------------
*      ���� ����� ������
*

@  2, 17 SAY "�����" GET slr PICTURE "@Z 99999"
@  2, 31 SAY "���⮪" GET rst PICTURE "@Z 9999999.99" WHEN .F.
@  4,  6 SAY "�����" GET f_pay PICTURE "@Z 99999"
@  4, 18 SAY "�� ��� � �㡫��" GET f_pay_r PICTURE "@Z 99999"
@  4, 41 SAY "��௫�� �" GET s_pay_v PICTURE "@M USD,��"
@  4, 56 GET s_pay PICTURE "@Z 99999"
@  6, 18 SAY "���室�騩 ���⮪" GET sv_rst PICTURE "@Z 9999999.99"
@  8, 18 GET nt1
@  9, 18 GET nt2
@ 11,  3 SAY "���. ���᫥��" GET s_dat PICTURE "@D"
@ 11, 30 SAY "���. ����." GET r_days PICTURE "9999"
@ 11, 48 SAY "���. ��." GET o_days PICTURE "9999"
@ 12, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  IF slr # slr_sav
    IF .NOT. Confirm_S(slr)
      slr = slr_sav
    ENDIF
  ENDIF
  slr = STR(slr,5)
  DO SetPrice WITH "�������!", slr
  f_pay = STR(f_pay,5)
  DO SetPrice WITH "�������!", f_pay
  f_pay_r = STR(f_pay_r,5)
  DO SetPrice WITH "�������!", f_pay_r
  s_pay = STR(s_pay,5)
  DO SetPrice WITH "�������!", s_pay
  sv_rst = STR(sv_rst,10,2)
  DO SetPrice WITH "�������!", sv_rst
  REPLACE SALARY    WITH slr, ;
          PREV_SUM  WITH f_pay, ;
          PAY_VAL   WITH IIF(s_pay_v = "USD", "$", "�"), ;
          PREV_SUMD WITH f_pay_r, ;
          PAY_SUM   WITH s_pay, ;
          SAV_REST  WITH sv_rst,;
          S_DATE    WITH s_dat, ;
          NOTE_1    WITH nt1,   ;
          NOTE_2    WITH nt2,   ;
          REST_ILL  WITH r_days,;
          REST_OTP  WITH o_days
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
USE

SELECT P_MODI_C
USE

SELECT K_MODI_C
USE

SELECT (sel_sav)

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                         �஢�ઠ ����� ������.                         �
*�                                                                        �
*����������������������������������������������������� 27.10.97 12:19:41 ͼ
PROCEDURE Tst_Blank
PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF slr < 0 .OR. f_pay < 0 .OR. f_pay_r < 0 .OR. s_pay < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ���� �᫮ � ������ �� ����� ���� ����� ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF f_pay > slr
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� �� ����� ���� ����� ������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF f_pay > slr/2
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ����� ����� �������� ������! �� �ࠢ��쭮? [ �� ] [ ��� ]"
  mss(3) = ""
  IF Out_Mess(7,"mss") = 2
    RETURN .F.
  ENDIF
ENDIF
  
RETURN .T.

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                   ���⢥ত���� ��⠭���� ��௫���.                    �
*�                                                                        �
*����������������������������������������������������� 21.01.98 16:01:48 ͼ
PROCEDURE Confirm_S
PARAMETER sss

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

*    ���⢥न� ��������� ������
*����������������������������������Ŀ
*�        ���� ����� 99999         �
*�         ����������Ŀ             �
*�   ����� �          �  ��� 9999   �
*�         ������������             �
*� < OK Ctrl-W > < �⪠������ Esc > �
*������������������������������������

PRIVATE ex, mn0, ye0, ret_v, s_sav
ex   = 1
mn0 = mn0_sav
ye0 = ye0_sav
ret_v = .F.

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 38, "���⢥न� ��������� ������"
@ 2, 10 SAY "���� ����� "+STR(sss,5)
@ 4,  5 SAY "�����"
@ 4, 25 SAY "���"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 11 GET mn0 PICTURE "@^ " FROM mon_list SIZE 1, 12
@ 4, 29 GET ye0 PICTURE "9999"
@ 6,  3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1
  SELECT K_MODI_C
  IF .NOT. SEEK(STR(P_MODI_C.CODE,5)+STR(ye0,4)+STR(mn0,2))
    APPEND BLANK
  ENDIF
  REPLACE CODE   WITH P_MODI_C.CODE, ;
          MN     WITH mn0,           ;
          YE     WITH ye0,           ;
          SALARY WITH Crpt_1(sss,5)
  SKIP
  IF EOF()
    ret_v = .T.
  ELSE
    IF CODE # P_MODI_C.CODE
      ret_v = .T.
    ENDIF
  ENDIF

  mn0_sav = mn0
  ye0_sav = ye0

  SELECT C_MODI_C

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ret_v