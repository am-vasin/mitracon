*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Slr_Rep      ��о�｀砒┴ ��むォ ��瓱�           06.11.97 10:29:18 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                          ����→� �� ��牆��皀.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Slr_Rep
PRIVATE usd_h, usd_r, usd_d, rub_r, kurs_w, is_prev

DIMENSION usd_h(2), usd_r(2), usd_d(2), rub_r(2)
kurs_w = kurs_mmvb

DO Prep_V

is_prev = 1
DO WHILE .T.
  IF View_Slr()
    is_prev = IIF(is_prev=1, 2, 1)
  ELSE
    EXIT
  ENDIF
ENDDO

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           ��ぃ�皰→� ����諷.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.11.97 10:29:42 夕
PROCEDURE Prep_V
PRIVATE w_h, w_r, w_d

usd_h = 0
usd_r = 0
usd_d = 0
rub_r = 0

USE (base_path+"PS_STAT")
SCAN
  *
  *  ��牆����
  *
  w_h = Crpt_2(REST)
  w_h = MAX(w_h,0)
  IF PAY_VAL = "$"
    w_d = Crpt_2(PAY_SUM)
    w_d = MIN(w_d, w_h)
    w_r = w_h-w_d
  ELSE
    w_r = Crpt_2(PAY_SUM)
    w_r = MIN(w_r, w_h)
    w_d = w_h-w_r
  ENDIF
  usd_h(2) = usd_h(2)+w_h
  usd_r(2) = usd_r(2)+w_r
  usd_d(2) = usd_d(2)+w_d
  rub_r(2) = rub_r(2)+ROUND(w_r*kurs_w,2)
  
  *
  *  �����
  *
  w_h = Crpt_2(PREV_SUM)
  w_d = Crpt_2(PREV_SUMD)
  w_r = w_h-w_d
  usd_h(1) = usd_h(1)+w_h
  usd_r(1) = usd_r(1)+w_r
  usd_d(1) = usd_d(1)+w_d
  rub_r(1) = rub_r(1)+ROUND(w_r*kurs_w,2)
ENDSCAN

USE

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                         蹍甃�矗 爛с�赳�皰�.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.11.97 10:44:47 夕
PROCEDURE View_Slr

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�          �瓮�� 9999999.99           �
*�                                     �
*� � 珮゛閻 9999999.99 �� �窶痺 99.999 �
*� 敖 �皰�� 陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �  9999999.99 $   9999999999 珮�  � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�< �ムオ�. Ctrl-W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE w_h, w_r, w_d, r_r, w_k
PRIVATE ex
ex = 1
w_k = kurs_w
w_h = usd_h(is_prev)
w_r = usd_r(is_prev)
w_d = usd_d(is_prev)
r_r = rub_r(is_prev)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 41, IIF(is_prev=1, "�����", "��牆����")
DO Sun_Bord WITH 5, 3, 7, 37, " �皰�� "
@ 6, 32 SAY "珮�"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 12 SAY "�瓮��" GET w_h PICTURE "@Z 9999999.99" VALID Recalc()
@ 4,  3 SAY "� 珮゛閻" GET w_r PICTURE "@Z  9999999.99" VALID Recalc()
@ 4, 23 SAY "�� �窶痺" GET w_k PICTURE "@Z  99.999" VALID Recalc()
@ 6,  6 GET w_d PICTURE "@Z 9999999.99" WHEN .F.
@ 6, 17 SAY "$  " GET r_r PICTURE "@Z 9999999.99" WHEN .F.
@ 8,  2 GET ex PICTURE "@*HT \! �ムオ�. Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                               �ムメ腑�.                                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.11.97 11:23:02 夕
PROCEDURE Recalc

w_d = w_h-w_r
r_r = ROUND(w_r*w_k,2)
SHOW GET w_d
SHOW GET r_r

RETURN .T.
