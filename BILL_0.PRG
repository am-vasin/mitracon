*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Bill_0       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 蹍������ �覩�瓷� �/┼� �ョ�皋 瘍モ�.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 19.01.1999 様�
PROCEDURE Bill_0
PARAMETERS l_num

PRIVATE s_sav, d_num, d_dat, d_fir

d_num = ""
d_dat = {}
d_fir = ""
s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_9119 AGAIN
IF .NOT. SEEK(l_num)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���瓷� N "+ALLTRIM(l_num)+" �� 痺薀痰≪モ"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

IF .NOT. EMPTY(BILL_NUM)
  d_num = LEFT(BILL_NUM,4)+"-"+ALLTRIM(RIGHT(BILL_NUM,6))
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                      �
*� < ���ョ���碎 > < �覩���碎 ���覃 > < �皖���碎瘴 Esc > �
*�                                                      �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                 �
*� < �覩���碎 > < �皖���碎瘴 Esc > �
*�                                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
IF EMPTY(d_num)
  DO D_Win_N WITH 6, 37, "�腑�"
ELSE
  DO D_Win_N WITH 6, 58, "�腑� N "+d_num
ENDIF

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*


IF EMPTY(d_num)
  @ 3, 3 GET ex PICTURE "@*HT \ �覩���碎 ;\? �皖���碎瘴 Esc "
ELSE
  @ 3, 3 GET ex PICTURE "@*HT \ ���ョ���碎 ;\ �覩���碎 ���覃 ;\? �皖���碎瘴 Esc "
ENDIF
READ CYCLE
IF EMPTY(d_num)
  ex = ex+1
ENDIF

DO CASE
CASE ex = 1
  DO P_Bill WITH BILL_FIRM, BILL_NUM, BILL_YEAR
CASE ex = 2
  DO Bill WITH l_num
ENDCASE

USE
SELECT (s_sav)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
