*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� For_V_K      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                          �砒モ か� ���キ皋��.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 04.06.1999 様�
PROCEDURE For_V_K
PRIVATE dt_0, dt_1, mn, ye, dt_w

mn = MONTH(DATE())
ye = YEAR(DATE())

dt_w = "01."+STR(mn,2)+"."+STR(ye,4)
dt_1 = CTOD(dt_w)-1
mn = mn-1
IF mn < 1
  mn = 12
  ye = ye-1
ENDIF
dt_w = "01."+STR(mn,2)+"."+STR(ye,4)
dt_0 = CTOD(dt_w)
dt_0 = DATE()
dt_1 = DATE()

IF .NOT. G_Period(dt_0, dt_1)
  RETURN
ENDIF

USE (base_path+"SALE") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO SALE
SET SKIP TO SALE
PRIVATE sd_000, sr_000, q_000, dt_w

IF .NOT. SEEK(DTOS(dt_0))
  q_000 = RECNO(0)
  IF BETWEEN(q_000,1,RECCOUNT())
    GO q_000
  ENDIF
ENDIF
STORE 0 TO sd_000, sr_000, q_000

dt_w = DOC_DATE
DO Wt_Mess WITH " ��め腑�... "
SCAN REST WHILE DOC_DATE <= dt_1
  IF dt_w # DOC_DATE
    dt_w = DOC_DATE
    @ WROWS()-1, 0 SAY PADC(DTOC(dt_w), WCOLS())
  ENDIF
  sd_000 = sd_000+SALE.QNT*SALE.PRICE_D
  sr_000 = sr_000+SALE.QNT*SALE.PRICE_R
  q_000  = q_000 +SALE.QNT
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

DO V_Res

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� V_Res        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          蹍甃�矗 爛с�赳���.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04.06.1999 陳�
PROCEDURE V_Res

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   ����腑痰〓  999 999          �
*�   �祠�� (�磧) 999 999 999.99   �
*�   �祠�� (USD) 999 999 999.99   �
*�                                �
*�            < OK >              �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 36, "�ェ祀赳��"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 5 SAY "����腑痰〓  "+TRANSFORM(q_000, "999 999")
@ 3, 5 SAY "�祠�� (�磧) "+TRANSFORM(sr_000, "999 999 999.99")
@ 4, 5 SAY "�祠�� (USD) "+TRANSFORM(sd_000, "999 999 999.99")
@ WROWS()-2, 14 GET ex PICTURE "@*HT \! OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
