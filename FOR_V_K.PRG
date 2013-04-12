*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� For_V_K      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                          ���� ��� �����⨭�.                          �
*�                                                                        �
*����������������������������������������������������������� 04.06.1999 �ͼ
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
DO Wt_Mess WITH " ������... "
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_Res        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          ��ᬮ�� १����.                          �
*�                                                                        �
*����������������������������������������������������������� 04.06.1999 ���
PROCEDURE V_Res

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

*��������������������������������Ŀ
*�   ������⢮  999 999          �
*�   �㬬� (��) 999 999 999.99   �
*�   �㬬� (USD) 999 999 999.99   �
*�                                �
*�            < OK >              �
*����������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 36, "�������"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 2, 5 SAY "������⢮  "+TRANSFORM(q_000, "999 999")
@ 3, 5 SAY "�㬬� (��) "+TRANSFORM(sr_000, "999 999 999.99")
@ 4, 5 SAY "�㬬� (USD) "+TRANSFORM(sd_000, "999 999 999.99")
@ WROWS()-2, 14 GET ex PICTURE "@*HT \! OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN