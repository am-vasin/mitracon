*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Vol_Mtk      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 蹍��Θ 腑爛� �������� � ��皰�諷 罐���                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 01/10/2002 様�
PROCEDURE Vol_Mtk

PRIVATE d0, d1, d_type, s0

d_type = SET("DATE")
SET DATE GERMAN
d0 = DTOC(DATE())
d0 = "01"+SUBSTR(d0,3)
d0 = CTOD(d0)
d1 = d0-1
d0 = GOMONTH(d0,-1)
SET DATE (d_type)
IF .NOT. G_Period(d0, d1)
  RETURN
ENDIF

USE (base_path+"STOCK") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (base_path+"SALE") SHARED AGAIN ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK
SELECT 0
USE (base_path+"SALE_TIT") SHARED AGAIN ORDER TAG HRON
SET FILTER TO LEFT(DOC_NUM,1) = "B"

DO Wt_Mess WITH "��ぃ�皰→� ����諷"
s0 = 0
d_type = SET("NEAR") = "OFF"
SET NEAR ON
SEEK DTOS(d0)
SCAN REST WHILE DOC_DATE <= d1
  SELECT SALE
  SEEK SALE_TIT.FIRM+LEFT(DTOS(SALE_TIT.DOC_DATE),4)+SALE_TIT.DOC_NUM
  SCAN REST WHILE SALE_TIT.FIRM=FIRM .AND. SALE_TIT.DOC_DATE=DOC_DATE .AND. SALE_TIT.DOC_NUM=DOC_NUM
    s0 = s0+QNT*STOCK.PRICE
  ENDSCAN
  SELECT SALE_TIT
ENDSCAN
USE
SELECT SALE
USE
SELECT STOCK
USE

DO Wt_Mess
DO View_Res

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_Res     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                               �ェ祀赳��                                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10.01.2002 陳�
PROCEDURE View_Res
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

*敖陳陳陳陳陳陳陳朕
*� 9999999999.999 �
*�     < OK >     �
*青陳陳陳陳陳陳陳潰

PRIVATE ex, c
ex = 1
c = ALLTRIM(TRANSFORM(s0, "99 999 999.999"))
c = PADC(c, 14)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 5, 20, "�皰��..."

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2, 3 SAY c
@ WROWS()-2, 7 GET ex PICTURE "@*HT \! OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
