*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Crs_Inv      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           �窶� �� �����き��.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 18.12.1998 様�
PROCEDURE Crs_Inv
PARAMETERS d_firm, d_num, d_date, m_name

PRIVATE s_sav, c1, c2

s_sav = SELECT()
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS TMP_8C18 AGAIN
IF .NOT. SEEK(d_firm+LEFT(DTOS(d_date),4)+d_num)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ ����� �����き��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

c1 = 0
c2 = 0
SCAN REST WHILE DOC_NUM = d_num .AND. DOC_DATE = d_date .AND. FIRM = d_firm
  c1 = c1+QNT*PRICE_D
  c2 = c2+QNT*PRICE_R
ENDSCAN
IF c1 # 0
  c1 = ABS(ROUND(c2/c1,3))
ENDIF
USE

c2 = Get_Curs("CURS", "", d_date)

SELECT (s_sav)

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
* �����き�� N A000-000000 �� 99.99.9999
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                      �
*�      �窶� �� �����き��  999.999      �
*�                                      �
*�      �窶� �� 99.99.9999 999.999      �
*�                                      �
*�        ��Μ�皀 艪�-�─磴�...         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 42, "�����き�� N "+LEFT(d_num,4)+"-"+ALLTRIM(RIGHT(d_num,6))+" �� "+DTOC(d_date)

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  8 SAY "�窶� �� �����き��  "+TRANSFORM(c1, "999.999")
@ 5,  8 SAY "�窶� �� "+DTOC(d_date)
@ 5, 27 SAY TRANSFORM(c2, "999.999")
@ WROWS()-2, 11 SAY "��Μ�皀 艪�-�─磴�..."
= INKEY(0,"MH")
IF TYPE("m_name") = "C"
  IF .NOT. EMPTY(m_name)
    SHOW MENU (m_name)
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
