*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Crs_Inv      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                           ���� �� ���������.                           �
*�                                                                        �
*����������������������������������������������������������� 18.12.1998 �ͼ
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
  mss(2) = CHR(0)+"��� ⠪�� ���������!"
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
* ��������� N A000-000000 �� 99.99.9999
*��������������������������������������Ŀ
*�                                      �
*�      ���� �� ���������  999.999      �
*�                                      �
*�      ���� �� 99.99.9999 999.999      �
*�                                      �
*�        ������ ��-�����...         �
*����������������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 9, 42, "��������� N "+LEFT(d_num,4)+"-"+ALLTRIM(RIGHT(d_num,6))+" �� "+DTOC(d_date)

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3,  8 SAY "���� �� ���������  "+TRANSFORM(c1, "999.999")
@ 5,  8 SAY "���� �� "+DTOC(d_date)
@ 5, 27 SAY TRANSFORM(c2, "999.999")
@ WROWS()-2, 11 SAY "������ ��-�����..."
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