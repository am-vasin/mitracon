*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Time_Inc     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             �牀� ��痰�→�                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 24.12.2005 様�
PROCEDURE Time_Inc
PARAMETERS s_code

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
PRIVATE s_sav, tmpC

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 ��━キ����┘ ��Ж罔� 陳陳陳陳陳陳陳陳陳� �
*� �.... ....:....!....:....!....: ....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�        �牀� ��痰�→� ....:....!....:.       �
*�               < �諷�� Esc >                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, tmpCode, tmpTime, tmpName
m.ex = 1

m.s_sav = SELECT()

USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS ST_5C24 IN 0
= SEEK(m.s_code, "ST_5C24")
m.tmpCode = ST_5C24.PRO_CODE
m.tmpName = ALLTRIM(ST_5C24.PREFIX)
m.tmpName = m.tmpName+IIF(EMPTY(m.tmpName), ""," ")+ALLTRIM(ST_5C24.NAME)

SELECT 0
DO Use_DMS WITH "DMS_5C24"
SET ORDER TO TAG CODE
m.tmpC = ALLTRIM(STR(m.s_code))
m.tmpC = PADR(m.tmpC, FSIZE("CODE", "DMS_5C24"))
= SEEK(m.tmpC, "DMS_5C24")
m.tmpTime = DMS_5C24.SUP_DATE

USE
SELECT (m.s_sav)

USE (m.base_path+"DEVELOP") SHARED AGAIN ORDER TAG CODE ALIAS PRO_5C24 IN ST_5C24
= SEEK(m.tmpCode, "PRO_5C24")
IF .NOT. EMPTY(PRO_5C24.B_NAME)
  m.tmpName = m.tmpName+" /"+ALLTRIM(PRO_5C24.B_NAME)+"/"
ENDIF
IF EMPTY(m.tmpTime)
  m.tmpTime = PRO_5C24.PERIOD
ENDIF
USE IN PRO_5C24
IF EMPTY(m.tmpTime)
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��� ��Ж罔� 痼�� ��痰�→� �� 礫����."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 49, "�牀� ��痰�→� か� ��Ж罔�"
DO Sun_Bord WITH  2,  3,  4, 45, " ��━キ����┘ ��Ж罔� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  4 SAY m.tmpName
@  5, 10 SAY "�牀� ��痰�→� "+m.tmpTime

@ WROWS()-2, FLOOR(WCOLS()/2-6) GET m.ex PICTURE "@*HT \? �諷�� Esc "

READ CYCLE

*IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  
*ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
