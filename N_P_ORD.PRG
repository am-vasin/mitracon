*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� N_P_Ord      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�    ��メ皰 �ョ�皋 ��錢��: �ョ�碎 痺���爿�� ��錢�� + �爛あ. 甌Г��┘     �
*�                            メ�� 矗ァ礇矚�.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 25.11.2006 様�
PROCEDURE N_P_Ord
PARAMETERS prmList, prmOrd
*
* prmList - ���甌� (C)
* prmOrd  - ��錢�� (N)

PRIVATE s_sav
m.s_sav = SELECT()
SELECT 0
USE m.base_path+"ORD_T" SHARED AGAIN ORDER TAG DOC_NUM ALIAS ORD_6B25
IF .NOT. SEEK(m.prmList+STR(m.prmOrd, 3))
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF

IF EMPTY(REQ_DATE)
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF

IF EMPTY(CHECK_S)
  DO Ord_Sum WITH LIST_NUM
ENDIF

= SEEK(m.prmList+STR(m.prmOrd, 3))
DO P_Ord_S WITH LIST_NUM, CHECK_S

USE
SELECT (m.s_sav)
RETURN
