*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� SOrd_Num     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ���竍�碎 ���ム ┃皀����讚�� ��錢��                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 24.11.2006 様�
PROCEDURE SOrd_Num
PARAMETERS prmList

*
*	prmList - ���ム 甎�瓷� (C)
*
PRIVATE s_sav, tmpNum

m.tmpNum = 0
m.s_sav  = SELECT()
SELECT 0
USE m.base_path+"ORD_T" SHARED AGAIN ALIAS O_TIT_5B15 ORDER TAG DOC_NUM
SEEK m.prmList
SCAN REST WHILE O_TIT_5B15.LIST_NUM = m.prmList
  m.tmpNum = MAX(m.tmpNum, VAL(O_TIT_5B15.CHECK_S))
ENDSCAN

USE

SELECT (m.s_sav)

RETURN m.tmpNum+1
