*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ord_Sum      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      ��Г��┘ ┃皀����讚�� ��錢��                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 24.11.2006 様�
PROCEDURE Ord_Sum
PARAMETERS prmList, prmOrd

*
*	prmList - ���ム 甎�瓷� (C);
*	prmOrd  - ���ム ��錢�� - メ�� 礫����, 皰 痺���爿�� ��鍼� 筮爼�珮モ瘴 皰�讓� か� �ゥ
*
PRIVATE s_sav, tmpNum, Cr_OK, tmpKey

m.tmpKey = m.prmList+IIF(TYPE("m.prmOrd")="N", STR(m.prmOrd, 3), "")
m.s_sav  = SELECT()
m.Cr_OK = .F.
SELECT 0
USE m.base_path+"ORD_T" SHARED AGAIN ALIAS TIT_5B15 ORDER TAG DOC_NUM
IF .NOT. SEEK(m.tmpKey)
  USE
  SELECT(m.s_sav)
  RETURN .F.
ENDIF

m.tmpNum = SOrd_Num(m.prmList)
m.tmpNum = STR(m.tmpNum, FSIZE("CHECK_S"))

IF TYPE("m.prmOrd")="N"
  IF EMPTY(CHECK_S) .AND. EMPTY(STO_MARK)
    REPLACE CHECK_S WITH m.tmpNum
    m.Cr_OK = .T.
  ENDIF
ELSE
  SCAN REST WHILE TIT_5B15.LIST_NUM = m.prmList FOR EMPTY(CHECK_S) .AND. EMPTY(STO_MARK)
    REPLACE CHECK_S WITH m.tmpNum
    m.Cr_OK = .T.
  ENDSCAN
ENDIF

USE

SELECT (m.s_sav)

RETURN m.Cr_OK
