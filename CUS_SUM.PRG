*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Cus_Sum      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               ��瘍モ ����膈�� 痺��� �� ��┘�矣 �� ぅ��.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 09/20/2000 様�
PROCEDURE Cus_Sum
PARAMETERS pCus, pDate, pFirm

PRIVATE s_sav, opend, tmpSum

s_sav = SELECT()
opend = USED("CUS_SUM")
IF opend
  SELECT CUS_SUM
ELSE
  SELECT 0
  USE (base_path+"INCMONEY") AGAIN ALIAS CUS_SUM ORDER TAG CUS_CODE
ENDIF
= SEEK(pCus)
SUM REST WHILE CUS_CODE = pCus ;
    FOR DOC_TYPE $ " " .AND. DOC_DATE = pDate .AND. pFirm = FIRM ;
    MONEY_R TO tmpSum
IF .NOT. opend
  USE
ENDIF
SELECT (s_sav)
RETURN tmpSum
