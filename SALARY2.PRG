*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Salary2      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �ぅ�讚�� ��痰� ����硅                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 04.04.2001 様�
PROCEDURE Salary2
PARAMETERS p_code, n_mn

PRIVATE s_sav, tmpM, tmpSum

s_sav = SELECT()
SELECT 0

USE (base_path+"SLR_MOVE") ORDER TAG PS_CODE1 DESCENDING ALIAS SM_1404 AGAIN
SET FILTER TO CODE = 2

tmpSum = 0
IF SEEK(STR(p_code,5))
  tmpM = Last_Day(p_code)
  tmpM = YEAR(tmpM)*12+MONTH(tmpM)
  SCAN REST WHILE PS_CODE = p_code .AND. tmpM-YEAR*12-MONTH < n_mn
    IF tmpM-YEAR*12-MONTH >= 0
      tmpSum = tmpSum+Crpt_2(TOTAL)
    ENDIF
  ENDSCAN
ENDIF

USE
SELECT (s_sav)

RETURN ROUND(tmpSum/n_mn,2)
