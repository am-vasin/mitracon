********************************************************************************
*    ���� Cls_Name       ���ࠡ��稪 ���३ ��ᨭ                13.03.2012    *
********************************************************************************
*                                                                              *
*                        ������������ ⮢�� ��� �����                        *
*                                                                              *
********************************************************************************
PROCEDURE Cls_Name
PARAMETERS prmCode, prmSelect
*
* prmCode   - ��� �����
* prmSelect - Select ������� ��� ⠡���� ����ᮢ
*
PRIVATE s_sav, retVal

m.s_sav = SELECT()
SELECT (prmSelect)
= SEEK(m.prmCode)
IF .NOT. EMPTY(GOOD_NAME)
  m.retVal = ALLTRIM(GOOD_NAME)
ELSE
  m.retVal = cNameOnl(ALLTRIM(NAME))
ENDIF

SELECT (m.s_sav)

RETURN m.retVal
