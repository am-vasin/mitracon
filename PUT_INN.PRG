*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Put_INN      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           ����瘡 ��� � ���.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02.09.2005 様�
PROCEDURE Put_INN
PARAMETERS c_code, prmINN, prmKPP

*
* c_code - ��� ��┘���
* prmINN - ���
* prmKPP - ���
*

PRIVATE s_sav

IF m.c_code <= 0
  RETURN
ENDIF
m.s_sav = SELECT()
SELECT 0
USE m.base_path+"CUS_BIL" ORDER TAG CUS_CODE SHARED AGAIN ALIAS CB_5902
IF .NOT. SEEK(m.c_code)
  APPEND BLANK
  REPLACE CUS_CODE WITH m.c_code
ENDIF
REPLACE INN WITH m.prmINN, ;
        KPP WITH m.prmKPP
IF EMPTY(CUS_NAME)
  USE m.base_path+"ACCOUNT" ORDER TAG CUS_CODE SHARED AGAIN ALIAS CUS_5902 IN 0
  IF SEEK(CUS_CODE, "CUS_5902")
    REPLACE CUS_NAME WITH CUS_5902.CUS_NAME
  ENDIF
  USE IN CUS_5902
ENDIF

USE

SELECT (m.s_sav)

RETURN
