*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Get_INN      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                          ���竍キ┘ ��� � ���.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02.09.2005 様�
PROCEDURE Get_INN
PARAMETERS c_code, prmINN, prmKPP

*
* c_code - ��� ��┘���
* prmINN - ���
* prmKPP - ���
*

PRIVATE s_sav

m.s_sav = SELECT()
SELECT 0
USE m.base_path+"CUS_BIL" ORDER TAG CUS_CODE SHARED AGAIN ALIAS CB_5902
m.prmINN = SPACE(FSIZE("INN"))
m.prmKPP = SPACE(FSIZE("KPP"))
IF m.c_code # 0
  IF SEEK(m.c_code)
    m.prmINN = CB_5902.INN
    m.prmKPP = CB_5902.KPP
  ENDIF
ENDIF
USE
SELECT (m.s_sav)

RETURN
