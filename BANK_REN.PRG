********************************************************************************
*    ���� Bank_Ren       ���ࠡ��稪 ���३ ��ᨭ                14.01.2012    *
********************************************************************************
*                                                                              *
*                        ���ᮢ�� ��२��������� ������                        *
*                                                                              *
********************************************************************************
PROCEDURE Bank_Ren

PRIVATE tmpShab

m.tmpShab = BankName("������ ��� ���᪠ �����", 40)
IF EMPTY(m.tmpShab)
  RETURN
ENDIF

DO Bank_Lst WITH m.tmpShab
DO Bank_Mrk

m.tmpShab = DBF("TMP_BANKS")
USE IN TMP_BANKS
DELETE FILE (m.tmpShab)
m.tmpShab = LEFT(m.tmpShab, LEN(m.tmpShab)-3)+"cdx"
DELETE FILE (m.tmpShab)

RETURN
