*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Tst_INN      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             蹍▲爲� ���.                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02.09.2005 様�
PROCEDURE Tst_INN
PARAMETERS c_code, prmINN

*
* c_code - ��� ��┘���
* prmINN - ���
*

PRIVATE s_sav, tmpCode, mss, ret

m.ret = .T.
m.tmpCode = 0
m.s_sav = SELECT()
SELECT 0
USE m.base_path+"ACCOUNT" ORDER TAG CUS_CODE SHARED AGAIN ALIAS CUS_5904
SELECT 0
USE m.base_path+"CUS_BIL" ORDER TAG INN SHARED AGAIN ALIAS CB_5904
SET RELATION TO CUS_CODE INTO CUS_5904
IF SEEK(m.prmINN)
  SCAN REST WHILE m.prmINN = INN FOR EMPTY(CUS_5904.DATE_OFF)
    IF CUS_CODE # m.c_code
      m.tmpCode = CUS_CODE
      EXIT
    ENDIF
  ENDSCAN
ENDIF
IF m.tmpCode # 0
  DIMENSION mss[5]
  mss[1] = ""
  mss[2] = "    ��������! ��┘��"
  mss[3] = '    "'+ALLTRIM(CUS_5904.CUS_NAME)+'"   '
  mss[4] = "    ━ゥ� ����� Ε ���. �皰 ���※�讚�? [ �� ] [ �モ ]   "
  mss[5] = ""
  m.ret = Out_Mess(7, "mss") = 1
ENDIF
USE IN CUS_5904
USE
SELECT (m.s_sav)

RETURN m.ret
