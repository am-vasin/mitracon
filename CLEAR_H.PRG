*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Clear_H      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                      ���⨥ ����⪨ "�� ᮡ����"                      �
*�                                                                        �
*����������������������������������������������������������� 17.12.2005 �ͼ
PROCEDURE Clear_H
PARAMETERS tmpList	&& ����� ᯨ᪠ (C)

PRIVATE s_sav, nc, no

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS LST_5C17
IF .NOT. SEEK(VAL(m.tmpList))
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF
SELECT 0
USE (m.base_path+"ORD_T") SHARED AGAIN ORDER TAG DOC_NUM ALIAS ORD_5C17
STORE 0 TO m.nc, m.no
SEEK m.tmpList
SCAN REST WHILE LIST_NUM = m.tmpList
  m.no = MAX(m.no, ORD_NUM)
  IF EMPTY(REQ_DATE)
    IF .NOT. EMPTY(CHECK_S)
      REPLACE CHECK_S WITH ""
    ENDIF
    REPLACE REQ_DATE WITH DATE()
  ENDIF
  IF EMPTY(STO_MARK)
    m.nc = m.nc+1
  ENDIF
ENDSCAN
USE
SELECT LST_5C17
REPLACE ORD_NUM   WITH m.no, ;
        ORD_COUNT WITH m.nc
USE

SELECT (m.s_sav)
RETURN