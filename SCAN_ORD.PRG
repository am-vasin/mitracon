*������������������������������������������������������������������������Ŀ
*�   ��楤�� Scan_Ord     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*���᫥������� ��� �� �।��� ����⮪ "�� ᮡ����"+��ࠢ����� �訡�� �
*�                                                                        �
*����������������������������������������������������������� 17.12.2005 ���
PROCEDURE Scan_Ord
PARAMETERS l_num	&& ����� ᯨ᪠ (C)

PRIVATE s_sav, no, nc

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS LT_5C24 ORDER TAG LST_NUM
SEEK VAL(m.l_num)

SELECT 0
USE (m.base_path+"ORD_T") SHARED AGAIN ALIAS OT_5C24 ORDER TAG DOC_NUM
STORE 0 TO m.no, m.nc

SEEK m.l_num
SCAN REST WHILE LIST_NUM = m.l_num
  IF .NOT. EMPTY(REQ_DATE)
    m.no = m.no+1	&& ���-�� ���, ����� ���� ᮡ����
  ENDIF
  IF .NOT. EMPTY(STO_MARK)
    m.nc = m.nc+1	&& ���-�� �⢥ত����� ���
  ENDIF
ENDSCAN
SEEK m.l_num
IF m.no = 0 .AND. m.nc = 0	&& �� ���� ��� �� ᮡ࠭�, 
                            &&    �� ���� �� ���� ᮡ����
  m.no = 0
  SCAN REST WHILE LIST_NUM = m.l_num	&& �� ��直� ������ ��頥� ����� �㬬. ���
    IF .NOT. EMPTY(CHECK_S)
      REPLACE CHECK_S WITH ""
    ENDIF
    m.no = MAX(m.no, ORD_NUM)
  ENDSCAN
  IF FOUND("LT_5C24")
    IF LT_5C24.ORD_NUM # m.no .OR. LT_5C24.ORD_COUNT # 0
      SELECT LT_5C24
      REPLACE LT_5C24.ORD_NUM   WITH m.no, ;
              LT_5C24.ORD_COUNT WITH 0
    ENDIF
  ENDIF
  USE IN LT_5C24
  USE IN OT_5C24
  SELECT (m.s_sav)
  RETURN
ENDIF

*
*   ��� �㦭� ᮡ����!
m.no = 0
m.nc = 0
SCAN REST WHILE LIST_NUM = m.l_num
  IF EMPTY(REQ_DATE)
    REPLACE REQ_DATE WITH DATE(), ;
            REQ_TIME WITH TIME()
  ENDIF
  IF EMPTY(STO_MARK)
    m.nc = m.nc+1		&& ���, �ॡ��騥 �����ન
  ENDIF
  m.no = MAX(m.no, ORD_NUM)
ENDSCAN
IF FOUND("LT_5C24")
  IF LT_5C24.ORD_NUM # m.no .OR. LT_5C24.ORD_COUNT # m.nc .OR. EMPTY(LT_5C24.DATE_REQ)
    SELECT LT_5C24
    REPLACE LT_5C24.ORD_NUM   WITH m.no,  ;
            LT_5C24.ORD_COUNT WITH m.nc,  ;
            LT_5C24.DATE_REQ  WITH DATE(),;
            LT_5C24.TIME_REQ  WITH TIME()
  ENDIF
ENDIF
USE IN LT_5C24
USE IN OT_5C24
SELECT (m.s_sav)
RETURN