*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� SeekInt      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 ���瓷 甎�瓷� �� ���ム� ┃皀爿モ-�����.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 11.01.2011 様�
PROCEDURE SeekInt

PRIVATE s_sav, shab, mss

m.shab = Get_ShI()
IF EMPTY(m.shab)
  RETURN 0
ENDIF
m.shab = ALLTRIM(m.shab)

m.s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_ARM DESCENDING SHARED AGAIN ALIAS SEEKINT
SEEK m.arm
LOCATE REST WHILE LST_ARM = m.arm FOR REQ_TYPE = "E" .AND. ALLTRIM(REQ_DOC) == m.shab
IF .NOT. FOUND()
 m.shab = 0
 DIMENSION mss[3]
 mss[1] = ""
 mss[2] = CHR(0)+"�爛え ��荐� 甎�瓷�� ������ ������ �モ..."
 mss[3] = ""
 DO Out_Mess WITH 7, "mss"
ELSE
 m.shab = RECNO()
ENDIF
USE
SELECT (m.s_sav)
RETURN m.shab
