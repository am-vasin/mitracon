*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Nm_Part      ��о�｀砒┴ CMP_LOG.PRG            �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �襪カキ┘ ��痰� ━キ�                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 05/18/2006 様�
PROCEDURE Nm_Part
PARAMETERS prmName, prmLen
*
*  prmName - �痳�き�� ��Б��┘,
*  prmLen  - 矗ァ礇��� か┃�
*

PRIVATE tmpLen
m.tmpLen = 25
IF TYPE("m.prmLen") ="N"
  m.tmpLen = m.prmLen
ENDIF
IF LEN(m.prmName) <= m.tmpLen
  RETURN PADR(m.prmName, m.tmpLen)
ENDIF
IF EMPTY(SUBSTR(m.prmName, m.tmpLen+1))
  RETURN LEFT(m.prmName, m.tmpLen)
ENDIF
RETURN LEFT(m.prmName, m.tmpLen-1)+""
