*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Articul      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     踳�÷�М���┘ ���� � �珥┴祀                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 25.12.2007 様�
PROCEDURE Articul
PARAMETERS prmCode

PRIVATE tmpNum, tmpRes

m.tmpNum = ALLTRIM(STR(prmCode))
m.tmpRes = ""
DO WHILE .NOT. EMPTY(m.tmpNum)
  IF LEN(m.tmpNum) <= 2
    m.tmpRes = m.tmpNum+m.tmpRes
    EXIT
  ENDIF
  m.tmpRes = "-"+RIGHT(m.tmpNum, 2)+m.tmpRes
  m.tmpNum = LEFT(m.tmpNum, LEN(m.tmpNum)-2)
ENDDO

RETURN PADL(m.tmpRes, 10)
