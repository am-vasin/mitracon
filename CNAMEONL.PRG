*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� cNameOnl     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 ��Б��┘ ���瘁� ．� 窶�↓�                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 04.04.2008 様�
PROCEDURE cNameOnl
PARAMETERS sName

PRIVATE i, ln

IF EMPTY(m.sName)
  RETURN ""
ENDIF
m.i = 1
m.ln = LEN(m.sName)
FOR m.i = 1 TO m.ln
  IF .NOT. SUBSTR(m.sName, m.i, 1) $ " 0123456789"
    EXIT
  ENDIF
ENDFOR

RETURN SUBSTR(m.sName, m.i)
