*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Left_        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�             ｣����腑�┘ か┃� 瓱�〓�讚��� ���� か� BROWSE.             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 28.01.1999 様�
PROCEDURE Left_
PARAMETERS str_p, ln

PRIVATE str_w

str_w = ALLTRIM(str_p)
IF LEN(str_w) > ln
  str_w = LEFT(str_w, ln-1)+""
ELSE
  str_w = PADR(str_w, ln)
ENDIF

RETURN str_w
