*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Key_Seek     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�           �痰���→� �÷�｀砒┴�� ���瓷� �� �����讚�� ＜�▲.            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 31.12.1999 様�
PROCEDURE Key_Seek

PRIVATE shab_l, shab_u, tmpChar, i, tmpCharL
shab_l = "�、�ぅ颱Ж�������珀矣粤肄蓍跌褄鈿"
shab_u = "���������������������������������"
FOR i = 1 TO 33
  tmpChar = "'"+SUBSTR(shab_u,i,1)+"'"
  tmpCharL = "'"+SUBSTR(shab_l,i,1)+"'"
  ON KEY LABEL &tmpCharL DO Seek_Key WITH &tmpCharL
  ON KEY LABEL &tmpChar DO Seek_Key WITH &tmpCharL
ENDFOR

RETURN
