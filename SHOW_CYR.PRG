*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Show_Cyr     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               �├���├�罔� 瓱�〓��� ��爬���肓 � 皀�痰�.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 06.08.2001 様�
PROCEDURE Show_Cyr
PARAMETERS string, l, c

PRIVATE i, c

@ l, c SAY ""
FOR i = 1 TO LEN(string)
  c = SUBSTR(string, i, 1)
  IF c $ "�、�ぅ颱Ж�������珀矣粤肄蓍跌褄鈿���������������������������������"
    @ ROW(), COL() SAY c COLOR (SCHEME(13,6))
  ELSE
    @ ROW(), COL() SAY c COLOR SCHEME 13
  ENDIF
ENDFOR

RETURN
