*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Adm_List     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�     ��玻オ罔� ぎ痰祚� � ������� ��｀腑�� �メ�� か� �が┃�痰��皰��.     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 15.09.2006 様�
PROCEDURE Adm_List

PRIVATE tmpArm

m.tmpArm = ""
DO Ch_Arm WITH m.tmpArm

IF .NOT. EMPTY(m.tmpArm)
  DO Let_List WITH m.tmpArm
ENDIF

RETURN