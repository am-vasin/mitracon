*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Pay_Type     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                              �┓ ���皀��                               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 31.08.2001 様�
PROCEDURE Pay_Type
PARAMETERS p_code, sgn

DO CASE
CASE p_code = " "
  IF TYPE("sgn") = "N"
    IF sgn < 0
      RETURN "��痳�き覃 �爐ム"
    ELSE
      RETURN "踸絎き覃 �爐ム"
    ENDIF
  ELSE
    RETURN "踸絎き覃 �爐ム"
  ENDIF
CASE p_code = "1"
  RETURN "�/� ������"
CASE p_code = "�"
  RETURN "�オ"
CASE p_code = "3"
  RETURN "��ぅ��. ���皀�"
CASE p_code = "�"
  RETURN "ｯ���� 腑爛� �/�"
ENDCASE

RETURN ""
