*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Time_        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*� 蹍������ �爛�÷�М����� ∇ガキ� ├ 瓱�〓�讚��� � 腮甄�〓� � �÷�皚�.  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 12/03/1998 様�
PROCEDURE Time_
PARAMETERS ini_v

PRIVATE h_, m_, s_, res_v
IF TYPE("ini_v") = "C"
  h_ = VAL(ALLTRIM(LEFT(ini_v,2)))
  m_ = VAL(ALLTRIM(SUBSTR(ini_v,4,2)))
  s_ = VAL(ALLTRIM(RIGHT(ini_v,2)))
  res_v = s_+60*m_+3600*h_
ELSE
  h_ = FLOOR(ini_v/3600)
  m_ = FLOOR(MOD(ini_v,3600)/60)
  s_ = MOD(ini_v,60)
  res_v = RIGHT(STR(100+h_),2)+":"+RIGHT(STR(100+m_),2)+":"+RIGHT(STR(100+s_),2)
ENDIF

RETURN res_v
