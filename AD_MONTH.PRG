*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Ad_Month      ��о�｀砒┴ ��むォ + ����         08.04.97 21:18:48 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ����←キ┘ ����瓱 � �� ��゛����.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Ad_Month
GO BOTTOM IN MOULD
IF BOF() .OR. EOF()
  t_month = MONTH(DATE())
  t_year  = YEAR(DATE())
ELSE
  t_month = MOULD.MONTH+1
  t_year  = MOULD.YEAR
ENDIF
IF t_month >12
  t_year = Year +1
  t_month = 1
ENDIF
DO Mould WITH t_month,t_year 

RETURN
