*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� P_Bound      ��о�｀砒┴ ���┃� ���矗┘���                 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�             ｯ爛ぅ�キ┘ ┃皀燿��� "ぎ牀��※Л�" か� 罐��               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 23.11.1999 様�
PROCEDURE P_Bound
PARAMETERS parm
EXTERNAL ARRAY prc_00
PRIVATE i, i_sav
IF TYPE ("prc_00(1,1)") # "N" 
  RETURN " "
ENDIF

i_sav = 1
FOR i = 1 TO ALEN(prc_00,1)  
  IF parm <= prc_00(i,1)
    EXIT
  ENDIF
  i_sav = i
ENDFOR
RETURN prc_00(i,2)    
