*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Nt_Lines     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�           �祠�� �牀��瘡�, ��А���� �� 痰牀�� �������� か┃�.           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 01/19/1999 様�
PROCEDURE Nt_Lines
PARAMETERS numb, lin_arr, vlt
*
* numb    - �痳�き�� 腮甄�
* lin_arr - ��瘁│: 腮甄� 蹕ガキ皰� - 腮甄� 痰牀�, �爬 √�ぅ - か┃� 痰牀�,
*                     �爬 �諷�ぅ - ���� 痰牀��.
* vlt     - ���鈞�: "�" ┼� �竅皰 - 珮゛�, "$" - ぎ���琺
*
DIMENSION lin_arr(ALEN(lin_arr))
PRIVATE s_line, f_line, r_line, n_line, j, vlt_w
vlt_w = "�"
IF TYPE("vlt") = "C"
  IF vlt = "$"
    vlt_w = "$"
  ENDIF
ENDIF
n_line = ALEN(lin_arr)
s_line = Num2RStr(FLOOR(numb))+IIF(vlt_w = "$", " � � "," 珮� ")+  ;
         RIGHT(STR(100+(numb-FLOOR(numb))*100),2)+IIF(vlt_w = "$", ""," ���.")
s_line = CHRTRAN(LEFT(s_line, 1), "��も膀葹�", "���������")+SUBSTR(s_line, 2)
FOR j = 1 TO n_line
  f_line = ""
  r_line = ""
  DO Break_Str WITH s_line, f_line, r_line, lin_arr(j)
  lin_arr(j) = f_line
  s_line = r_line
ENDFOR

RETURN
