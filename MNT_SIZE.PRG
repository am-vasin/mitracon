*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Mnt_Size     ��о�｀砒┴ ��むォ ��瓱�           23.03.98 10:11:44 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�         ����腑痰〓 Л���� ��甄� ぅ�. 皰腦� か� 腮甄�〓�� ����.         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Mnt_Size
PARAMETER f_name

PRIVATE arr_w, i, n

IF EMPTY(ALIAS())
  RETURN 0
ENDIF
n = AFIELDS(arr_w)
IF n = 0
  RETURN 0
ENDIF

FOR i = 1 TO n
  IF UPPER(ALLTRIM(f_name)) == arr_w(i,1)
    RETURN arr_w(i,4)
  ENDIF
ENDFOR

RETURN 0
