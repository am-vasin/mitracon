*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Copy_W_D     ��о�｀砒┴ ��むォ ��瓱�           30.09.98 18:10:17 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�         ����牀���┘ ����� � �ムオ�え牀→�� ├ Windows� � DOS.          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Copy_W_D
PARAMETERS s_file, d_file
PRIVATE s_n, d_n, buff, d_t, w_t

d_t = LEFT(Table_D(),66)
w_t = LEFT(Table_W(),66)
DELETE FILE (d_file)
s_n = FOPEN(s_file)
d_n = FCREATE(d_file)
DO WHILE .NOT. FEOF(s_n)
  buff = FREAD(s_n, 70)
  buff = CHRTRAN(buff, w_t, d_t)
  = FWRITE(d_n, buff, LEN(buff))
ENDDO
= FCLOSE(s_n)
= FCLOSE(d_n)

RETURN
