*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� F_Stuff      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    ���皀�痰��� ��め����→� � �����.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 05/20/1999 様�
PROCEDURE F_Stuff
PARAMETERS f_name, src, res

PRIVATE pos, nf

pos = F_Seek(f_name, src)
IF pos = -1
  RETURN -1
ENDIF

nf = FOPEN(f_name, 2)
= FSEEK(nf,pos)
= FWRITE(nf,res)
= FCLOSE(nf)

RETURN 0
