*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Rem_Lst      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ��玻オ罔� �甎�瑯キ���� 甎�瓷�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 07/16/1999 様�
PROCEDURE Rem_Lst
PARAMETERS l_num

PUSH KEY CLEAR
PRIVATE f_name, s_sav
s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS L_9715 AGAIN
f_name = SYS(3)
SEEK STR(l_num,6)
COPY REST WHILE LIST_NUM = STR(l_num,6) TO (tmpo_path+f_name)
DO WHILE SEEK(STR(l_num,6))
  REPLACE LIST_NUM WITH " "
ENDDO
SEEK "      "
DELETE REST WHILE EMPTY(LIST_NUM)
APPEND FROM (tmpo_path+f_name)
DELETE FILE (tmpo_path+f_name+".DBF")
USE
SELECT (s_sav)
POP KEY
RETURN
