*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Rem_Lst       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                     ฎเเฅชๆจ๏ จแฏฎเ็ฅญญฎฃฎ แฏจแช .                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 07/16/1999 ออผ
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
