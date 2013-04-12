*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Rem_Kas       งเ กฎโ็จช €ญคเฅฉ  แจญ           14.01.98 09:29:43 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                   ฎแแโ ญฎขซฅญจฅ ช แแ๋ ฏเฅคฏเจ๏โจ๏                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rem_Kas

USE (base_path+"PARMS")
d_s = DAT_START
USE (base_path+"KASSA_M")
SET FILTER TO DOC_DATE >= d_s
STORE 0 TO s00_r1, s00_d1
SUM SUM_R, SUM_D TO s00_r1, s00_d1
USE (base_path+"KASSA_S")
CLEAR

@ 10, 10 SAY "ฎซซ เ๋: เ แ็."+STR(SUM_D_S+s00_d1,15,4)+" ฏฎ ก งฅ"+STR(SUM_D, 15,4)
@ 12, 10 SAY "ใกซจ:   เ แ็."+STR(SUM_R_S+s00_r1,15,2)+" ฏฎ ก งฅ"+STR(SUM_R, 15,2)

REPLACE SUM_D_S WITH SUM_D-s00_d1
REPLACE SUM_R_S WITH SUM_R-s00_r1
CLEAR

RETURN
