*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Pk_Base       งเ กฎโ็จช €ญคเฅฉ  แจญ           05.10.98 11:08:37 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      “ฏ ชฎขช  ไ ฉซฎข ก ง๋ ค ญญ๋ๅ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Pk_Base
SET PRINTER TO (log_path+"NIGHT.TXT") ADDITIVE
SET DEVICE TO PRINT

PRIVATE f_list, i, err_lst, err_cod
DIMENSION f_list(6), err_lst(1)

f_list(1) = "LIST_DET"
f_list(2) = "LIST_TIT"
f_list(3) = "RYN_L"
f_list(4) = "RYN_L_T"
f_list(5) = "RYN_1_L"
f_list(6) = "RYN_1_T"

err_cod = 0
err_lst(1) = 1705

FOR i = 1 TO ALEN(f_list)
  IF .NOT. File_O(base_path+f_list(i)+".DBF")
    LOOP
  ENDIF
  err_cod = 0
  USE (base_path+f_list(i)) EXCLUSIVE
  IF err_cod = 0
    PACK
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+  ;
                     " ” ฉซ "+f_list(i)+" ใฏ ชฎขช  - OK!"
  ELSE
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+  ;
                     " ” ฉซ "+f_list(i)+" ใฏ ชฎขช  - ญฅ ใค ซฎแ์ ฏฎซใ็จโ์ คฎแโใฏ!"
  ENDIF
  USE
ENDFOR

SET PRINTER TO PRN:
SET DEVICE TO SCREEN

RETURN
