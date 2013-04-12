*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ R_Index       งเ กฎโ็จช €ญคเฅฉ  แจญ           16.09.98 17:28:31 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                    ฅจญคฅชแ ๆจ๏ ไ ฉซฎข ก ง๋ ค ญญ๋ๅ.                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE R_Index

SET PRINTER TO (log_path+"NIGHT.TXT") ADDITIVE
SET DEVICE TO PRINT

PRIVATE f_list, i, err_lst, err_cod
DIMENSION f_list(4), err_lst(1)

f_list(1) = "STOCK"
f_list(2) = "LIST_DET"
f_list(3) = "ACCOUNT"
f_list(4) = "PERSONS"

err_cod = 0
err_lst(1) = 1705

FOR i = 1 TO ALEN(f_list)
  err_cod = 0
  USE (base_path+f_list(i)) EXCLUSIVE
  IF err_cod = 0
    REINDEX
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+  ;
                     " ” ฉซ "+f_list(i)+" เฅจญคฅชแ ๆจ๏ - OK!"
  ELSE
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+  ;
                     " ” ฉซ "+f_list(i)+" เฅจญคฅชแ ๆจ๏ - ญฅ ใค ซฎแ์ ฏฎซใ็จโ์ คฎแโใฏ!"
  ENDIF
  USE
ENDFOR

SET PRINTER TO PRN:
SET DEVICE TO SCREEN

RETURN
