*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Mnt_Size      งเ กฎโ็จช €ญคเฅฉ  แจญ           23.03.98 10:11:44 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ         ฎซจ็ฅแโขฎ งญ ชฎข ฏฎแซฅ คฅแ. โฎ็ชจ คซ๏ ็จแซฎขฎฃฎ ฏฎซ๏.         บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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
