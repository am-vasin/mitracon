*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Dir_List      งเ กฎโ็จช €ญคเฅฉ  แจญ           13.02.97 13:17:49 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                    ”ฎเฌจเฎข ญจฅ แฏจแช  คจเฅชโฎเจฉ.                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Dir_List
PARAMETER pth

PRIVATE a_w, n1, n2, i

n1 = ADIR(a_w, pth+"*", "D")
n2 = 0

FOR i = 1 TO n1
  IF "D" $ a_w(i,5)
    n2 = n2+1
    DIMENSION wk_list(n2)
    wk_list(n2) = PADR(a_w(i,1),8)+IIF(LEFT(a_w(i,1),1) = ".", "     ", " <DIR>")
  ENDIF
ENDFOR

IF n2 = 0
  DIMENSION wk_list(1)
  wk_list(1) = "              "
ENDIF

= ASORT(wk_list)

RETURN
