*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  SeekName      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                     ”ฎเฌจเฎข ญจฅ จฌฅญจ คซ๏ ฏฎจแช .                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 02/11/2000 ออผ
PROCEDURE SeekName
PARAMETERS pVal

PRIVATE retVal

IF TYPE("upper_1") # "C"
  PRIVATE upper_1, upper_2
  STORE "" TO upper_1, upper_2
  DO SetUpper
ELSE
  IF LEN(upper_1) < 10
    DO SetUpper
  ENDIF
ENDIF

retVal = CHRTRAN(ALLTRIM(pVal), upper_1, upper_2)
retVal = STRTRAN(retVal, " ")

RETURN retVal
