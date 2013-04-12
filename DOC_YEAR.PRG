*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Doc_Year      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                ฎค คซ๏ คฎชใฌฅญโ  ข แจฌขฎซ์ญฎฌ ไฎเฌ โฅ.                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 16.12.2004 ออผ
PROCEDURE Doc_Year
PARAMETERS pDate

DO CASE
CASE TYPE("m.pDate") = "N"
  RETURN STR(m.pDate,4)
CASE TYPE("m.pDate") = "C"
  RETURN m.pDate
CASE TYPE("m.pDate") = "D"
  RETURN STR(YEAR(m.pDate),4)
ENDCASE
RETURN ""
