*
*           Поизиция Митракона?
*
PROCEDURE Is_Mitr
PARAMETER prm
PRIVATE tmpAddr, s_sav

IF TYPE("prm") = "C"	&& Адрес
  tmpAddr = LEFT(prm, 2)
ELSE					&& Код
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"STOCK") SHARED AGAIN ALIS STO_TST ORDER TAG CODE
  SEEK prm
  tmpAddr = LEFT(ADDRESS, 2)
  USE
  SELECT (s_sav)
ENDIF

tmpAddr = LEFT(tmpAddr,1)+CHRTRAN(SUBSTR(tmpAddr, 2, 1), ;
          "ABCDEFGHIJKLMNOPQRSTUVWXYZ", ;
          "AAAAAAAAAAAAAAAAAAAAAAAAAA")

RETURN tmpAddr # "1A"
