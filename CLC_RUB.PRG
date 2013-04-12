*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               ”ใญชๆจ๏ ฏฅเฅแ็ฅโ  คฎซซ เฎข ข เใกซจ.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Clc_Rub
PARAMETERS dollars, kurs_p00, nds_p00
PRIVATE kurs_w00, nds_w00
kurs_w00 = IIF(TYPE("kurs_p00") = "N", kurs_p00, kurs)
nds_w00 = IIF(TYPE("nds_p00") = "N", nds_p00, nds_flt)

RETURN Round_6(dollars*kurs_w00, nds_w00)
