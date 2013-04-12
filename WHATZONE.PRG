*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  WhatZone      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                   ฏเฅคฅซฅญจฅ งฎญ๋ แชซ ค  ฏฎ  คเฅแใ.                   บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.01.2000 ออผ
PROCEDURE WhatZone
PARAMETERS addr

IF .NOT. USED("ZONES")
  IF FILE_O(base_path+"ZONES.DBF")
    USE (base_path+"ZONES.DBF") ORDER TAG ZONE AGAIN IN 0
  ELSE
    RETURN "  "
  ENDIF
ENDIF

IF SEEK(LEFT(addr,2), "ZONES")
*IF SEEK(LEFT(addr,1), "ZONES")
  RETURN ZONES.CHAR
ENDIF

RETURN "**"
