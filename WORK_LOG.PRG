*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Work_Log      งเ กฎโ็จช €ญคเฅฉ  แจญ           13.02.98 11:22:21 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                             ฏจแ์ ข ฆใเญ ซ.                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Work_Log
PARAMETERS nt, u, p
*
* nt - โฅชแโ ฏเจฌฅ็ ญจ๏;
* u  - ชฎค ฏฎซ์งฎข โฅซ๏;
* p  - เ กฎ็ฅฅ ฌฅแโฎ.
*

PRIVATE s_sav

IF .NOT. File_O(base_path+"WORK_LOG.DBF")
  RETURN
ENDIF

s_sav = SELECT()

USE (base_path+"WORK_LOG.DBF") ORDER TAG HRON ALIAS "WL000" AGAIN
REPLACE DATE_ WITH DATE(), ;
        TIME_ WITH TIME(), ;
        NOTE  WITH nt,     ;
        WHO   WITH u,      ;
        WHERE WITH p
USE
SELECT (s_sav)

RETURN
