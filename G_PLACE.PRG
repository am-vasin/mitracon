*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ G_Place       งเ กฎโ็จช €ญคเฅฉ  แจญ           01.04.97 11:54:03 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ           €ซ์โฅเญ โจขญ๋ฉ จ กฅง ซ์โฅเญ โจขญ๋ฉ ข๋กฎเ ฏซฎ้ คฎช.           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE G_Place
PARAMETER l0, c0, result
*
*   l0, c0  - ชฎฎเคจญ โ๋.
*   result - ฅแซจ ็จแซฎ -  ซ์โฅเญ โจขญ๋ฉ ข๋กฎเ,
*             ฌ แแจข ซฎฃ. ฏฅเฅฌฅญญ๋ๅ - กฅง ซ์โฅเญ โจขญ๋ฉ ข๋กฎเ.
*

PRIVATE i, n, sel_sav, pic, p_names

sel_sav = SELECT()
SELECT 0
USE (base_path+"PLACES") ALIAS Pl000 AGAIN
n = RECCOUNT()
IF TYPE("result") # "N"
  DIMENSION result(n)
  FOR i = 1 TO n
    @ l0+i-1, c0 GET result(i) PICTURE "@*C "+P_NAME
    SKIP
  ENDFOR
ELSE
  pic = "@*R "
  FOR i = 1 TO n
    pic = pic+P_NAME+";"
    SKIP
  ENDFOR
  pic = LEFT(pic, LEN(pic)-1)
  @ l0, c0 GET result PICTURE pic
ENDIF

USE

SELECT (sel_sav)

RETURN
