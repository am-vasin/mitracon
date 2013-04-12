*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Key_Seek      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ           “แโ ญฎขช  ฎกเ กฎโ็จชฎข ฏฎจแช  ฏฎ ญ ็ ซ์ญฎฉ กใชขฅ.            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 31.12.1999 ออผ
PROCEDURE Key_Seek

PRIVATE shab_l, shab_u, tmpChar, i, tmpCharL
shab_l = " กขฃคฅ๑ฆงจฉชซฌญฎฏเแโใไๅๆ็่้์๋๊ํ๎๏"
shab_u = "€…๐‘’“”•–—"
FOR i = 1 TO 33
  tmpChar = "'"+SUBSTR(shab_u,i,1)+"'"
  tmpCharL = "'"+SUBSTR(shab_l,i,1)+"'"
  ON KEY LABEL &tmpCharL DO Seek_Key WITH &tmpCharL
  ON KEY LABEL &tmpChar DO Seek_Key WITH &tmpCharL
ENDFOR

RETURN
