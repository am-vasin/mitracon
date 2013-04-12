*
*   è•‡•ÂÆ§ ≠† ≠Æ¢Î• Ê•≠Î
*
PROCEDURE New_Pri

USE STOCK
s01 = 0
s02 = 0

DELETE FILE d:\s_list.dbf

SELECT 0

CREATE DBF d:\s_list ;
  ( code n(7), prefix c(4), name c(25), producer c(8), qnt n(6),  ;
    old_p n(8), new_p n(8) )

SELECT STOCK
SCAN FOR PRICE_R # 0
  IF PRICE_R < 100
    s02 = s02+(QNT+QNT_SAVE)*(120-PRICE_R)
    SELECT s_list
    APPEND BLANK
    REPLACE code     WITH STOCK.code, ;
            prefix   WITH STOCK.prefix, ;
            name     WITH STOCK.name, ;
            producer WITH STOCK.producer, ;
            qnt      WITH STOCK.qnt+STOCK.QNT_SAVE, ;
            old_p    WITH STOCK.old_p, ;
            new_p    WITH 120
    SELECT STOCK
  ELSE
    s01 = s01+(QNT+QNT_SAVE)*(ROUND_6(PRICE_R)-PRICE_R)
  ENDIF
ENDSCAN

CLOSE DATABASES
clear
? s01, s02


RETURN
*…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª
*∫ î†©´ Round_6      ê†ß‡†°Æ‚Á®™ Ä≠§‡•© Ç†·®≠           03.02.97 12:39:32 ∫
*«ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ∂
*∫                                                                        ∫
*∫             è‡Æ£‡†¨¨† Æ™‡„£´•≠®Ô Á®·´† §Æ ™‡†‚≠Æ£Æ Ë•·‚®.              ∫
*∫                                                                        ∫
*»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº
PROCEDURE Round_6
PARAMETER v

PRIVATE w
w = v % 60
RETURN IIF(w # 0, v+60-w, v)
