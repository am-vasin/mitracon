PROCEDURE set_upr
PRIVATE j
upr = ""
For j = 1 to 255
  upr = upr+CHR(j)
ENDFOR
upr = CHRTRAN(upr,"abcdefghijklmnopqrstuvwxyz ¡¢£¤¥¦§¨©ª«¬­®¯àáâãäåæçèéêëìíîï", ;
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ")

RETURN upr
