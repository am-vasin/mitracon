*ЪДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДї
*і   Џа®жҐ¤га  Draw_Line    ђ §а Ў®взЁЄ Ђ­¤аҐ© ‚ бЁ­                      і
*ГДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДґ
*і                                                                        і
*і                     Џа®аЁб®ўЄ  «Ё­ЁЁ ЈЁбв®Ја ¬¬л.                      і
*і                                                                        і
*АДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДД 19.01.2000 ДДЩ
PROCEDURE D_L
PARAMETERS c0, l0, p0, p1, col0, col1, colb
PRIVATE str_w

IF p1 = 0
  RETURN
ENDIF
str_w = REPLICATE("Ы", FLOOR(p1/2))+IIF(p1 % 2 = 1, "Э", "")
@ c0, l0 SAY str_w COLOR (col1+"/"+colb)
str_w = REPLICATE("Ы", FLOOR(p0/2))+IIF(p0 % 2 = 1, "Э", "")
IF p0 = p1
  @ c0, l0 SAY str_w COLOR (col0+"/"+colb)
ELSE
  @ c0, l0 SAY str_w COLOR (col0+"/"+col1)
ENDIF

RETURN


