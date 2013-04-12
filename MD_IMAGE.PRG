*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º                à®£à ¬¬  ¯®¤£®â®¢ª¨ ¬ áá¨¢®¢ BROWSE                   º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Md_Image
PARAMETER stop_
PRIVATE i, dr, s, fnd, s1, s2, s3, s4, rt

*l_br0  = 3           && ¥à¢ ï áâà®ª  BROWSE
*c_br0  = 2           && ¥à¢ ï ¯®§¨æ¨ï BROWSE
*ln_br0 = WROWS()-4   && Š®«¨ç¥áâ¢® áâà®ª BROWSE

fl_br0 = 1           && “ª § â¥«ì BROWSE

recs_m = ""          && ®¬¥à  § ¯¨á¥© ®á­®¢­®£® ä ©« 
recs_c = ""          && ®¬¥à  § ¯¨á¥© á¢ï§ ­­®£® ä ©« 
imag_br = ""         && à¥¤áâ ¢«¥­¨¥ áâà®ª BROWSE
vals_br = ""         && ‡­ ç¥­¨ï ®¡à ¡ âë¢ ¥¬ëå ¯®«¥© 

rt = 0
dr = 1
FOR i = 1 TO ln_br0
  IF stop_ .AND. rt = 0
    rt = INKEY()
    IF rt > 0
      IF c_t = SYS(15, chr_type, CHR(rt)) = "*"
        RETURN rt
      ENDIF
    ENDIF
  ENDIF
  fnd = SEEK(l_c+STR(CODE,7),"LD")
  s1 = STR(RECNO(),9)+CHR(13)
  s2 = IIF(fnd,STR(RECNO("LD"),9), SPACE(9))+CHR(13)
  s4 = IIF(fnd, STR(LD.QNT,6), "      ")+CHR(13)
  s3 = PREFIX+" "+NAME+" "+IIF(QNT > 0, " ", "±")+" "+s4
  IF dr = 1
    recs_m  = recs_m+s1
    recs_c  = recs_c+s2
    imag_br = imag_br+s3
    vals_br = vals_br+s4
  ELSE
    recs_m  = s1+recs_m
    recs_c  = s2+recs_c
    imag_br = s3+imag_br
    vals_br = s4+vals_br
    fl_br0  = fl_br0+1
  ENDIF
  SKIP dr
  IF EOF()
    s1 = VAL(ALLTRIM(LEFT(recs_m,9)))
    GO s1
    dr = -1
    SKIP dr
  ENDIF
  IF BOF()
    EXIT
  ENDIF
ENDFOR

RETURN rt
