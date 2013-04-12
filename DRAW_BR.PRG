*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ฏเฎเจแฎขชจ BROWSE                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Draw_Br
PRIVATE i

*l_br0  = 3           && ฅเข ๏ แโเฎช  BROWSE
*c_br0  = 2           && ฅเข ๏ ฏฎงจๆจ๏ BROWSE
*ln_br0 = WROWS()-4   && ฎซจ็ฅแโขฎ แโเฎช BROWSE

*fl_br0 = 1           && “ช ง โฅซ์ BROWSE

*recs_m = ""          && ฎฌฅเ  ง ฏจแฅฉ ฎแญฎขญฎฃฎ ไ ฉซ 
*recs_c = ""          && ฎฌฅเ  ง ฏจแฅฉ แข๏ง ญญฎฃฎ ไ ฉซ 
*imag_br = ""         && เฅคแโ ขซฅญจฅ แโเฎช BROWSE
*vals_br = ""         && ญ ็ฅญจ๏ ฎกเ ก โ๋ข ฅฌ๋ๅ ฏฎซฅฉ 

FOR i = 1 TO ln_br0
  @ l_br0+i-1, c_br0 SAY Get_Part(imag_br,i)
ENDFOR
i = l_br0+fl_br0-1
@ i, c_br0 FILL TO i, WCOLS()-3 COLOR SCHEME 1
RETURN

PROCEDURE Get_Part
PARAMETERS shab, n_part
PRIVATE i0, i1

IF n_part = 1
  i0 = 1
ELSE
  i0 = AT(CHR(13), shab, n_part-1)+1
ENDIF
i1 = AT(CHR(13), shab, n_part)

RETURN SUBSTR(shab, i0, i1-i0)

