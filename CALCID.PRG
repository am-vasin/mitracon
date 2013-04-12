*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  CalcId        §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                 áç¥â ¨¤¥­â¨ä¨ª â®à  ¤«ï ¤¨áâà¨¡ãâ¨¢ .                 º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 24.05.2001 ÍÍ¼
PROCEDURE CalcId
PARAMETERS cus

PRIVATE nf, buf, i, res

nf = FOPEN(base_path+"SRCID.TXT")
buf = FGETS(nf)
buf = ALLTRIM(buf)
= FCLOSE(nf)

nf = 1
FOR i = 1 TO LEN(buf)
  nf = nf*10
ENDFOR
nf = cus*nf+VAL(buf)

res = ""
DO WHILE nf > 0
  i = nf % 16
  nf = (nf-i)/16
  res = SUBSTR("0123456789ABCDEF", i+1, 1)+res
ENDDO

DELETE FILE (base_path+"RESID.TXT")
nf = FCREATE(base_path+"RESID.TXT")
= FPUTS(nf, res)
= FCLOSE(nf)
PRIVATE mss
DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+"‘£¥­¥à¨à®¢ ­ ª«îç "+res
mss(3) = CHR(0)+"¨ ¯®¬¥é¥­ ¢ ä ©« "+base_path+"RESID.TXT"
mss(4) = ""
DO Out_Mess WITH 5, "mss"
