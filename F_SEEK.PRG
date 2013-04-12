*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  F_Seek        งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                     ฎจแช ฏฎงจๆจจ แโเฎชจ ข ไ ฉซฅ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 05/20/1999 ออผ
PROCEDURE F_Seek
PARAMETERS f_name, patt

PRIVATE buf1, buf2, pos1, pos2, nf, l_buff, res, i, s00
pos1 = 0
pos2 = 0
buf1 = ""
buf2 = ""

nf = FOPEN(f_name)
l_buff = MAX(20, LEN(patt))
buf1 = FREAD(nf, l_buff)
res = -1
IF FEOF(nf)
  res = AT(patt, buf1)-1
ELSE
  s00 = -1
  DO WHILE .NOT. FEOF(nf)
    buf2 = FREAD(nf, l_buff)
    i = AT(patt, buf1+buf2)
    IF i # 0
      res = s00+i
      EXIT
    ENDIF
    s00 = s00+l_buff
    buf1 = buf2
  ENDDO
ENDIF

= FCLOSE(nf)

RETURN res
