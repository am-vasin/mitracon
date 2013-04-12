*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Copy_D_W      งเ กฎโ็จช €ญคเฅฉ  แจญ           30.09.98 18:10:17 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ         ฎฏจเฎข ญจฅ ไ ฉซ  แ ฏฅเฅชฎคจเฎขชฎฉ จง DOS  ข Windows.          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Copy_D_W
PARAMETERS s_file, d_file
PRIVATE s_n, d_n, buff, d_t, w_t

d_t = Table_D()
w_t = Table_W()
DELETE FILE (d_file)
s_n = FOPEN(s_file)
d_n = FCREATE(d_file)
DO WHILE .NOT. FEOF(s_n)
  buff = FREAD(s_n, 70)
  buff = CHRTRAN(buff, d_t, w_t)
  = FWRITE(d_n, buff, LEN(buff))
ENDDO
= FCLOSE(s_n)
= FCLOSE(d_n)

RETURN
