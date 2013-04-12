*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  CalcId1       งเ กฎโ็จช  ซจญ  ฌจโเจฅญชฎ                 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                 แ็ฅโ จคฅญโจไจช โฎเ  คซ๏ คจแโเจกใโจข .                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 30.05.2001 ออผ
PROCEDURE CalcId1
PARAMETERS cus

PRIVATE nf, nnf, buf, res, mss

nf = FOPEN(base_path+"SRCID.TXT")
ef = FSEEK(nf, 0, 2)
bf = FSEEK(nf, 0)

buf = FGETS(nf,ef)
buf = ALLTRIM(buf)
= FCLOSE(nf)

nf = ALLTRIM(STR(cus)) + buf

nf = DEC_HEX(VAL(nf))

DELETE FILE (base_path+"RESID.TXT")
res = FCREATE(base_path+"RESID.TXT")
= FPUTS(res,nf)
= FCLOSE(res)

PRIVATE mss
DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+"‘ฃฅญฅเจเฎข ญ ชซ๎็ "+nf
mss(3) = CHR(0)+"จ ฏฎฌฅ้ฅญ ข ไ ฉซ "+base_path+"RESID.TXT"
mss(4) = ""
DO Out_Mess WITH 5, "mss"

RETURN