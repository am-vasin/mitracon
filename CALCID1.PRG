*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� CalcId1      ��о�｀砒┴ ���┃� ���矗┘���                 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                ��瘍モ ┐キ皋筥��皰�� か� え痰爬＜皋��.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 30.05.2001 様�
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
mss(2) = CHR(0)+"��キム�牀��� ��鈑 "+nf
mss(3) = CHR(0)+"� ���ラキ � ���� "+base_path+"RESID.TXT"
mss(4) = ""
DO Out_Mess WITH 5, "mss"

RETURN