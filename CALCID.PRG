*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� CalcId       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                ��瘍モ ┐キ皋筥��皰�� か� え痰爬＜皋��.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 24.05.2001 様�
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
mss(2) = CHR(0)+"��キム�牀��� ��鈑 "+res
mss(3) = CHR(0)+"� ���ラキ � ���� "+base_path+"RESID.TXT"
mss(4) = ""
DO Out_Mess WITH 5, "mss"
