PROCEDURE Crpt_2
PARAMETER vl
PRIVATE s
s = vl
DO ClrPrice WITH "Пиастры!",s
RETURN VAL(ALLTRIM(s))
