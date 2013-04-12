PROCEDURE Crpt_1
PARAMETER vl, l1, l2
PRIVATE s, lw1, lw2
lw1 = IIF(TYPE("l1") = "N", l1, 10)
lw2 = IIF(TYPE("l2") = "N", l2, 0)
s = STR(vl, lw1, lw2)
DO SetPrice WITH "Пиастры!",s
RETURN s
