*
*   Программа деноминации в базе данных
*
PROCEDURE Tst_Div
SET EXCLU OFF
SET DELE ON
DO Set_Lwr
path_d = "d:\div_100\"
path_s = "z:\store\base\"

n_fil = ADIR(l_fil, path_s+"*.DBF")

FOR i111 = 1 TO n_fil
  CLEAR
  @ 1, 10 SAY "Файл "+l_fil(i111,1)+ STR(i111,20)+"/"+ALLTRIM(STR(n_fil))
  USE (path_s+l_fil(i111,1)) ALIAS SSS
  SELECT 0
  USE (path_d+l_fil(i111,1)) ALIAS DDD EXCLU
  n_f = MIN(AFIELDS(stru_), 3)
  nh = RECCOUNT()
  nf = 0
  SCAN
    nf = nf+1
    @ 10, 10 SAY STR(nf)+STR(nh)
    SCATTER TO buf_1
    SELECT SSS
    SCATTER TO buf_2
    FOR i = 1 TO n_f
      IF buf_1(i) # buf_2(i)
        WAIT WINDOW "Не сходится!"
      ENDIF
    ENDFOR
    SKIP
    SELECT DDD
  ENDSCAN
  SELECT DDD
  USE
  SELECT SSS
  USE
ENDFOR  
