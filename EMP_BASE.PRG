n = ADIR(arr_w, "base\*.dbf")
FOR i = 1 TO n
  USE "base\"+arr_w(i,1)
  COPY STRUCTURE WITH CDX TO "d:\tmp\"+arr_w(i,1)
ENDFOR

USE
