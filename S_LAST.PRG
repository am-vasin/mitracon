do set_lwr

n = ADIR(arr, "base\*.dbf")
for i = 1 to n
  use ("base\"+arr(i,1))
  m = AFIELDS(arr1)
  FOR j = 1 TO m
    IF LEFT(arr1(j,1),5) = "LAST_"
      ? arr(i,1)
      EXIT
    ENDIF
  ENDFOR
ENDFOR

USE
