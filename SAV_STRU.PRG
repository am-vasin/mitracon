n = ADIR(arr, "BASE\*.DBF")
for i = 1 to n
  USE ("BASE\"+arr(i,1))
  COPY STRU TO ("\TMP\"+arr(i,1)) WITH CDX
ENDFOR
USE
