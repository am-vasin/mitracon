PROCEDURE Reindex
PRIVATE err_cod, err_lst, a_dbf, n, i

n = ADIR(a_dbf,base_path+"*.DBF")

DIMENSION err_lst(1)
err_lst(1) = 1705
CLEAR
FOR i = 1 TO n
  err_cod = 0
  ? a_dbf(i,1)
  USE (base_path+a_dbf(i,1)) EXCLUSIVE
  IF err_cod = 0
    REINDEX
    ?? "   OK"
  ELSE
    ?? "   Файл занят!"
  ENDIF
ENDFOR

USE

RETURN
