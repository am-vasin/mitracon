*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  CopyStru      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                ”ฎเฌจเฎข ญจฅ ฏใแโ๋ๅ โ กซจๆ ก๋ง๋ ค ญญ๋ๅ.                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 07/11/2000 ออผ
PROCEDURE CopyStru
PARAMETERS s_path, d_path

PRIVATE arr_list, n, i, err

SET EXCLU OFF
DO Set_Lwr

n = ADIR(arr_list, s_path+"*.DBF")
ON ERROR err = ERROR()
err = 0
FOR i = 1 TO n
  err = 0
  USE s_path+(arr_list(i,1))
  IF err = 0
    WAIT WINDOW arr_list(i,1) NOWAIT
    COPY STRUCTURE WITH CDX TO (d_path+arr_list(i,1))
  ELSE
    WAIT WINDOW arr_list(i,1)+" ่จกช  "+ALLTRIM(STR(err))
  ENDIF
ENDFOR
ON ERROR
USE
RETURN
**********************************************************
PROCEDURE set_lwr
PRIVATE j
ERASE lwr
PUBLIC lwr
lwr = ""
For j = 1 to 255
  lwr = lwr+CHR(j)
ENDFOR
lwr = stuff(lwr,65,26,"abcdefghijklmnopqrstuvwxyz")
lwr = stuff(lwr,128,32," กขฃคฅฆงจฉชซฌญฎฏเแโใไๅๆ็่้๊๋์ํ๎๏")
RETURN
