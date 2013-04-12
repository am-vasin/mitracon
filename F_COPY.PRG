PROCEDURE F_Copy
f1 = "Z:\STORE\BASE\LIST_DET.DBF"
f2 = "D:\TMP1\LIST_D.DBF"
sz = File_Size(f1)
nf1 = FOPEN(f1)
nf2 = FCREATE(f2)
CLEAR
@ 10, 35 SAY STR(sz)
sz = 0
DO WHILE .T.
  buff = FREAD(nf1, 1024)
  = FWRITE(nf2, buff)
  IF FEOF(nf1)
    EXIT
  ENDIF
  sz = sz+1024
  @ 12, 35 SAY STR(sz)
ENDDO

= FCLOSE(nf1)

= FCLOSE(nf2)

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฎฏเฅคฅซฅญจ๏ เ งฌฅเ  ไ ฉซ .                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE File_Size
PARAMETERS f_nam

PRIVATE arr_w
IF ADIR(arr_w, f_nam) = 0
  RETURN 0
ELSE
  RETURN arr_w(1,2)
ENDIF
