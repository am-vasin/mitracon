*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                   บ
*บ        เฎฃเ ฌฌ  ฏฎซใ็ฅญจ๏ แฅเจฉญฎฃฎ ญฎฌฅเ  คจแช .                บ
*บ                                                                   บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Fdd_Ser
PARAMETER drive

PRIVATE s_w, f_name, n_f, s, i, pth

pth = SET("DEFAULT")+CURDIR()

f_name = SYS(3)

IF LEN(drive) = 1
  s_w = "! vol "+drive+": > "+pth+f_name+"."
ELSE
  s_w = "! vol "+drive+" > "+pth+f_name+"."
ENDIF

&s_w

n_f = FOPEN(pth+f_name+".")

s = ""

DO WHILE .T.
  s_w = FGETS(n_f)
  i = LEN(s_w)
  IF i > 8
    IF SUBSTR(s_w, i-4, 1) = "-"
      s = RIGHT(s_w,9)
    ENDIF
  ENDIF
  IF FEOF(n_f)
    EXIT
  ENDIF
*  i = AT("Serial Number is", s_w)
*  IF i > 0
*    s = ALLTRIM(SUBSTR(s_w,i+16))
*    EXIT
*  ENDIF
ENDDO

= FCLOSE(n_f)
DELETE FILE (f_name+".")

RETURN s
