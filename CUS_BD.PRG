*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Cus_BD        งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ              ฎฏจเฎข ญจฅ . . ชซจฅญโฎข คซ๏ .  ฃ เฎข .               บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 20.11.1998 ออผ
PROCEDURE Cus_BD

PRIVATE stru_, blank, cnt, buff, buffr, i, n, cnt, f_name

USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
= AFIELDS(buff)
n = 0
FOR i = 1 TO ALEN(buff,1)
  IF INLIST(buff(i,1), "SUBSYS", "CUS_CODE", "CUS_NAME")
    n = n+1
    DIMENSION stru_(n,4)
    stru_(n,1) = buff(i,1)
    stru_(n,2) = buff(i,2)
    stru_(n,3) = buff(i,3)
    stru_(n,4) = buff(i,4)
  ENDIF
ENDFOR

SELECT 0
USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE
= AFIELDS(buff)
FOR i = 2 TO ALEN(buff,1)
  n = n+1
  DIMENSION stru_(n,4)
  stru_(n,1) = buff(i,1)
  stru_(n,2) = buff(i,2)
  stru_(n,3) = buff(i,3)
  stru_(n,4) = buff(i,4)
ENDFOR

RELEASE buff

SELECT 0
DELETE FILE (tmpo_path+"CUS_BD.DBF")
CREATE DBF (tmpo_path+"CUS_BD.DBF") FROM ARRAY stru_
SCATTER TO blank BLANK
SCATTER TO buffr BLANK

SELECT ACCOUNT
SET RELATION TO CUS_CODE INTO CLI_INFO
SCAN
  WAIT WINDOW CUS_NAME NOWAIT
  SELECT CUS_BD
  APPEND BLANK
  IF FOUND("CLI_INFO")
    SELECT CLI_INFO
    SCATTER TO buff
    FOR i = 2 TO ALEN(buff)
      buffr(i+2) = buff(i)
    ENDFOR
    SELECT CUS_BD
    GATHER FROM buffr
  ELSE
    GATHER FROM blank
  ENDIF
  REPLACE SUBSYS   WITH ACCOUNT.SUBSYS,  ;
          CUS_CODE WITH ACCOUNT.CUS_CODE,  ;
          CUS_NAME WITH ACCOUNT.CUS_NAME
  SELECT ACCOUNT
ENDSCAN

CLOSE DATABASES
f_name = New_File("","DBF","ใค  ฏฎฌฅแโจโ์ เฅงซ์โ โ?")
IF .NOT. EMPTY(f_name)
  DO Wt_Mess WITH " ฏจแ์.."
  COPY FILE (tmpo_path+"CUS_BD.DBF") TO (f_name)
  DO Wt_Mess
ENDIF

DELETE FILE (tmpo_path+"CUS_BD.DBF")

RETURN
