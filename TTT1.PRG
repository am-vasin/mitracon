use z:\store\base\account order tag cus_code
sele 0
use z:\store\base\cli_info order tag cus_code
sele 0
use ttt
set rela to cus_code into account
scan
  SELECT CLI_INFO
  IF .NOT. SEEK(ttt.cus_code)
    APPEND BLANK
    REPLACE CUS_CODE WITH ttt.CUS_CODE, name WITH ACCOUNT.CUS_NAME
  ENDIF
  replace sale with .T., f_other with "ùä"
  select ttt
endscan

return
