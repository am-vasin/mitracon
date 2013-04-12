*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  ขขฎค  ค ญญ๋ๅ ฏฎ ชซจฅญโใ:                     บ
*บ             ชเฅคจโ , "ง ฌฎเฎฆฅญญฎฃฎ" คฅฏฎงจโ , ฏเจฌฅ็ ญจ๏.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cus_Note

PRIVATE what_do, pl

what_do = "list"

pl = Choose_P()
HIDE MENU Main_M

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
DO Use_Dummy
DO Use_Link

DO WHILE .T.
  DO CASE
  CASE what_do = "list"
    SELECT ACCOUNT
    what_do = IIF(Custs("", .F., .F., .F., pl), "set parm", "exit")
  CASE what_do = "set parm"
     DO V_Note WITH .T.
    what_do = "list"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

CLOSE DATABASES

RETURN
