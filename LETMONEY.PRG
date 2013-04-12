*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ     เฎฃเ ฌฌ  เฎขฅเชจ คฎฏใแโจฌฎแโจ โจฏ  ฏซ โฅฆ  คซ๏ ฏเฅคฏเจจ๏โจ๏      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE LetMoney
PARAMETERS tmpFirm, tmpPay
PRIVATE s_sav, tmp
s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") SHARED AGAIN ALIAS LETMONEY ORDER TAG FIRM_CODE
SEEK tmpFirm
tmp = IIF(EMPTY(tmpPay), "", tmpPay)
tmp = .NOT. tmp $ H_TYPES
USE
SELECT (s_sav)

RETURN tmp
