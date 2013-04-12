*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ฏฎซใ็ฅญจ๏ คฅไฎซโญฎฃฎ ฏใโจ.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Path
PARAMETERS pth, k      && ฎงขเ ้ ฅฌ๋ฉ ฏใโ์ จ ชซ๎็ ข ไ ฉซฅ ฏใโฅฉ

PRIVATE sel_sav, ln

ln = LEN(pth)
IF .NOT. File_O(base_path+"def_path.dbf")
  RETURN
ENDIF

sel_sav = SELECT()
SELECT 0
USE (base_path+"DEF_PATH")
LOCATE FOR UPPER(PADR(k, FSIZE("KEY"))) == UPPER(KEY)
IF FOUND()
  pth = ALLTRIM(DEF_PATH)
ENDIF
USE
SELE (sel_sav)

RETURN
