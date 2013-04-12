*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  RemAdd        งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                  ฅจญคฅชแ ๆจ๏ ไ ฉซ  คฎฏ. ญฎฌฅญชซ โใเ๋                  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 23.03.2008 ออผ
PROCEDURE RemAdd
PARAMETER rst

PRIVATE wrk_path, err_sav, s_sav, nf, mss, err, mss1
SET TALK OFF
SET EXCLU OFF

m.mss = "OK"

m.err = 0
m.err_sav = ON("ERROR")
ON ERROR m.err = ERROR()
m.wrk_path = SYS(2004)+"BASE\"

DELETE FILE (m.wrk_path+"RemAdd.tmp")
DELETE FILE (m.wrk_path+"RemAdd.log")
m.s_sav = SELECT()
SELECT 0
USE (m.wrk_path+"ADD_STO") EXCLUSIVE

ON ERROR &err_sav
IF m.err # 0
  m.mss = "ERROR "+ALLTRIM(STR(m.err))
ELSE
  REINDEX
ENDIF
USE
SELECT (m.s_sav)

IF m.rst
  DIMENSION mss1[3]
  IF "ERROR" $ mss
    mss1[1] = ""
    mss1[2] = CHR(0)+"ฅ ใค ฅโแ๏ ง ๅข โจโ์ โ กซจๆใ คซ๏ เฅจญคฅชแ ๆจจ. ฎขโฎเจโฅ ฏฎงฆฅ."
    mss1[3] = ""
    DO Out_Mess WITH 7, "mss1"
  ELSE
    mss1[1] = ""
    mss1[2] = CHR(0)+"ฅจญคฅชแ ๆจ๏ ฏเฎ่ซ  ใแฏฅ่ญฎ"
    mss1[3] = ""
    DO Out_Mess WITH 5, "mss1"
  ENDIF
  RETURN
ELSE
  m.nf = FCREATE(m.wrk_path+"RemAdd.tmp")
  = FPUT(m.nf, m.mss)
  = FCLOSE(m.nf)
  RENAME (m.wrk_path+"RemAdd.tmp") TO (m.wrk_path+"RemAdd.log")
  WAIT WINDOW " ชเฎฉโฅ ํโฎ ฎชญฎ" NOWAIT
  QUIT
ENDIF
