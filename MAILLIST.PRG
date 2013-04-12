*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  MailList      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                 ‘ฎงค ญจฅ แฏจแช  ฏฎ ง ๏ขชฅ จง Win_Sale                  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 27.02.2006 ออผ
PROCEDURE MailList
PARAMETERS fName	&& ไ ฉซ ฏ เ ฌฅโเฎข

SET TALK OFF
SET DELETED ON
SET EXCLUSIVE OFF

PRIVATE nf, pth, tmpFile, tmpArr, logArr

DO Set_Lwr
m.nf = FOPEN(m.fName)	&& ฎโชเ๋ข ฅฌ ไ ฉซ ฏ เ ฌฅโเฎข
IF m.nf < 0
  QUIT
ENDIF
m.pth= FGET(nf)			&& ใโ์ ช ก งฅ ค ญญ๋ๅ Sale

m.tmpFile = FGET(m.nf)	&& ACCOUNT
USE (m.pth+"ML_LOG") SHARED AGAIN
APPEND BLANK
REPLACE MDATE WITH DATE(), ;
        MTIME WITH TIME()
SELECT 0
USE (m.pth+"C_LOG_N") SHARED AGAIN ALIAS DET_LOG
APPEND FROM (m.tmpFile)
SELECT ML_LOG
REPLACE C_REC_N WITH RECNO("DET_LOG")
USE IN DET_LOG
SELECT 0
USE (m.tmpFile) ALIAS SRC
SCATTER TO tmpArr
SELECT 0
USE (m.pth+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS MAIN
IF SEEK(SRC.CUS_CODE)
  SCATTER TO logArr
  GATHER FROM tmpArr
  SELECT 0
  USE (m.pth+"C_LOG_O") SHARED AGAIN ALIAS DET_LOG
  APPEND FROM ARRAY logArr
  SELECT ML_LOG
  REPLACE C_REC_O WITH RECNO("DET_LOG")
  USE IN DET_LOG
ELSE
  APPEND FROM ARRAY tmpArr
ENDIF
USE IN SRC
DELETE FILE (m.tmpFile+".dbf")

m.tmpFile = FGET(m.nf)	&& CUS_BIL

SELECT 0
USE (m.pth+"B_LOG_N") SHARED AGAIN ALIAS DET_LOG
APPEND FROM (m.tmpFile)
SELECT ML_LOG
REPLACE B_REC_N WITH RECNO("DET_LOG")
USE IN DET_LOG

SELECT 0
USE (m.tmpFile) ALIAS SRC
SCATTER TO tmpArr
SELECT MAIN
USE (m.pth+"CUS_BIL") SHARED AGAIN ORDER TAG CUS_CODE ALIAS MAIN
IF SEEK(SRC.CUS_CODE)
  SCATTER TO logArr
  GATHER FROM tmpArr
  SELECT 0
  USE (m.pth+"B_LOG_O") SHARED AGAIN ALIAS DET_LOG
  APPEND FROM ARRAY logArr
  SELECT ML_LOG
  REPLACE B_REC_O WITH RECNO("DET_LOG")
  USE IN DET_LOG
ELSE
  APPEND FROM ARRAY tmpArr
ENDIF
USE IN SRC
DELETE FILE (m.tmpFile+".dbf")

m.tmpFile = FGET(m.nf)	&& CLI_INFO

SELECT 0
USE (m.pth+"I_LOG_N") SHARED AGAIN ALIAS DET_LOG
APPEND FROM (m.tmpFile)
SELECT ML_LOG
REPLACE I_REC_N WITH RECNO("DET_LOG")
USE IN DET_LOG

SELECT 0
USE (m.tmpFile) ALIAS SRC
SCATTER TO tmpArr
SELECT MAIN
USE (m.pth+"CLI_INFO") SHARED AGAIN ORDER TAG CUS_CODE ALIAS MAIN
IF SEEK(SRC.CUS_CODE)
  SCATTER TO logArr
  GATHER FROM tmpArr
  SELECT 0
  USE (m.pth+"I_LOG_O") SHARED AGAIN ALIAS DET_LOG
  APPEND FROM ARRAY logArr
  SELECT ML_LOG
  REPLACE I_REC_O WITH RECNO("DET_LOG")
  USE IN DET_LOG
ELSE
  APPEND FROM ARRAY tmpArr
ENDIF
USE IN MAIN
USE IN SRC
DELETE FILE (m.tmpFile+".dbf")

m.tmpFile = FGET(m.nf)	&& LIST_TIT
USE (m.pth+"LIST_TIT") SHARED AGAIN ALIAS MAIN
APPEND FROM (m.tmpFile)
DELETE FILE (m.tmpFile+".dbf")
DELETE FILE (m.tmpFile+".fpt")

m.tmpFile = FGET(m.nf)	&& LIST_DET
USE (m.pth+"LIST_DET") SHARED AGAIN ALIAS MAIN
APPEND FROM (m.tmpFile)
DELETE FILE (m.tmpFile+".dbf")
DELETE FILE (m.tmpFile+".fpt")

= FCLOSE(m.nf)
DELETE FILE (m.fName)
QUIT

*********************************************************
PROCEDURE set_lwr
PRIVATE m.j
ERASE lwr
PUBLIC lwr
m.lwr = ""
For m.j = 1 to 255
  m.lwr = m.lwr+CHR(m.j)
ENDFOR
m.lwr = stuff(m.lwr,65,26,"abcdefghijklmnopqrstuvwxyz")
m.lwr = stuff(m.lwr,128,32," กขฃคฅฆงจฉชซฌญฎฏเแโใไๅๆ็่้๊๋์ํ๎๏")
RETURN
