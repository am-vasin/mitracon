*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Send_Dem      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      เฅเฅค ็  แฏเฎแ .                                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 11.01.2004 ออผ
PROCEDURE Send_Dem
PRIVATE mss, f_name, tmpPth, tmpRec, tmpNum, tmpName
tmpPth = tmpo_path
USE (base_path+"DEMAND")
IF EOF() .OR. BOF()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"” ฉซ แฏเฎแ  ฏใแโ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF
GO BOTTOM
IF .NOT. EMPTY(PASSED)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"แฅ ใฆฅ ฎโฏเ ขซฅญฎ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF

DO WHILE .T.
  f_name = New_File(tmpPth, "DBF", "” ฉซ, ชใค  ฏฎฌฅแโจโ์ เฅงใซ์โ โ")
  IF EMPTY(f_name)
    USE
    RETURN
  ENDIF
  IF File_O(f_name)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"’ ชฎฉ ไ ฉซ ใฆฅ แใ้ฅแโขใฅโ!  ฌฅญจโ์ ฅฃฎ? [   ] [ ฅโ ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      DELETE FILE (f_name)
    ELSE
      LOOP
    ENDIF
  ENDIF
  EXIT
ENDDO

DO Wt_Mess WITH "ฎคฃฎโฎขช  ค ญญ๋ๅ"
DO WHILE .T.
  tmpRec = RECNO()
  SKIP -1
  IF BOF()
    tmpNum = 1
    GO TOP
    tmpRec = RECNO()
    EXIT
  ENDIF
  IF .NOT. EMPTY(PASSED)
    tmpNum = VAL(SUBSTR(PASSED, 5))
    IF VAL(LEFT(PASSED, 4)) # YEAR(DATE())
      tmpNum = 1
    ENDIF
    GO tmpRec
    EXIT
  ENDIF
ENDDO
tmpNum = LEFT(DTOS(DATE()), 4)+STR(tmpNum, 5)
REPLACE REST PASSED WITH tmpNum
GO tmpRec
m.tmpName = SYS(3)
COPY REST WHILE .NOT. EMPTY(PASSED) TO (m.tmpo_path+m.tmpName)
DO Wt_Mess
USE
USE (m.tmpo_path+m.tmpName) ALIAS TEMP
COPY STRUCTURE EXTENDED TO (m.tmpo_path+"_STRU")
USE (m.tmpo_path+"_STRU")
APPEND BLANK
REPLACE FIELD_NAME WITH "MANAGER", ;
        FIELD_TYPE WITH "C",       ;
        FIELD_LEN  with 32
USE
CREATE (f_name) FROM (m.tmpo_path+"_STRU")
USE (f_name) ALIAS RES
APPEND FROM (m.tmpo_path+m.tmpName)
DELETE FILE (m.tmpo_path+"_STRU.DBF")
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
USE (base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE IN 0
SET RELATION TO WHO INTO PERSONS
REPLACE ALL MANAGER WITH PERSONS.FAMILY
USE 
USE IN PERSONS

RETURN
