*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Changes       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                        ฎจแช ง ฌฅญ คซ๏ ฏฎงจๆจจ.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 09/24/1999 ออผ
PROCEDURE Changes
PARAMETERS position

PRIVATE s_sav, r_part

s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"CHANGES.DBF")
CREATE DBF (tmpo_path+"CHANGES.DBF") ;
  ( CODE     N( 7), ;
    SOURCE   C(40) )
SELECT 0
*
*    ๋โ ฅฌแ๏ แฎกเ โ์  ญ ซฎฃจ ฏฎ ชฎเญ๏ฌ
*
IF File_O(path_comm+"SUBJECT.DBF")
  USE (path_comm+"SUBJECT") ORDER TAG STO_CODE AGAIN ALIAS RT_9924
  SET FILTER TO STO_CODE # 0
  IF SEEK(position)  && ฎงจๆจ๏ ฅแโ์ ข ไ ฉซฅ
    r_part = ROOT
    SET ORDER TO TAG ROOT
    SET FILTER TO STO_CODE # position .AND. STO_CODE # 0  && —โฎก๋ ญฅ ใช ง โ์ ญ  แฅก๏
    IF .NOT. EMPTY(r_part)    && € ก๋ซ ซจ ชฎเฅญ์!?
      SEEK r_part
      SCAN REST WHILE r_part = ROOT
        SELECT CHANGES
        APPEND BLANK
        REPLACE CODE   WITH RT_9924.STO_CODE, ;
                SOURCE WITH "ฎเฅญ์"
        SELECT RT_9924
      ENDSCAN
    ENDIF
  ENDIF
  USE
ENDIF
*
*   ้ฅฌ ข แฏจแชฅ  ญ ซฎฃฎข
*
USE (base_path+"ANALOG") ORDER TAG CODE ALIAS AN_9924 AGAIN
IF SEEK(position)
  SCAN REST WHILE position = CODE_1
    SELECT CHANGES
    APPEND BLANK
    REPLACE CODE   WITH AN_9924.CODE_2, ;
            SOURCE WITH AN_9924.SOURCE
    SELECT AN_9924
  ENDSCAN
ENDIF
USE

SELECT CHANGES
USE

SELECT (s_sav)

RETURN
