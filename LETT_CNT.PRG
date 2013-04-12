*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Lett_Cnt      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                     ฎซจ็ฅแโขฎ ญฅฏเฎ็จโ ญญ๋ๅ ฏจแฅฌ                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 26.07.2006 ออผ
PROCEDURE Lett_Cnt

PRIVATE s_sav, tmpCnt, now

m.s_sav  = SELECT()
m.now    = DTOS(DATE())+TIME()
m.tmpCnt = 0
SELECT 0
USE (m.base_path+"LETT_IN") SHARED AGAIN ALIAS TMP_L_5A28 ORDER TAG ADDRESS
IF TYPE("m.User") = "N"
  IF SEEK("* "+STR(m.User, 5))
    IF DTOS(SHOW_DATE)+SHOW_TIME <= m.now
      SCAN REST WHILE EMPTY(READ_DATE) .AND. ADDRESS_T = " " .AND. ADDRESS = STR(m.User, 5) ;
                      .AND. DTOS(SHOW_DATE)+SHOW_TIME <= m.now
        m.tmpCnt = m.tmpCnt+1
      ENDSCAN
    ENDIF
  ENDIF
ENDIF

m.tmpArm = PADR(m.arm, 5)
IF SEEK("**"+m.arm)
  IF DTOS(SHOW_DATE)+SHOW_TIME <= m.now
    SCAN REST WHILE EMPTY(READ_DATE) .AND. ADDRESS_T = "*" .AND. ADDRESS = m.tmpArm ;
                    .AND. DTOS(SHOW_DATE)+SHOW_TIME <= m.now
      m.tmpCnt = m.tmpCnt+1
    ENDSCAN
  ENDIF
ENDIF

USE
SELECT (m.s_sav)

RETURN m.tmpCnt
