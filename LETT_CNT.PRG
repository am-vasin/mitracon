*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Lett_Cnt     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ����腑痰〓 �ク牀腮����諷 ��瓮�                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 26.07.2006 様�
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
