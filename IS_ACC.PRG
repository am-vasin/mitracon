*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Is_Acc       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        蹍▲爲� ����腮� ぎ痰祚�                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 17.10.2006 様�
PROCEDURE Is_Acc
PARAMETERS accType, us_code
*
*  accType - 皋� ぎ痰祚�,
*  us_code - ��� ���讌���皀�� (メ�� �� 礫����, 皰 user).

PRIVATE s_sav, tmpUser

m.tmpUser = IIF(TYPE("m.us_code") = "N", m.us_code, m.user)

IF m.tmpUser = 0
 RETURN .T.
ENDIF

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"ACC_PERS") SHARED AGAIN ORDER TAG PERS_CODE ALIAS A_61017
IF .NOT. SEEK(m.tmpUser)
  USE
  SELECT (m.s_sav)
  RETURN .F.
ENDIF

m.tmpUser = .NOT. EMPTY(SUBSTR(ACCESS, m.accType, 1))
USE
SELECT (m.s_sav)
RETURN m.tmpUser
