*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Is_Ur        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  �←錺矚� �� ��┘�� 釵┐�腑瓷━ ��罧�                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 05.08.2003 様�
PROCEDURE Is_Ur
PARAMETERS c_code
PRIVATE s_sav, tmpUr

s_sav = SELECT()
SELECT 0
USE (base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_3805 ORDER TAG CUS_CODE
SEEK c_code
tmpUr = .NOT. MARK1 $ "12"
USE
SELECT (s_sav)

RETURN tmpUr
