*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�     蹍������ 牀▲爲� ぎ�竅皋��痰� 皋�� ���皀�� か� �爛く爬�閧��      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE LetMoney
PARAMETERS tmpFirm, tmpPay
PRIVATE s_sav, tmp
s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") SHARED AGAIN ALIAS LETMONEY ORDER TAG FIRM_CODE
SEEK tmpFirm
tmp = IIF(EMPTY(tmpPay), "�", tmpPay)
tmp = .NOT. tmp $ H_TYPES
USE
SELECT (s_sav)

RETURN tmp
