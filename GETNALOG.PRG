*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� GetNalog     맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�              뤲�｀젹쵟 ��ャ曄�⑨ 췅ギ짛 ㄻ� �誓ㄿ黍汀⑨.               �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 07.07.1999 컴�
PROCEDURE GetNalog
PARAMETERS f_code
PRIVATE f_tmp, s_sav

IF TYPE("f_code") # "C"
  RETURN 0
ENDIF
f_tmp = PADR(f_code,3)
IF EMPTY(f_tmp)
  RETURN 0
ENDIF
IF f_tmp == prc_firm
  RETURN pay_prc
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F9707 AGAIN
IF .NOT. SEEK(f_tmp)
  USE
  SELECT(s_sav)
  RETURN 0
ENDIF
pay_prc = P_NALOG
prc_firm = f_tmp
USE
SELECT(s_sav)
RETURN pay_prc
