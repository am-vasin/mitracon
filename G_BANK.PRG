*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� G_Bank       맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                         귣‘� 줎�첓 �� 召ⓤ첓.                         �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 15.01.1999 컴�
PROCEDURE G_Bank
PRIVATE l_name1, l_name2

STORE "" TO l_name1, l_name2

b_sw000 = .F.
SHOW GET b_sw000
DO Ch_Bank WITH f_c000, f_n000, l_name1, b_c000, b_n000, l_name2
SHOW GET b_n000
SHOW GET f_n000
SHOW GET f_c000
RETURN .T.
