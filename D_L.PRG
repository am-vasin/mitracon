*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Draw_Line    맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                     뤲�黍貰˚� エ�Ŀ （飡�｀젹щ.                      �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 19.01.2000 컴�
PROCEDURE D_L
PARAMETERS c0, l0, p0, p1, col0, col1, colb
PRIVATE str_w

IF p1 = 0
  RETURN
ENDIF
str_w = REPLICATE("�", FLOOR(p1/2))+IIF(p1 % 2 = 1, "�", "")
@ c0, l0 SAY str_w COLOR (col1+"/"+colb)
str_w = REPLICATE("�", FLOOR(p0/2))+IIF(p0 % 2 = 1, "�", "")
IF p0 = p1
  @ c0, l0 SAY str_w COLOR (col0+"/"+colb)
ELSE
  @ c0, l0 SAY str_w COLOR (col0+"/"+col1)
ENDIF

RETURN


