           WHEN Draw_Foot()    ;
*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Draw_Foot    맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                   맖率�� ㄾ��キⓥ�レ�莘 Þ兒席졿⑧.                    �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 20.10.1998 컴�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   궙� ℓα�, 黍率��...
*
@ WROWS()-2, 2 SAY PREFIX COLOR SCHEME 1
@ ROW(), COL()+1 SAY NAME COLOR SCHEME 1
@ ROW(), COL()+1 SAY PRODUCER COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
