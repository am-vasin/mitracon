*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Tst_GTD      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  蹍▲爲� ��ぎо�皀�讚諷 ��Ж罔� ���.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 06.08.2001 様�
PROCEDURE Tst_GTD
PRIVATE c, n_line, escape

USE path_comm+"GTD" ORDER TAG NAME 
DEFINE WINDOW tmp_win FROM 1, 1 TO WROWS()-2, WCOLS()-2 SHADOW NONE COLOR SCHEME 13
ACTIVATE WINDOW tmp_win
@ 0, 0 SAY PADC("��Ж罔� ��� � ��爬���罐�",WCOLS()) COLOR SCHEME 5
DO Sun_Bord WITH 1, 1, WROWS()-2, WCOLS()-2
@ 1,  2 SAY "���ム ���"
@ 1, 23 SAY "��━キ����┘"

escape = .F.
n_line = 2
SCAN FOR Is_Cyr(NAME)
  @ n_line, 2 SAY GTD_NUM+"�"
  DO Show_Cyr WITH NAME, n_line, COL()
  n_line = n_line+1
  IF n_line = WROWS()-2
    @ WROWS()-1, 2 SAY PADC(" Esc - �諷��, �遏�� む磽�� ���※�� - �牀ぎ�Θ碎 ", WCOLS()-4)
    c = INKEY(0, "H")
    @ 2, 2 CLEAR TO WROWS()-3, WCOLS()-3
    @ WROWS()-1, 0 SAY SPACE(WCOLS())
    n_line = 2
    IF c = 27
      escape = .T.
      EXIT
    ENDIF
  ENDIF
ENDSCAN

IF .NOT. escape
  @ WROWS()-1, 2 SAY PADC(" ���ユ �牀甃�矗�, ��Μ�皀 �遏竡 ���※蓊 ", WCOLS()-4)
  = INKEY(0, "H")
ENDIF
USE
RELEASE WINDOW tmp_win

RETURN
