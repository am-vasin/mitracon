*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ 甌Г���� ����.                                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE D_W
PARAMETERS w_nam, ln, wd, tit, board
*
*  w_nam - ━� ����;
*  ln    - か┃� ����;
*  wd    - 荐爬�� ����;
*  tit   - ������〓�;
*  board - �����: 0 - �� 爬甌��碎, 1 - �え��爿��, 2 - あ�����
*
PRIVATE b_w, c, l
IF TYPE("board") = "N"
  b_w = board
ELSE
  b_w = 1
ENDIF
l = CEILING((WROWS("")-ln)/2)
c = FLOOR  ((WCOLS("")-wd)/2)

DEFINE WINDOW (w_nam) FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (w_nam)
@ 0, 0 SAY PADC(tit,WCOLS()) COLOR SCHEME 5
DO CASE
CASE b_w = 1
  @ 1, 1 TO WROWS()-1, WCOLS()-2
CASE b_w = 2
  @ 1, 1 TO WROWS()-1, WCOLS()-2 DOUBLE
ENDCASE
RETURN
