*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� G_Place      ��о�｀砒┴ ��むォ ��瓱�           01.04.97 11:54:03 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�           ��赳ム��皋↓覃 � ．���赳ム��皋↓覃 �襦�� �����ぎ�.           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE G_Place
PARAMETER l0, c0, result
*
*   l0, c0  - ���爐┃�硅.
*   result - メ�� 腮甄� - ��赳ム��皋↓覃 �襦��,
*             ��瘁│ ���. �ムガキ�諷 - ．���赳ム��皋↓覃 �襦��.
*

PRIVATE i, n, sel_sav, pic, p_names

sel_sav = SELECT()
SELECT 0
USE (base_path+"PLACES") ALIAS Pl000 AGAIN
n = RECCOUNT()
IF TYPE("result") # "N"
  DIMENSION result(n)
  FOR i = 1 TO n
    @ l0+i-1, c0 GET result(i) PICTURE "@*C "+P_NAME
    SKIP
  ENDFOR
ELSE
  pic = "@*R "
  FOR i = 1 TO n
    pic = pic+P_NAME+";"
    SKIP
  ENDFOR
  pic = LEFT(pic, LEN(pic)-1)
  @ l0, c0 GET result PICTURE pic
ENDIF

USE

SELECT (sel_sav)

RETURN
