*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Ch_Curs      맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                           귣‘� 殊캙 ゃ褻�.                            �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 18.02.1999 컴�
PROCEDURE Ch_Curs
PARAMETERS c_code, c_name

*
*  룯誓Д��瑜 貰飡�輾⑨ ㄻ� 췅˘짛與�
*

PRIVATE stat_type     && 뮜� 췅˘짛與�: 0 - �α�젺쩆設�硫;
                                        1 - ∥젺�;
                                        2 - BROWSE - �젩エ��;
                                        3 - BROWSE - 召ⓤ��.
PRIVATE what_do       && 닾� 誓┬쵟.
PRIVATE menu_name     && 닾� 졹Þ魚����． Д��.
PRIVATE last_mouse    && 귖�э ��笹ⅳ�ⅲ� 췅쬊殊� ㎖�黍��� き��え.
PRIVATE win_name      && 닾� �き� ( �ぎ� ㄻ� BROWSE ).

*
*   뇿��キ畑� ㎛좂��⑨Ж ��誓Д��瑜 貰飡�輾⑨...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    뫌ㄵ逝졻�レ췅� �졹筍 �昔｀젹щ:
*

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�  旼컴컴컴컴컴컴컴컴컴컴컴컴커  �
*�  �                          �  �
*�  읕컴컴컴컴컴컴컴컴컴컴컴컴켸  �
*�< OK Ctrl-W > < 롡첓쭬筍碎 Esc >�
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸

PRIVATE ex, arr_c, arr_n, point, n, ln
ex = SELECT()
SELECT 0
USE (base_path+"COURSE") ALIAS CV_9218 AGAIN
n = 0
point = 1
SCAN
  n = n+1
  DIMENSION arr_c(n), arr_n(n)
  arr_c(n) = B_NAME
  arr_n(n) = L_NAME
  IF PADR(c_code,10) = arr_c(n)
    point = n
  ENDIF
ENDSCAN
USE
SELECT (ex)
IF n = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"뜢 貰ℓ젺� �� �ㄽ�． 殊캙..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

ex = 1
ln = MIN(n, WROWS("")-15)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && 뜝 ㏇篆Ł ��쬊惜硫 笹晨젵!
DO Prp_Nav_2
DO D_Win_N WITH ln+6, 36, "귣‥黍收 ゃ褻..."

*------------------------------------------------------------------------
*      궋�� ��ゥ� ∥젺첓
*
@ 2, 4 GET point PICTURE "@& " FROM arr_n SIZE ln+2, 28
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? 롡첓쭬筍碎 Esc "

READ CYCLE

IF ex = 1
  *
  * 롡�젩졻猶젰� ∥젺�
  c_code = arr_c(point)
  IF TYPE("c_name") = "C"
    c_name = arr_n(point)
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
