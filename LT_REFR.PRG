*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� LT_Refr      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      ｡��←キ┘ ������→� 甎�瓷�.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 14.09.2001 様�
PROCEDURE LT_Refr
PARAMETERS l_num

PRIVATE s_sav

PRIVATE lc, sum_list, sw_def, sw_due, sw_ord, sw_dis, sw_spec, ;
           sl_mark, st_mark
STORE .F. TO lc, sum_list, sw_def, sw_due, sw_ord, sw_dis, sw_spec, ;
             sl_mark, st_mark
s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") AGAIN ALIAS TIT_1914 ORDER TAG LST_NUM
IF .NOT. SEEK(l_num)
  USE
  SELECT (s_sav)
ENDIF

lc = STR(l_num,6)
DO See_Ord WITH lc, sum_list, sw_def, sw_due, sw_ord, sw_dis, sw_spec, ;
                sl_mark, st_mark
REPLACE R_TOTAL WITH sum_list,              ;
        IS_DEF  WITH IIF(sw_def, "*", " "), ;
        IS_DUE  WITH IIF(sw_due, "*", " "), ;
        IS_DIS  WITH IIF(.NOT. EMPTY(PRESENT), "*", IIF(sw_dis, "*", " ")), ;
        IS_SPEC WITH IIF(sw_spec,"*", " "), ;
        NOTE_STO WITH IIF(st_mark, "*", " "), ;
        NOTE_SAL WITH IIF(sl_mark, "*", " ")
USE
SELECT (s_sav)
RETURN
