*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� V_Memo       ��о�｀砒┴ ��むォ ��瓱�           16.01.98 16:19:13 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    蹍甃�矗 甌ぅ爨━��� MEMO-����.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE V_Memo
PARAMETER memo_name, mem_width, tit

PRIVATE n_lins, i, n, s, f_name, sv, s_sv

f_name = SYS(3)
s_sv = SELECT()
SELECT 0
CREATE DBF (tmpo_path+f_name) (TXT C(mem_width))
USE (tmpo_path+f_name) ALIAS TEMP
sv = SET("MEMOWIDTH")
SET MEMOWIDTH TO mem_width
SELECT (s_sv)
n_lines = MEMLINES(&memo_name)
FOR i = 1 TO n_lines
  s = MLINE(&memo_name,i)
  SELECT TEMP
  APPEND BLANK
  REPLACE TXT WITH s
  SELECT (s_sv)
ENDFOR

DO V_Mess WITH "TEMP", tit
  
USE
DELETE FILE (tmpo_path+f_name+".DBF")
SELECT (s_sv)
SET MEMOWIDTH TO sv

RETURN
