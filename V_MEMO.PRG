*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ V_Memo        งเ กฎโ็จช €ญคเฅฉ  แจญ           16.01.98 16:19:13 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                    เฎแฌฎโเ แฎคฅเฆจฌฎฃฎ MEMO-ฏฎซ๏.                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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
