*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Nt_Lines     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║           Сумма прописью, разбитая на строки заданной длины.           ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 01/19/1999 ══╝
PROCEDURE Nt_Lines
PARAMETERS numb, lin_arr, vlt
*
* numb    - исходное число
* lin_arr - массив: число элементов - число строк, при входе - длины строк,
*                     при выходе - сами строки.
* vlt     - валюта: "Р" или пусто - рубли, "$" - доллары
*
DIMENSION lin_arr(ALEN(lin_arr))
PRIVATE s_line, f_line, r_line, n_line, j, vlt_w
vlt_w = "Р"
IF TYPE("vlt") = "C"
  IF vlt = "$"
    vlt_w = "$"
  ENDIF
ENDIF
n_line = ALEN(lin_arr)
s_line = Num2RStr(FLOOR(numb))+IIF(vlt_w = "$", " У Е "," руб ")+  ;
         RIGHT(STR(100+(numb-FLOOR(numb))*100),2)+IIF(vlt_w = "$", ""," коп.")
s_line = CHRTRAN(LEFT(s_line, 1), "нодтчпшсв", "НОДТЧПШСВ")+SUBSTR(s_line, 2)
FOR j = 1 TO n_line
  f_line = ""
  r_line = ""
  DO Break_Str WITH s_line, f_line, r_line, lin_arr(j)
  lin_arr(j) = f_line
  s_line = r_line
ENDFOR

RETURN
