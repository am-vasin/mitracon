*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ฎชฎญ.                                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Def_Win
PARAMETERS w_nam_1, w_nam_2, w_nam_3, ln, wd, m_tit, tit, foot
PRIVATE ln1, l, c, n, l_arr_t, l_arr_f

IF TYPE("tit(1)") # "C"
  l_arr_t = 0
ELSE
  l_arr_t = ALEN(tit)
  DIMENSION tit(l_arr_t)
ENDIF

IF TYPE("foot(1)") # "C"
  l_arr_f = 0
ELSE
  l_arr_f = ALEN(foot)
  DIMENSION foot(l_arr_f)
ENDIF

ln1 = ln+MAX(l_arr_t,1)+MAX(l_arr_f,1)+1

l = CEILING((WROWS("")-ln1)/2)
c = FLOOR((WCOLS("")-wd-4)/2)

DEFINE WINDOW (w_nam_1) FROM l, c TO l+ln1-1, c+wd+3 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (w_nam_1)
@ 0, 0 SAY PADC(m_tit,WCOLS()) COLOR SCHEME 5

IF l_arr_t > 1
  FOR i = 1 TO l_arr_t-1
    @ i, 2 SAY PADC(tit(i), wd)
  ENDFOR
ENDIF

i = MAX(l_arr_t,1)
@ i, 1 TO i+ln+1, WCOLS()-2
IF l_arr_t # 0
  @ l_arr_t, 2 SAY PADC(tit(l_arr_t), wd,"ฤ")
ENDIF

n = WROWS()-l_arr_f-1
IF l_arr_f # 0
  @ n+1, 2 SAY PADC(foot(1), wd,"ฤ")
  FOR i = 2 TO l_arr_f
    @ i+n, 2 SAY PADC(foot(i), wd)
  ENDFOR
ENDIF

l = l+MAX(l_arr_t,1)+1
c = c+2
DEFINE WINDOW (w_nam_2) FROM l, c TO l+ln-1, c+wd-1 NONE COLOR SCHEME 13
DEFINE WINDOW (w_nam_3) FROM l-3, c-2 TO l+ln, c+wd+20   ;
                           NONE COLOR SCHEME 13
RETURN

