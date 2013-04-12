*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа печати отчета по деньгам.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE P_Rep

PARAMETERS inv_n, inv_d
*
*  inv_n - номер документа ( внутренний формат );
*  inv_d - либо дата документа либо год в символьном или числовом формате;
*

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p, f_p, d_year, p_drctry
PRIVATE sel_rem, sb, det_str, str_w, ffeed, w01, v_tmp

sel_rem = SELECT()

IF TYPE("inv_d") = "D"
  d_year = LEFT(DTOS(inv_d),4)
ENDIF
IF TYPE("inv_d") = "N"
  d_year = STR(inv_d,4)
ENDIF
IF TYPE("inv_d") = "C"
  d_year = inv_d
ENDIF

PRIVATE who_r, who_g, d_n, d_d, sum_w, sum_r, sum_0, s_sum, a_name, nt
d_n = LEFT(inv_n,4)+"-"+ALLTRIM(RIGHT(inv_n,6))

SELECT 0
USE (base_path+"KASSA") ORDER TAG DOC_NUM ALIAS KASSA0 AGAIN
IF .NOT. SEEK(d_year+inv_n)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет документа с номером "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF

d_d = TRANSFORM(DOC_DATE,"@D")
sum_w = TRANSFORM(TOTAL,"@Z ## ### ###.##")
sum_r = TRANSFORM(MONEY_REST,"@Z ## ### ###.##")
sum_0 = TRANSFORM(KASSA_REST,"@Z ## ### ###.##")
s_sum = TRANSFORM(TOTAL+KASSA_REST,"@Z ## ### ###.##") && sum_w + sum_0
a_name = WHERE
who_r = WHO_REP
who_g = WHO_GET
det_str = DETAIL
nt      = NOTE
v_tmp   = IIF(VALUTA = "$", "USD", "Руб")

USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS ARM_0 AGAIN
IF .NOT. SEEK(a_name)
  a_name = ""
ELSE
  a_name = ARM_N
ENDIF

USE (base_path+"USERS") ORDER TAG USER_CODE ALIAS USER_0 AGAIN
IF .NOT. SEEK(who_r)
  who_r = ""
ELSE
  who_r = USER_NAME
ENDIF

IF .NOT. SEEK(who_g) .OR. EMPTY(who_g)
  who_g = ""
ELSE
  who_g = USER_NAME
ENDIF

*{N_Doc    } Номер документа
*{D_Doc   } Дата документа
*{Arm } Рабочее место
*{Sum_Tot    } Сумма (сдача денег в бухгалтерию)
*{Rest_Money } Переходящий (декларированный) остаток
*{Rest_Kassa } Остаток по кассе после сдачи денег
*{Sum_Kassa  } Наличность в кассе
*{Salesman     } Продавец
*{Boss         } Бухгалтер
*{Note                } Примечание
*{V} - Валюта

DIMENSION sb(11,2)
sb( 1,1) = "{N_Doc    }"
sb( 1,2) = d_n

sb( 2,1) = "{D_Doc   }"
sb( 2,2) = d_d

sb( 3,1) = "{Arm }"
sb( 3,2) = a_name

sb( 4,1) = "{Sum_Tot    }"
sb( 4,2) = sum_w

sb( 5,1) = "{Salesman     }"
sb( 5,2) = who_r

sb( 6,1) = "{Boss         }"
sb( 6,2) = who_g

sb( 7,1) = "{Note                }"
sb( 7,2) = nt

sb( 8,1) = "{V}"
sb( 8,2) = nt

sb( 9,1) = "{Rest_Money }"
sb( 9,2) = sum_r

sb(10,1) = "{Rest_Kassa }"
sb(10,2) = sum_0

sb(11,1) = "{Sum_Kassa  }"
sb(11,2) = s_sum

USE (base_path+"DOC_FORM")
LOCATE FOR "MON_REP" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(DOC_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(DOC_H,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(DOC_F,i)
ENDFOR

USE (base_path+"CUPURY") ORDER TAG CODE

DO Ini_Prn WITH "Денежный отчет продавца N "+d_n, p_ln, lft, n_cp, fnt, ffeed, p_drctry

j = 0

FOR i = 1 TO ALEN(t_p)
  str_w = t_p(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR

SELECT CUPURY
j = FSIZE("CODE")
DO WHILE .NOT. EMPTY(det_str)
  str_w = LEFT(det_str,j)
  SEEK str_w
  str_w = CUPURY.NAME
  det_str = SUBSTR(det_str,j+1)

  i = AT("!",det_str)
  IF i = 0
    EXIT
  ENDIF
  w01 = LEFT(det_str,i-1)
  str_w = str_w+PADL(w01,10)+" "+TRANSFORM(CUPURY.WAIGHT*VAL(w01),"@Z ## ### ###.##")
  det_str = SUBSTR(det_str,i+1)
  @ PROW()+1, 0 SAY str_w
ENDDO  
USE

FOR i = 1 TO ALEN(f_p)
  str_w = f_p(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR

DO Term_Prn WITH ""

SELECT (sel_rem)
RETURN
