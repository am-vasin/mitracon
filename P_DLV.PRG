*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла P_Dlv        Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Печать заявки на доставку.                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 30.03.2000 ══╝
PROCEDURE P_Dlv
PARAMETERS d_num, d_date

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, l_num, d_ver
PRIVATE t_p, f_p, f_d, d_year, k00, sw, ffeed, p_drctry, d_date
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg, s_pg, str_w, a_code
PRIVATE nds, nn, c_w, is_c, s_sav

IF TYPE("d_date") = "D"
  d_year = LEFT(DTOS(d_date),4)
ENDIF
IF TYPE("d_date") = "N"
  d_year = STR(d_date,4)
ENDIF
IF TYPE("d_date") = "C"
  d_year = d_date
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"DELIVERY") ORDER TAG DOC_NUM ALIAS DLV_0330 AGAIN
IF .NOT. SEEK(d_year+d_num)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет заявки на доставку с номером "+ALLTRIM(d_num)
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PS_0330 AGAIN
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LST_0330 AGAIN
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_0330 AGAIN
SEEK DLV_0330.CUS_CODE
SELECT DLV_0330

DIMENSION sb(50,2)
sb( 1,1) = "{N док-та}"
sb( 1,2) = PADR(ALLTRIM(d_num),10)

sb( 2,1) = "{Д. дата }"
sb( 2,2) = DTOC(DOC_DATE)

sb( 3,1) = "{Сп.   }"
sb( 3,2) = STR(LST_NUM,6)

sb( 4,1) = "{Накл1. N     }"
sb( 4,2) = "           "

sb( 5,1) = "{Накл1. Д }"
sb( 5,2) = "          "

IF SEEK(STR(LST_NUM,6), "LST_0330")
  IF .NOT. EMPTY(LST_0330.SAL_NUM)
    sb( 4,2) = LEFT(LST_0330.SAL_NUM,4)+"-"+ ;
               ALLTRIM(SUBSTR(LST_0330.SAL_NUM,5))+"/"+LST_0330.FIRM
    sb( 5,2) = DTOC(LST_0330.SAL_DATE)
  ENDIF
ENDIF

sb( 6,1) = "{Сп. 2 }"
sb( 6,2) = TRANSFORM(NEW_LIST, "@Z 999999")

sb( 7,1) = "{Накл2. N     }"
sb( 7,2) = "           "

sb( 8,1) = "{Накл2. Д }"
sb( 8,2) = "          "

IF .NOT. EMPTY(NEW_LIST)
  IF SEEK(STR(NEW_LIST,6), "LST_0330")
    IF .NOT. EMPTY(LST_0330.SAL_NUM)
      sb( 7,2) = LEFT(LST_0330.SAL_NUM,4)+"-"+ ;
                 ALLTRIM(SUBSTR(LST_0330.SAL_NUM,5))+"/"+  ;
                 LST_0330.FIRM
      sb( 8,2) = DTOC(LST_0330.SAL_DATE)
    ENDIF
  ENDIF
ENDIF

sb( 9,1) = "{Д. Дост.}"
sb( 9,2) = DTOC(DLV_DATE)

sb(10,1) = "{Клиент                                          }"
sb(10,2) = CUS_0330.CUS_NAME

sb(11,1) = "{Вокзал                }"
sb(11,2) = STAT_NAME

sb(12,1) = "{ПоездN}"
sb(12,2) = TRAIN_NO

sb(13,1) = "{Поезд направление }"
sb(13,2) = TRAIN_NAM

sb(14,1) = "{При}"
sb(14,2) = ARRIVE

sb(15,1) = "{Отп}"
sb(15,2) = LEAVE

sb(16,1) = "{}"
sb(16,2) = CARRIAGE

sb(17,1) = "{Имя В }"
sb(17,2) = CARR_NAME

sb(18,1) = "{Проводник     }"
sb(18,2) = CONDUCTOR

sb(19,1) = "{Передать      }"
sb(19,2) = COND_TEXT

sb(20,1) = "{Получить}"
sb(20,2) = TO_GET

sb(21,1) = "{Курс }"
sb(21,2) = COURCE

sb(22,1) = "{От кого для кого 1                                        }"
sb(22,2) = TEXT01

sb(23,1) = "{От кого для кого 2                                        }"
sb(23,2) = TEXT02

sb(24,1) = "{От кого для кого 3                                        }"
sb(24,2) = TEXT03

sb(25,1) = "{1}"
sb(25,2) = IIF(EXA_WGHT, "[X]","[ ]")

sb(26,1) = "{Вес}"
sb(26,2) = WAIGHT

sb(27,1) = "{Вр1}"
sb(27,2) = CALL_TIME

sb(28,1) = "{2}"
sb(28,2) = IIF(TO_PAY, "[X]","[ ]")

sb(29,1) = "{Сум}"
sb(29,2) = PAY_SUM

sb(30,1) = "{Кому сообщить 1                                           }"
sb(30,2) = TO_MESS1

sb(31,1) = "{Кому сообщить 2                                           }"
sb(31,2) = TO_MESS2

sb(32,1) = "{3}"
sb(32,2) = IIF(IS_DELIV, "[X]","[ ]")

sb(33,1) = "{Дата 1  }"
sb(33,2) = DTOC(ENTER_DAT)

sb(34,1) = "{Вр 1  }"
sb(34,2) = ENTER_TIM

sb(35,1) = "{Кто создал                }"
IF SEEK(SALESMAN, "PS_0330")
  sb(35,2) = PADR(ALLTRIM(PS_0330.FAMILY)+" "+LEFT(PS_0330.NAME,1)+" "+LEFT(PS_0330.S_NAME,1),28)
ELSE
  sb(35,2) = PADR("?",28)
ENDIF

sb(36,1) = "{Дата 2  }"
sb(37,1) = "{Вр 2  }"
sb(38,1) = "{Кто корректировал         }"
IF EMPTY(MOD_DATE)
  sb(36,2) = SPACE(10)
  sb(37,2) = SPACE(8)
  sb(38,2) = SPACE(28)
ELSE
  sb(36,2) = DTOC(MOD_DATE)
  sb(37,2) = MOD_TIME
  IF SEEK(WHO_MOD, "PS_0330")
    sb(38,2) = PADR(ALLTRIM(PS_0330.FAMILY)+" "+LEFT(PS_0330.NAME,1)+" "+LEFT(PS_0330.S_NAME,1),28)
  ELSE
    sb(38,2) = PADR("?",28)
  ENDIF
ENDIF

sb(39,1) = "{Дата 3  }"
sb(40,1) = "{Вр 3  }"
sb(41,1) = "{Кто прочитал              }"
IF EMPTY(READ_DATE)
  sb(39,2) = SPACE(10)
  sb(40,2) = SPACE(8)
  sb(41,2) = SPACE(28)
ELSE
  sb(39,2) = DTOC(READ_DATE)
  sb(40,2) = READ_TIME
  IF SEEK(WHO_READ, "PS_0330")
    sb(41,2) = PADR(ALLTRIM(PS_0330.FAMILY)+" "+LEFT(PS_0330.NAME,1)+" "+LEFT(PS_0330.S_NAME,1),28)
  ELSE
    sb(41,2) = PADR("?",28)
  ENDIF
ENDIF

sb(42,1) = "{Дата 4  }"
sb(43,1) = "{Вр 4  }"
sb(44,1) = "{Кто вызвал курьера        }"
IF EMPTY(CALL_DATE)
  sb(42,2) = SPACE(10)
  sb(43,2) = SPACE(8)
  sb(44,2) = SPACE(28)
ELSE
  sb(42,2) = DTOC(CALL_DATE)
  sb(43,2) = CALL_TIM
  IF SEEK(WHO_CALL, "PS_0330")
    sb(44,2) = PADR(ALLTRIM(PS_0330.FAMILY)+" "+LEFT(PS_0330.NAME,1)+" "+LEFT(PS_0330.S_NAME,1),28)
  ELSE
    sb(44,2) = PADR("?",28)
  ENDIF
ENDIF

sb(45,1) = "{Дата 5  }"
sb(46,1) = "{Вр 5  }"
sb(47,1) = "{Кто отправил курьера      }"
IF EMPTY(GAVE_DATE)
  sb(45,2) = SPACE(10)
  sb(46,2) = SPACE(8)
  sb(47,2) = SPACE(28)
ELSE
  sb(45,2) = DTOC(GAVE_DATE)
  sb(46,2) = GAVE_TIME
  IF SEEK(WHO_GAVE, "PS_0330")
    sb(47,2) = PADR(ALLTRIM(PS_0330.FAMILY)+" "+LEFT(PS_0330.NAME,1)+" "+LEFT(PS_0330.S_NAME,1),28)
  ELSE
    sb(47,2) = PADR("?",28)
  ENDIF
ENDIF

sb(48,1) = "{Дата 6  }"
sb(49,1) = "{Вр 6  }"
sb(50,1) = "{Кто закрыл заявку         }"
IF EMPTY(TERM_DATE)
  sb(48,2) = SPACE(10)
  sb(49,2) = SPACE(8)
  sb(50,2) = SPACE(28)
ELSE
  sb(48,2) = DTOC(TERM_DATE)
  sb(49,2) = TERM_TIME
  IF SEEK(WHO_TERM, "PS_0330")
    sb(50,2) = PADR(ALLTRIM(PS_0330.FAMILY)+" "+LEFT(PS_0330.NAME,1)+" "+LEFT(PS_0330.S_NAME,1),28)
  ELSE
    sb(50,2) = PADR("?",28)
  ENDIF
ENDIF

USE
SELECT CUS_0330
USE
SELECT LST_0330
USE
SELECT PS_0330

USE (base_path+"DOC_FORM")
LOCATE FOR "DELIVERY" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

DO Ini_Prn WITH "Заявка на дост.", p_ln, lft, n_cp, fnt, ffeed, p_drctry

FOR i = 1 TO ALEN(t_p)
  str_w = t_p(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  IF i = 1
    @ PROW(), PCOL() SAY str_w
  ELSE
    @ PROW()+1, 0 SAY str_w
  ENDIF
ENDFOR

EJECT

USE

DO Term_Prn WITH ""

SELECT (s_sav)

RETURN
