*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_Act_A      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Печать приемного акта с адресами                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.02.2002 ──┘
PROCEDURE P_Act_A
PARAMETERS tmpFirm, tmpDoc, tmpDate

PRIVATE d_year

IF TYPE("tmpDate") = "D"
  d_year = LEFT(DTOS(tmpDate),4)
ENDIF
IF TYPE("tmpDate") = "N"
  d_year = STR(tmpDate,4)
ENDIF
IF TYPE("tmpDate") = "C"
  d_year = tmpDate
ENDIF

PRIVATE s_sav				&& Здесь сохраняем номер рабочей области
PRIVATE d_year				&& Год документа
PRIVATE d_n					&& Номер док-та для печати
PRIVATE k00					&& Ключ для поиска документа
PRIVATE sb					&& Псевдополя документа
PRIVATE sb_det				&& Псевдополя детальной строки
PRIVATE p_ln				&& Днина страницы в строках
PRIVATE fnt					&& Шрифт+Ориентация страницы
PRIVATE n_cp				&& Количество копий
PRIVATE lft					&& Левое поле
PRIVATE ffeed				&& Завершение док-та
PRIVATE p_drctry			&& Направление печати
PRIVATE t_d					&& Заголовок док-та
PRIVATE f_d					&& Подвал док-та
PRIVATE t_p					&& Заголовок страницы
PRIVATE f_p					&& Подвал страницы
PRIVATE det					&& Образ детальной строки
PRIVATE n_p_det1			&& Кол-во детальных строк на странице
PRIVATE n_p_det				&& Кол-во детальных строк на странице
PRIVATE n_pg				&& Кол-во страниц в документе
PRIVATE f_pg				&& Текущая страница
PRIVATE dep_cod				&& Код клиента

PRIVATE n, i, j, k, nn, str_w, tmpPic

s_sav = SELECT()

*
*   Открываем таблицы
*
k00 = tmpFirm+d_year+tmpDoc
SELECT 0
USE (base_path+"ACT_S_T") ALIAS TIT_20220 AGAIN ORDER TAG DOC_NUM
IF .NOT. SEEK(k00)
  USE
  SELECT (s_sav)
  RETURN
ENDIF
IF TIT_20220.VALUTA = "$"
  tmpPic = "9999999999.9999"
ELSE
  tmpPic = "999999999999.99"
ENDIF

SELECT 0
USE (base_path+"ACCOUNT") ALIAS ACC_20220 AGAIN ORDER TAG CUS_CODE
SEEK TIT_20220.SUP_CODE

SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_20220 AGAIN ORDER TAG CODE
SEEK TIT_20220.WHO_ACT

SELECT 0
USE (base_path+"FIRMS") ALIAS FRM_20220 AGAIN ORDER TAG FIRM_CODE
SEEK TIT_20220.FIRM

f_name = SYS(3)
DO Wt_Mess WITH "Подготовка документа"
DO Print_Tmp WITH f_name
DO Wt_Mess

***************************************************
*
*   Поля документа
*
DIMENSION sb(14,2)
*
sb(01,1) = "{Стр.   }"		&& Страница

*
*		Идентификатор документа
*
sb(02,1) = "{Док. №   }"	&& Номер документа
sb(02,2) = PADR(ALLTRIM(TIT_20220.DOC_NUM), LEN(sb(02,1)))

sb(03,1) = "{Дата    }"		&& Дата док-та
sb(03,2) = DTOC(TIT_20220.DOC_DATE)

sb(04,1) = "{Менеджер                    }"		&& Автор док-та
sb(04,2) = ALLTRIM(PRS_20220.FAMILY)+" "+LEFT(PRS_20220.NAME, 1)+" "+LEFT(PRS_20220.S_NAME, 1)
sb(04,2) = PADR(sb(04,2), LEN(sb(04,1)))
*
*		Клиент
*
sb(05,1) = "{Поставщик                                       }"
							&& Имя поставщика
sb(05,2) = ACC_20220.CUS_NAME
sb(05,2) = PADR(sb(05,2), LEN(sb(05,1)))

sb(06,1) = "{Получатель                                      }"
							&& Имя клиента
sb(06,2) = FRM_20220.L_NAME
sb(06,2) = PADR(sb(05,2), LEN(sb(05,1)))

sb(07,1) = "{Пок./Реал}"		&& Покупка / Реализация
sb(07,2) = IIF(TIT_20220.FLG_BUY = "B", "Покупка   ", "Реализация")

sb(08,1) = "{Накл. № }"		&& Номер накладной
sb(08,2) = TIT_20220.EXP_NUM

sb(09,1) = "{Дата Нак}"		&& Дата накладной
sb(09,2) = DTOC(TIT_20220.EXP_DATE)

sb(10,1) = "{%НДС}"			&& % НДС
IF TIT_20220.VALUTA = "Р"
  sb(10,2) = STR(TIT_20220.NDS_, 6, 1)
ELSE
  sb(10,2) = "   0.0"
ENDIF

sb(11,1) = "{Итого Док    }"	&& Общая сумма по документу
sb(11,2) = "               "

sb(12,1) = "{Итого Стр    }"	&& Общая сумма по странице
sb(12,2) = "               "

sb(13,1) = "{Итого 0 Док  }"	&& Сумма без НДС по документу
sb(13,2) = "               "

sb(14,1) = "{Итого 0 Стр  }"	&& Сумма без НДС по странице
sb(14,2) = "            "
***************************************************
*
*   Поля детальной строки
*
DIMENSION sb_det(15,2)
*
sb_det(01,1) = "{Пр}"						&& Префикс
sb_det(02,1) = "{Наименование           }"	&& Наименование
sb_det(03,1) = "{Фирма }"					&& Фирма производитель
sb_det(04,1) = "{Цена      }"				&& Цена без НДС
sb_det(05,1) = "{К-во}"						&& Кол-во
sb_det(06,1) = "{Сумма 0      }"			&& Сумма без НДС
sb_det(07,1) = "{Сумма        }"			&& Сумма с НДС
sb_det(08,1) = "{ГТД №                   }"	&& № ГТД
sb_det(09,1) = "{Страна        }"			&& Страна по ГТД
sb_det(10,1) = "{№№}"						&& Строка ГТД
sb_det(11,1) = "{Док }"						&& Кол-во по документу
sb_det(12,1) = "{%НДС}"						&& % НДС
sb_det(12,2) = STR(TIT_20220.NDS_, 6, 1)
sb_det(13,1) = "{Адрес }"					&& Адрес
sb_det(14,1) = "{Доп. адрес    }"			&& Доп. адрес
sb_det(15,1) = "{К}"						&& Код страны

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "ACT_ADDR" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

*
*		Шаблон заголовка док-та
*
n = MEMLINES(DOC_H)
IF n = 0
  DIMENSION t_d(1)
  t_d(1) = ""
ELSE
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

*
*		Шаблон заголовка страницы
*
n = MEMLINES(PAGE_H)
IF n = 0
  DIMENSION t_p(1)
  t_p(1) = ""
ELSE
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

*
*		Шаблон подвала страницы
*
n = MEMLINES(PAGE_F)
IF n = 0
  DIMENSION f_p(1)
  f_p(1) = ""
ELSE
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

*
*		Шаблон подвала док-та
*
n = MEMLINES(DOC_F)
IF n = 0
  DIMENSION f_d(1)
  f_d(1) = ""
ELSE
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ENDIF

*
*		Шаблон детальной строки
*
n = MEMLINES(DETAIL)
IF n = 0
  DIMENSION det(1)
  det(1) = ""
ELSE
  DIMENSION det(n)
  FOR i = 1 TO n
    det(i) = MLINE(DETAIL,i)
  ENDFOR
ENDIF

USE

DO Ini_Prn WITH "Замены", p_ln, lft, n_cp, fnt, ffeed, p_drctry

SELECT PRINT_TMP
GO TOP

nn = RECCOUNT()		&& Всего детальных ГРУПП
n_p_det  = p_ln-ALEN(t_d)-MAX(ALEN(f_p),ALEN(f_d))
							&& Кол-во строк под детальную часть на первой странице
n_p_det1 = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
							&& Кол-во строк под детальную часть на НЕпервой странице
n_p_det  = FLOOR(n_p_det/ALEN(det))		&& Кол-во детальных ГРУПП на первой странице
n_p_det1 = FLOOR(n_p_det1/ALEN(det))	&& Кол-во детальных ГРУПП на НЕпервой странице
n_pg = 1
nn = nn-n_p_det		&& Сколько детальных групп на всех страницах без первой

IF n_p_det <= 0		&& Забили всю страницу под заголовки... ни каких страниц!
  n_p_det = RECCOUNT()
ELSE
  n_pg = 1+CEILING(MAX(nn, 0)/n_p_det1)
ENDIF
PRIVATE sum_d, sum_0_d, sum_p, sum_0_p

STORE 0 TO sum_d, sum_0_d
FOR f_pg = 1 TO n_pg		&& По страницам...
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))	&& Страница/Страниц
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1   && Заголовок НЕ первой страницы
    EJECT
    n_p_det = n_p_det1		&& Переставляем длину страницы...
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
  ELSE        && Заголовок первой страницы
    FOR i = 1 TO ALEN(t_d)
      str_w = t_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF
  *
  *   Детальная часть страницы
  * 
  i = 0		&& Счетчик детальных групп
  STORE 0 TO sum_p, sum_0_p
  DO WHILE .T.

    ****************************************************
    *
    *     Заполняем поля детальной группы
    *

    * sb_det(01,1) = "{Pr}"			&& Префикс
    sb_det(01,2) = PRINT_TMP.PREFIX

    * sb_det(02,1) = "{Name                   }"	&& Наименование
    sb_det(02,2) = PRINT_TMP.NAME

    * sb_det(03,1) = "{Prod  }"		&& Фирма производитель
    sb_det(03,2) = PRINT_TMP.PRODUCER

    * sb_det(04,1) = "{Price   }"	&& Цена
    IF TIT_20220.VALUTA = "$"
      sb_det(04,2) = STR(PRINT_TMP.PRICE, 10, 4)
    ELSE
      sb_det(04,2) = STR(PRINT_TMP.PRICE, 10, 2)
    ENDIF

    * sb_det(05,1) = "{Qnt }"		&& Кол-во
    sb_det(05,2) = STR(PRINT_TMP.QNT, 6)

    *sb_det(06,1) = "{Сумма 0      }"			&& Сумма без НДС
    sb_det(06,2) = TRANSFORM(PRINT_TMP.SUM_0, tmpPic)

    *sb_det(07,1) = "{Сумма        }"			&& Сумма с НДС
    sb_det(07,2) = TRANSFORM(PRINT_TMP.SUM_TOT, tmpPic)

    *sb_det(08,1) = "{ГТД №                   }"	&& № ГТД
    sb_det(08,2) = PRINT_TMP.GTD_NUM

    *sb_det(09,1) = "{Страна        }"			&& Страна по ГТД
    sb_det(09,2) = PRINT_TMP.COUNTRY

    *sb_det(15,1) = "{К}"						&& Код страны
    sb_det(15,2) = PRINT_TMP.COUNTRYC

    *sb_det(10,1) = "{№№}"						&& Строка ГТД
    sb_det(10,2) = PRINT_TMP.GTD_LINE

    *sb_det(11,1) = "{Док }"					&& Кол-во по документу
    sb_det(11,2) = PRINT_TMP.GTD_NUM

    *sb_det(12,1) = "{%НДС}"					&& % НДС

    *sb_det(13,1) = "{Адрес }"					&& Адрес
    sb_det(13,2) = PRINT_TMP.ADDR

    *sb_det(14,1) = "{Доп. адрес    }"			&& Доп. адрес
    sb_det(14,2) = PRINT_TMP.MEMO_ST


    sum_0_p = sum_0_p + PRINT_TMP.SUM_0
    sum_0_d = sum_0_d + PRINT_TMP.SUM_0
    sum_p   = sum_p   + PRINT_TMP.SUM_TOT
    sum_d   = sum_d   + PRINT_TMP.SUM_TOT

    FOR k = 1 TO ALEN(det)		&& Подстановка полей и печать детальной группы
      str_w = det(k)
      FOR j = 1 TO ALEN(sb_det,1)
        str_w = STRTRAN(str_w, sb_det(j,1), sb_det(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR

    i = i+1
    SKIP
    IF EOF("PRINT_TMP")
      EXIT
    ENDIF
    IF  i >= n_p_det
      EXIT
    ENDIF
  ENDDO

  *
  *    Окончание страницы
  *
  sb(12,2) = TRANSFORM(sum_p, tmpPic)
  sb(14,2) = TRANSFORM(sum_0_p, tmpPic)

  IF f_pg = n_pg		&& Последняя страница?
    sb(11,2) = TRANSFORM(sum_d, tmpPic)
    sb(13,2) = TRANSFORM(sum_0_d, tmpPic)
    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR

USE

DO Term_Prn WITH "", (tmpo_path+f_name+".DBF")

DELETE FILE (tmpo_path+f_name+".DBF")
DELETE FILE (tmpo_path+f_name+".CDX")

SELECT TIT_20220
USE
SELECT ACC_20220
USE
SELECT PRS_20220
USE
SELECT FRM_20220
USE

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Print_Tmp    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.02.2002 ──┘
PROCEDURE Print_Tmp
PARAMETER tmpName

PRIVATE s_sav, tmpPrice, s0, s1

s_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ALIAS STO_20220 ORDER TAG CODE
SELECT 0
USE (base_path+"ACT_S") SHARED AGAIN ALIAS DET_20220 ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STO_20220
SELECT 0
CREATE DBF (tmpo_path+tmpName) ;
  ( CODE		N( 7), ;
    PREFIX		C( 4), ;
    NAME		C(35), ;
    PRODUCER	C( 8), ;
    ADDR		C( 8), ;
    MEMO_ST		C(16), ;
    GTD_NUM		C(26), ;
    COUNTRY		C(16), ;
    COUNTRYC	C( 3), ;
    GTD_LINE	C( 4), ;
    QNT_DOC		N( 6), ;
    QNT			N( 6), ;
    PRICE		N(12, IIF(TIT_20220.VALUTA = "Р", 2, 4)),;
    SUM_0		N(15, IIF(TIT_20220.VALUTA = "Р", 2, 4)),;
    SUM_TOT		N(15, IIF(TIT_20220.VALUTA = "Р", 2, 4)) )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS PRINT_TMP

SELECT DET_20220
SEEK k00
SCAN REST WHILE DOC_NUM = TIT_20220.DOC_NUM .AND. ;
                FIRM    = TIT_20220.FIRM    .AND. ;
                DOC_DATE= TIT_20220.DOC_DATE
  SELECT PRINT_TMP
  IF TIT_20220.VALUTA = "$"
    tmpPrice = DET_20220.PRICE_D
    s0 = tmpPrice*DET_20220.QNT_INC
    s1 = s0
  ELSE
    tmpPrice = DET_20220.PRICE_0
    s0 = tmpPrice*DET_20220.QNT_INC
    s1 = ROUND(s0*(100+TIT_20220.NDS_)/100, 2)
  ENDIF
  APPEND BLANK
  REPLACE	CODE		WITH DET_20220.CODE,     ;
			PREFIX		WITH STO_20220.PREFIX,   ;
			NAME		WITH STO_20220.NAME,     ;
			PRODUCER	WITH STO_20220.PRODUCER, ;
			ADDR		WITH STO_20220.ADDRESS,  ;
			MEMO_ST		WITH STO_20220.MEMO_ST,  ;
			GTD_NUM		WITH DET_20220.GTD_NUM,  ;
			COUNTRY		WITH DET_20220.COUNTRY,  ;
			COUNTRYC	WITH DET_20220.COUNTRYC, ;
			GTD_LINE	WITH DET_20220.GTD_LINE, ;
			QNT_DOC		WITH DET_20220.QNT_EXP,  ;
			QNT			WITH DET_20220.QNT_INC,  ;
			PRICE		WITH tmpPrice,           ;
			SUM_0		WITH s0,                 ;
			SUM_TOT		WITH s1
  SELECT DET_20220
ENDSCAN

USE
SELECT STO_20220
USE
SELECT PRINT_TMP
INDEX ON ADDR TAG ADDR 
SELECT (s_sav)

RETURN
