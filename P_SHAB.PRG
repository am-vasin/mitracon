*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_???        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Печать документа                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 19.09.2001 ──┘
PROCEDURE P_???
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

PRIVATE n, i, j, k, nn, str_w

s_sav = SELECT()

*
*   Открываем таблицы
*
k00 = tmpFirm+d_year+tmpDoc
SELECT 0
USE (base_path+"?????") ALIAS ???????? AGAIN ORDER TAG ??????????
SEEK ?????????

SELECT 0
USE (base_path+"????????") ALIAS ??????? AGAIN ORDER TAG ?????????
SEEK ??????????

f_name = SYS(3)
DO Wt_Mess WITH "Подготовка документа"
DO Print_Tmp WITH f_name
DO Wt_Mess

***************************************************
*
*   Поля документа
*
DIMENSION sb(?????,2)
*
sb(01,1) = "{Page   }"		&& Страница

*
*		Идентификатор документа
*
sb(02,1) = "{Doc_Num  }"	&& Номер документа
sb(02,2) = ??????

sb(03,1) = "{Date    }"		&& Дата док-та
sb(03,2) = ???????

sb(04,1) = "{Manager                     }"		&& Автор док-та
sb(04,2) = ALLTRIM(?????.FAMILY)+" "+LEFT(?????.NAME, 1)+" "+LEFT(??????.S_NAME, 1)
sb(04,2) = PADR(sb(04,2), LEN(sb(04,1)))
*
*		Клиент
*
sb(05,1) = "{Customer                                        }"
							&& Имя клиента
sb(05,2) = ????????.CUS_NAME
sb(05,2) = PADR(sb(05,2), LEN(sb(05,1)))

***************************************************
*
*   Поля детальной строки
*
DIMENSION sb_det(????,2)
*
sb_det(01,1) = "{Pr}"						&& Префикс
sb_det(02,1) = "{Name                   }"	&& Наименование
sb_det(03,1) = "{Prod  }"					&& Фирма производитель
sb_det(04,1) = "{Price     }"				&& Цена
sb_det(05,1) = "{Qnt }"						&& Кол-во

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "????????" == ALLTRIM(UPPER(DOC_NAME))

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
    IF ???????.IS_USD
      sb_det(04,2) = STR(PRINT_TMP.PRICE, 10, 4)
    ELSE
      sb_det(04,2) = STR(PRINT_TMP.PRICE, 10, 2)
    ENDIF

    * sb_det(05,1) = "{Qnt }"		&& Кол-во
    sb_det(05,2) = STR(PRINT_TMP.QNT, 6)

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
  IF f_pg = n_pg		&& Последняя страница?
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

DO Term_Prn WITH "", (tmpo_path+f_name+".DBF"), dep_cod

DELETE FILE (tmpo_path+f_name+".DBF")
DELETE FILE (tmpo_path+f_name+".CDX")

SELECT ????????????
USE
SELECT ????????????
USE

SELECT (s_sav)

RETURN
