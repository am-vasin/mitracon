*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                   Шаблон для печати документов.                       ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE P_Test
PARAMETERS f_code, doc_n, doc_d
*
*  f_code - фирма;
*  doc_n  - номер документа;
*  doc_d  - либо дата накладной, либо год в символьном или числовом формате
*

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

PRIVATE n, i, j, k, nn, str_w

s_sav = SELECT()

*
*   Формируем год док-та из параметра
IF TYPE("doc_d") = "D"
  d_year = LEFT(DTOS(doc_d),4)
ENDIF
IF TYPE("doc_d") = "N"
  d_year = STR(doc_d,4)
ENDIF
IF TYPE("doc_d") = "C"
  d_year = doc_d
ENDIF

*
*    Номер док-та для печати
*
d_n = LEFT(doc_n,1)+"-"+ALLTRIM(SUBSTR(doc_n,2))

*
*   Открываем таблицы
*
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS TIT_1111 AGAIN
k00 = f_code+d_year+doc_n
IF .NOT. SEEK(k00)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет док_та с номером "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

SELECT 0
USE (base_path+"CUS_BIL") ALIAS CB_1111 AGAIN ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"CLI_INFO") ALIAS CI_1111 AGAIN ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACCOUNT") ALIAS CUS_1111 AGAIN ORDER TAG CUS_CODE
SET RELATION TO CUS_CODE INTO CB_1111
SET RELATION TO CUS_CODE INTO CI_1111 ADDITIVE
SEEK TIT_1111.CUS_CODE

SELECT 0
USE (base_path+"FIRMS") ALIAS FRM_1111 AGAIN ORDER TAG FIRM_CODE
SEEK TIT_1111.FIRM

SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_1111 AGAIN ORDER TAG CODE
SEEK TIT_1111.WHO

SELECT 0
USE (base_path+"ARM_LST") ALIAS ARM_1111 AGAIN ORDER TAG ARM_CODE
SEEK TIT_1111.WHERE

f_name = SYS(3)
DO Wt_Mess WITH "Подготовка документа"
DO Prep_Lst WITH f_name    && ???????????????????????????????
DO Wt_Mess

DIMENSION sb(22,2)
*
sb(01,1) = "{Page}"			&& Страница

*
*		Идентификатор документа
*
sb(02,1) = "{Doc_No   }"	&& Номер док_та
sb(02,2) = d_n

sb(03,1) = "{Doc_Dat }"		&& Дата док-та
sb(03,2) = DTOC(TIT_1111.DOC_DATE)

sb(04,1) = "{Autor                       }"
							&& Автор док-та
sb(04,2) = ALLTRIM(PRS_1111.FAMILY)+" "+LEFT(PRS_1111.NAME, 1)+" "+LEFT(PRS_1111.S_NAME, 1)
sb(04,2) = PADR(sb(04,2), LEN(sb(04,1)))
*
*		Предприятие
*
sb(05,1) = "{Firm                                            }"
							&& Предприятие
sb(05,2) = FRM_1111.L_NAME
sb(05,2) = PADR(sb(05,2), LEN(sb(05,1)))

sb(06,1) = "{Firm Address                                              }"
							&& Адрес предприятия
sb(06,2) = FRM_1111.ADDRESS
sb(06,2) = PADR(sb(06,2), LEN(sb(06,1)))

sb(07,1) = "{Firm phone}"	&& Телефон предприятия
sb(07,2) = FRM_1111.PHONE
sb(07,2) = PADR(sb(07,2), LEN(sb(07,1)))

sb(08,1) = "{Firm INN  }"	&& ИНН предприятия
sb(08,2) = FRM_1111.INN
sb(08,2) = PADR(sb(08,2), LEN(sb(08,1)))

sb(09,1) = "{Boss                  }"
							&& Директор предприятия
sb(09,2) = FRM_1111.BOSS
sb(09,2) = PADR(sb(09,2), LEN(sb(09,1)))

sb(10,1) = "{Accountant            }"
							&& Гл. бухгалтер предприятия
sb(10,2) = FRM_1111.ABAK
sb(10,2) = PADR(sb(10,2), LEN(sb(10,1)))

sb(11,1) = "{Cashier               }"
							&& Кассир предприятия
sb(11,2) = FRM_1111.KASSIR
sb(11,2) = PADR(sb(11,2), LEN(sb(11,1)))

*
*		Клиент
*
sb(12,1) = "{Customer                                        }"
							&& Имя клиента
sb(12,2) = CUS_1111.CUS_NAME
sb(12,2) = PADR(sb(12,2), LEN(sb(12,1)))

sb(13,1) = "{Cus Address                                               }"
							&& Адрес клиента
sb(13,2) = CB_1111.ADDRESS
sb(13,2) = PADR(sb(13,2), LEN(sb(13,1)))

sb(14,1) = "{Cus Phone     }"
							&& Телефон клиента
sb(14,2) = CB_1111.PHONE
sb(14,2) = PADR(sb(14,2), LEN(sb(14,1)))

sb(15,1) = "{Cus Bank                              }"
							&& Банк клиента
sb(15,2) = CB_1111.BANK
sb(15,2) = PADR(sb(15,2), LEN(sb(15,1)))

sb(16,1) = "{Corr Bill         }"
							&& Коррсчет клиента
sb(16,2) = CB_1111.CORR_BILL
sb(16,2) = PADR(sb(16,2), LEN(sb(16,1)))

sb(17,1) = "{Cus BIK           }"
							&& БИК клиента
sb(17,2) = CB_1111.BIK
sb(17,2) = PADR(sb(17,2), LEN(sb(17,1)))

sb(18,1) = "{Cus INN   }"
							&& ИНН клиента
sb(18,2) = CB_1111.INN
sb(18,2) = PADR(sb(18,2), LEN(sb(18,1)))

sb(19,1) = "{Cus City      }"
							&& Город клиента
sb(19,2) = CB_1111.CITY
sb(19,2) = PADR(sb(19,2), LEN(sb(19,1)))

sb(20,1) = "{OKO}"			&& ОКОНХ клиента
sb(20,2) = CB_1111.OKOHX
sb(20,2) = PADR(sb(20,2), LEN(sb(20,1)))

sb(21,1) = "{OKPO  }"		&& ОКПО клиента
sb(21,2) = CB_1111.OKPO
sb(21,2) = PADR(sb(21,2), LEN(sb(21,1)))

sb(22,1) = "{Cus Note                                                              }"
							&& Примечание для клиента
sb(22,2) = CB_1111.NOTE
sb(22,2) = PADR(sb(22,2), LEN(sb(22,1)))

DIMENSION sb_det(6,2)
*
sb_det(01,1) = "{Pr}"			&& Префикс
sb_det(02,1) = "{Name                   }"
								&& Наименование
sb_det(03,1) = "{Prod  }"		&& Фирма производитель
sb_det(04,1) = "{Qnt }"			&& Кол-во
sb_det(05,1) = "{Price }"		&& Цена
sb_det(06,1) = "{Sum     }"		&& Сумма

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "P_TEST" == ALLTRIM(UPPER(DOC_NAME))

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

DO Ini_Prn WITH "?????????????????"+d_n, p_ln, lft, n_cp, fnt, ffeed, p_drctry

SELECT TMP_LST
GO TOP
nn = RECCOUNT()
n_p_det  = p_ln-ALEN(t_d)-MAX(ALEN(f_p),ALEN(f_d))
n_p_det1 = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
n_p_det  = FLOOR(n_p_det/ALEN(det))
n_p_det1 = FLOOR(n_p_det1/ALEN(det))
n_pg = 1
nn = nn-n_p_det

IF n_p_det <= 0  && Забили всю страницу под заголовки... ни каких страниц!
  n_p_det = RECCOUNT()
ELSE
  nn = MAX(nn-n_p_det, 0)
  n_pg = 1+CEILING(nn/n_p_det1)
ENDIF
nn = 0
IF n_pg # 0
  FOR f_pg = 1 TO n_pg
    sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
    sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
    IF f_pg # 1   && Заголовок НЕ первой страницы
      EJECT
      n_p_det = n_p_det1
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
    i = 0
    DO WHILE .T.
*     sb_det(01,1) = "{Pr}"			&& Префикс
      sb_det(01,2) = TMP_LST.PREFIX
*     sb_det(02,1) = "{Name                   }"
									&& Наименование
      sb_det(02,2) = TMP_LST.NAME
*     sb_det(03,1) = "{Prod  }"		&& Фирма производитель
      sb_det(03,2) = TMP_LST.PRODUCER
*     sb_det(04,1) = "{Qnt }"		&& Кол-во
      sb_det(04,2) = STR(TMP_LST.QNT, 6)
*     sb_det(05,1) = "{Price }"		&& Цена
      sb_det(05,2) = STR(TMP_LST.PRICE, 8, 2)
*     sb_det(06,1) = "{Sum     }"		&& Сумма
      sb_det(06,2) = STR(TMP_LST.PRICE*TMP_LST.QNT, 10, 2)
      FOR k = 1 TO ALEN(det)
        str_w = det(k)
        FOR j = 1 TO ALEN(sb_det,1)
          str_w = STRTRAN(str_w, sb_det(j,1), sb_det(j,2) )
        ENDFOR
        @ PROW()+1, 0 SAY str_w
      ENDFOR
      i = i+1
      SKIP
      IF EOF("TMP_LST")
        EXIT
      ENDIF
      IF  i >= n_p_det
        EXIT
      ENDIF
    ENDDO
*
*    Окончание страницы
*
    IF f_pg = n_pg
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
ENDIF

DO Term_Prn WITH "", ""

USE
DELETE FILE (tmpo_path+f_name+".DBF")
DELETE FILE (tmpo_path+f_name+".CDX")

SELECT TIT_1111
USE
SELECT CB_1111
USE
SELECT CI_1111
USE
SELECT CUS_1111
USE
SELECT FRM_1111
USE
SELECT PRS_1111
USE
SELECT ARM_1111
USE

SELECT (s_sav)

RETURN



***********************************************************************
PROCEDURE Prep_Lst
PARAMETER f_name

PRIVATE s_sav, i

s_sav = SELECT()
SELECT 0
CREATE DBF (tmpo_path+f_name) ;
  ( PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT      N( 6), ;
    PRICE    N(8,2)  )
USE (tmpo_path+f_name) ALIAS TMP_LST
SELECT 0
USE (base_path+"STOCK") AGAIN ALIAS ST_1111 ORDER TAG NAME

i = 0
SCAN NEXT 100
  i = i+1
  SELECT TMP_LST
  APPEND BLANK
  REPLACE PREFIX   WITH ST_1111.PREFIX,   ;
          NAME     WITH ST_1111.NAME,     ;
          PRODUCER WITH ST_1111.PRODUCER, ;
          QNT      WITH i % 10 +1,        ;
          PRICE    WITH ST_1111.PRICE_R
  SELECT ST_1111
ENDSCAN
USE
SELECT (s_sav)

RETURN
