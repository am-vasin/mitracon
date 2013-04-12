PARAMETERS firm_c, doc_n, doc_d  &&, flgs
*
*  firm_c - код предприятия;
*  doc_n  - номер документа;
*  doc_d  - либо дата документа, либо год в символьном или числовом формате;
*  flg    - флажки.
*

PRIVATE s_sav   && Номер рабочей области для возврата
PRIVATE d_year  && Год документа
PRIVATE d_n     && Номер документа в текстовом виде
PRIVATE d_d     && Дата документа в текстовом виде
PRIVATE c_name  && Имя клиента (если таковой имеет место)
PRIVATE c_code  && Код клиента (если таковой имеет место)
PRIVATE sb      && Массив описание псевдополей
PRIVATE t_d     && Массив со строками шаблона "заголовок докумета"
PRIVATE t_p     && Массив со строками шаблона "заголовок страницы"
PRIVATE f_d     && Массив со строками шаблона "подвал докумета"
PRIVATE f_p     && Массив со строками шаблона "подвал страницы"
PRIVATE n, j    && Счетчики, индексы, параметры цикла
PRIVATE p_ln    && Длина страницы в строках
PRIVATE fnt     && Шрифт: " " - обычный, "1" - сжатый (condenced)
PRIVATE n_cp    && Число копий
PRIVATE lft     && Поля слева в символах
PRIVATE ffeed   && Завершение документа: не отрицетельное - прогон строк,
                &&      отрицательное - прогон формата
PRIVATE p_drcty && Направление печати (принтер)
PRIVATE k00     && Ключ для поиска документа

s_sav = SELECT()

*
* Формируем год документа
IF TYPE("doc_n") = "D"
  d_year = LEFT(DTOS(doc_n),4)
ENDIF
IF TYPE("doc_n") = "N"
  d_year = STR(doc_n,4)
ENDIF
IF TYPE("doc_n") = "C"
  d_year = doc_n
ENDIF

*
* Символьный номер документа
d_n = LEFT(doc_n,4)+"-"+ALLTRIM(RIGHT(doc_n,6)) !!!!!!!!!!!!!!!
d_n = ALLTRIM(doc_n)                            !!!!!!!!!!!!!!!

*
*  Заголовки документов...
SELECT 0
USE (base_path+"!!!!!_TIT") ORDER TAG DOC_NUM ALIAS TIT_PRINT AGAIN
k00 = firm_c+d_year+doc_n        !!!!!!!!!!!!!!!!!!!
*
*  Ищем...
IF .NOT. SEEK(k00)  && Не нашли...
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*
*  Ищем клиента...
IF .NOT. EMPTY(CUS_CODE)
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_NNNN AGAIN
  SEEK TIT_PRINT.CUS_CODE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_CODE
  USE
ENDIF
SELECT TIT_PRINT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

*
*   Подготовка временного файла.
*
*  ВНИМАНИЕ! При выходе из файла все открытые в программе печати файлы
*            кроме временного должны быть закрыты!
*            Временный файл не может иметь индексов!
*
DO Wt_Mess WITH "Подготовка списка"
DO Prep_Tmp
DO Wt_Mess

*
*  Описываем псевдополя
DIMENSION sb(??,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Doc_Num  }"
sb( 2,2) = d_n

sb( 3,1) = "{Doc_Date}"
sb( 3,2) = d_d

sb( 4,1) = "{Customer         }"
sb( 4,2) = c_name
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

*
* Изучаем описание документа
SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "!!!!!!!!!!" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN   && Длина страницы в строках
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT  && Шрифт: " " - обычный, "1" - сжатый (condenced)
n_cp = DOC_FORM.N_COPIES   && Число копий
lft  = DOC_FORM.LEFT_FIELD && Поля слева в символах
ffeed =DOC_FORM.F_FEED     && Завершение документа: 
                           &&      не отрицетельное - прогон строк,
                           &&      отрицательное - прогон формата
p_drctry = DOC_FORM.P_DIR  && Направление печати (принтер)

*
*  Формируем шаблоны для заголовков и подвалов
*
n = MEMLINES(PAGE_H)  &&     Заголовок страницы (не первой)
IF n > 0
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ELSE
  DIMENSION t_p(1)
  t_p(1) = ""
ENDIF

n = MEMLINES(DOC_H)  &&     Заголовок первой страницы
IF n > 0
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ELSE
  DIMENSION t_d(ALEN(t_p))
  FOR j = 1 TO ALEN(t_p)
    t_d(j) = t_p(j)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)  &&     Подвал страницы (не последней!)
IF n > 0
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ELSE
  DIMENSION f_p(1)
  f_p(1) = ""
ENDIF

n = MEMLINES(DOC_F)  &&     Подвал последней страницы
IF n > 0
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ELSE
  DIMENSION f_d(ALEN(f_p))
  FOR j = 1 TO ALEN(f_p)
    f_d(j) = f_p(j)
  ENDFOR
ENDIF

USE

SELECT TMP_LST

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

PRIVATE n_p_det  && Число детальных строк на странице
PRIVATE n_pg     && Число страниц в документе
PRIVATE f_pg     && Текущая страница документа при печати
PRIVATE str_w    && Образ печатаемой строки
PRIVATE sum_p0, sum_p1 && ...Суммы по страницам
PRIVATE sum_d0, sum_d1 && ...Суммы по документу
PRIVATE nn       && Номер строки по порядку...

n_p_det = p_ln-MAX(ALEN(f_p),ALEN(f_d))-MAX(ALEN(t_p),ALEN(t_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
ENDIF

*
*  Заголовок документа
sb( 1,2) = "1/"+ALLTRIM(STR(n_pg))
sb( 1,2) = PADL(sb(1,2), LEN(sb(1,1)))
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

STORE 0 TO sum_d0, sum_d1, nn
FOR f_pg = 1 TO n_pg    && По страницам...

  IF f_pg # 1  && Заголовок страницы (не первой!)
    *  Если имеет место нумерация страниц...
    sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
    sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
    EJECT
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
  ENDIF
  STORE 0 TO sum_p0, sum_p1
  IF RECCOUNT() # 0
    *
    *  Детальные строки очередной страницы
    FOR i = 1 TO n_p_det
      nn = nn+1
      @ PROW()+1,0 SAY !!!!!!!!!!!!!!!!!
      sum_p0 = sum_p0+!!!!!!!!!!!
      sum_p1 = sum_p1+!!!!!!!!!!!
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sb( !!!!!,2) = TRANSFORM(sum_p0, "9999999.99")
  sb( !!!!!,2) = PADL(sb(2,2), LEN(sb(2,1)))
  sb( !!!!!,2) = TRANSFORM(sum_p1, "9999999.99")
  sb( !!!!!,2) = PADL(sb(2,2), LEN(sb(2,1)))
  sum_d0 = sum_d0+sum_p0
  sum_d1 = sum_d1+sum_p1
  *
  *  Окончание очередной страницы (не последней!)
  *
  IF f_pg # n_pg
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
  ENDIF
ENDFOR
sb( !!!!!,2) = TRANSFORM(sum_d0, "9999999.99")
sb( !!!!!,2) = PADL(sb(2,2), LEN(sb(2,1)))
sb( !!!!!,2) = TRANSFORM(sum_d1, "9999999.99")
sb( !!!!!,2) = PADL(sb(2,2), LEN(sb(2,1)))
*
*   Окончание последней страницы
*
FOR i = 1 TO ALEN(f_d)
  str_w = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR
str_w = DBF("TMP_LST")
USE

DO Term_Prn WITH "", str_w, IIF(TYPE("c_code") = "N", c_code, .F.)

DELETE FILE (str_w)
SELECT (sel_rem)
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа подготовки содержимого списка.                 ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Prep_Tmp
PRIVATE tag_n, tag_exp
SELECT 0
CREATE DBF (tmpo_path+"PRINT.TMP")
   (PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT      N( 6), ;
    PRC      N( 8,2), ;
    SUM_T    N(10,2))
USE (tmpo_path+"PRINT.TMP") ALIAS TMP_LST EXCLUSIVE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

*
*   Номенклатура
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_PRINT AGAIN
*
*   Детальный файл
SELECT 0
USE (base_path+"!!!!!_D") ORDER TAG DOC_NUM ALIAS DET_PRINT AGAIN
SET RELATION TO CODE INTO ST_PRINT

*
*   Определяем индексное выражение
*
tag_n = 1
DO WHILE .NOT. TAG() == TAG(tag_n)
  tag_n = tag_n+1
ENDDO
tag_exp = SYS(14,tag_n)


= SEEK(k00)

SCAN REST WHILE EVALUATE(tag_exp) = k000   && По строкам документа
  SELECT TMP_LST
  APPEND BLANK
  REPLACE PREFIX   WITH ST_PRINT.PREFIX,  ;
          NAME     WITH ST_PRINT.NAME,    ;
          PRODUCER WITH ST_PRINT.PRODUCER,;
          QNT      WITH DET_PRINT.QNT,      ;
          PRC      WITH DET_PRINT.PRICE_R,  ;
          SUM_T    WITH DET_PRINT.PRICE_R*DET_PRINT.QNT
  ENDIF
  SELECT DET_PRINT
ENDSCAN
USE

SELECT ST_PRINT
USE

SELECT TIT_PRINT
USE

SELECT TMP_LST

RETURN
