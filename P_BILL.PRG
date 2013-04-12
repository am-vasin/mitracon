*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла P_Bill       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                             Печать счета.                              ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 01/19/1999 ══╝
PROCEDURE P_Bill
PARAMETERS firm_c, doc_n, doc_d  &&, flgs
*
*  firm_c - код предприятия;
*  doc_n  - номер документа;
*  doc_d  - либо дата документа, либо год в символьном или числовом формате;
*  flg    - флажки.
*
*IF .NOT. List()
*  RETURN
*ENDIF

PRIVATE s_sav   && Номер рабочей области для возврата
PRIVATE d_year  && Год документа
PRIVATE d_n     && Номер документа в текстовом виде
PRIVATE tmpList && Номер списка
PRIVATE d_d     && Дата документа в текстовом виде
PRIVATE a_code  && Koд работника, оприходовавшего деньги 
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
PRIVATE is_r    && Тип счета - рублевый
PRIVATE tmpType && Тип документа
PRIVATE tmpDCode && Код документа

PRIVATE wDoc, wFirm, wDate

s_sav = SELECT()

*
* Формируем год документа
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
* Символьный номер документа
d_n = LEFT(doc_n,4)+"-"+ALLTRIM(RIGHT(doc_n,6))

*
*  Заголовки документов...
SELECT 0
USE (base_path+"BILLS") ORDER TAG DOC_NUM ALIAS TIT_PRINT AGAIN
k00 = firm_c+d_year+doc_n
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
  SELECT (s_sav)
  RETURN
ENDIF
tmpList = TRANSFORM(NUM_LIST, "@Z 999999")
tmpType = 0
IF FSIZE("DOC_TYPE") # 0
  tmpType = IIF(.NOT. EMPTY(DOC_TYPE), 1, 0)
ENDIF
wFirm = FIRM
wDoc  = DOC_NUM
wDate = DOC_DATE
is_r = VALUTA = "Р"
a_code = WHO

*
*  Ищем клиента...
IF .NOT. EMPTY(CUS_CODE)
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_PRINT AGAIN
  SEEK TIT_PRINT.CUS_CODE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_CODE
  USE
ELSE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_NAME
ENDIF

SELECT TIT_PRINT
d_d = DTOC(DOC_DATE)

*
*   Подготовка временного файла.
*
*  ВНИМАНИЕ! При выходе из файла все открытые в программе печати файлы
*            кроме временного и заголовка должны быть закрыты!
*            Временный файл не может иметь индексов!
*

DO Wt_Mess WITH "Подготовка списка"
DO Prep_Tmp
DO Wt_Mess
SELECT TIT_PRINT

*
*  Описываем псевдополя
DIMENSION sb(37,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Doc_Num  }"
sb( 2,2) = d_n

sb( 3,1) = "{Doc_Date}"
sb( 3,2) = d_d

sb(36,1) = "{Lst }"
sb(36,2) = TRANSFORM(TIT_PRINT.NUM_LIST, "@Z 999999")

sb( 4,1) = "{Firm             }"
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS TMP_PRINT AGAIN
SEEK TIT_PRINT.FIRM
sb(28, 1) = "{Руководитель          }"
sb(28, 2) = ALLTRIM(BOSS)
sb(29, 1) = "{Гл. бухгалтер         }"
sb(29, 2) = ALLTRIM(ABAK)
sb(30, 1) = "{Кассир                }"
sb(30, 2) = ALLTRIM(KASSIR)

sb(27,1) = "{%NSP}"
sb(27,2) = P_NALOG

sb( 4,2) = ALLTRIM(L_NAME)

sb( 5,1) = "{INN     }"
sb( 5,2) = INN

sb( 6,1) = "{Bank             }"
USE (base_path+"BANKS") ORDER TAG BANK ALIAS TMP_PRINT AGAIN
SEEK TIT_PRINT.FIRM+TIT_PRINT.BANK
sb( 6,2) = ALLTRIM(L_NAME)

sb( 7,1) = "{Acc_Bill}"
sb( 7,2) = ALLTRIM(ACC_NO)

sb( 8,1) = "{Korr_Bill}"
sb( 8,2) = ALLTRIM(CORR_NO)

sb( 9,1) = "{BIK    }"
sb( 9,2) = ALLTRIM(BIK)

sb(10,1) = "{OKONH}"
sb(10,2) = ""

sb(11,1) = "{OKPO }"
sb(11,2) = ""
USE

sb(12,1) = "{Customer         }"
sb(12,2) = c_name

sb(13,1) = "{Sum_0        }"
sb(13,2) = ""

sb(14,1) = "{Sum_H        }"
sb(14,2) = ""

sb(15,1) = "{Sum_0_Pag    }"
sb(15,2) = ""

sb(16,1) = "{Sum_H_Pag    }"
sb(16,2) = ""

sb(17,1) = "{Sum_NDS      }"
sb(17,2) = ALLTRIM(TRANSFORM(TIT_PRINT.BILL_SUM-TIT_PRINT.SUM_0,"999 999 999 999.99"))

sb(18,1) = "{Prc_NDS}"
sb(18,2) = ALLTRIM(STR(TIT_PRINT.NDS_,5,1))

sb(19,1) = "{Sum_Note1                                                         }"

sb(20,1) = "{Sum_Note2                                                         }"

sb(21,1) = "{Sum_Note3                                                         }"

PRIVATE s_note
DIMENSION s_note(3)
s_note(1) = LEN(sb(19,1))
s_note(2) = LEN(sb(21,1))
s_note(3) = LEN(sb(21,1))
DO Nt_Lines WITH TIT_PRINT.BILL_SUM, s_note, TIT_PRINT.VALUTA
sb(19,2) = s_note(1)
sb(20,2) = s_note(2)
sb(21,2) = s_note(3)

sb(23,1) = "{Sum_H_NSP}"
sb(23,2) = ROUND(TIT_PRINT.BILL_SUM*(100+sb(27,2))/100,2)

sb(24,1) = "{Sum_Note1+NSP                                                     }"

sb(25,1) = "{Sum_Note2+NSP                                                     }"

sb(26,1) = "{Sum_Note3+NSP                                                     }"

DIMENSION s_note(3)
s_note(1) = LEN(sb(19,1))
s_note(2) = LEN(sb(21,1))
s_note(3) = LEN(sb(21,1))
DO Nt_Lines WITH sb(23,2), s_note, TIT_PRINT.VALUTA
sb(24,2) = s_note(1)
sb(25,2) = s_note(2)
sb(26,2) = s_note(3)
sb(27,2) = STR(sb(27,2), 6, 1)
sb(23,2) = STR(sb(23,2), 11, 2)

sb(22,1) = "{Autor                       }"
sb(33,2) = SPACE(16)
sb(34,2) = SPACE(6)
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
sb(35,1) = "{E-mail менеджера}"
sb(33,2) = ""
sb(34,2) = ""
sb(35,2) = ""
IF a_code = 0
  sb(22,2) = "САМ!"
ELSE
  IF SEEK (a_code)
    sb(22,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
    sb(33,2) = W_PHONE1
    sb(34,2) = W_PHONE2
  ELSE
    sb(22,2) = "?!!"
  ENDIF
  sb(35,2) = ALLTRIM(EML)
ENDIF         
sb(22,2) = PADR(sb(22,2),LEN(sb(22,1)))

sb(31,1) = "{ИНН клиента }"
sb(37,1) = "{КПП клиента   }"
sb(32,1) = "{Адрес клиента                                               }"
sb(33,1) = "{Phone         }"
sb(34,1) = "{Phn }"

USE (base_path+"CUS_BIL") ORDER TAG CUS_CODE ALIAS CB9903 SHARED AGAIN
IF SEEK(c_code, "CB9903")
  IF .NOT. EMPTY(CUS_NAME)
    sb(12,2) = CUS_NAME
  ENDIF
  sb(31,2) = INN
  sb(32,2) = ADDRESS
  sb(37,2) = KPP
ELSE
  sb(31,2) = ""
  sb(32,2) = ""
  sb(37,2) = ""
ENDIF
USE

* Изучаем описание документа
SELECT TIT_PRINT
USE (base_path+"DOC_FORM")
IF tmpType = 0
  tmpDCode = IIF(is_r, "BILL", "BILL$")
ELSE
  tmpDCode = IIF(is_r, "LBILL", "LBILL$")
ENDIF

  LOCATE FOR tmpDCode == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN-1 && Длина страницы в строках
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
PRIVATE tmpLen

n_p_det = p_ln-MAX(ALEN(f_p),ALEN(f_d))-MAX(ALEN(t_p),ALEN(t_d))
n_p_det = FLOOR(n_p_det/2)
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
*  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
  DO CASE
  CASE RECCOUNT() < FLOOR((p_ln-ALEN(f_d)-ALEN(t_d))/2)	&& Всего одна страница
    n_pg = 1
  CASE RECCOUNT() < FLOOR((p_ln-ALEN(f_d)-ALEN(t_d))/2)+ ;
                    FLOOR((p_ln-ALEN(f_p)-ALEN(t_p))/2)		&& Всего две страницы
    n_pg = 2
  OTHERWISE
    m.tmpLen = RECCOUNT()-FLOOR((p_ln-ALEN(t_d)-ALEN(f_p))/2)
    n_pg = 1+FLOOR(m.tmpLen/FLOOR((p_ln-ALEN(t_p)-ALEN(f_p))/2))
    m.tmpLen = m.tmpLen % FLOOR((p_ln-ALEN(t_p)-ALEN(f_p))/2)
    IF m.tmpLen = 0
      n_pg = n_pg-1
      m.tmpLen = FLOOR((p_ln-ALEN(t_p)-ALEN(f_p))/2)
    ENDIF
    IF m.tmpLen > FLOOR((p_ln-ALEN(t_p)-ALEN(f_d))/2)
      n_pg = n_pg+2
    ELSE
      n_pg = n_pg+1
    ENDIF
  ENDCASE
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
  * Длина очередной страницы
  DO CASE
  CASE n_pg = 1
    n_p_det = RECCOUNT()
  CASE n_pg = 2
    IF f_pg = 1
      n_p_det = FLOOR((p_ln-ALEN(f_p)-ALEN(t_d))/2)
      IF n_p_det = RECCOUNT()
        n_p_det = n_p_det-1
      ENDIF
    ELSE
      n_p_det = RECCOUNT()-nn
    ENDIF
  OTHERWISE
    DO CASE
    CASE f_pg = 1
      n_p_det = FLOOR((p_ln-ALEN(f_p)-ALEN(t_d))/2)
    CASE f_pg = n_pg
      n_p_det = RECCOUNT()-nn
    CASE f_pg = n_pg-1
      n_p_det = MIN(FLOOR((p_ln-ALEN(f_p)-ALEN(t_p))/2), RECCOUNT()-nn-1)
    OTHERWISE
      n_p_det = FLOOR((p_ln-ALEN(f_p)-ALEN(t_p))/2)
    ENDCASE
  ENDCASE
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
      @ PROW()+1,0 SAY NAME+TRANSFORM(QNT,"@Z 99999")+" "+  ;
                            TRANSFORM(PRC, "@Z 999999999.99")+ ;
                            TRANSFORM(SUM_0, "@Z 9999999999.99")+ ;
               IIF(is_r,TRANSFORM(SUM_T, "@Z 9999999999.99"),"")
      @ PROW()+1,0 SAY CLASS
      sum_p0 = sum_p0+SUM_0
      sum_p1 = sum_p1+SUM_T
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sb(15,2) = TRANSFORM(sum_p0, "9999999999.99")
  sb(15,2) = PADL(sb(15,2), LEN(sb(15,1)))
  sb(16,2) = TRANSFORM(sum_p1, "9999999999.99")
  sb(16,2) = PADL(sb(16,2), LEN(sb(16,1)))
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
ENDFOR
sb(13,2) = TRANSFORM(sum_d0, "9999999999.99")
sb(13,2) = PADL(sb(13,2), LEN(sb(13,1)))
sb(14,2) = TRANSFORM(sum_d1, "9999999999.99")
sb(14,2) = PADL(sb(14,2), LEN(sb(14,1)))
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

DO Term_Prn WITH "", str_w, IIF(TYPE("c_code") = "N", c_code, .F.), "BILL0", wFirm, wDoc, wDate

DELETE FILE (str_w)
SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Заготовка содержимого документа.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 01/19/1999 ──┘
PROCEDURE Prep_Tmp

PRIVATE tag_n, tag_exp, cName
SELECT 0
CREATE DBF (tmpo_path+"PRINT.TMP") ;
   (NAME     C(49), ;
    CLASS    C(64), ;
    QNT      N(10), ;
    PRC      N(13,2), ;
    SUM_0    N(15,2), ;
    SUM_T    N(15,2))
USE (tmpo_path+"PRINT.TMP") ALIAS TMP_LST EXCLUSIVE

*
*   Детальный файл
SELECT 0
USE (base_path+"CLASSES") ORDER TAG CODE ALIAS TMP_CLASS AGAIN
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS TMP_STO AGAIN
*SET RELATION TO LEFT(CLASS, 2) INTO TMP_CLASS
*SET RELATION TO CLASS INTO TMP_CLASS
SELECT 0
USE (base_path+"BILLS_D") ORDER TAG DOC_NUM ALIAS DET_PRINT AGAIN
SET RELATION TO CODE INTO TMP_STO

*
*   Определяем индексное выражение
*
tag_n = 1
DO WHILE .NOT. TAG() == TAG(tag_n)
  tag_n = tag_n+1
ENDDO
tag_exp = SYS(14,tag_n)

= SEEK(k00)

SCAN REST WHILE EVALUATE(tag_exp) = k00   && По строкам документа
  m.cName = Cls_Name(TMP_STO.CLASS, "TMP_CLASS")
  SELECT TMP_LST
  APPEND BLANK
  REPLACE NAME     WITH DET_PRINT.NAME,   ;
          CLASS    WITH m.cName,   		  ;
          QNT      WITH DET_PRINT.QNT,    ;
          PRC      WITH DET_PRINT.PRICE_0,;
          SUM_0    WITH DET_PRINT.SUM_0,  ;
          SUM_T    WITH DET_PRINT.SUM_T
  SELECT DET_PRINT
ENDSCAN
USE
USE IN TMP_CLASS
USE IN TMP_STO

SELECT TMP_LST

RETURN


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура List         Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│             Выбор печати счета со списком или без списка.              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.04.2001 ──┘
PROCEDURE List

*
*  Переменные состояния для навигации
*

PRIVATE stat_type     && Тип навигации: 0 - нестандартный;
                                        1 - бланк;
                                        2 - BROWSE - таблица;
                                        3 - BROWSE - список.
PRIVATE what_do       && Имя режима.
PRIVATE menu_name     && Имя асинхронного меню.
PRIVATE last_mouse    && Время последнего нажатия звериной кнопки.
PRIVATE win_name      && Имя окна ( окон для BROWSE ).

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌───────────────────────────────┐
*│                               │
*│ < Без списка > < Со списком > │
*│                               │
*└───────────────────────────────┘

PRIVATE ex, p_flg, v_sel
v_sel = SELECT()
ex = 1
p_flg = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 35, "Печать счета"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3, 3 GET ex PICTURE "@*HT \ Без списка ;\ Со списком "
READ CYCLE

IF ex = 1
  p_flg = .T.
  DO P_Bill_0 WITH firm_c, doc_n, doc_d
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
SELECT (v_sel)

RETURN ex = 2
