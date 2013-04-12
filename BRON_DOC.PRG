*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Bron_Doc     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Список забронированного.                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 07/03/2000 ══╝
PROCEDURE Bron_Doc

DO Prep_Tmp
DO Prn_Doc
SELECT BR_TMP0
USE
DELETE FILE (tmpo_path+"BR_TMP0.DBF")
DELETE FILE (tmpo_path+"BR_TMP0.CDX")
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07/03/2000 ──┘
PROCEDURE Prep_Tmp

PRIVATE tmpNum, tmpCust, tmpSal
DELETE FILE (tmpo_path+"BR_TMP0.DBF")
DELETE FILE (tmpo_path+"BR_TMP0.CDX")

CREATE DBF (tmpo_path+"BR_TMP0") ;
  (  CODE     N( 7), ;
     PREFIX   C( 4), ;
     NAME     C(35), ;
     PRODUCER C( 8), ;
     QNT_HOLE N( 6), ;
     REST     N( 6), ;
     SALESMAN C(24), ;
     ORD_DATE D,     ;
     QNT_ORD  N( 6), ;
     LIST_NUM C( 6), ;
     CUSTOMER C(50)   )

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_0703

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_0703

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO_0703

SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS ORDT_0703
SET RELATION TO WHO INTO WHO_0703

SELECT 0
USE (base_path+"ORD_D") ORDER TAG DOC_NUM ALIAS ORDD_0703
SET RELATION TO LIST_NUM+STR(ORD_NUM,3) INTO ORDT_0703
SET RELATION TO CODE INTO STO_0703 ADDITIVE

SELECT 0
USE (base_path+"LIST_TIT") ALIAS LST_0703
SET RELATION TO CLI_CODE INTO CUS_0703

SCAN FOR EMPTY(SAL_NUM)
  SELECT ORDD_0703
  tmpNum = STR(LST_0703.LST_NUM,6)
  SEEK tmpNum
  IF FOUND("CUS_0703")
    tmpCust = CUS_0703.CUS_NAME
  ELSE
    tmpCust = LST_0703.LST_NAME
  ENDIF
  SCAN REST WHILE LIST_NUM = tmpNum
    IF FOUND("WHO_0703")
      tmpSal = WHO_0703.FAMILY
    ELSE
      tmpSal = ""
    ENDIF
    SELECT BR_TMP0
    APPEND BLANK
    REPLACE CODE     WITH ORDD_0703.CODE,    ;
            PREFIX   WITH STO_0703.PREFIX,   ;
            NAME     WITH STO_0703.NAME,     ;
            PRODUCER WITH STO_0703.PRODUCER, ;
            REST     WITH STO_0703.QNT,      ;
            SALESMAN WITH tmpSal,            ;
            ORD_DATE WITH ORDT_0703.ORD_DATE,;
            QNT_ORD  WITH ORDD_0703.QNT,     ;
            LIST_NUM WITH tmpNum,            ;
            CUSTOMER WITH tmpCust
    SELECT ORDD_0703
  ENDSCAN
  SELECT LST_0703
ENDSCAN

SELECT STO_0703
USE

SELECT CUS_0703
USE

SELECT WHO_0703
USE

SELECT ORDT_0703
USE

SELECT ORDD_0703
USE

SELECT LST_0703
USE

SELECT BR_TMP0
INDEX ON NAME+PREFIX+PRODUCER+STR(CODE) TAG NAME

tmpSal = -1
tmpCus = 0
SCAN
  IF CODE # tmpSal
    tmpSal = CODE
    tmpCus = 0
  ENDIF
  tmpCus = tmpCus+QNT_ORD
  REPLACE QNT_HOLE WITH tmpCus
ENDSCAN

tmpSal = -1
tmpCus = 0
SET ORDER TO TAG NAME DESCENDING
SCAN
  IF CODE # tmpSal
    tmpSal = CODE
    tmpCus = QNT_HOLE
  ENDIF
  REPLACE QNT_HOLE WITH tmpCus
ENDSCAN
SET ORDER TO TAG NAME ASCENDING

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prn_Doc      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Печать документа                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.07.2000 ──┘
PROCEDURE Prn_Doc

PRIVATE sb

DIMENSION sb(3, 2)

sb( 1,1) = "{Ст/Стц  }"
sb( 1,2) = ""

sb( 2,1) = "{Дата    }"
sb( 2,2) = DTOC(DATE())

sb( 3,1) = "{Время }"
sb( 3,2) = TIME()

* Изучаем описание документа
SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "BRON_DOC" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN   && Длина страницы в строках
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT   && Шрифт: " " - обычный, "1" - сжатый (condenced)
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

SELECT BR_TMP0

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
PRIVATE tmpCode
tmpCode = -1

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
      IF tmpCode # CODE
        str_w = PREFIX+" "+NAME+" "+PRODUCER+STR(REST,7)+STR(QNT_HOLE,7)+" "
        tmpCode = CODE
      ELSE
        str_w = SPACE(54)
      ENDIF
      @ PROW()+1,0 SAY str_w+STR(QNT_ORD,6)+" "+LEFT(SALESMAN,15)+" "+  ;
                             LIST_NUM+" "+DTOC(ORD_DATE)+" "+LEFT(CUSTOMER,34)
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
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
COPY TO (tmpo_path+"TMP000.DBF")

DO Term_Prn WITH "", tmpo_path+"TMP000.DBF", .F.

DELETE FILE (tmpo_path+"TMP000.DBF")

RETURN
