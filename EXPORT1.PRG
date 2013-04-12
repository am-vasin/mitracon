*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║            Программа формирования DBF файла для EXCEL.                ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Export1

*
*  Переменные состояния для навигвции
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

                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
*------------------------------------------------------------------------

PRIVATE div_l_n, div_l_c, div_n, f_name
USE (base_path+"DIVISION")
div_n = 0
SCAN
  div_n = div_n+1
  DIMENSION div_l_n(div_n), div_l_c(div_n)
  div_l_n(div_n) = B_NAME
  div_l_c(div_n) = CODE
ENDSCAN

PUSH KEY CLEAR

PRIVATE sw_div, sw_empt, sw_new, what_div, what_dat

sw_div   = 1
sw_empt  = .T.
sw_new   = 1
what_div = 1
what_dat = {}
f_name = SPACE(40)

PRIVATE is_code, is_date
PRIVATE is_prf, is_name, is_firm, is_div, is_addr
PRIVATE is_pod, is_por,  is_prd, is_prr,  is_po1d, is_po1r
PRIVATE is_qs,  is_qf,   is_qb,  is_pack, is_round, is_class, is_corpus
PRIVATE is_credit
PRIVATE is_qntopt
PRIVATE sw_list

STORE .T. TO is_prf, is_name, is_firm,  is_qs
STORE .F. TO is_code, is_date, is_div, is_addr, is_pod, is_prd, is_prr, is_qf, is_qb
STORE .F. TO is_po1r, is_po1d, is_credit
STORE .F. TO is_pack, is_round, is_class, is_corpus, is_por, is_qntopt
sw_list = 1

*" ┌─ Состав позиций ─────────────────────────────────────────┐"
*" │┌─ Разделы ─────────────────┐┌─ Новые позиции ───────────┐│"
*" ││ ( ) Все  ( ) Один раздел  ││ ( ) Все  ( ) Новые        ││"
*" ││   ┌──────────────────╖    ││ созданные после ДД/ММ/ГГ  ││"
*" ││   │                  ║    ││                           ││"
*" ││   ╘══════════════════╝    ││                           ││"
*" │└───────────────────────────┘└───────────────────────────┘│"
*" │          [ ] Пустые ( отсутствующие на складе )          │"
*" └──────────────────────────────────────────────────────────┘"
*" ┌─ Состав полей ───────────────────────────────────────────┐"
*" │ [ ] Код (CODE)               [ ] Цена опт. USD (PRICE1)  │"
*" │ [ ] Дата создания (DATE_ON)  [ ] Цена опт. руб. (PRICE2) │"
*" │ [ ] Префикс (PREFIX)         [ ] Цена розн. USD (PRICE3) │"
*" │ [ ] Наименование (NAME)      [ ] Цена розн. руб.(PRICE4) │"
*" │ [ ] Фирма (FIRM)             [ ] Нал. на складе (QNT1)   │"
*" │ [ ] Раздел (DIV)             [ ] Нал. в филиалах (QNT2)  │"
*" │ [ ] Адрес (ADDRESS)          [ ] Бронь (QNT3)            │"
*" └──────────────────────────────────────────────────────────┘"
*" Куда пишем ( файл ) ....:....!....:....!....:....!....:....!"
*"              < OK Ctrl-W > < Отказаться Esc >               "

*" ┌─ Состав позиций ─────────────────────────────────────────┐"
*" │┌─ Разделы ─────────────────┐┌─ Новые позиции ───────────┐│"
*" ││ ( ) Все  ( ) Один раздел  ││ ( ) Все  ( ) Новые        ││"
*" ││   ┌──────────────────╖    ││ созданные после ДД/ММ/ГГ  ││"
*" ││   │                  ║    ││                           ││"
*" ││   ╘══════════════════╝    ││                           ││"
*" │└───────────────────────────┘└───────────────────────────┘│"
*" │          [ ] Пустые ( отсутствующие на складе )          │"
*" └──────────────────────────────────────────────────────────┘"
*"                       < Состав полей >                      "
*" Куда пишем ( файл ) ....:....!....:....!....:....!....:....!"
*"              < OK Ctrl-W > < Отказаться Esc >               "

DO Prp_Nav_2
DO D_Win_N  WITH 15, 66, "Заполните таблицу"
DO Draw_Box WITH  2,  3, 10, 62, " Состав позиций "
DO Draw_Box WITH  3,  4,  8, 32, " Разделы "
DO Draw_Box WITH  3, 33,  8, 61, " Новые позиции "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@  4,  6 GET sw_div   PICTURE "@*RH Все ;Один раздел" WHEN div_n > 1 VALID V_1()
IF div_n > 0
  @  5,  9 GET what_div PICTURE "@^ " FROM div_l_n  SIZE 1, 18 WHEN sw_div = 2
ENDIF

@  4, 38 GET sw_new   PICTURE "@*RH Все ;Новые" VALID V_2() WHEN V_2()
@  5, 51 GET what_dat PICTURE "@D " WHEN sw_new = 2 VALID V_3()
@  9, 14 GET sw_empt  PICTURE "@*C Пустые ( отсутствующие на складе )" WHEN V_2()
*@ 12,  5 GET is_code PICTURE "@*C Код (CODE)"
*@ 13,  5 GET is_date PICTURE "@*C Дата создания (DATE_ON)"
*@ 14,  5 GET is_prf  PICTURE "@*C Префикс (PREFIX)"
*@ 15,  5 GET is_name PICTURE "@*C Наименование (NAME)"
*@ 16,  5 GET is_firm PICTURE "@*C Фирма (FIRM)"
*@ 17,  5 GET is_div  PICTURE "@*C Раздел (DIV)"
*@ 18,  5 GET is_addr PICTURE "@*C Адрес (ADDRESS)"
*@ 12, 34 GET is_pod  PICTURE "@*C Цена опт. USD (PRICE1)"
*@ 13, 34 GET is_por  PICTURE "@*C Цена опт. руб. (PRICE2)"
*@ 14, 34 GET is_prd  PICTURE "@*C Цена розн. USD (PRICE3)"
*@ 15, 34 GET is_prr  PICTURE "@*C Цена розн. руб.(PRICE4)"
*@ 16, 34 GET is_qs   PICTURE "@*C Нал. на складе (QNT1)"
*@ 17, 34 GET is_qf   PICTURE "@*C Нал. в филиалах (QNT2)"
*@ 18, 34 GET is_qb   PICTURE "@*C Бронь (QNT3)"
@ 11, 22 GET sw_list PICTURE "@*N \ Состав полей " VALID F_Flags()
@ 12,  3 SAY "Куда пишем ( файл )" GET f_name && VALID Tst_File()
@ 13, 16 GET ex      PICTURE"@*HT \! OK Ctrl-W ;\? Отказаться Esc "

@ 6, 10 SAY SPACE(16)
@ 5, 35 SAY SPACE(26)

READ CYCLE VALID No_Fields()

RELEASE WINDOW (win_name)

IF ex = 1
  IF sw_div = 2
    what_div = div_l_c(what_div)
  ENDIF
  DO Wt_Mess WITH "Заполняем файл..."
  PRIVATE n_f
  IF is_corpus
    SELECT 0
    USE (path_comm+"CORPUS") ORDER TAG CODE
    SELECT 0
    USE (path_comm+"SUBJECT") ORDER TAG STO_CODE
    SET RELATION TO COR_CODE INTO CORPUS
  ENDIF
  SELECT DIVISION
  SET ORDER TO TAG CODE
  SELECT 0
  USE (base_path+"DEVELOP") ORDER TAG CODE
  SELECT 0
  USE (base_path+"CLASSES") ORDER TAG CODE
  SELECT 0
  USE (base_path+"STOCK") ORDER TAG NAME
  IF is_corpus
    SET RELATION TO PRO_CODE INTO DEVELOP,   ;
                    DIV_CODE INTO DIVISION,  ;
                    CLASS    INTO CLASSES,   ;
                    CODE     INTO SUBJECT
  ELSE
    SET RELATION TO PRO_CODE INTO DEVELOP,   ;
                    CLASS    INTO CLASSES,   ;
                    DIV_CODE INTO DIVISION
  ENDIF
  n_f = Crea_F()
  DO Fill_Res
  DO Wt_Mess
ENDIF
*--------------------------------------------------------------------------

CLOSE DATABASES
POP KEY
RETURN

&&--------------------------------------------------------------------------
&&
&&     Valid для выбора разделов
&&
PROCEDURE V_1

IF sw_div = 1
  @ 6, 10 SAY SPACE(16)
ELSE
  IF div_n > 0
    SHOW GET what_div
  ENDIF
ENDIF
RETURN .T.

&&--------------------------------------------------------------------------
&&
&&     Valid для выбора новых позиций
&&
PROCEDURE V_2
IF sw_new = 1
  @ 5, 35 SAY SPACE(26)
ELSE
  @ 5, 35 SAY "созданные после"
  SHOW GET what_dat
ENDIF
RETURN .T.

&&--------------------------------------------------------------------------
&&
&&     Valid для выхода из даты
&&
PROCEDURE V_3

IF EMPTY(what_dat)
  sw_new = 1
  SHOW GET sw_new
  @ 5, 35 SAY SPACE(26)
ENDIF

RETURN .T.

&&--------------------------------------------------------------------------
&&
&&     Проверка наличия в запросе хотя бы одного поля
&&
PROCEDURE No_Fields
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF .NOT. Tst_File()
  RETURN .F.
ENDIF

IF .NOT. (is_code .OR. is_prf  .OR. is_name .OR. is_por .OR. ;
          is_qs   .OR. is_firm .OR. is_div .OR. ;
          is_pack .OR. is_round .OR. is_class .OR. is_corpus .OR. ;
          is_addr .OR. is_pod  .OR. is_prd .OR. ;
          is_prr  .OR. is_qf   .OR. is_qb .OR. is_credit)

  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Что-нибудь надо же вывести - Вы не указали ни одного поля."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

&&--------------------------------------------------------------------------
&&
&&     Проверка корректности имени файла и пути
&&
PROCEDURE Tst_File
PRIVATE mss, pt, i

IF ex = 2 .OR. READKEY() % 265 = 12
  RETURN .T.
ENDIF

pt = ALLTRIM(f_name)
IF LEN(pt) > 2
  IF INLIST(LEFT(pt,2), "a:", "A:", "b:", "B:")
    DO WHILE .NOT. Flop(LEFT(UPPER(pt),1))
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Дискета "+LEFT(pt,2)+" не установлена. Как поступим? [ Установим ] [ Сменим файл ]"
      mss(3) = ""
      IF Out_Mess(7, "mss") = 2
        RETURN .F.
      ENDIF
    ENDDO
  ENDIF
ENDIF

i = RAT("\", f_name)
IF EMPTY(f_name) .OR. i >= LEN(ALLTRIM(f_name))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Будьте так любезны, укажите имя файла."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

i = RAT("\", f_name)
IF i # 0
  pt = LEFT(f_name, i-1)
  IF LEN(pt) > 0 .AND. RIGHT(pt,1) # ":"
    IF .NOT. Is_Dir(pt)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Путь, который Вы указали, ни куда не ведет..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

pt = ALLTRIM(f_name)

IF .NOT. "." $ pt
  pt = pt+".DBF"
ENDIF

IF File_O(pt)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Такой файл уже существует. Как поступим? [ Удалим ] [ Сменим файл ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN .F.
  ENDIF
  DELETE FILE (pt)
ENDIF

RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Программа создания DBF файла.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Crea_F
PRIVATE _stru, n

n = 0
IF is_code
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "CODE"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("CODE","STOCK")
  _stru(n,4) = 0
ENDIF

IF is_prf
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "PREFIX"
  _stru(n,2) = "C"
  _stru(n,3) = FSIZE("PREFIX","STOCK")
  _stru(n,4) = 0
ENDIF

IF is_name
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "NAME"
  _stru(n,2) = "C"
  _stru(n,3) = FSIZE("NAME","STOCK")
  _stru(n,4) = 0
ENDIF

IF is_firm
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "FIRM"
  _stru(n,2) = "C"
  _stru(n,3) = FSIZE("B_NAME","DEVELOP")
  _stru(n,4) = 0
ENDIF

IF is_class
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "CLASS"
  _stru(n,2) = "C"
  _stru(n,3) = FSIZE("NAME","CLASSES")
  _stru(n,4) = 0
ENDIF

IF is_div
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "DIV"
  _stru(n,2) = "C"
  _stru(n,3) = FSIZE("B_NAME","DIVISION")
  _stru(n,4) = 0
ENDIF

IF is_corpus
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "CORPUS"
  _stru(n,2) = "C"
  _stru(n,3) = FSIZE("NAME","CORPUS")
  _stru(n,4) = 0
ENDIF

IF is_addr
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "ADDRESS"
  _stru(n,2) = "C"
  _stru(n,3) = FSIZE("ADDRESS","STOCK")
  _stru(n,4) = 0
ENDIF

IF is_round
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "QNT_ROUND"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("QNT_PACK","STOCK")
  _stru(n,4) = 0
ENDIF

IF is_pack
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "QNT_PACK"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("QNT_ROUND","STOCK")
  _stru(n,4) = 0
ENDIF

IF is_date
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "DATE_ON"
  _stru(n,2) = "D"
  _stru(n,3) = 8
  _stru(n,4) = 0
ENDIF

IF is_pod
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "PRICE1"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("PRICE","STOCK")
  _stru(n,4) = 4
ENDIF

IF is_por
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "PRICE2"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("PRICE_R","STOCK")
  _stru(n,4) = 2
ENDIF

IF is_prd
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "PRICE3"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("PRICE","STOCK")
  _stru(n,4) = 4
ENDIF

IF is_prr
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "PRICE4"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("PRICE_R","STOCK")
  _stru(n,4) = 2
ENDIF

IF is_po1d
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "PRICE5"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("PRICE","STOCK")
  _stru(n,4) = 4
ENDIF

IF is_po1r
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "PRICE6"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("PRICE_R","STOCK")
  _stru(n,4) = 2
ENDIF

IF is_qntopt
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "QNT_O"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("QNT_OPT","STOCK")
  _stru(n,4) = 0
ENDIF

IF is_qs
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "QNT1"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("QNT","STOCK")
  _stru(n,4) = 0
ENDIF

IF is_qf
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "QNT2"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("QNT_REAL","STOCK")
  _stru(n,4) = 0
ENDIF
IF is_qb
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "QNT3"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("QNT_SAVE","STOCK")
  _stru(n,4) = 0
ENDIF

IF is_credit
  n = n+1
  DIMENSION _stru(n,4)
  _stru(n,1) = "QNT_CREDIT"
  _stru(n,2) = "N"
  _stru(n,3) = FSIZE("QNT_CREDIT","STOCK")
  _stru(n,4) = 0
ENDIF

n = ALLTRIM(f_name)
IF .NOT. "." $ n
  n = n+".DBF"
ENDIF

CREATE DBF (n) FROM ARRAY _stru
USE (n) ALIAS OUTPUT EXCLUSIVE

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                 Программа заполнения DBF файла.                       ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Fill_Res
PRIVATE arr, n_arr, n, cnt, w

n_arr = FCOUNT("OUTPUT")
DIMENSION arr(n_arr)

SELECT STOCK
GO TOP
cnt = 0

SCAN
  cnt = cnt+1
  @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)),WCOLS())
  IF sw_div = 2
    IF DIV_CODE # what_div
      LOOP
    ENDIF
  ENDIF
  
  IF sw_new = 2 .AND. DATE_ON < what_dat
    LOOP
  ENDIF

  IF .NOT. sw_empt .AND. QNT = 0
    LOOP
  ENDIF
  
  n = 1
  IF is_code
    arr(n) = CODE
    n = n+1
  ENDIF

  IF is_prf
    arr(n) = PREFIX
    n = n+1
  ENDIF

  IF is_name
    arr(n) = NAME
    n = n+1
  ENDIF

  IF is_firm
    arr(n) = IIF(FOUND("DEVELOP"), DEVELOP.B_NAME, "")
    n = n+1
  ENDIF

  IF is_class
    arr(n) = CLASSES.NAME
    n = n+1
  ENDIF

  IF is_div
    arr(n) = IIF(FOUND("DIVISION"), DIVISION.B_NAME, "")
    n = n+1
  ENDIF

  IF is_corpus
    arr(n) = IIF(FOUND("CORPUS"), CORPUS.NAME, "")
    n = n+1
  ENDIF

  IF is_addr
    arr(n) = ADDRESS
    n = n+1
  ENDIF

  IF is_round
    arr(n) = QNT_PACK
    n = n+1
  ENDIF

  IF is_pack
    arr(n) = QNT_ROUND
    n = n+1
  ENDIF

  IF is_date
    arr(n) = DATE_ON
    n = n+1
  ENDIF

  IF is_pod
    arr(n) = IIF(sale_mode = "Mitracon", PRICE_O, PRICE)
    n = n+1
  ENDIF

  IF is_por
    arr(n) = IIF(sale_mode = "Mitracon", PRICE_O_R, PRICE_R)
    n = n+1
  ENDIF

  w = RetPrice(NAME, PRICE, EMPTY(STOP_PUR), IIF(EMPTY(CLASS), DIV_CODE, CLASS))
  IF is_prd
    arr(n) = IIF(sale_mode = "Mitracon", PRICE, w)
    n = n+1
  ENDIF

  IF is_prr
    arr(n) = IIF(sale_mode = "Mitracon", PRICE_R, Clc_Rub(w))
    n = n+1
  ENDIF

  IF is_po1d
    arr(n) = IIF(sale_mode = "Mitracon", PRICE_O1, ROUND((w+PRICE)/2,4))
    n = n+1
  ENDIF

  IF is_po1r
    arr(n) = IIF(sale_mode = "Mitracon", PRICE_O1_R, Clc_Rub(ROUND((w+PRICE)/2,4)))
    n = n+1
  ENDIF

  IF is_qntopt
    arr(n) = QNT_OPT
    n = n+1
  ENDIF

  IF is_qs
    arr(n) = QNT
    n = n+1
  ENDIF

  IF is_qf
    arr(n) = QNT_REAL
    n = n+1
  ENDIF

  IF is_qb
    arr(n) = QNT_SAVE+QNT_REZ
    n = n+1
  ENDIF

  IF is_credit
    arr(n) = QNT_CREDIT
    n = n+1
  ENDIF

  SELECT OUTPUT
  APPEND FROM ARRAY arr
  SELECT STOCK
ENDSCAN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура F_Flags      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Установка флагов полей.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 16.04.2000 ──┘
PROCEDURE F_Flags
PRIVATE tmpList, tmpPoint

tmpPoint = 0

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_code, "√", " ")+"Код (CODE)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_prf, "√", " ")+"Префикс (PREFIX)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_name, "√", " ")+"Наименование (NAME)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_firm, "√", " ")+"Фирма (FIRM)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_div, "√", " ")+"Раздел (DIV)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_class, "√", " ")+"Класс (CLASS)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_round, "√", " ")+"Норма отпуска (QNT_ROUND)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_pack, "√", " ")+"Упаковка (QNT_PACK)"

IF File_O(path_comm+"SUBJECT.DBF") .AND.   ;
   File_O(path_comm+"CORPUS.DBF"))
  tmpPoint = tmpPoint+1
  DIMENSION tmpList(tmpPoint)
  tmpList(tmpPoint) = IIF(is_corpus, "√", " ")+"Корпус (CORPUS)"
ENDIF

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_addr, "√", " ")+"Адрес (ADDRESS)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_date, "√", " ")+"Дата создания (DATE_ON)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_pod, "√", " ")+"Цена опт. USD (PRICE1)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_por, "√", " ")+"Цена опт. руб. (PRICE2)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_prd, "√", " ")+"Цена розн. USD (PRICE3)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_prr, "√", " ")+"Цена розн. руб.(PRICE4)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_po1d, "√", " ")+"Цена мелкоопт. USD (PRICE5)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_po1r, "√", " ")+"Цена мелкоопт. руб.(PRICE6)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_qntopt, "√", " ")+"Количество мелкоопт. (QNT_O)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_qs, "√", " ")+"Нал. на складе (QNT1)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_qf, "√", " ")+"Нал. в филиалах (QNT2)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_qb, "√", " ")+"Бронь (QNT3)"

tmpPoint = tmpPoint+1
DIMENSION tmpList(tmpPoint)
tmpList(tmpPoint) = IIF(is_credit, "√", " ")+"Тов. кредит (QNT_CREDIT)"

FOR tmpPoint = 1 TO ALEN(tmpList)
  tmpList(tmpPoint) = PADR(tmpList(tmpPoint), 30)
ENDFOR

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

PRIVATE ex, to_mark
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH MIN(ALEN(tmpList)+4, 20), 34, "Состав полей"
@ WROWS()-1, 1 SAY PADC("Ins - Отметка,  Esc - Выход", WCOLS()-2)
*------------------------------------------------------------------------
*      Ввод полей бланка
*
tmpPoint = 1
ON KEY LABEL Ins DO Set_Mark
DO WHILE .T.
  to_mark = .F.
  tmpPoint = MAX(tmpPoint,1)
  tmpPoint = MIN(tmpPoint,ALEN(tmpList))
  @ 1, 1 MENU tmpList, ALEN(tmpList), WROWS()-4
  READ MENU TO tmpPoint
  IF READKEY() = 12
    EXIT
  ENDIF
  IF to_mark
    IF EMPTY(LEFT(tmpList(tmpPoint),1))
      tmpList(tmpPoint) = "√"+SUBSTR(tmpList(tmpPoint),2)
    ELSE
      tmpList(tmpPoint) = " "+SUBSTR(tmpList(tmpPoint),2)
    ENDIF
    tmpPoint = tmpPoint+1
  ENDIF
ENDDO

tmpPoint = 0
tmpPoint = tmpPoint+1
is_code = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_prf = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_name = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_firm = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_div = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_class = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_round = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_pack = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

IF File_O(path_comm+"SUBJECT.DBF") .AND.   ;
   File_O(path_comm+"CORPUS.DBF"))
  tmpPoint = tmpPoint+1
  is_corpus = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))
ENDIF

tmpPoint = tmpPoint+1
is_addr = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_date = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_pod = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_por = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_prd = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_prr = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_po1d = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_po1r = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_qntopt = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_qs = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_qf = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_qb = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))

tmpPoint = tmpPoint+1
is_credit = .NOT. EMPTY(LEFT(tmpList(tmpPoint),1))
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Mark     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Выход для установки отметки.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 16.04.2000 ──┘
PROCEDURE Set_Mark

to_mark = .T.
KEYBOARD "{Enter}"
RETURN
