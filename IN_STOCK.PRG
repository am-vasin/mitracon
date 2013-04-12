*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл In_Stock     Разработчик Андрей Васин           15.06.99          ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║             Внесение позиций поставщика в свою номенклатуру.           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE In_Stock

PRIVATE s_code, s_name, i0, i1
s_code = 0
s_name = ""
IF .NOT. Prp_Tmp()
  RETURN
ENDIF

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "Отметки"
ON PAD Mark OF (menu_name) ACTIVATE POPUP Mark

  DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Mark PROMPT "Уст./Снять" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Mark DO Sw_Mode WITH "Mark"

  DEFINE BAR  2 OF Mark PROMPT "Инвертировать все" KEY Ctrl-A, "*"
  ON SELECTION BAR 2 OF Mark DO Sw_Mode WITH "Not"

  DEFINE BAR  3 OF Mark PROMPT "Очистить все" KEY Ctrl-A, "-"
  ON SELECTION BAR 3 OF Mark DO Sw_Mode WITH "Clear"

DEFINE PAD Move OF (menu_name) PROMPT "Только отм." KEY Ctrl-A, "F2"
ON SELECTION PAD Move OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Seek_C OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek_C OF (menu_name) Do Sw_Mode WITH "Seek_C"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
SELECT 0
DO Use_Dummy
SELECT 0
DO Use_Link
SELECT IN_STOCK

*
*   Расчет размеров окна
*
PRIVATE ln, wd, interr1, interr2
DIMENSION interr1(5), interr2(5)
interr1(1) = 9
interr2(1) = "{Tab}"
interr1(2) = 42
interr2(2) = "*"
interr1(3) = 45
interr2(3) = "-"
interr1(4) = -1
interr2(4) = "{F2}"
interr1(5) = -6
interr2(5) = "{F7}"
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = 71

*
*┌Отм─────Наименование──────────────────────────────────Дата созд.──Нал.─┐
*│MMM PPPP NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN PPPPPPPP DDDDDDDDDD 999999│
*└─────────────────────────────────────────────────────────────┘
*┌─ Цена поставщика ───────────────────────────────────────────┐
*│ 99999.99руб   99999.9999$                                   │
*└─────────────────────────────────────────────────────────────┘

DO D_Wins WITH ln, wd, ALLTRIM(s_name), 1, 1
@ 1, 9 SAY "Поиск"
@ 1, 15 SAY PADR("Enter",25) COLOR (SCHEME(14,2))

@ 2, 2 SAY "Отм─────Наименование──────────────────────────────────Дата созд.──Нал."

@ WROWS()-3,  3 SAY " Цена поставщика "
@ WROWS()-2, 11 SAY "руб           $"
what_do = "List"

DO WHILE .T.
 
  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Tab      DO Mrk
    ON KEY LABEL "*"      DO Inv_Mrk
    ON KEY LABEL "-"      DO Clr_Mrk
    ON KEY LABEL F2       DO Sw_Mode WITH "Filter"
    ON KEY LABEL F7       DO Sw_Mode WITH "Seek_C"
    ON KEY LABEL Enter    DO Seek_P

*
*    BROWSE - меню
*
    BROWSE FIELDS MRK:H="", ;
                  PREFIX:H="", ;
                  NAME:H="", ;
                  PRODUCER:H="", ;
                  DATE_ON:H="", ;
                  F000 = IIF(SUP_QNT < 0, " Есть ", TRANSFORM(SUP_QNT, "@Z 999999")):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_F() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Mark"     && Установить/Снять отметку

    DO Mrk
    what_do = "List"

  CASE what_do = "Seek_C"   && Контекстный поиск

    DO Seek_C
    what_do = "List"

  CASE what_do = "Clear"    && Очистить все отметки

    DO Clr_Mrk
    what_do = "List"

  CASE what_do = "Not"      && Ивертировать все отметки

    DO Inv_Mrk
    what_do = "List"

  CASE what_do = "Filter"   && Список/Номенклатура

    DO Filter
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

SET ORDER TO TAG NAME1
GO TOP
IF EOF() .OR. BOF()
  s_code = 0
ELSE
  s_code = 1
ENDIF

RELEASE MENU (menu_name) EXTENDED

CLEAR WINDOWS
CLOSE DATABASES

IF s_code # 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  Добавим отмеченные позиции в номенклатуру? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 1
    USE (base_path+"STOCK")
    SELECT 0
    USE (tmpo_path+"IN_STOCK") ORDER TAG NAME1
    SCAN
      i0 = 1
      DO WHILE i0 <= 25
        IF ISDIGIT(SUBSTR(NAME, i0, 1))
          EXIT
        ENDIF
        i0 = i0+1
      ENDDO
      IF i0 <= 25
        i1 = 25
        DO WHILE i1 >= i0
          IF ISDIGIT(SUBSTR(NAME, i1, 1))
            EXIT
          ENDIF
          i1 = i1-1
        ENDDO
      ENDIF
      IF i0 <= 25
        i1 = SUBSTR(NAME, i0, i1-i0+1)
      ELSE
        i1 = ""
      ENDIF
      SELECT STOCK
      APPEND BLANK
      REPLACE CODE      WITH IN_STOCK.CODE,     ;
              PREFIX    WITH IN_STOCK.PREFIX,   ;
              NAME      WITH IN_STOCK.NAME,     ;
              PRODUCER  WITH IN_STOCK.PRODUCER, ;
              PREFIX1   WITH SeekName(IN_STOCK.PREFIX),   ;
              NAME1     WITH SeekName(IN_STOCK.NAME),     ;
              PRODUCER1 WITH SeekName(IN_STOCK.PRODUCER), ;
              PRO_CODE  WITH IN_STOCK.PRO_CODE, ;
              NUM_PART  WITH i1, ;
              DATE_ON   WITH DATE(), ;
              WHO_ADD   WITH user, ;
              WHERE_ADD WITH arm
      SELECT IN_STOCK
    ENDSCAN
  ENDIF
ENDIF
CLOSE DATABASES
DELETE FILE (tmpo_path+"IN_STOCK.DBF")
DELETE FILE (tmpo_path+"IN_STOCK.CDX")

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Поиск по начальным символам.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.06.97 10:43:18 ═╝
PROCEDURE Seek_P
PRIVATE prf, s, ln, rc_sav, i

ACTIVATE WINDOW (win_name+"_M") SAME

prf = ""
ln = 25
DO WHILE .T.
  ACTIVATE WINDOW (win_name+"_M") SAME
  @ 1, 15 SAY REPLICATE("▒",25) COLOR SCHEME 1
  @ 1, 15 SAY prf COLOR SCHEME 1
  s = INKEY(0)
  *
  *   Исследуем на предмет наличия обработчика
  *
  i = ASCAN(interr1, s)
  IF i # 0
    KEYBOARD interr2(i)
    EXIT
  ENDIF
  IF s < 0       && Функциональная клавиша
    IF s = -9
      KEYBOARD "{F10}"
      EXIT
    ENDIF
    LOOP
  ENDIF
  IF BETWEEN(s,  32,126) .OR.   ;
     BETWEEN(s, 128,175) .OR.   ;
     BETWEEN(s, 224,241)     && Символ
    IF LEN(prf) >= 25        && Все! Больше некуда!
      LOOP
    ENDIF
    rc_sav = RECNO()
    IF SEEK(prf+CHR(s))
      prf = prf+CHR(s)
      SHOW WINDOW (ALIAS()) REFRESH
    ELSE
      ?? CHR(7)
      GO rc_sav
    ENDIF
  ENDIF
  IF s = 127       && Back Space
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      = SEEK(prf)
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
  ENDIF
  IF s = 13        && Enter
    EXIT
  ENDIF
  IF s = 24        && Down Arrow
    KEYBOARD "{DnArrow}"
    EXIT
  ENDIF
  IF s = 5         && Up Arrow
    KEYBOARD "{UpArrow}"
    EXIT
  ENDIF
  IF s = 3         && Page Down
    KEYBOARD "{PgDn}"
    EXIT
  ENDIF
  IF s = 18        && Page Up
    KEYBOARD "{PgUp}"
    EXIT
  ENDIF
  IF s = 27        && Escape
    KEYBOARD "{Esc}"
    EXIT
  ENDIF
ENDDO

@ 1, 15 SAY PADR("Enter",25) COLOR (SCHEME(14,2))
  
ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_Tmp      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│            Выбор поставщика и подготовка временного файла.             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.06.1999 ──┘
PROCEDURE Prp_Tmp
PRIVATE c_names, c_codes, n_c

n_c = 0
USE (base_path+"CUSTOMER") ORDER TAG NAME
SCAN FOR .NOT. EMPTY(SUP)
  n_c = n_c+1
  DIMENSION c_names(n_c), c_codes(n_c)
  c_names(n_c) = B_NAME
  c_codes(n_c) = CODE
ENDSCAN
USE
IF n_c = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не создали ни одного поставщика!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

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


PRIVATE ln, point
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
ln = MIN(WROWS("")-5, n_c+3)
DO D_Win_N WITH ln, LEN(c_names(1))+4, "Поставщики"
point = 1
*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  point = MIN(point, n_c)
  point = MAX(point, 1)
  @ 1, 1 MENU c_names, n_c, WROWS()-3
  READ MENU TO point
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    s_code = c_codes(point)
    s_name = c_names(point)
    EXIT
  ENDIF
ENDDO
POP KEY
RELEASE WINDOW (win_name)

IF EMPTY(s_name)
  RETURN .F.
ENDIF
DO Wt_Mess WITH "Подготовка данных"
DELETE FILE (tmpo_path+"IN_STOCK.DBF")
DELETE FILE (tmpo_path+"IN_STOCK.CDX")
CREATE DBF (tmpo_path+"IN_STOCK") ;
     ( MRK      C( 3), ;
       CODE     N( 7), ;
       PREFIX   C( 4), ;
       NAME     C(35), ;
       PRODUCER C( 8), ;
       PRO_CODE N( 5), ;
       PRICE_R  N(8,2), ;
       PRICE_D  N(8,4), ;
       SUP_QNT  N( 6), ;
       DATE_ON  D  )

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"OFFER") ORDER TAG SUP_CODE

SEEK s_code
SCAN REST WHILE s_code = SUP_CODE
  IF EMPTY(CODE)
    LOOP
  ENDIF
  IF SEEK(CODE,"STOCK")
    LOOP
  ENDIF
  SELECT IN_STOCK
  APPEND BLANK
  REPLACE CODE     WITH OFFER.CODE,    ;
          PREFIX   WITH OFFER.PREFIX,  ;
          NAME     WITH OFFER.NAME,    ;
          PRODUCER WITH OFFER.PRODUCER,;
          PRO_CODE WITH OFFER.PRO_CODE,;
          PRICE_R  WITH OFFER.PRICE_R, ;
          PRICE_D  WITH OFFER.PRICE_D, ;
          SUP_QNT  WITH OFFER.SUP_QNT, ;
          DATE_ON  WITH OFFER.DATE_ON
  SELECT OFFER
ENDSCAN
DO Wt_Mess

USE
SELECT STOCK
USE
SELECT IN_STOCK
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"В предложении поставщика нет ни одной новой позиции!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN .F.
ENDIF

INDEX ON NAME+PREFIX+PRODUCER TAG NAME1 FOR .NOT. EMPTY(MRK)
INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE
GO TOP

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Вывод информационной строки.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 14.06.97 14:49:18 ═╝
PROCEDURE Show_F

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-2,  3 SAY PRICE_R PICTURE "@Z 99999.99" COLOR SCHEME 1
@ WROWS()-2, 17 SAY PRICE_D PICTURE "@Z 99999.9999" COLOR SCHEME 1

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Mrk          Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Установить/Снять отметку.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.06.1999 ──┘
PROCEDURE Mrk

REPLACE IN_STOCK.MRK WITH IIF(EMPTY(IN_STOCK.MRK), " √ ", "")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clr_Mrk      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Очистить все отметки.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.06.1999 ──┘
PROCEDURE Clr_Mrk
PRIVATE r_sav, o_sav
o_sav = TAG()
r_sav = RECNO()
SCAN
  REPLACE IN_STOCK.MRK WITH ""
ENDSCAN

SET ORDER TO (o_sav)

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SHOW WINDOW (ALIAS()) REFRESH SAME

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Inv_Mrk      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Инвертировать все отметки.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.06.1999 ──┘
PROCEDURE Inv_Mrk
PRIVATE r_sav, o_sav
o_sav = TAG()
r_sav = RECNO()
SET ORDER TO
SCAN
  REPLACE IN_STOCK.MRK WITH IIF(EMPTY(IN_STOCK.MRK), " √ ", "")
ENDSCAN

SET ORDER TO (o_sav)

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SHOW WINDOW (ALIAS()) REFRESH SAME
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Filter       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Установка/Снятие фильтра.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.06.1999 ──┘
PROCEDURE Filter

PRIVATE o_sav
o_sav = TAG()

IF "1" $ o_sav
  SET ORDER TO TAG NAME
  DEFINE PAD Move OF (menu_name) PROMPT "Только отм." KEY Ctrl-A, "F2" BEFORE Seek_C
ELSE
  SET ORDER TO TAG NAME1
  DEFINE PAD Move OF (menu_name) PROMPT "Все позиции" KEY Ctrl-A, "F2" BEFORE Seek_C
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_C       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Контекстный поиск.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.06.1999 ──┘
PROCEDURE Seek_C
PRIVATE shb, r_sav, o_sav

shb = G_Shab()
IF EMPTY(shb)
  RETURN
ENDIF
r_sav = RECNO()
DO Wt_Mess WITH "Поиск..."
SELECT LINK
ZAP
SELECT IN_STOCK
SCAN
  IF shb $ NAME
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("IN_STOCK")
    SELECT IN_STOCK
  ENDIF
ENDSCAN

IF RECCOUNT("LINK") = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  IF BETWEEN(r_sav,1,RECCOUNT())
    GO r_sav
  ENDIF
  RETURN
ENDIF

DO V_0001

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Shab       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Ввод шаблона для поиска.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.06.1999 ──┘
PROCEDURE G_Shab
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

*┌──────────────────────────────────┐
*│    ....:....!....:....!....:     │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE s, ex
s = ""
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 5, 38, "Задайте шаблон для поиска"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 6 EDIT s SIZE 1, 25 COLOR SCHEME 1
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex # 1
  s = ""  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN s

