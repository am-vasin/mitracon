*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Rev_Inc      Разработчик Андрей Васин           06.05.97 14:58:44 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║              Программа приходования товара от покупателя.              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Rev_Inc

PRIVATE mss, d_names, d_codes, n, w, i000

*
*   Список разделов номенклатуры
*
USE (base_path+"DIVISION")
n = 0
SCAN
  n = n+1
  DIMENSION w(n,2)
  w(n,1) = B_NAME
  w(n,2) = CODE
ENDSCAN
= ASORT(w,1)

DIMENSION d_names(n), d_codes(n)
FOR i000 = 1 TO n
  d_codes(i000) = w(i000,2)
  d_names(i000) = w(i000,1)
ENDFOR
RELEASE w, i000

USE
DO Use_Dummy
DO Use_Link

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"INC_TIT")

SELECT 0
USE (base_path+"ACT_S")

SELECT 0
USE (base_path+"ACT_S_T")

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG DUE_CODE

* NEW 04.01.2000 - Я вставил след. 2 строки чтобы не было ошибки
* при покупке товара ("ошибка 13 Alias DUE_NAM not found
* в программе INC_LIST, строка 169") Вот эта строка:
* SET RELATION TO NAME+PREFIX+PRODUCER INTO DUE_NAM ADDITIVE

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG DUE_NAM ALIAS DUE_NAM AGAIN

SELECT 0
USE (base_path+"INC_LIST") ORDER TAG L_CODE
DELETE FILE (tmpo_path+"TMP_INC.DBF")
COPY STRUCTURE TO (tmpo_path+"TMP_INC")
SELECT 0
USE (tmpo_path+"TMP_INC") EXCLUSIVE

SELECT 0
USE (base_path+"INC_DUE") ORDER TAG L_CODE

SELECT 0
USE (base_path+"INC_NEW") ORDER TAG L_NAME
DELETE FILE (tmpo_path+"TMP_NEW.DBF")
COPY STRUCTURE TO (tmpo_path+"TMP_NEW")
SELECT 0
USE (tmpo_path+"TMP_NEW") EXCLUSIVE
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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Корр." KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "Доб." KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "Удал." KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) Do Sw_Mode WITH "Del"

DEFINE PAD Price OF (menu_name) PROMPT "Цены" KEY Ctrl-A, "F2"
ON SELECTION PAD Price OF (menu_name) Do Sw_Mode WITH "Price"

DEFINE PAD Act OF (menu_name) PROMPT "Прих." KEY Ctrl-A, "Tab"
ON SELECTION PAD Act OF (menu_name) Do Sw_Mode WITH "Act"

DEFINE PAD Exit OF (menu_name) PROMPT "Вых." KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-10   && Количество видимых строк BROWSE
wd = 33+IIF(SET("CENTURY")="ON", 10, 8)

DO D_Wins WITH ln, wd, "Заготовки приемных актов", 0, 0
what_do = "List"
@ 1,  3 SAY " Поставщик "
@ 1, 20 SAY " Накл. N "
@ 1, 31 SAY " Дата "
@ 1, IIF(SET("CENTURY")="ON", 41, 39) SAY "Цены"


DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    SELECT INC_TIT
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "Add"
    ON KEY LABEL Del   Do Sw_Mode WITH "Del"
    ON KEY LABEL F2    Do Sw_Mode WITH "Price"
    ON KEY LABEL Tab   Do Sw_Mode WITH "Act"

*
*    BROWSE - меню
*
    BROWSE FOR SUP_CODE # 0   ;
           FIELDS INC_TIT.SUP_NAME:H="":16,     ;
                  INC_TIT.INC_DOC:H="":10,     ;
                  INC_TIT.INC_DATE:H="",     ;
                  IS_PRICE:H="":4,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND REST NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && Коррекция приемного акта
    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO Inc_List WITH .T.
    ENDIF
    what_do = "List"

  CASE what_do = "Add"     && Добавление заготовки

    DO Rev_Tit
    what_do = "List"

  CASE what_do = "Del"     && Удаление заготовки

    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Действительно удалить? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      DO Del_Tit
    ENDIF

    what_do = "List"

  CASE what_do = "Price"   && Установка режима

    DO Prices
    what_do = "List"

  CASE what_do = "Act"     && Приход

    DO Prep_Act
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
*DELETE FILE (tmpo_path+"TMP_INC.DBF")
*DELETE FILE (tmpo_path+"TMP_NEW.DBF")

RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Установка режима.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.05.97 21:43:41 ═╝
PROCEDURE Prices
*
*  Переменные состояния для навигации
*

IF .NOT. Get_L()
  RETURN
ENDIF

REPLACE INC_TIT.IS_PRICE WITH IIF(EMPTY(INC_TIT.IS_PRICE), "Р", " "),  ;
        INC_TIT.LOCKED   WITH "***"

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Захватить список!                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 11:22:51 ═╝
PROCEDURE Get_L

PRIVATE mss, sel_sav, arm_name

IF INC_TIT.LOCKED # "***"
  sel_sav = SELECT()
  SELECT 0
  USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS ARM000 AGAIN
  IF SEEK(INC_TIT.LOCKED)
    arm_name = ALLTRIM(ARM_N)
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"ВНИМАНИЕ! Этот список в работе, АРМ "+'"'+arm_name+'"!'
    mss(3) = CHR(0)+"  Как поступим? [ Пусть работают ] [ Наплевать! ]"
    mss(4) = ""
    IF Out_Mess(7, "mss") = 1
      USE
      SELECT (sel_sav)
      RETURN .F.
    ENDIF
  ENDIF
  USE
  SELECT (sel_sav)
ENDIF

REPLACE INC_TIT.LOCKED WITH arm

RETURN .T.
