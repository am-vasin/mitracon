*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Skl_Type     Разработчик Андрей Васин           23.10.97 08:59:08 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                              Типы выплат.                              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Slr_Type

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

DEFINE PAD Corr OF (menu_name) PROMPT "Коррекция" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "Добавление" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "Add"

DEFINE PAD Move_T OF (menu_name) PROMPT "Переместить"
ON PAD Move_T OF (menu_name) ACTIVATE POPUP Move_T

  DEFINE POPUP Move_T MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR 1 OF Move_T PROMPT "Переместить вниз" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Move_T Do Sw_Mode WITH "move_dn"
  
  DEFINE BAR 2 OF Move_T PROMPT "Переместить вверх" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Move_T Do Sw_Mode WITH "move_up"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

USE (base_path+"SLR_TYPE") ORDER TAG RANG

DO Use_Dummy
SELECT SLR_TYPE

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 100   && Количество видимых строк BROWSE
wd = 24

DO D_Wins WITH ln, wd, "Типы выплат", 0, 0
what_do = IIF(BETWEEN(RECNO("SLR_TYPE"), 1, RECCOUNT("SLR_TYPE")),   ;
          "List", "Add")

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "Add"
    ON KEY LABEL F1    Do Sw_Mode WITH "move_dn"
    ON KEY LABEL F2    Do Sw_Mode WITH "move_up"

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:24:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"   && Коррекция

    IF CODE < 0
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Этот тип коррекции не подлежит!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DO Modi_T WITH "C"
    ENDIF
    what_do = "List"

  CASE what_do = "Add"    && Добавление

    DO Modi_T WITH "A"
    what_do = IIF(BETWEEN(RECNO("SLR_TYPE"), 1, RECCOUNT("SLR_TYPE")),  ;
               "List", "Exit")

  CASE what_do = "move_dn"   && Переместить вниз

    DO DownUp WITH 1
    what_do = "List"

  CASE what_do = "move_up"   && Переместить вверх

    DO DownUp WITH -1
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                  Модификация/добавление типа выплаты.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 23.10.97 09:52:46 ═╝
PROCEDURE Modi_T
PARAMETER a_or_c
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

*
*┌─────────────────────────────────────────────┐
*│        ┌─ Имя начисления ─────────┐         │
*│        │ ░░░░▒░░░░▓░░░░▒░░░░▓░░░░ │         │
*│        └──────────────────────────┘         │
*│┌─ Калькуляция ─────────────────────────────┐│
*││┌─ Тип ─────────────┐┌─ Параметры ────────┐││
*│││( ) Сумма          ││                    │││
*│││( ) Фиксиров. цена ││Единицы ░░░░▒░░░░▓░░│││
*│││( ) Переменная цена││                    │││
*│││( ) Оклад          ││  Коэфф. 9999.999   │││
*│││( ) Раб. день      ││  Цена 9999999.99   │││
*│││( ) Раб. час       ││                    │││
*││└───────────────────┘└────────────────────┘││
*│└───────────────────────────────────────────┘│
*│      < OK Ctrl-W > < Отказаться Esc >       │
*└─────────────────────────────────────────────┘


PRIVATE nm, s_type, unt, k, pr, ex
IF a_or_c = "A"
  nm     = SPACE(24)
  s_type = 1
  unt    = SPACE(12)
  k      = 1
  pr     = 0
ELSE
  nm     = SLR_TYPE.NAME
  s_type = VAL(ALLTRIM(SLR_TYPE.TYPE))
  unt    = SLR_TYPE.UNIT
  k      = SLR_TYPE.KOEFF
  pr     = SLR_TYPE.PRICE
ENDIF
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 17, 49, "Тип начисления"
DO Sun_Bord WITH  2, 10,  4, 37, " Имя начисления "
DO Sun_Bord WITH  5,  2, 14, 46, " Калькуляция "
DO Sun_Bord WITH  6,  3, 13, 23, " Тип "
DO Sun_Bord WITH  6, 24, 13, 45, " Параметры "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3, 12 GET nm WHEN Show_P()
@  7,  4 GET s_type PICTURE   ;
   "@*RV Сумма;Фиксиров. цена;Переменная цена;Оклад;Раб. день;Раб. час" ;
   VALID Show_P()
@  8, 33 GET unt WHEN INLIST(s_type, 2, 3, 4, 5, 6)
@ 10, 34 GET k PICTURE "@Z 9999.999"  WHEN INLIST(s_type, 4, 5, 6)
@ 10, 32 GET pr PICTURE "@Z 9999999.99"  WHEN INLIST(s_type, 2, 3)
@ 15,  8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Визуализация пол "Параметры".                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 23.10.97 14:59:08 ═╝
PROCEDURE Show_P

@ 7, 25 CLEAR TO 12, 44

IF INLIST(s_type, 2, 3, 4, 5, 6)
  @ 8, 25 SAY "Единицы"
  SHOW GET unt
ENDIF

IF INLIST(s_type, 4, 5, 6)
  @ 10, 27 SAY "Коэфф."
  SHOW GET k
ENDIF

IF INLIST(s_type, 2, 3)
  @ 10, 27 SAY "Цена"
  SHOW GET pr
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Выход из бланка.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 23.10.97 16:06:24 ═╝
PROCEDURE Tst_Blank
PRIVATE cd

IF ex = 2 .OR. MOD(READKEY(), 256) = 12
  RETURN .T.
ENDIF

IF EMPTY(nm)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Название типа начисления дожно быть!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF s_type = 1
  k = 0
  pr = 0
ENDIF


IF INLIST(s_type, 2, 3)
  IF pr <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Расценка должна быть больше нуля!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  k = 0
ENDIF

IF INLIST(s_type, 4, 5, 6)
  IF k <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Коэффициет должен быть больше нуля!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  pr = 0
ENDIF

IF a_or_c = "A"
  PRIVATE o_save, n_rang
  o_save = TAG()
  SET ORDER TO TAG CODE
  GO BOTTOM
  cd = CODE+1
  cd = MAX(cd, 1)
  SET ORDER TO RANG
  GO BOTTOM
  n_rang = 1
  IF .NOT. EOF() .AND. .NOT. BOF()
    n_rang = RANG+1
  ENDIF
  SET ORDER TO TAG (o_save)
  APPEND BLANK
ELSE
  n_rang = RANG
  cd = CODE
ENDIF

REPLACE CODE  WITH cd,   ;
        NAME  WITH nm,   ;
        TYPE  WITH STR(s_type,10), ;
        PRICE WITH pr,   ;
        KOEFF WITH k,    ;
        UNIT  WITH unt,  ;
        RANG  WITH n_rang

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Перемещение типа начисления.                    ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 20.03.2002 ═╝
PROCEDURE DownUp
PARAMETERS rng
PRIVATE r_sav

IF EOF() .OR. BOF()
  RETURN
ENDIF
r_sav = RECNO()

SKIP rng
IF BOF() .OR. EOF()
  GO r_sav
  RETURN
ENDIF

REPLACE RANG WITH RANG - rng
GO r_sav
REPLACE RANG WITH RANG + rng

RETURN