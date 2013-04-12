*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Zones        Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                          Описание зон склада.                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 05.01.2000 ══╝
PROCEDURE Zones


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
PRIVATE s_sav         && Номер рабочей области для сохранения и восстановления!

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"ZONES") ALIAS Z_0105 ORDER TAG ZONE AGAIN

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "Коррекция" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 27

DO D_Wins WITH ln, wd, "Зоны склада", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - меню
*
    BROWSE FIELDS CHAR:H="", NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F  ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && Корреция записи

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Modi_Rec
    ENDIF
    what_do = "List"

  CASE what_do = "Del"       && Удаление

    IF .NOT. BOF() .AND. .NOT. EOF()
      DELETE
    ENDIF
    what_do = "List"

  CASE what_do = "Add"       && Добавление

    DO Modi_Rec WITH .T.
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_Rec     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Коррекция/добавление записи.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.01.2000 ──┘
PROCEDURE Modi_Rec
PARAMETER new

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

*┌───────────────────────────────────┐
*│ ┌─Код──Название─────────────────┐ │
*│ │ !!  !!!!!!!!!!!!!!!!!!!!!!!!  │ │
*│ └───────────────────────────────┘ │
*│ < OK Ctrl-W > < Отказаться Esc >  │
*└───────────────────────────────────┘

PRIVATE ex, cd, nm
IF new
  cd = "  "
  nm = SPACE(24)
ELSE
  cd = CHAR
  nm = NAME
ENDIF

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 39, IIF(new, "Добавление", "Кокрреция")+" зоны"
DO Sun_Bord WITH  2,  3,  4, 35, "Код──Название"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 5 GET cd && PICTURE "A99"
@ 3, 9 GET nm

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Code()

IF ex = 1
  *
  * Отрабатываем бланк
  IF new
    SET DELETED OFF
    LOCATE FOR DELETED()
    IF FOUND()
      RECALL
    ELSE
      APPEND BLANK
    ENDIF
    SET DELETED ON
  ENDIF
  REPLACE CHAR WITH cd, NAME WITH nm
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Code     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Проверка уникальности кода.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.01.2000 ──┘
PROCEDURE Tst_Code

PRIVATE r_sav
IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(cd)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Код зоны не может быть пустым..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF cd = "**"
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Такой код зоны недопустим!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

r_sav = RECNO()
IF .NOT. new
  SET FILTER TO RECNO() # r_sav
ENDIF

IF SEEK(cd)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Зона с таким кодом уже есть..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SET FILTER TO
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
  RETURN .F.
ENDIF

SET FILTER TO
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
RETURN .T.
