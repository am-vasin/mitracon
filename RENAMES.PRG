*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Renames      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Журнал коррекции номенклатуры.                     ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 11/03/1998 ══╝
PROCEDURE Renames
PARAMETER context

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
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P8B03 AGAIN

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS A8B03 AGAIN

SELECT 0
USE (base_path+"DEVELOP") ORDER TAG CODE ALIAS D18B03 AGAIN

SELECT 0
USE (base_path+"DEVELOP") ORDER TAG CODE ALIAS D28B03 AGAIN

SELECT 0
USE (base_path+"STO_LOG") ALIAS S8B03 AGAIN
SET RELATION TO OLD_PRO  INTO D18B03
SET RELATION TO NEW_PRO  INTO D28B03 ADDITIVE
SET RELATION TO WHO_CORR INTO P8B03  ADDITIVE
SET RELATION TO WHERE_C  INTO A8B03  ADDITIVE
GO BOTTOM

*
*   Параметры для поиска
*
PRIVATE s_pattern, s_dir, s_all
PRIVATE choose, ret_v
ret_v = 0
IF TYPE("context") = "C"
  choose = .T.
  s_pattern = context
ELSE
  s_pattern = ""
  choose = .F.
ENDIF
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF choose
  DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"
ENDIF

DEFINE PAD Seek_1 OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek_1 OF (menu_name) DO Sw_Mode WITH "Seek_1"

DEFINE PAD Seek_2 OF (menu_name) PROMPT "Продолжить поиск" KEY Ctrl-A, "Ctrl-F7"
ON SELECTION PAD Seek_2 OF (menu_name) DO Sw_Mode WITH "Seek_2"

DEFINE PAD Cnt_M OF (menu_name) PROMPT "Подсчет переименований" KEY Ctrl-A, "F1"
ON SELECTION PAD Cnt_M OF (menu_name) DO Sw_Mode WITH "Cnt_M"

IF choose
  DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ELSE
  DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ENDIF
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

s_dir  = 1
s_all  = 1

*
*   Расчет размеров окна
*
PRIVATE ln, wd, cnt_sav
ln = WROWS("")-9   && Количество видимых строк BROWSE
snt_sav = SET("CENTURY") = "OFF"
SET CENTURY ON
wd = IIF(SET("CENTURY") = "ON", 72, 70)
*
*┌─Дата────────Старое наименование────────────Новое наименование──────────┐
*│ДД.ММ.ГГГГ ▒▒▒▒ ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ ▒▒▒▒ ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒│
*└────────────────────────────────────────────────────────────────────────┘
*┌─Кто корректировал───────────Рабочее место────Фирма────Фирма────Время───┐
*│▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ ▒▒▒▒▒▒▒▒ ▒▒▒▒▒▒▒▒ ▒▒▒▒▒▒▒▒│
*└────────────────────────────────────────────────────────────────────────┘

DO D_Wins WITH ln, wd, "Журнал коррекции номенклатуры", 0, 1
@ 1, 2 SAY "─Дата────────Старое наименование────────────Новое наименование"
@ WROWS()-3, 2 SAY "─Кто корректировал───────────Рабочее место────Фирма────Фирма────Время"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    IF choose
      ON KEY LABEL Enter KEYBOARD CHR(23)
    ENDIF
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek_1"
    ON KEY LABEL Ctrl+F7 DO Sw_Mode WITH "Seek_2"
    ON KEY LABEL F1 DO Sw_Mode WITH "Cnt_M"
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM

*
*    BROWSE - меню
*
    BROWSE FIELDS DAT_CORR:P="@D":H="", ;
                  OLD_PRF:4:H="",  ;
                  OLD_NAM:25:H="", ;
                  NEW_PRF:4:H="",  ;
                  NEW_NAM:25:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      IF choose
        what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
      ELSE
        what_do = "Exit"
      ENDIF
    ENDIF

  CASE what_do = "Choose"    && Выбор

    ret_v = CODE
    EXIT

  CASE what_do = "Seek_1"    && Поиск

    DO Seek_1
    what_do = "List"

  CASE what_do = "Seek_2"    && Продолжение поиска

    DO Seek_2 WITH s_pattern, s_dir, 2
    what_do = "List"

  CASE what_do = "Cnt_M"     && Подчет переименований

    DO Cnt_Move
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
SELECT P8B03
USE
SELECT A8B03
USE
SELECT D18B03
USE
SELECT D28B03
USE
SELECT S8B03
USE
SELECT (s_sav)
IF snt_sav
  SET CENTURY OFF
ENDIF

RETURN ret_v

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
PRIVATE u_name, arm_name
u_name = ""
arm_name = A8B03.ARM_N
IF S8B03.WHERE_C = "***"
  arm_name = "Поставщик"
ELSE
  IF WHO_CORR = 0
    u_name = "САМ корректировал!"
  ELSE
    u_name = ALLTRIM(P8B03.FAMILY)+" "+LEFT(P8B03.NAME,1)+" "+LEFT(P8B03.S_NAME,1)
  ENDIF
ENDIF
u_name = PADR(u_name,28)
arm_name = PADR(arm_name, FSIZE("ARM_N", "A8B03"))

@ WROWS()-2, 2 SAY u_name COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY arm_name COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY LEFT(D18B03.B_NAME,8) COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY LEFT(D28B03.B_NAME,8) COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY TIM_CORR COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Заполнение таблицы поиска.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.11.1998 ──┘
PROCEDURE Seek_1

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

*┌─────────────────────────────────────────────────┐
*│ ┌─ Шаблон для поиска ─────────────────────────┐ │
*│ │                                             │ │
*│ └─────────────────────────────────────────────┘ │
*│ ┌─ Параметры поиска ──────────────────────────┐ │
*│ │  ( ) Во всем списке  ( ) С текущей позиции  │ │
*│ │ ( ) От конца к началу ( ) От начала к концу │ │
*│ └─────────────────────────────────────────────┘ │
*│        < OK Ctrl-W > < Отказаться Esc >         │
*└─────────────────────────────────────────────────┘

PRIVATE w_pattern, w_dir, w_all
PRIVATE ex
w_pattern = s_pattern
w_dir = s_dir
w_all = s_all
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 11, 53, "Контекстный поиск"
DO Sun_Bord WITH  2,  3,  4, 49, " Шаблон для поиска "
DO Sun_Bord WITH  5,  3,  8, 49, " Параметры поиска "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 5 EDIT w_pattern SIZE 1, 43 PICTURE "@K" COLOR (SCHEME(1,1))
@ 6, 6 GET w_all PICTURE "@*RH Во всем списке ;С текущей позиции"
@ 7, 5 GET w_dir PICTURE "@*RH От конца к началу;От начала к концу"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  s_pattern = w_pattern
  s_dir = w_dir
  s_all = w_all
  DO Seek_2 WITH s_pattern, s_dir, s_all
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)PRETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_2       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Собственно поиск.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.11.1998 ──┘
PROCEDURE Seek_2
PARAMETERS patt, dir, all

PRIVATE r_sav, dir_w
r_sav = RECNO()
dir_w = IIF(dir = 1, -1, 1)

DO Wt_Mess WITH "Поиск..."
IF all = 1
  IF dir_w = 1
    GO TOP
  ELSE
    GO BOTTOM
  ENDIF
ELSE
  SKIP dir_w
ENDIF

DO WHILE .NOT. BOF() .AND. .NOT. EOF()
  IF patt $ OLD_PRF+" "+OLD_NAM .OR. ;
     patt $ NEW_PRF+" "+NEW_NAM
    EXIT
  ENDIF
  SKIP dir_w
ENDDO

DO Wt_Mess

IF EOF() .OR. BOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Шаблон не найден..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

RETURN
*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Cnt_Move     Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Количество переименований позиций.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 19.04.2002 ──┘
PROCEDURE Cnt_Move
PRIVATE s_sav, d01, d02, mss, cnt_s

s_sav = SELECT()
d01 = DATE() - 30
d02 = DATE()

DO G_Period WITH d01, d02
SELECT S8B03
COUNT TO cnt_s FOR DAT_CORR >= d01 .AND. DAT_CORR <= d02

IF cnt_s = 0
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"За период с "+DTOC(d01)+" по "+DTOC(d02)+" переименования позиций"
  mss(3) = CHR(0)+" не проводились."
  mss(4) = ""
  DO Out_Mess WITH 5, "mss"
ELSE
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"За период с "+DTOC(d01)+" по "+DTOC(d02)+" было проведено "+ALLTRIM(STR(cnt_s))
  mss(3) = CHR(0)+" переименований позиций."
  mss(4) = ""
  DO Out_Mess WITH 5, "mss"
ENDIF  

SELECT(s_sav)
RETURN