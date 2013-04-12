*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл New_Name     Разработчик Андрей Васин           20.07.97 14:49:25 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║               Просмотр журнала переименований в филиале.               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE New_Name
*
*  Переменные состояния для навигации
*

IF .NOT. File_O(log_path+"NEW_NAME.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Журнал переименований пуст!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
USE (log_path+"NEW_NAME")
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Журнал переименований пуст!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF
GO BOTTOM
DO Use_Dummy

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

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск по контексту" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD S_Seek OF (menu_name) PROMPT "Прод. поиска" KEY Ctrl-A, "F8"
ON SELECTION PAD S_Seek OF (menu_name) DO Sw_Mode WITH "S_Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

SELECT NEW_NAME

*
*   Расчет размеров окна
*
PRIVATE ln, wd, prf_w
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 72, 70)

DO D_Wins WITH ln, wd, "Перечень измененных маркировок", 0, 0
what_do = "List"
prf_w = ""

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek"
    ON KEY LABEL F8 DO Sw_Mode WITH "S_Seek"

*
*    BROWSE - меню
*
    BROWSE FIELDS DIS_DAT:H="",     ;
                  OLD = PADR(ALLTRIM(OLD_PRF)+" "+ ;
                             ALLTRIM(OLD_NAME)+" "+ ;
                             ALLTRIM(OLD_PROD),30), ;
                  NEW = PADR(ALLTRIM(NEW_PRF)+" "+ ;
                             ALLTRIM(NEW_NAME)+" "+ ;
                             ALLTRIM(NEW_PROD),30), ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Seek"    && Контекстный поиск

    DO Seek_C
    what_do = "List"

  CASE what_do = "S_Seek"  && Продолжение  поиска

    DO S_Seek
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Программа контекстного поиска.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 20.07.97 21:14:05 ═╝
PROCEDURE Seek_C
PRIVATE rc_n, mss

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

*        Шаблон для поиска
*┌─────────────────────────────────┐
*│                                 │
*│    ....:....!....:....!....:    │
*│                                 │
*│< OK Ctrl-W > < Отказаться Esc > │
*└─────────────────────────────────┘

PRIVATE ex
                     &&   Объявляем и заполняем поля бланка
ex = 1
prf_w = PADR(prf_w,25)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 37, "Шаблон для поиска"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  6 GET prf_w
@ 5,  2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

prf_w = ALLTRIM(prf_w)
IF ex = 1 .AND. .NOT. EMPTY(prf_w)
  rc_n = RECNO()
  LOCATE FOR prf_w $ OLD_PRF .OR. ;
             prf_w $ OLD_NAME .OR. ;
             prf_w $ OLD_PROD .OR. ;
             prf_w $ NEW_PRF .OR. ;
             prf_w $ NEW_NAME .OR. ;
             prf_w $ NEW_PROD
  IF .NOT. FOUND()
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Ниченго не найдено..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    GO rc_n
  ENDIF
ENDIF
*--------------------------------------------------------------------------

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║               Программа продолжения контекстного поиска.               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 20.07.97 21:29:34 ═╝
PROCEDURE S_Seek
PRIVATE rc_n, mss
IF EMPTY(prf_w)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не задали шаблон для поиска..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
rc_n = RECNO()
CONTINUE
IF .NOT. FOUND()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Больше ниченго не найдено..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  GO rc_n
ENDIF

RETURN
