*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Get_Sup      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                            Выбор поставщика                            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 04.06.2000 ══╝
PROCEDURE Get_Sup
PARAMETERS s_code, b_nm, l_nm
PRIVATE seek_shab
seek_shab = SPACE(25)
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
DO Use_Link
SELECT 0
IF TYPE("s_code") = "N"
  USE (base_path+"CUSTOMER") ORDER TAG CODE AGAIN ALIAS SUP_0604
  SET FILTER TO .NOT. EMPTY(SUP)
  IF .NOT. SEEK(s_code)
    SET ORDER TO TAG NAME
    GO TOP
  ELSE
    SET ORDER TO TAG NAME
  ENDIF
ELSE
  USE (base_path+"CUSTOMER") ORDER TAG CODE AGAIN ALIAS SUP_0604
  SET FILTER TO .NOT. EMPTY(SUP)
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 16

DO D_Wins WITH ln, wd, "Поставщики", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"

*
*    BROWSE - меню
*
    BROWSE FIELDS SUP_0604.B_NAME:16,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

    IF ALIAS() = "SUP_0604"
      IF TYPE("s_code") = "N"
        s_code = CODE
      ENDIF
      IF TYPE("b_nm") = "C"
        b_nm = B_NAME
      ENDIF
      IF TYPE("l_nm") = "C"
        l_nm = L_NAME
      ENDIF
      EXIT
    ENDIF
    SET RELATION TO
    SELECT SUP_0604
    SET ORDER TO TAG NAME
    what_do = "List"

  CASE what_do = "Seek"    && Контекстный поиск
    DO Seek_C
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
SELECT SUP_0604
USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_C       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Контекстный поиск.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.02.2000 ──┘
PROCEDURE Seek_C

IF .NOT. Get_Shab()
  RETURN
ENDIF
IF EMPTY(seek_shab)
  RETURN
ENDIF

PRIVATE s_sav, tmpShab
tmpShab = SYS(15, lwr, ALLTRIM(seek_shab))
s_sav = RECNO("SUP_0604")
SELECT LINK
SET RELATION TO
ZAP
SELECT SUP_0604
DO Wt_Mess WITH "Поиск..."
SCAN
  IF tmpShab $ SYS(15, lwr, B_NAME)
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("SUP_0604")
    SELECT SUP_0604
  ENDIF
ENDSCAN
DO Wt_Mess
IF RECCOUNT("LINK") = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не было найдено!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT SUP_0604
  SET ORDER TO TAG NAME
  IF BETWEEN(s_sav, 1, RECCOUNT())
    GO s_sav
  ENDIF
ELSE
  SELECT SUP_0604
  SET ORDER TO
  SELECT LINK
  SET RELATION TO LNK INTO SUP_0604
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Shab     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Шаблон для поиска.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.02.2000 ──┘
PROCEDURE Get_Shab
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

*┌─────────────────────────────────┐
*│  ┌─ Шаблон для поиска ───────┐  │
*│  │ ....:....!....:....!....: │  │
*│  └───────────────────────────┘  │
*│< OK Ctrl-W > < Отказаться Esc > │
*└─────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 37, "Контекстный поиск"
DO Sun_Bord WITH  2,  4,  4, 32, " Шаблон для поиска "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 6 GET seek_shab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
