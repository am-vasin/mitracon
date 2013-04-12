*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Brend_Sp     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                  Бренды со специальными количествами                   ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 10/24/2008 ══╝
PROCEDURE Brend_Sp

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
PRIVATE tmpShab, tmpRec, mss

*
*   Заполняем значениями переменные состояния...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"BREND_SP") SHARED AGAIN ORDER TAG CODE
SET FILTER TO CODE # 0
USE (m.base_path+"BREND_SP") SHARED AGAIN ORDER TAG CODE ALIAS MODI IN 0
SELECT 0
USE (m.base_path+"DEVELOP") SHARED AGAIN ORDER TAG B_NAME
SET RELATION TO CODE INTO BREND_SP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modi OF (menu_name) PROMPT "Коррекция" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modi OF (menu_name) DO Sw_Mode WITH "Modi"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 15

DO D_Wins WITH m.ln, m.wd, "Бренды", 0, 0
@ 1, 2 SAY "─Имя─────Кол-во"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek"

*
*    BROWSE - меню
*
    BROWSE FIELDS tmpName = LEFT(B_NAME, 8):H="",	;
                  BREND_SP.QNT:H="":P="@Z",			;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*           WHEN Draw_Foot()    ;
*

    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Modi")
    ENDIF

  CASE m.what_do = "Modi"    && Коррекция

    DO Sp_Modi
    m.what_do = "List"

  CASE m.what_do = "Seek"    && Поиск

    m.tmpShab = Get_S(SPACE(8))
    IF .NOT. EMPTY(m.tmpShab)
      m.tmpRec = RECNO()
      IF .NOT. SEEK(m.tmpShab)
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"Ничего не найдено..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.tmpRec, 1, RECCOUNT())
          GO m.tmpRec
        ENDIF
      ENDIF
    ENDIF
    m.what_do = "List"

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
USE IN DEVELOP
USE IN BREND_SP
USE IN MODI

SELECT (m.s_sav)

RETURN

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

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Sp_Modi      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Коррекция                                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 23.10.2008 ──┘
PROCEDURE Sp_Modi

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*   Коррекция
*┌──────────────────────────────────┐
*│                                  │
*│   Справочное количество 99999    │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, "Коррекция"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

m.tmpQnt = 0
IF SEEK(DEVELOP.CODE, "BREND_SP")
  m.tmpQnt = BREND_SP.QNT
ENDIF

@ 3, 5 SAY "Справочное количество" GET m.tmpQnt PICTURE "@Z 99999"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF m.ex = 1
  *
  * Отрабатываем бланк
  DO CASE
  CASE m.tmpQnt = 0 .AND. FOUND("BREND_SP")
    SELECT MODI
    GO RECNO("BREND_SP")
    REPLACE CODE WITH 0, QNT WITH 0
  CASE m.tmpQnt # 0 .AND. FOUND("BREND_SP")
    SELECT MODI
    GO RECNO("BREND_SP")
    REPLACE QNT WITH m.tmpQnt
  CASE m.tmpQnt # 0 .AND. .NOT. FOUND("BREND_SP")
    SELECT MODI
    GO TOP
    IF CODE # 0 .OR. BOF() .OR. EOF()
      APPEND BLANK
    ENDIF
    REPLACE CODE WITH DEVELOP.CODE, QNT WITH m.tmpQnt
  ENDCASE
  SELECT DEVELOP
  IF .NOT. BOF() .AND. .NOT. EOF()
    GO RECNO()
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
