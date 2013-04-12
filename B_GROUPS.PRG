*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла B_Groups     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                    Группы фирм - производителей ЭК.                    ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 16.03.2000 ══╝
PROCEDURE B_Groups

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
PRIVATE mss

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
USE (base_path+"B_GROUPS") ALIAS BG_0316 AGAIN

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "ГТД" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Ins OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Ins OF (menu_name) DO Sw_Mode WITH "Ins"

DEFINE PAD Del OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Firms OF (menu_name) PROMPT "Фирмы" KEY Ctrl-A, "Tab"
ON SELECTION PAD Firms OF (menu_name) DO Sw_Mode WITH "Firms"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 68
*
*┌─Группа───────────────────N ГТД──────────────────────Страна─────────┐
*│....:....!....:....!.... ....:....!....:....!....:. ....:....!....:.│
*└────────────────────────────────────────────────────────────────────┘

DO D_Wins WITH ln, wd, "Группы фирм - призводителей ЭК", 0, 0
@ 1, 2 SAY "─Группа─────────────────────────N ГТД────────────────Страна"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Ins"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Firms"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:24:H="", GTD_NUM:26:H="", COUNTRY:16:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Del"       && Удаление

    IF RECNO() = 1
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Эту группу нельзя удалять!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DELETE
      SKIP
      IF EOF() .OR. BOF()
        GO BOTTOM
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Modify"    && Коррекция

    DO Add_Line
    what_do = "List"

  CASE what_do = "Ins"       && Добавление

    DO Add_Line WITH .T.
    what_do = "List"

  CASE what_do = "Firms"     && Список фирм

    DO F_List
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
*│   Процедура Add_Line     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Добавление/коррекция группы.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 16.03.2000 ──┘
PROCEDURE Add_Line
PARAMETERS new

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

*┌───────────────────────────────────────┐
*│  Имя группы ....:....!....:....!....  │
*│                                       │
*│    N ГТД ....:....!....:....!....:.   │
*│                                       │
*│      Страна ....:....!....:.          │
*│                                       │
*│   < OK Ctrl-W > < Отказаться Esc >    │
*└───────────────────────────────────────┘

PRIVATE ex, n1, n2, n3, d_sav
ex = 1
IF new
  n1 = SPACE(24)
  n2 = SPACE(26)
  n3 = SPACE(16)
ELSE
  n1 = NAME
  n2 = GTD_NUM
  n3 = COUNTRY
ENDIF
  
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 43, IIF(new, "Добавление", "Коррекция")+" группы"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 4 SAY "Имя группы" GET n1
@ 4, 6 SAY "N ГТД" GET n2
@ 6, 8 SAY "Страна" GET n3
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  IF new
    d_sav = SET("DELETED") = "ON"
    SET DELETED OFF
    LOCATE FOR DELETED()
    IF FOUND()
      RECALL
      REPLACE DEV_LIST WITH ""
    ELSE
      APPEND BLANK
    ENDIF
    IF d_sav
      SET DELETED ON
    ENDIF
  ENDIF
  REPLACE NAME   WITH  n1, ;
          GTD_NUM WITH n2, ;
          COUNTRY WITH n3
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура F_List       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Состав групп.                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 16.03.2000 ──┘
PROCEDURE F_List

PRIVATE s_sav         && Номер рабочей области для сохранения и восстановления!
PRIVATE r_sav, tmpList, tmpRec, i, n, w, mss
r_sav = RECNO()
s_sav = SELECT()

n = 0
SCAN
  n = n+1
  DIMENSION tmpList(n), tmpRec(n), tmpNames(n)
  tmpList(n)  = DEV_LIST
  tmpRec(n)     = RECNO()
  tmpNames(n) = NAME
ENDSCAN

SELECT 0
DELETE FILE (tmpo_path+"TMP_GR.DBF")
CREATE DBF (tmpo_path+"TMP_GR") ;
    ( F_CODE N( 5), ;
      F_NAME C( 8), ;
      G_REC  N( 3), ;
      G_NAME C(24) )

SELECT 0
USE (base_path+"DEVELOP") ORDER TAG B_NAME ALIAS DV_0316
SCAN
  w = 1
  IF n > 1
    FOR i = 1 TO n
      IF " "+ALLTRIM(STR(CODE))+" " $ tmpList(i)
        w = i
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  SELECT TMP_GR
  APPEND BLANK
  REPLACE F_CODE WITH DV_0316.CODE,   ;
          F_NAME WITH DV_0316.B_NAME, ;
          G_REC  WITH tmpRec(w)
  SELECT DV_0316
ENDSCAN
USE
SELECT TMP_GR
SET RELATION TO G_REC INTO BG_0316

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
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Change OF (menu_name) PROMPT "Группа" KEY Ctrl-A, "Enter"
ON SELECTION PAD Change OF (menu_name) DO Sw_Mode WITH "Change"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-7   && Количество видимых строк BROWSE
wd = 33
*┌─Фирма────Группа─────────────────┐
*│....:... ....:....!....:....!....│
*└─────────────────────────────────┘

DO D_Wins WITH ln, wd, "Группы", 0, 0
@ 1, 2 SAY "─Фирма────Группа"
what_do = "List"

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    GO RECNO()   && Эмпирика!!!
    BROWSE FIELDS F_NAME:8:H="", BG_0316.NAME:24:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY  ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Change")
    ENDIF

  CASE what_do = "Change"    && Выбор группы

    DO Choose
    what_do = "List"

  OTHERWISE

    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Сохранить изменения? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(5, "mss") = 1
      FOR i = 1 TO n
        tmpList(i) = " "
      ENDFOR
      SCAN
        i = ASCAN(tmpRec, G_REC)
        IF i > 1
          tmpList(i) = tmpList(i)+ALLTRIM(STR(F_CODE))+" "
        ENDIF
      ENDSCAN
      FOR i = 2 TO n
        SELECT BG_0316
        GO tmpRec(i)
        REPLACE DEV_LIST WITH tmpList(i)
        SELECT TMP_GR
      ENDFOR
    ENDIF
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
SET RELATION TO
USE
DELETE FILE (tmpo_path+"TMP_GR.DBF")
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT (s_sav)
GO r_sav

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Choose       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор группы для бренда.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 17.03.2000 ──┘
PROCEDURE Choose

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

PRIVATE tmpPoint
tmpPoint = MAX(ASCAN(tmpRec, G_REC),1)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH MIN(ALEN(tmpNames)+3, WROWS("")-11), LEN(tmpNames(1))+4, "Группа для "+ALLTRIM(F_NAME)
*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  tmpPoint = MAX(tmpPoint,1)
  tmpPoint = MIN(tmpPoint,ALEN(tmpNames))
  @ 1, 1 MENU tmpNames, ALEN(tmpNames), WROWS("")-3
  READ MENU TO tmpPoint
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    REPLACE G_REC WITH tmpRec(tmpPoint)
    EXIT
  ENDIF
ENDDO

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
