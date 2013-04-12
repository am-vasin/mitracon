*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Lst_Pers     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║            Безальтернативный выбор сотрудника предприятия.             ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 01.09.1999 ══╝
PROCEDURE Lst_Pers
PARAMETERS tmpAlias

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
USE (base_path+"DEPS") ORDER TAG CODE AGAIN ALIAS DP_9901

SELECT 0
USE (base_path+"PERSONS") ORDER TAG NAME AGAIN ALIAS PR_9901
SET RELATION TO DEP_NO INTO DP_9901, CODE INTO (m.tmpAlias)

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Отметка" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD All OF (menu_name) PROMPT "Отметить все" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD All OF (menu_name) DO Sw_Mode WITH "All"

DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр"
ON PAD Filter OF (menu_name) ACTIVATE POPUP Filter

  DEFINE POPUP Filter MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Filter PROMPT "Установить" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Filter DO Sw_Mode WITH "Dep"

  DEFINE BAR  2 OF Filter PROMPT "Очистить" KEY Ctrl-A, "Shift-Tab"
  ON SELECTION BAR 2 OF Filter DO Sw_Mode WITH "Clr"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, d_name, rc_sav
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("FAMILY")+FSIZE("NAME")+FSIZE("S_NAME")+2
d_name = PADR("Все отделы", FSIZE("B_NAME","DP_9901"))

DO D_Wins WITH ln, wd, "Сотрудники предприятия", 1, 1
@ 1, 3 SAY "Фильтр"
@ WROWS()-3, 3 SAY " Отдел "
what_do = "List"
ln = 0

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 10 SAY d_name
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "All"
    ON KEY LABEL Tab DO Sw_Mode WITH "Dep"
    ON KEY LABEL BackTab DO Sw_Mode WITH "Clr"
    DO Set_Key

*
*    BROWSE - меню
*
    IF "DEP" $ TAG()
      BROWSE KEY STR(m.ln,3) ;
             FIELDS &tmpAlias..MARK:H="", ;
                    F000 = FAMILY-(" "+NAME)-(" "+S_NAME):H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Foot()    ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS &tmpAlias..MARK:H="", ;
                    F000 = FAMILY-(" "+NAME)-(" "+S_NAME):H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Foot()    ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Отметить

    IF .NOT. EOF() .AND. .NOT. BOF()
      SELECT (m.tmpAlias)
      IF .NOT. SEEK(PR_9901.CODE)
        APPEND BLANK
        REPLACE CODE WITH PR_9901.CODE
        IF FSIZE("NAME") # 0
          REPLACE NAME WITH ALLTRIM(PR_9901.FAMILY)+" "+LEFT(PR_9901.NAME, 1)+" "+LEFT(PR_9901.S_NAME, 1)
        ENDIF
      ENDIF
      REPLACE MARK WITH IIF(EMPTY(MARK), "√", " ")
      SELECT PR_9901
    ENDIF

    what_do = "List"

  CASE what_do = "All"    && Отметить все

    m.rc_sav = RECNO()
    IF "DEP" $ TAG()
      SEEK STR(m.ln,3)
      SCAN REST WHILE DEP_NO = m.ln
        SELECT (m.tmpAlias)
        IF .NOT. SEEK(PR_9901.CODE)
          APPEND BLANK
          REPLACE CODE WITH PR_9901.CODE
          IF FSIZE("NAME") # 0
            REPLACE NAME WITH ALLTRIM(PR_9901.FAMILY)+" "+LEFT(PR_9901.NAME, 1)+" "+LEFT(PR_9901.S_NAME, 1)
          ENDIF
        ENDIF
        REPLACE MARK WITH "√"
        SELECT PR_9901
      ENDSCAN
    ELSE
      SCAN
        SELECT (m.tmpAlias)
        IF .NOT. SEEK(PR_9901.CODE)
          APPEND BLANK
          REPLACE CODE WITH PR_9901.CODE
          IF FSIZE("NAME") # 0
            REPLACE NAME WITH ALLTRIM(PR_9901.FAMILY)+" "+LEFT(PR_9901.NAME, 1)+" "+LEFT(PR_9901.S_NAME, 1)
          ENDIF
        ENDIF
        REPLACE MARK WITH "√"
        SELECT PR_9901
      ENDSCAN
    ENDIF
    IF BETWEEN(m.rc_sav, 1, RECCOUNT())
      GO m.rc_sav
    ENDIF

    what_do = "List"

  CASE what_do = "Clr"        && Очистить фильтр

    ln = 0
    SET ORDER TO TAG NAME
    d_name = PADR("Все отделы", FSIZE("B_NAME","DP_9901"))
    what_do = "List"

  CASE what_do = "Dep"        && Установить фильтр

    DO Ch_Dep WITH ln, wd, d_name
    IF ln # 0
      SET ORDER TO TAG DEP_NAME
    ENDIF
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
SELECT DP_9901
USE
SELECT (s_sav)

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

@ WROWS()-2, 3 SAY DP_9901.B_NAME

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Key      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Установка обработчиков для букв.                    │
*│                                                                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 01.09.1999 ──┘
PROCEDURE Set_Key

PRIVATE shb_l, shb_u, s, s1, i
shb_u = "ABCDEFGHIJKLMNOPQASTUVWXYZАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯ"
shb_l = SYS(15, lwr, shb_u)

FOR i = 1 TO LEN(shb_l)
  s =  SUBSTR(shb_l,i,1)
  s1 = '"'+s+'"'
  ON KEY LABEL (s) DO Seek_ WITH &s1
ENDFOR
FOR i = 1 TO LEN(shb_u)
  s =  SUBSTR(shb_u,i,1)
  s1 = '"'+SUBSTR(shb_l,i,1)+'"'
  ON KEY LABEL (s) DO Seek_ WITH &s1
ENDFOR
*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Поиск по первой букве.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 01.09.1999 ──┘
PROCEDURE Seek_
PARAMETERS lett

PRIVATE r_sv

r_sv = RECNO()

IF "DEP" $ TAG()
  SEEK STR(ln,3)+lett
ELSE
  SEEK lett
ENDIF
IF .NOT. FOUND()
  ?? CHR(7)
  IF BETWEEN(r_sv,1,RECCOUNT())
    GO r_sv
  ENDIF
ENDIF

RETURN
