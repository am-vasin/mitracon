*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ch_Class     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Выбор класса номенклатуры.                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 03.09.1999 ══╝
PROCEDURE Ch_Class
PARAMETERS c_code, c_name

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
PRIVATE i, flt_code, sav_code
s_sav = SELECT()
flt_code = ALLTRIM(c_code)

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"CLASSES") ORDER TAG CODE DESCENDING ALIAS Cl_9903 AGAIN
SET FILTER TO EMPTY(DATE_OFF)
IF .NOT. SEEK(flt_code) .OR. EMPTY(flt_code)
  flt_code = ""
ENDIF
SELECT 0
CREATE DBF (tmpo_path+"TMP____1") ;
   ( CODE C(12),  ;
     NAME C(FSIZE("NAME", "Cl_9903")+2), ;
     DEL  L )
INDEX ON SYS(15, lwr, NAME) TAG NAME
FOR i = 2 TO 6
  COPY STRUCTURE TO (tmpo_path+"TMP____"+STR(i,1)) WITH CDX
ENDFOR
FOR i = 2 TO 6
  SELECT 0
  USE (tmpo_path+"TMP____"+STR(i,1)) ORDER TAG NAME EXCLUSIVE
ENDFOR

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Dn_Level OF (menu_name) PROMPT "След. уровень" KEY Ctrl-A, "Tab"
ON SELECTION PAD Dn_Level OF (menu_name) DO Sw_Mode WITH "Dn_Level"

DEFINE PAD Up_Level OF (menu_name) PROMPT "Пред. уровень" KEY Ctrl-A, "Shift-Tab"
ON SELECTION PAD Up_Level OF (menu_name) DO Sw_Mode WITH "Up_Level"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-15   && Количество видимых строк BROWSE
wd = 66

DO D_Wins WITH ln, wd, "Классы", 0, 6
@ WROWS()-8, 3 SAY " Уровни "
sav_code = flt_code
flt_code = ""
DO WHILE .T.
  DO Prp_Level
  flt_code = LEFT(sav_code, LEN(flt_code)+2)
  LOCATE FOR ALLTRIM(CODE) = flt_code
  IF .NOT. FOUND()
    GO TOP
  ENDIF
  @ WROWS()-7+LEN(flt_code)/2-1, 2 SAY LEFT(NAME,64)
  IF LEN(flt_code) >= LEN(sav_code)
    EXIT
  ENDIF
ENDDO
flt_code = LEFT(sav_code, LEN(sav_code)-2)
@ WROWS()-7+LEN(flt_code)/2, WCOLS()-3 SAY ""
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3

    DO Prp_Nav_1
    ON KEY LABEL Enter   KEYBOARD CHR(23)
    ON KEY LABEL Tab     DO Sw_Mode WITH "Dn_Level"
    ON KEY LABEL BackTab DO Sw_Mode WITH "Up_Level"

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    c_code = CODE
    IF TYPE("c_name") = "C"
      c_name = LEFT(NAME,64)
    ENDIF
    EXIT

  CASE what_do = "Dn_Level"  && След. уровень

    DO Dn_Level
    what_do = "List"

  CASE what_do = "Up_Level"  && Пред. уровень

    DO Up_Level
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

SELECT Cl_9903
USE
FOR i = 1 TO 6
  flt_code = "TMP____"+STR(i,1)
  SELECT (flt_code)
  USE
  DELETE FILE (tmpo_path+flt_code+".DBF")
  DELETE FILE (tmpo_path+flt_code+".CDX")
ENDFOR

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
@ WROWS()-7+LEN(flt_code)/2, 2 SAY LEFT(NAME, FSIZE("NAME")-2)
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_Level    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│             Формирование временного файла текущего уровня.             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.09.1999 ──┘
PROCEDURE Prp_Level

PRIVATE a_name, ln, sw_char

a_name = "TMP____"+STR(LEN(flt_code)/2+1,1)
SELECT Cl_9903
ln = LEN(flt_code)
SEEK LEFT(flt_code,ln)
sw_char = "  "
SCAN REST WHILE LEFT(CODE, ln) = LEFT(flt_code,ln) ;
     FOR .NOT. EMPTY(SUBSTR(CODE, ln+1, 2))
  IF .NOT. EMPTY(SUBSTR(CODE, ln+3))
    sw_char = " "
    LOOP
  ENDIF
  SELECT (a_name)
  APPEND BLANK
  REPLACE CODE WITH Cl_9903.CODE, ;
          NAME WITH Cl_9903.NAME+sw_char, ;
          DEL  WITH .NOT. EMPTY(Cl_9903.DATE_OFF)
  SELECT Cl_9903
  sw_char = "  "
ENDSCAN

SELECT (a_name)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Dn_Level     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           На уровень вниз...                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.09.1999 ──┘
PROCEDURE Dn_Level

IF RIGHT(NAME, 1) = " "
  RETURN
ENDIF

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-7+LEN(flt_code)/2, WCOLS()-3 SAY " "
flt_code = ALLTRIM(CODE)
DO Prp_Level
@ WROWS()-7+LEN(flt_code)/2, WCOLS()-3 SAY ""

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Up_Level     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           На уровень вверх...                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.09.1999 ──┘
PROCEDURE Up_Level
PRIVATE a_name
IF LEN(flt_code) = 0
  RETURN
ENDIF

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-7+LEN(flt_code)/2, 2 CLEAR TO WROWS()-2, WCOLS()-3
flt_code = LEFT(flt_code, LEN(flt_code)-2)
ZAP
a_name = "TMP____"+STR(LEN(flt_code)/2+1,1)
SELECT (a_name)

@ WROWS()-7+LEN(flt_code)/2, WCOLS()-3 SAY ""

RETURN
