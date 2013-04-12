*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Own_Cus      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Выбор из списка собств. клиентов.                    ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 29.12.2000 ══╝
PROCEDURE Own_Cus
PARAMETERS tmpCode, tmpNm

PRIVATE s_sav, r_no, tmpName, mss

r_no = 0
s_sav = SELECT()
SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) (CUS_CODE N( 8), CUS_NAME C(50))
USE (tmpo_path+tmpName) ALIAS TLST_0C29 EXCLUSIVE
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG WHO_NEW AGAIN ALIAS ACCNT_0C29
SEEK user
SCAN REST WHILE WHO_NEW = user
  SELECT TLST_0C29
  APPEND BLANK
  REPLACE CUS_CODE WITH ACCNT_0C29.CUS_CODE, ;
          CUS_NAME WITH ACCNT_0C29.CUS_NAME
  IF tmpCode = CUS_CODE
    r_no = RECNO()
  ENDIF
  SELECT ACCNT_0C29
ENDSCAN

USE
IF RECCOUNT("TLST_0C29") = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"За Вами не закреплено ни одного клиента!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT TLST_0C29
  tmpName = DBF()
  USE
  DELETE FILE (tmpName)
  DELETE FILE (LEFT(tmpName, LEN(tmpName)-3)+"cdx")
  SELECT (s_sav)
  RETURN
ENDIF
SELECT TLST_0C29
INDEX ON SYS(15, lwr, CUS_NAME) TAG NAME
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

SELECT 0
DO Use_Dummy
SELECT TLST_0C29
IF r_no # 0
  GO r_no
ELSE
  GO TOP
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "Контекстный поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(WROWS("")-11, RECCOUNT())   && Количество видимых строк BROWSE
wd = 50

DO D_Wins WITH ln, wd, "Список клиентов", 0, 0
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
    BROWSE FIELDS CUS_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    tmpCode = CUS_CODE
    IF TYPE("tmpNm") = "C"
      tmpNm = CUS_NAME
    ENDIF
    EXIT

  CASE what_do = "Seek"      && Контекстный поиск

    DO C_Seek
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

SELECT TLST_0C29
tmpName = DBF()
USE
DELETE FILE (tmpName)
DELETE FILE (LEFT(tmpName, LEN(tmpName)-3)+"cdx")
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура C_Seek       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Контекстный поиск.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.04.2001 ──┘
PROCEDURE C_Seek

PRIVATE tmpPatt, s_sav, fileName, r_sav, mss

tmpPatt = Get_Shab()
IF EMPTY(tmpPatt)
  RETURN
ENDIF

DO Wt_Mess WITH "Поиск..."
s_sav = SELECT()
fileName = SYS(3)
SELECT 0
CREATE DBF (tmpo_path+fileName) ;
  ( LINK     N(10),  ;
    CUS_NAME C(FSIZE("CUS_NAME", "TLST_0C29")) )
USE (tmpo_path+fileName) ALIAS TMPSEEK
SELECT TLST_0C29
r_sav = RECNO()
tmpPatt = SYS(15, lwr, tmpPatt)
SCAN
  IF tmpPatt $ SYS(15, lwr, CUS_NAME)
    SELECT TMPSEEK
    APPEND BLANK
    REPLACE LINK     WITH RECNO("TLST_0C29"), ;
            CUS_NAME WITH TLST_0C29.CUS_NAME
    SELECT TLST_0C29
  ENDIF
ENDSCAN

DO Wt_Mess

IF RECCOUNT("TMPSEEK") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT TMPSEEK
  USE
  DELETE FILE (tmpo_path+fileName+".DBF")
  IF BETWEEN(r_sav, 1, RECCOUNT("TLST_0C29"))
    GO r_sav IN TLST_0C29
  ENDIF
  SELECT (s_sav)
  RETURN
ENDIF

DO Ch_Found

SELECT TMPSEEK
USE
DELETE FILE (tmpo_path+fileName+".DBF")
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Found     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Выбор из найденного...                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.04.2001 ──┘
PROCEDURE Ch_Found


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
SELECT TMPSEEK
GO TOP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 50

DO D_Wins WITH ln, wd, "То, что удалось найти...", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS CUS_NAME:H="":50,    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    GO TMPSEEK.LINK IN TLST_0C29
    EXIT

  OTHERWISE
    IF BETWEEN(r_sav, 1, RECCOUNT("TLST_0C29"))
      GO r_sav IN TLST_0C29
    ENDIF
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
SELECT (s_sav)

RETURN
