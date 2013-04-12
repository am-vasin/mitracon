*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла User_Cus     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Список клиентов менеджера                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 28.12.2000 ══╝
PROCEDURE User_Cus
PARAMETERS super

PRIVATE curUserC, tmpName, curUserN, s_sav, mss, patt
patt = ""

tmpName = SYS(3)
curUserC = user
s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS P_0C28
SEEK user
curUserN = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
USE
CREATE DBF (tmpo_path+tmpName) ;
   ( CUS_CODE  N( 8), CUS_NAME C(50) )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_LIST
INDEX ON CUS_CODE TAG CUS_CODE
INDEX ON SYS(15, lwr, CUS_NAME) TAG CUS_NAME ADDITIVE

DO Prp_List

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
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
SELECT 0
DO Use_Dummy
SELECT TMP_LIST

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF super
  DEFINE PAD Manager OF (menu_name) PROMPT "Менеджер" KEY Ctrl-A, "Tab"
  ON SELECTION PAD Manager OF (menu_name) DO Sw_Mode WITH "Manager"
ENDIF

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

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
wd = 50

DO D_Wins WITH ln, wd, "Список клиентов менеджера", 1, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M")
    @ 1, 2 SAY "Менеджер "+PADR(curUserN, 30)
    DO Prp_Nav_1
    ON KEY LABEL Del DO Sw_Mode WITH "Del"
    ON KEY LABEL Ins DO Sw_Mode WITH "Add"
    ON KEY LABEL F7  DO Sw_Mode WITH "Seek"
    IF super
      ON KEY LABEL Tab DO Sw_Mode WITH "Manager"
    ENDIF

*
*    BROWSE - меню
*
    BROWSE FIELDS CUS_NAME,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "Del"    && Удаляем

    IF .NOT. BOF() .AND. .NOT. EOF()
      DELETE
    ENDIF
    what_do = "List"

  CASE what_do = "Add"    && Добавляем

    DO Add_Cus
    what_do = "List"

  CASE what_do = "Manager"    && Выбор менеджера

    DO C_Manager
    what_do = "List"

  CASE what_do = "Seek"      && Поиск

    patt = Get_Shab(patt, "Укажите шаблон для поиска")
    DO Seek_Cus WITH patt
    what_do = "List"

  OTHERWISE

    DO Save_List
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
SELECT TMP_LIST
tmpName = DBF()
USE
DELETE FILE (tmpName)
DELETE FILE (LEFT(tmpName, LEN(tmpName)-3)+"cdx")
RETURN
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_List     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Подготовка списка клиентов для менеджера                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 28.12.2000 ──┘
PROCEDURE Prp_List

SELECT TMP_LIST
ZAP
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG WHO_NEW AGAIN ALIAS ACC_0C28
SEEK curUserC
SCAN REST WHILE WHO_NEW = curUserC
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE CUS_CODE WITH ACC_0C28.CUS_CODE, ;
          CUS_NAME WITH ACC_0C28.CUS_NAME
  SELECT ACC_0C28
ENDSCAN
USE

SELECT TMP_LIST
GO TOP
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Add_Cus      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Добавить клиента в список                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 28.12.2000 ──┘
PROCEDURE Add_Cus

PRIVATE s_sav, skp

s_sav = SELECT()
skp = .F.
SELECT 0
DO Use_Dummy
SELECT 0
DO Use_Link
SELECT 0
USE (base_path+"ACCOUNT") AGAIN ORDER TAG CUS_NAME
IF Custs("", .F., .F., .F., sys_char)
  SELECT TMP_LIST
  SET ORDER TO TAG CUS_CODE
  IF .NOT. SEEK(ACCOUNT.CUS_CODE)
    IF .NOT. EMPTY(ACCOUNT.WHO_NEW) .AND. ACCOUNT.WHO_NEW # curUserC
      SELECT 0
      USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS PER_0C29
      SEEK ACCOUNT.WHO_NEW
      PRIVATE mss
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"Этим клиентом уже занимается "+ALLTRIM(FAMILY)+" "+ ;
               LEFT(NAME,1)+" "+LEFT(S_NAME,1)
      mss(3) = CHR(0)+"  Меняем менеджера? [ Да ] [ Нет ]"
      mss(4) = ""
      skp = Out_Mess(7, "mss") = 2
      USE
      SELECT TMP_LIST
    ENDIF
    IF .NOT. skp
      APPEND BLANK
      REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
              CUS_NAME WITH ACCOUNT.CUS_NAME
    ENDIF
  ENDIF
  SET ORDER TO TAG CUS_NAME
ENDIF
SELECT ACCOUNT
USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_Cus     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Контекстный поиск клиента.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.12.2000 ──┘
PROCEDURE Seek_Cus
PARAMETERS patt

IF EMPTY(patt)
  RETURN
ENDIF

DO Prp_Seek WITH patt
DO Choose_C

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_Seek     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Отбор позиций по контексту.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.12.2000 ──┘
PROCEDURE Prp_Seek
PARAMETERS patt

PRIVATE s_sav, tmpName, r_sav, patt_w
patt_w = SYS(15, lwr, patt)
s_sav = SELECT()
SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ;
  ( LINK     N(10), ;
    CUS_NAME C(50)  )
USE (tmpo_path+tmpName) ALIAS FOUND

SELECT TMP_LIST
r_sav = RECNO()
SCAN
  IF patt $ SYS(15, lwr, CUS_NAME)
    SELECT FOUND
    APPEND BLANK
    REPLACE LINK     WITH RECNO("TMP_LIST"), ;
            CUS_NAME WITH TMP_LIST.CUS_NAME
    SELECT TMP_LIST
  ENDIF
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Choose_C     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│               Выбор клиента из отбранного по контексту.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.12.2000 ──┘
PROCEDURE Choose_C

PRIVATE tmpName, mss

SELECT FOUND
tmpName = DBF()
*
*    А был ли мальчик?
IF RECCOUNT("FOUND") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE(tmpName)        && Приборка...
  SELECT TMP_DEM
  RETURN
ENDIF
GO TOP
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
ln = MIN(WROWS("")-11, RECCOUNT())   && Количество видимых строк BROWSE
wd = 50
DO D_Wins WITH ln, wd, "Результат поиска", 0, 0
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
    BROWSE FIELDS CUS_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"	&& Юзер выбрал...

    GO FOUND.LINK IN TMP_LIST
    EXIT

  OTHERWISE					&& Юзер отказался

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
DELETE FILE(tmpName)        && Приборка...
SELECT TMP_LIST

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Save_List    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Сохранение списка.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 29.12.2000 ──┘
PROCEDURE Save_List

PRIVATE mss, s_sav

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"Сохраним изменения? [ Да ] [ Нет ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 2
  RETURN
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG WHO_NEW AGAIN ALIAS CUS_0C29
DO WHILE SEEK(CurUserC)
  REPLACE WHO_NEW WITH 0
ENDDO
SET ORDER TO TAG CUS_CODE
SELECT TMP_LIST
SCAN
  SELECT CUS_0C29
  SEEK TMP_LIST.CUS_CODE
  REPLACE WHO_NEW WITH CurUserC
  SELECT TMP_LIST
ENDSCAN

ZAP
SELECT CUS_0C29
USE

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура C_Manager    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Смена менеджера                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 29.12.2000 ──┘
PROCEDURE C_Manager

PRIVATE tmpC, tmpN, mss

tmpC = curUserC
tmpN = curUserN

DO Ch_Pers WITH tmpC, tmpN

IF tmpC # curUserC
  DO Save_List
  curUserC = tmpC
  curUserN = tmpN
  DO Prp_List
ENDIF
