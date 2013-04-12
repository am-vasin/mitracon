*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Own_Dirs     Разработчик Андрей Васин           10.03.98 12:36:46 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║            Сопоставление портов направлениям печати.                   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Own_Dirs
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
PRIVATE pt_ok         && Содержит ли OWN_DIRS поле PRNTYPE
*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy

SELECT 0
IF .NOT. File_O(base_path+"PRN_DIRS.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не создан общий список направлений печати!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
USE (base_path+"PRN_DIRS.DBF")
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Общий список направлений печати пуст!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF
SELECT 0
IF .NOT. File_O(base_path+"PRNSETUP.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не создан общий список установок принтеров!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
USE (base_path+"PRNSETUP.DBF")
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Общий список установок принтеров пуст!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF

pt_ok=.F.
SELECT 0
IF .NOT. File_O(prmo_path+"OWN_DIRS.DBF")
  CREATE DBF (prmo_path+"OWN_DIRS.DBF") (PORT_NAME C(8), NOTE C(24), PRNTYPE N(3))
  APPEND BLANK
  REPLACE PRNTYPE WITH 1, PORT_NAME WITH "PRN:"
  * По умолчанию все принтеры типа 1
ELSE
  USE (prmo_path+"OWN_DIRS.DBF")
  FOR i=1 TO FCOUNT()
    IF TRIM(FIELD(i))="PRNTYPE"
      pt_ok=.T.
    ENDIF
  ENDFOR
  IF !pt_ok
  	ERASE tmpo_path+'own_str.DBF'
	ERASE tmpo_path+'own_NEW.DBF'
    *  Структура OWN_DIRS в OWN_STR
	COPY STRUCTURE EXTENDED TO tmpo_path+'own_str'
	SELECT 0
	USE (tmpo_path+'own_str.DBF')
	* Исправляем структуру - добавляем нехватающее поле
	APPEND BLANK
	REPLACE Field_Name with 'PRNTYPE', Field_type with 'N', Field_Len with 3, Field_dec with 0
	USE
	* Создаем новую OWN_DIRS с добавленным полем
	CREATE tmpo_path+'own_new' FROM tmpo_path+'own_str.DBF'
	* Заполняем данными из старой OWN_DIRS
	APPEND FROM prmo_path+'own_dirs'
	SELECT OWN_DIRS
	USE
	* "Переименовываем" 
	ERASE prmo_path+'OWN_DIRS.DBF'
	SELECT OWN_NEW
	COPY TO prmo_path+'OWN_DIRS'
	USE prmo_path+'OWN_DIRS'
	REPLACE ALL PRNTYPE WITH 1 && По умолчанию все принтеры типа 1
  ENDIF	
ENDIF

SELECT PRN_DIRS
SCAN
  SELECT OWN_DIRS
  IF .NOT. BETWEEN(RECNO("PRN_DIRS"), 1, RECCOUNT())
    APPEND BLANK
  ENDIF
  IF EMPTY(PORT_NAME)
    REPLACE PORT_NAME WITH "PRN:", PRNTYPE WITH 1
  ENDIF
  SELECT PRN_DIRS
ENDSCAN

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = RECCOUNT("PRN_DIRS")   && Количество видимых строк BROWSE
wd = FSIZE("NAME","PRN_DIRS")+  ;
     FSIZE("PORT_NAME","OWN_DIRS")+  ;
     FSIZE("PR_NAME","PRNSETUP")+2

SELECT OWN_DIRS
SET RELATION TO RECNO() INTO PRN_DIRS
SET RELATION TO PRNTYPE INTO PRNSETUP ADDITIVE 
* Рядом с номерами типов названия

DO D_Wins WITH ln, wd, "Направления печати", 0, 0
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
	GO TOP
    BROWSE FIELDS PRN_DIRS.NAME:H="",         ;
           OWN_DIRS.PORT_NAME:H="",           ;
           PRNSETUP.PR_NAME:H="",             ;
           DUMMY.F:H="" FREEZE DUMMY.F        ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && Коррекция

    DO Corr_Dir
    GO RECNO()
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
*║                   Коррекция/добавление направления.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.03.98 13:00:42 ═╝
PROCEDURE Corr_Dir
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

*┌─────────────────────────────────────────┐
*│            Порт ....:....!              │
*│              ┌────────────────────────┐ │
*│ Тип принтера │....:....!....:....!....│ │
*│              └────────────────────────┘ │
*│    < OK Ctrl-W > < Отказаться Esc >     │
*└─────────────────────────────────────────┘

PRIVATE ex, pt, nt, ar_pty, pty
ex = 1             &&
pt = PORT_NAME
nt = NOTE
pty = PRNTYPE

SELECT PRNSETUP
COPY TO ARRAY ar_pty FIELDS PR_NAME
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 45, ALLTRIM(PRN_DIRS.NAME)
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 4, 3 SAY "Тип принтера"
@ 2,13 SAY "Порт" GET pt VALID T_Port()
@ 3, 16 GET pty PICTURE "@^" FROM ar_pty SIZE 1, 24
@ 6, 4 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE && VALID T_Port()

SELECT OWN_DIRS
IF ex = 1
  REPLACE PORT_NAME WITH pt, PRNTYPE WITH pty
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       А не пустое ли имя порта?!                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.03.98 13:08:10 ═╝
PROCEDURE T_Port

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(pt)
  pt = "PRN:"
  SHOW GET pt
ENDIF

RETURN .T.
