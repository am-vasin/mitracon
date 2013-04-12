*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Seek_Eml     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                            Поиск по E-mail                             ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 11.01.2011 ══╝
PROCEDURE Seek_Eml

PRIVATE tmpSh, s_sav, tmpName, tmpRec

m.tmpSh = Get_ShE()
m.tmpSh = LOWER(ALLTRIM(m.tmpSh))
IF EMPTY(m.tmpSh)
  RETURN 0
ENDIF
m.s_sav = SELECT()
SELECT 0
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	LINK	N(10),	;
		C_NAME	C(50),	;
		C_EML	C(32)	)
USE (m.tmpo_path+m.tmpName) ALIAS TMP_0110 EXCLUSIVE
SELECT 0
USE (base_path+"ACCOUNT") ALIAS CL_0110 AGAIN ORDER TAG CUS_CODE
SELECT 0
USE (base_path+"CLI_INFO") ALIAS CI_0110 AGAIN
SET RELATION TO CUS_CODE INTO CL_0110
DO Wt_Mess WITH "Поиск"
SCAN FOR tmpSh $ LOWER(EML)
  SELECT TMP_0110
  APPEND BLANK
  REPLACE LINK		WITH RECNO("CL_0110"),	;
          C_NAME	WITH CL_0110.CUS_NAME,	;
          C_EML		WITH CI_0110.EML
  SELECT CI_0110
ENDSCAN
DO Wt_Mess

USE IN CL_0110
USE IN CI_0110
IF RECCOUNT("TMP_0110") = 0
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Ничего не найден..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN TMP_0110
  DELETE FILE (m.tmpo_path+m.tmpName+".DBF")
  SELECT (m.s_sav)
  RETURN 0
ENDIF

SELECT TMP_0110
INDEX ON LOWER(C_NAME) TAG C_NAME
GO TOP

m.tmpRec = Ch_EML()
USE IN TMP_0110
DELETE FILE (m.tmpo_path+m.tmpName+".DBF")
DELETE FILE (m.tmpo_path+m.tmpName+".CDX")
SELECT (m.s_sav)

RETURN m.tmpRec

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Eml       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор клиента из спмска                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.01.2011 ──┘
PROCEDURE Ch_Eml

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
PRIVATE r_val
*
*   Заполняем значениями переменные состояния...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT TMP_0110

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
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 50

DO D_Wins WITH m.ln, m.wd, "Клиенты", 0, 1
m.what_do = "List"
@ WROWS()-2, 2 SAY "E-mail"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS C_NAME,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Eml()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    m.r_val = TMP_0110.LINK
    EXIT

  OTHERWISE

    m.r_val = 0
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
SELECT (m.s_sav)

RETURN m.r_val

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Eml    Разработчик Андрей Васин                       │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Eml

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-2, 9 SAY C_EML
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
