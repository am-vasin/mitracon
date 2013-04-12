*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Lett_Who     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Список получателей сообщения.                      ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 22.10.2005 ══╝
PROCEDURE Lett_Who
PARAMETERS tmpID		&& код сообщения


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
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE tmpName, tmpTit
m.tmpName = SYS(3)

DO TmpList

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE base_path+"LETT_OUT" SHARED AGAIN ALIAS LT_5A22 ORDER TAG MESS_ID
SEEK m.tmpID
m.tmpTit = TITLE
USE
SELECT TMP_LIST

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = MIN(WROWS("")-11, RECCOUNT())   && Количество видимых строк BROWSE
m.wd = FSIZE("NAME")+FSIZE("R_TIME")+FSIZE("R_DATE")+2

DO D_Wins WITH m.ln, m.wd, "Получатели сообщения", 1, 1

@ 1, 2 SAY "Тема "
@ 1, COL() SAY m.tmpTit COLOR SCHEME 1
@ 2, 2 SAY PADR("Получатель", FSIZE("NAME")+1, "─")+"Сообщ. прочитано"
@ WROWS()-3, 2 SAY "Кем прочитано"

m.what_do = "List"

GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="",   ;
                  R_DATE:H="", ;
                  R_TIME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

*  CASE m.what_do = "Choose"    && Просмотр списка
*
*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    m.what_do = "List"
*
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
USE IN TMP_LIST
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
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

@ WROWS()-2, 2 SAY WHO_RD COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TmpList      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.10.2005 ──┘
PROCEDURE TmpList

PRIVATE s_sav, tmpN

m.s_sav = SELECT()
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	NAME	C(40), ;
		R_DATE	C(10), ;
		R_TIME	C( 5), ;
		WHO_RD	C(40)  )
USE (m.tmpo_path+m.tmpName) ALIAS TMP_LIST
SELECT 0
USE base_path+"PERSONS" SHARED AGAIN ALIAS WHO_5A22 ORDER TAG CODE
SELECT 0
USE base_path+"ARM_LST" SHARED AGAIN ALIAS WHERE_5A22 ORDER TAG ARM_C
SELECT 0
USE base_path+"LETT_IN" SHARED AGAIN ALIAS LT_5A22 ORDER TAG MESS_ID
SEEK m.tmpID

SCAN REST WHILE m.tmpID = MESS_ID
  SELECT TMP_LIST
  APPEND BLANK
  IF EMPTY(LT_5A22.ADDRESS_T)
    = SEEK(VAL(LT_5A22.ADDRESS), "WHO_5A22")
    m.tmpN = ALLTRIM(WHO_5A22.FAMILY)+" "+LEFT(WHO_5A22.NAME, 1)+" "+LEFT(WHO_5A22.S_NAME, 1)
  ELSE
    = SEEK(LEFT(LT_5A22.ADDRESS, FSIZE("ARM_C", "WHERE_5A22")), "WHERE_5A22")
    m.tmpN = WHERE_5A22.ARM_N
  ENDIF
  REPLACE NAME WITH m.tmpN
  IF .NOT. EMPTY(LT_5A22.READ_DATE)
    = SEEK(LT_5A22.WHO_READ, "WHO_5A22")
    m.tmpN = ALLTRIM(WHO_5A22.FAMILY)+" "+LEFT(WHO_5A22.NAME, 1)+" "+LEFT(WHO_5A22.S_NAME, 1)
    REPLACE	R_DATE	WITH DTOC(LT_5A22.READ_DATE),    ;
			R_TIME	WITH LEFT(LT_5A22.READ_TIME, 5), ;
			WHO_RD	WITH m.tmpN
  ENDIF
  SELECT LT_5A22
ENDSCAN

USE IN LT_5A22
USE IN WHERE_5A22
USE IN WHO_5A22

SELECT (m.s_sav)

RETURN
