*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Seek_P_N     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                  Поиск позиции в списке по примечанию                  ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 25.04.2006 ══╝
PROCEDURE Seek_P_N

PRIVATE tmpShab, s_sav, tmpName, tmpV, mss

m.tmpShab = Get_Shab("", "Поиск по примечанию")
IF EMPTY(m.tmpShab)
  RETURN 0
ENDIF
m.tmpShab = SYS(15, m.lwr, m.tmpShab)
m.s_sav = SELECT()
SELECT 0
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
  ( REC_NO N(10) )
USE (m.tmpo_path+m.tmpName) ALIAS TMP_6426
SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS LD_6426 ORDER TAG L_NAME
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS LT_6426 ORDER TAG LST_NUM
SET RELATION TO STR(LST_NUM, 6) INTO LD_6426
SET SKIP TO LD_6426
DO Wt_Mess WITH "Поиск"
SCAN FOR .NOT. EMPTY(LD_6426.NOTE)
  m.tmpV = LD_6426.NOTE
  IF m.tmpShab $ SYS(15, m.lwr, m.tmpV)
    SELECT TMP_6426
    APPEND BLANK
    REPLACE REC_NO WITH RECNO("LD_6426")
  ENDIF
ENDSCAN
DO Wt_Mess

USE IN LD_6426
USE IN LT_6426

IF RECCOUNT("TMP_6426") = 0
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Ничего не найдено..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN TMP_6426
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  RETURN 0
SELECT (m.s_sav)
ENDIF

m.tmpV = Ch_Lst()

USE IN TMP_6426
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")

SELECT (m.s_sav)

RETURN m.tmpV

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Lst       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                              Выбор списка                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.04.2006 ──┘
PROCEDURE Ch_Lst

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
PRIVATE retVal
m.retVal = 0

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS ST_6424 ORDER TAG CODE
SELECT 0
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS ARM_6424 ORDER TAG ARM_C
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_6424 ORDER TAG CUS_CODE
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS LT_6424 ORDER TAG LST_NUM
SET RELATION TO LST_ARM INTO ARM_6424, CLI_CODE INTO CUS_6424
SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS LD_6424
SET RELATION TO VAL(LIST_NUM) INTO LT_6424, CODE INTO ST_6424
SELECT TMP_6426
SET RELATION TO REC_NO INTO LD_6424
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
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 63

DO D_Wins WITH m.ln, m.wd, "Позиции с примечанием", 0, 5
@ 1, 2 SAY ;
 "─────Наименование───────────────────────Список─Рабочее место"
* PPPP NNNNNNNNNNNNNNNNNNNNNNNNN PPPPPPPP 999999 AAAAAAAAAAAAAAAA
@ WROWS()-7, 3 SAY "Примечание"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS prf000 = IIF(EMPTY(LD_6424.CODE), LD_6424.PREFIX, ST_6424.PREFIX):4:H="" ,    ;
                  nam000 = IIF(EMPTY(LD_6424.CODE), LD_6424.NAME, ST_6424.NAME):25:H="" ,       ;
                  prd000 = IIF(EMPTY(LD_6424.CODE), LD_6424.PRODUCER, ST_6424.PRODUCER):8:H="" ,;
                  LD_6424.LIST_NUM:H="", ;
                  ARM_6424.ARM_N:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    m.retVal = RECNO("LT_6424")
    IF LT_6424.LST_ARM # m.arm
      m.retVal = -m.retVal
    ENDIF
    EXIT

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
USE IN LT_6424
USE IN LD_6424
USE IN ST_6424
USE IN CUS_6424
USE IN ARM_6424

SELECT (m.s_sav)

RETURN m.retVal

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
@ WROWS()-6, 2 EDIT LD_6424.NOTE SIZE 3, 63 COLOR (SCHEME(1,1))
@ WROWS()-2, 2 SAY "Клиент" GET CUS_6424.CUS_NAME
CLEAR GETS

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
