*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Cnt_Del      Разработчик Андрей Васин           09.09.98 10:57:03 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║               Подсчет удаленных записей в номенклатуре.                ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Cnt_Del

DO Prep_Lst
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет удаленных записей!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"CNT_DEL.DBF")
  DELETE FILE (tmpo_path+"CNT_DEL.CDX")
  RETURN
ENDIF

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
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD On_Off OF (menu_name) PROMPT "Удалить/Восстановить" KEY Ctrl-A, "Enter"
ON SELECTION PAD On_Off OF (menu_name) DO Sw_Mode WITH "On_Off"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy
SELECT CNT_DEL
GO TOP

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 10
wd = FSIZE("NAME")+FSIZE("PREFIX")+FSIZE("PRODUCER")+4

DO D_Wins WITH ln, wd, "Удаленные позиции", 0, 0
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
    BROWSE FIELDS PREFIX:H="",    ;
                  NAME:H="",  ;
                  PRODUCER:H="", SW:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "On_Off")
    ENDIF

  CASE what_do = "On_Off"    && Удаление/Восстановление

    SELECT STOCK
    GO CNT_DEL.RC_NO
    IF EMPTY(CNT_DEL.SW)
      DELETE
      SELECT CNT_DEL
      REPLACE SW WITH "▒"
    ELSE
      RECALL
      SELECT CNT_DEL
      REPLACE SW WITH " "
    ENDIF
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
*║                  Подготовка списка удаленных записей.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 09.09.98 10:58:12 ═╝
PROCEDURE Prep_Lst
PRIVATE del_sw, cnt
del_sw = SET("DELETED") = "ON"

SET DELETED OFF

DELETE FILE (tmpo_path+"CNT_DEL.DBF")
DELETE FILE (tmpo_path+"CNT_DEL.CDX")
CREATE DBF (tmpo_path+"CNT_DEL") ;
  ( RC_NO    N(10), ;
    CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(78), ;
    PRODUCER C( 8), ;
    SW       C( 1)  )
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE (base_path+"STOCK")
DO Wt_Mess WITH "Просмотр номенклатуры..."

cnt = 0
SCAN
  cnt = cnt+1
  @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)),WCOLS())
  IF .NOT. DELETED()
    LOOP
  ENDIF
  SELECT CNT_DEL
  APPEND BLANK
  REPLACE RC_NO    WITH RECNO("STOCK"), ;
          CODE     WITH STOCK.CODE,     ;
          PREFIX   WITH STOCK.PREFIX,   ;
          NAME     WITH STOCK.NAME,     ;
          PRODUCER WITH STOCK.PRODUCER, ;
          SW       WITH "▒"
  SELECT STOCK
ENDSCAN

DO Wt_Mess

SELECT CNT_DEL
IF del_sw
  SET DELETED ON
ENDIF

RETURN
