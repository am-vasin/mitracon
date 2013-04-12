*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Lst_Wght     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                    Вес позиций, входящих в список.                     ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 06.02.2001 ══╝
PROCEDURE Lst_Wght
PARAMETERS tmpNum

PRIVATE s_sav, tmpName, b_title, w_tot, is_exa

b_title = ""
is_exa  = " "
w_tot   = 0

s_sav = SELECT()
IF .NOT. Prp_Tmp()
  SELECT (s_sav)
  RETURN
ENDIF

DO View_W

SELECT TMP_WGHT
tmpName = DBF()
USE
DELETE FILE (tmpName)
DELETE FILE (LEFT(tmpName, LEN(tmpName)-3)+"cdx")

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_Tmp      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.02.2001 ──┘
PROCEDURE Prp_Tmp

PRIVATE mss, s_sav, tmpName

IF EMPTY(path_comm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет доступа к файлам MANAGERа!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF .NOT. File_O(path_comm+"SUBJECT.DBF") .OR. .NOT. File_O(path_comm+"CORPUS.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет доступа к файлам MANAGERа!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

s_sav = SELECT()
SELECT 0

tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ;
  ( PREFIX   C( 4),  ;
    NAME     C(35),  ;
    PRODUCER C( 8),  ;
    CORPUS   C(16),  ;
    QNT      N( 6),  ;
    WGHT     N( 8,3) )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_WGHT
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE (path_comm+"CORPUS") AGAIN ALIAS COR_1206 ORDER TAG CODE
SELECT 0
USE (path_comm+"SUBJECT") AGAIN ALIAS SUB_1206 ORDER TAG STO_CODE
SET RELATION TO COR_CODE INTO COR_1206
SELECT 0
USE (base_path+"LIST_TIT") AGAIN ORDER TAG LST_NUM ALIAS LST_1206
SEEK tmpNum
b_title = "Список № "+ALLTRIM(STR(tmpNum))+" "+ALLTRIM(LST_NAME)
USE (base_path+"STOCK") AGAIN ORDER TAG CODE ALIAS STO_1206
SELECT 0
USE (base_path+"LIST_DET") AGAIN ORDER TAG LIST ALIAS LST_1206
SET RELATION TO CODE INTO STO_1206
SET RELATION TO CODE INTO SUB_1206 ADDITIVE
SEEK STR(tmpNum,6)
SCAN REST WHILE LIST_NUM = STR(tmpNum,6)
  SELECT TMP_WGHT
  APPEND BLANK
  REPLACE PREFIX   WITH STO_1206.PREFIX,  ;
          NAME     WITH STO_1206.NAME,    ;
          PRODUCER WITH STO_1206.PRODUCER,;
          CORPUS   WITH COR_1206.NAME,    ;
          QNT      WITH LST_1206.QNT,     ;
          WGHT     WITH COR_1206.WEIGHT
  IF WGHT # 0
    w_tot = w_tot+QNT*WGHT
  ELSE
    is_exa = "~"
  ENDIF
  SELECT LST_1206
ENDSCAN
USE

SELECT STO_1206
USE

SELECT SUB_1206
USE

SELECT COR_1206
USE

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_W       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Просмотр веса заявки.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.02.2001 ──┘
PROCEDURE View_W

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
SELECT TMP_WGHT

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
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 73
*
*┌──────Наименование──────────────────────Кол-во──Вес изд.─Общий вес поз.──┐
*│PPPP nnnnnnnnnNnnnnnnnnnNnnnnn PPPPPPPP 999999 9 999.999 99 999 999.999 V│
*└─────────────────────────────────────────────────────────────────────────┘
*┌─────────────────────────────────────────────────────────────────────────┐
*│ Корпус kkkk_kkkk_kkkk_k               Общий вес заявки ~99 999 999.999  │
*└─────────────────────────────────────────────────────────────────────────┘

DO D_Wins WITH ln, wd, b_title, 0, 2
@ WROWS()-2,  3 SAY "Корпус"
@ WROWS()-2, 41 SAY "Общий вес заявки "
@ WROWS()-2, COL() SAY is_exa+TRANSFORM(w_tot, "99 999 999.999") COLOR (SCHEME(1,1))
@ 1, 2 SAY "──────Наименование──────────────────────Кол-во──Вес изд.─Общий вес поз."
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
    BROWSE FIELDS PREFIX:H="",             ;
                  Brw_Name=Nm_Part(NAME):25:H="", ;
                  PRODUCER:H="",           ;
                  QNT:H="",                ;
                  WGHT:P="9 999.999":H="", ;
                  F001 = QNT*WGHT:P="99 999 999.999":H="", ;
                  F002 = IIF(QNT*WGHT=0, "√", " "):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

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
@ WROWS()-3, 2 SAY "Полное название"
@ ROW(), COL()+1 SAY PREFIX COLOR (SCHEME(1,1))
@ ROW(), COL()+1 SAY NAME COLOR (SCHEME(1,1))
@ ROW(), COL()+1 SAY PRODUCER COLOR (SCHEME(1,1))
@ WROWS()-2, 10 SAY CORPUS COLOR (SCHEME(1,1))
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
