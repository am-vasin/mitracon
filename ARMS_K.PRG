*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Arms_K       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║              Раскладка основной кассы по рабочим местам.               ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 03/17/1999 ══╝
PROCEDURE Arms_K

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
*  Задайте период
*┌────────────────────────────────────────────────────┐
*│ Начальная дата ДД.ММ.ГГГГ Конечная дата ДД.ММ.ГГГГ │
*│                                                    │
*│          < OK Ctrl-W > < Отказаться Esc >          │
*└────────────────────────────────────────────────────┘

PRIVATE ex, dt0, dt1, s_sav, min_d
ex = 1
s_sav = SELECT()
SELECT 0
USE (base_path+"PARMS")
min_d = DAT_START
USE
SELECT (s_sav)
STORE DATE() TO dt0, dt1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 56, "Задайте период"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2,  3 SAY "Начальная дата" GET dt0
@ 2, 29 SAY "Конечная дата" GET dt1
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_D()

IF ex = 1
  *
  * Отрабатываем бланк
  DO View_
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_D        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Проверка корректности интервала.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/17/1999 ──┘
PROCEDURE Tst_D
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF dt0 < min_d
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Данные хранятся только с "+DTOC(min_d)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF dt0 > dt1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"У Вас начальная дата больше конечной..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│               Расчет и просмотр кассы по рабочим местам.               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/17/1999 ──┘
PROCEDURE View_

PRIVATE s_sav, n

s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"V_K_A.DBF")
DELETE FILE (tmpo_path+"V_K_A.CDX")
CREATE DBF (tmpo_path+"V_K_A.DBF") ;
  ( ARM_C C( 3),  ;
    ARM_N C(16),  ;
    SUM_R N(15,2),;
    SUM_D N(15,4)  )
INDEX ON ARM_C TAG ARM_C
SELECT 0
USE (base_path+"KASSA_M") ORDER TAG HRON ALIAS K_9317 AGAIN
IF .NOT. SEEK(DTOS(dt0))
  n = RECNO(0)
  IF BETWEEN(n, 1, RECCOUNT())
    GO n
  ENDIF
ENDIF
SCAN REST WHILE DOC_DATE <= dt1
  SELECT V_K_A
  IF .NOT. SEEK(K_9317.WHERE)
    APPEND BLANK
    REPLACE ARM_C WITH K_9317.WHERE
  ENDIF
  REPLACE SUM_R WITH SUM_R+K_9317.SUM_R ,;
          SUM_D WITH SUM_D+K_9317.SUM_D
  SELECT K_9317
ENDSCAN
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS A_9317 AGAIN
SELECT V_K_A
SET RELATION TO ARM_C INTO A_9317
REPLACE ALL ARM_N WITH A_9317.ARM_N
SELECT A_9317
USE
DO Use_Dummy
SELECT V_K_A
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Движения денег не было!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE

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

  DEFINE PAD Choose OF (menu_name) PROMPT "Операции" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

  DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
  ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


  *
  *   Расчет размеров окна
  *
  PRIVATE ln, wd
  ln = WROWS("")-11   && Количество видимых строк BROWSE
  wd = 48
  DO D_Wins WITH ln, wd, "Сумма по рабочим местам", 0, 0
  @ 1, 2 SAY "Раб. место─────────────Рубли───────────USD"
  what_do = "List"
*
*┌Раб. место─────────────Рубли─────────────USD────┐
*│....:....!....:. 999_999_999.99 999_999_999.9999│
*└────────────────────────────────────────────────┘

  DO WHILE .T.

    DO CASE

    CASE what_do = "List"    && Просмотр списка

      statys_type = 3
      DO Prp_Nav_1
      ON KEY LABEL Enter KEYBOARD CHR(23)

  *
  *    BROWSE - меню
  *
      BROWSE FIELDS ARM_N:H="", ;
                    SUM_R:H="":P="999 999 999.99", ;
                    SUM_D:H="":P="999 999 999.9999", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
      ON KEY
      IF what_do = "List"
        IF READKEY() % 256 = 12
          what_do = "Exit"
        ELSE
          what_do = "Choose"
        ENDIF
      ENDIF

    CASE what_do = "Choose"    && Выбор

      DO Prep_List WITH dt0, dt1, ARM_C
      DO Viewlist WITH SUM_R, SUM_D
      what_do = "List"

    OTHERWISE

      EXIT

    ENDCASE

  ENDDO

  RELEASE WINDOW (win_name+"_I")
  RELEASE WINDOW (win_name+"_E")
  RELEASE WINDOW (win_name+"_M")
  RELEASE MENU (menu_name) EXTENDED

ENDIF

USE
DELETE FILE (tmpo_path+"V_K_A.DBF")
DELETE FILE (tmpo_path+"V_K_A.CDX")
SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_List    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Подготовка списка                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 14.02.2002 ──┘
PROCEDURE Prep_List
PARAMETERS d0, d1, tmpArm

PRIVATE tmpName, s_sav, r_sav, tmpRec

s_sav = SELECT()
SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ( LINK N(10) )
USE (tmpo_path+tmpName) ALIAS TMP_LIST
SELECT VIEW
r_sav = RECNO()
IF .NOT. SEEK(DTOS(d0))
  tmpRec = RECNO(0)
  IF BETWEEN(tmpRec, 1, RECCOUNT())
    GO tmpRec
  ENDIF
ENDIF
SCAN REST WHILE DOC_DATE <= d1
  IF WHERE # tmpArm
    LOOP
  ENDIF
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE LINK WITH RECNO("VIEW")
  SELECT VIEW
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT (s_sav)

RETURN
