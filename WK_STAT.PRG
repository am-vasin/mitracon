*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Wk_Stat      Разработчик Андрей Васин           26.02.98 10:34:45 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Статистика по пунктам меню.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Wk_Stat
PRIVATE s, q, q_h

USE (base_path+"MENU_LST") ORDER TAG LEVELS
SELECT 0
DELETE FILE (tmpo_path+"MENU_TMP.DBF")
CREATE DBF (tmpo_path+"MENU_TMP.DBF") ;
   ( P_PROMPT C(FSIZE("P_PROMPT", "MENU_LST")+4), ;
     QNT      N(6),                           ;
     PART     N(6,4) )
SELECT 0
IF .NOT. File_O(base_path+"WORK_LOG.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет журнала."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"MENU_TMP.DBF")
  RETURN
ENDIF

DO Wt_Mess WITH "Подсчитываем..."

USE (base_path+"WORK_LOG.DBF") ORDER TAG HRON

SELECT MENU_LST
SCAN
  s = ""
  IF LEVEL2 # 0
    s = s+"  "
    IF LEVEL3 # 0
      s = s+"  "
    ENDIF
  ENDIF
  s = s+ALLTRIM(P_PROMPT)
  IF EMPTY(COMM_STR)
    s = PADR(s, FSIZE("P_PROMPT","MENU_TMP")-1,"=")+""
  ENDIF
  SELECT MENU_TMP
  APPEND BLANK
  REPLACE P_PROMPT WITH s
  SELECT MENU_LST
ENDSCAN
USE
SELECT MENU_TMP
q_h = 0
SCAN FOR RIGHT(P_PROMPT,1) # ""
  SELECT WORK_LOG
  COUNT FOR ALLTRIM(MENU_TMP.P_PROMPT)+"  (ПРОДАЖИ)" $ WORK_LOG.NOTE TO q
  q_h = q_h+q
  SELECT MENU_TMP
  REPLACE QNT WITH q
ENDSCAN
IF q_h = 0
  DO Wt_Mess
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не найдено ни одного пункта!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  REPLACE ALL PART WITH QNT/q_h
  DO Wt_Mess
  DO View_L
ENDIF
CLOSE DATABASES
DELETE FILE (tmpo_path+"MENU_TMP.DBF")
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Просмотр результата.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 26.02.98 12:17:19 ═╝
PROCEDURE View_L
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

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
DO Use_Dummy
SELECT MENU_TMP
*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = FSIZE("P_PROMPT", "MENU_TMP")+14

DO D_Wins WITH ln, wd, "Частота обращения к пунктам меню", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - меню
*
    BROWSE FIELDS P_PROMPT:H="", ;
                  QNT:H="":P="@Z 999999", ;
                  PART:H="":P="@Z 9.9999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN
