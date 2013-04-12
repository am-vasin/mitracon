*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Cmp_Log      Разработчик Андрей Васин           17.03.98 12:50:12 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║      Сравнение архива для ВК с данными утверждения спецдвижения.       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Cmp_Spec

PRIVATE f_name, n, dt, s, dt_w, dt_e, n0

dt_w = DATE()
dt_e = DATE()

IF TYPE("vk_spec") # "C"
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Режим сбора информации не установлен."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

*n = FOPEN(base_path+"FOR_VK.TXT")
*f_name = FGETS(n, 120)
*f_name = ALLTRIM(f_name)
*= FCLOSE(n)

f_name = vk_spec
IF .NOT. "." $ f_name
  f_name = f_name+".dbf"
ENDIF

IF .NOT. File_O(f_name)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет файла архива."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF .NOT. Get_Date()
  RETURN
ENDIF

DO Wt_Mess WITH "Подготовка данных..."

DELETE FILE (tmpo_path+"ARCH_S.DBF")
DELETE FILE (tmpo_path+"ARCH_S.CDX")
DELETE FILE (tmpo_path+"ARCH_T.DBF")
DELETE FILE (tmpo_path+"ARCH_T.CDX")

CREATE DBF (tmpo_path+"ARCH_S") ;
  ( DOC_NUM  C(10), ;
    DOC_DATE D,     ;
    CODE     N( 7), ;
    QNT      N( 6) )
INDEX ON DOC_NUM TAG DOC_NUM

SELECT 0
CREATE DBF (tmpo_path+"ARCH_T") ;
  ( DOC_NUM  C(10), ;
    DOC_DATE D,     ;
    CHECK_A  C(6),  ;
    CHECK_S  C(6)   )
INDEX ON LEFT(DTOS(DOC_DATE),4)+DOC_NUM TAG DOC_NUM
    
SELECT 0
USE (f_name) ORDER TAG DATE_CODE ALIAS ARCH
SEEK DTOS(dt_w)
IF .NOT. FOUND()
  n0 = RECNO(0)
  IF BETWEEN(n0, 1, RECCOUNT())
    GO n0
  ENDIF
ENDIF
SCAN REST WHILE DATE <= dt_e
  SELECT ARCH_S
  APPEND BLANK
  REPLACE DOC_NUM  WITH ARCH.DOC_NUM,  ;
          DOC_DATE WITH ARCH.DATE,     ;
          CODE     WITH ARCH.STO_CODE, ;
          QNT      WITH ARCH.QTY
  SELECT ARCH
ENDSCAN

USE
SELECT ARCH_S
s = 0
n = "          "
SCAN
  IF DOC_NUM # n
    IF .NOT. EMPTY(n)
      SELECT ARCH_T
      APPEND BLANK
      REPLACE DOC_NUM  WITH n,   ;
              DOC_DATE WITH ARCH_S.DOC_DATE, ;
              CHECK_A  WITH RIGHT(STR(1000000+s),7), ;
              CHECK_S  WITH "▒▒▒▒▒▒"
      SELECT ARCH_S
    ENDIF
    n = DOC_NUM
    dt = DOC_DATE
    s = 0
  ENDIF
  s = (s+ABS(QNT)*CODE) % 1000000
ENDSCAN

IF .NOT. EMPTY(n)
  SELECT ARCH_T
  APPEND BLANK
  REPLACE DOC_NUM  WITH n,   ;
          DOC_DATE WITH dt, ;
          CHECK_A  WITH RIGHT(STR(1000000+s),7), ;
          CHECK_S  WITH "▒▒▒▒▒▒"
ENDIF
SELECT ARCH_S
USE (base_path+"LET_SPEC") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"LET_SP_T") ORDER TAG HRON

SEEK DTOS(dt_w)
IF .NOT. FOUND()
  n0 = RECNO(0)
  IF BETWEEN(n0, 1, RECCOUNT())
    GO n0
  ENDIF
ENDIF
SCAN REST WHILE DOC_DATE <= dt_e
  SELECT LET_SPEC
  s = 0
  SEEK LEFT(DTOS(LET_SP_T.DOC_DATE),4)+LET_SP_T.DOC_NUM
  SCAN REST WHILE LET_SP_T.DOC_DATE = DOC_DATE .AND. LET_SP_T.DOC_NUM = DOC_NUM
    s = (s+ABS(QNT)*CODE) % 1000000
  ENDSCAN
  SELECT ARCH_T
  IF .NOT. SEEK(LEFT(DTOS(LET_SP_T.DOC_DATE),4)+LET_SP_T.DOC_NUM)
    APPEND BLANK
    REPLACE DOC_NUM  WITH LET_SP_T.DOC_NUM,  ;
            DOC_DATE WITH LET_SP_T.DOC_DATE, ;
            CHECK_A  WITH "▒▒▒▒▒▒",          ;
            CHECK_S  WITH RIGHT(STR(1000000+s),7)
  ELSE
    REPLACE CHECK_S WITH RIGHT(STR(1000000+s),7)
  ENDIF
  SELECT LET_SP_T
ENDSCAN

DO Wt_Mess

USE
SELECT ARCH_T

IF RECCOUNT() # 0
  DO View_Res
ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Утверждений за период с "+DTOC(dt_w)+" по "+DTOC(dt_e)+" не обнеружено!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

CLOSE DATABASES

DELETE FILE (tmpo_path+"ARCH_S.DBF")
DELETE FILE (tmpo_path+"ARCH_S.CDX")
DELETE FILE (tmpo_path+"ARCH_T.DBF")
DELETE FILE (tmpo_path+"ARCH_T.CDX")
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                               Ввод даты.                               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 17.03.98 15:53:14 ═╝
PROCEDURE Get_Date

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

*          Проверка архива
*┌──────────────────────────────────┐
*│                                  │
*│    C DD.MM.YYYY по DD.MM.YYYY    │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*│                                  │
*└──────────────────────────────────┘

PRIVATE ex
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 38, "Проверка архива"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  6 SAY "С" GET dt_w
@ 3, 19 SAY "по" GET dt_e
@ 5,  3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Просмотр результата.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 17.03.98 16:00:42 ═╝
PROCEDURE View_Res

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
PRIVATE flt, rc_sav
flt = .F.

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать док-та" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Filter OF (menu_name) PROMPT "Только расх." KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
DO Use_Dummy
SELECT ARCH_T
*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 37, 35)

DO D_Wins WITH ln, wd, "Утверждения с "+DTOC(dt_w)+" по "+DTOC(dt_e), 0, 0
what_do = "List"
@ 1, 2 SAY IIF(SET("CENTURY") = "ON", ; 
           "─N док-та─────Дата─────Архив──Док-т", ;
           "─N док-та────Дата────Архив──Док-т")
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F6    DO Sw_Mode WITH "Filter"

*
*    BROWSE - меню
*
    BROWSE FIELDS DOC_NUM:H="", ;
                  DOC_DATE:H="", ;
                  CHECK_A:H="":6, ;
                  CHECK_S:H="":6, ;
                  Is_Eq = IIF(CHECK_A = CHECK_S, " ", "√"):1:H="" , ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && Печать документа

    DO P_L_Spec WITH DOC_NUM, DOC_DATE
    what_do = "List"
    
  CASE what_do = "Filter"    && Установка/Снятие филитра

    IF .NOT. flt
      rc_sav = RECNO()
      SET FILTER TO CHECK_A # CHECK_S
      GO TOP
      IF EOF() .OR. BOF()
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Нет расхождений!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        SET FILTER TO
        IF BETWEEN(rc_sav, 1, RECCOUNT("ARCH_T"))
          GO rc_sav IN ARCH_T
        ENDIF
      ELSE
        IF BETWEEN(rc_sav, 1, RECCOUNT("ARCH_T"))
          GO rc_sav IN ARCH_T
        ENDIF
        DEFINE PAD Filter OF (menu_name) PROMPT "Все." BEFORE Exit KEY Ctrl-A, "F6"
        flt = .T.
      ENDIF
    ELSE
      SET FILTER TO
      DEFINE PAD Filter OF (menu_name) PROMPT "Только расх." KEY Ctrl-A, "F6"
      flt = .F.
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
