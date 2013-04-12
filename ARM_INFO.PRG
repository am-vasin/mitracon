*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Arm_Info     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Информация о рабочем месте.                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 01.09.1999 ══╝
PROCEDURE Arm_Info
PARAMETERS arm_cd, hide_name

PRIVATE s_sav, tmpArm

s_sav = SELECT()

tmpArm = arm_cd

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C AGAIN ALIAS A1_9901
IF .NOT. EMPTY(tmpArm)
  IF .NOT. SEEK(tmpArm)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Некорректный код рабочего места!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
ENDIF

PRIVATE prs_c, prs_n, a_name, a_place, a_phone

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS P_9901

SELECT 0
USE (base_path+"ARM_INFO") ORDER TAG ARM_C AGAIN ALIAS A2_9901
prs_c = 0
prs_n = ""
prs_c = 0
a_place = SPACE(FSIZE("LOCATE"))
a_phone = SPACE(FSIZE("PHONE"))
a_name  = SPACE(FSIZE("ARM_N", "A1_9901"))
IF .NOT. EMPTY(tmpArm)
  a_name  = A1_9901.ARM_N
  IF SEEK(tmpArm)
    prs_c = WHO
    a_place = LOCATE
    a_phone = PHONE
    IF SEEK(prs_c, "P_9901") .AND. prs_c # 0
      prs_n = ALLTRIM(P_9901.FAMILY)+" "+LEFT(P_9901.NAME,1)+" "+LEFT(P_9901.S_NAME,1)
    ELSE
      prs_c = 0
    ENDIF
  ENDIF
ENDIF

prs_n = PADR(prs_n, 28)

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

*┌─────────────────────────────────────────────────────┐
*│ Название ....:....!....:.  Телефон ....:....!....:. │
*│                                                     │
*│         Где находится ....:....!....:....!          │
*│                                                     │
*│   [ ] Пользователь ....:....!....:....!....:....!   │
*│                                                     │
*│          < OK Ctrl-W > < Отказаться Esc >           │
*└─────────────────────────────────────────────────────┘

PRIVATE ex, prs_sw
ex = 1
prs_sw = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 57, "Описание рабочего места"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2,  3 SAY "Название" GET a_name WHEN .NOT. hide_name
@ 2, 30 SAY "Телефон" GET a_phone
@ 4, 11 SAY "Где находится" GET a_place
@ 6,  5 GET prs_sw PICTURE "@*C Пользователь" VALID S_Prs()
@ 6, 22 GET prs_n WHEN .F.

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * Отрабатываем бланк
  IF EMPTY(tmpArm)
    SELECT A1_9901
    GO BOTTOM
    IF EOF() .OR. BOF()
      tmpArm = "  1"
    ELSE
      tmpArm = STR(VAL(ARM_C)+1, 3)
    ENDIF
    APPEND BLANK
    REPLACE ARM_C WITH tmpArm, ;
            ARM_S WITH sys_char
    SELECT A2_9901
  ENDIF
  IF .NOT. SEEK(tmpArm)
    APPEND BLANK
  ENDIF
  REPLACE ARM_C  WITH tmpArm, ;
          LOCATE WITH a_place,  ;
          PHONE  WITH a_phone,  ;
          WHO    WITH prs_c
  REPLACE A1_9901.ARM_N WITH a_name
ENDIF
*--------------------------------------------------------------------------

POP KEY
USE
SELECT A1_9901
USE
SELECT P_9901
USE
SELECT (s_sav)
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Prs        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Выбор сотрудника.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.09.1999 ──┘
PROCEDURE S_Prs

IF .NOT. prs_sw
  RETURN .T.
ENDIF

prs_sw = .F.

DO Ch_Pers WITH prs_c, prs_n

SHOW GET prs_sw
SHOW GET prs_n

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Проверка бланка.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.01.2004 ──┘
PROCEDURE Tst_Blank
PRIVATE mss

IF ex # 1
  RETURN .T.
ENDIF

IF EMPTY(a_name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Название рабочего места не может быть пустым."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
