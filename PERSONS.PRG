*╔══════════════════════════════════════════════════════════════════════╗
*║                                                                      ║
*║           Программа работы со списком сотрудников.                   ║
*║                                                                      ║
*╚══════════════════════════════════════════════════════════════════════╝
PROCEDURE Persons

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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
*
*  Список отделов предприятия
*
PRIVATE dep_c, dep_n, dep_ln, mss
USE (base_path+"DEPS") ORDER TAG B_NAME
dep_ln = 0
SCAN
  dep_ln = dep_ln+1
  DIMENSION dep_c(dep_ln), dep_n(dep_ln)
  dep_c(dep_ln) = CODE
  dep_n(dep_ln) = B_NAME
ENDSCAN
USE

IF dep_ln = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного отдела!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "add"

DEFINE PAD Edit OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Edit OF (menu_name) Do Sw_Mode WITH "edit"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

USE (base_path+"PERSONS") ORDER TAG NAME
DO Use_Dummy
SELECT PERSONS

*
*   Расчет размеров окна
*
PRIVATE ln, wd, pic
ln = 100   && Количество видимых строк BROWSE
wd = FSIZE("FAMILY")+5
pic = "@S"+ALLTRIM(STR(FSIZE("FAMILY")+5))

DO D_Wins WITH ln, wd, "Список сотрудников", 0, 0

IF EOF() .OR. BOF()
  what_do = "add"
ELSE
  what_do = "list"
ENDIF

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "add"
    DO Key_Seek

*
*    BROWSE - меню
*
    BROWSE FIELDS FIO = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+"."+LEFT(S_NAME,1)+".":H="":P=pic, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E") COLOR SCHEME 13
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "edit")
    ENDIF

  CASE what_do = "add"    && Добавить

    DO Corr_Name WITH "A"
    IF EOF() .OR. BOF()
      what_do = "exit"
    ELSE
      what_do = "list"
    ENDIF

  CASE what_do = "edit"   && Корректировать

    DO Corr_Name WITH "C"
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN


*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║             Программа коррекции карточки сотрудника.                  ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Corr_Name
PARAMETER mode

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

PRIVATE ex, fm, nm, slr, s_nm, n_d, dt_0, pos, wp1, wp2, pl, rm, hadd, bd, w
PRIVATE hp1, hc1, hpNote1, hp2, hc2, hpNote2, hp3, hc3, hpNote3
PRIVATE hr1, hr2, mn1, mn2, tmpEml
                                    &&   Объявляем и заполняем поля бланка
ex   = 1                            &&

IF mode = "A"
  fm   = SPACE(FSIZE("FAMILY"))
  nm   = SPACE(FSIZE("NAME"))
  s_nm = SPACE(FSIZE("S_NAME"))
  n_d  = 1
  slr  = 0
  dt_0 = {}
  pos  = SPACE(FSIZE("POSITION"))
  wp1  = SPACE(FSIZE("W_PHONE1"))
  wp2  = SPACE(FSIZE("W_PHONE2"))
  tmpEml  = SPACE(FSIZE("EML"))
  pl   = SPACE(FSIZE("PLACE"))
  rm   = SPACE(FSIZE("ROOM"))
  hadd = SPACE(FSIZE("H_ADDRESS"))
  hc1  = SPACE(FSIZE("H_P_CODE1"))
  hp1  = SPACE(FSIZE("H_PHONE1"))
  hpNote1  = SPACE(FSIZE("H_P_NOTE1"))
  hc2  = SPACE(FSIZE("H_P_CODE2"))
  hp2  = SPACE(FSIZE("H_PHONE2"))
  hpNote2  = SPACE(FSIZE("H_P_NOTE2"))
  hc3  = SPACE(FSIZE("H_P_CODE3"))
  hp3  = SPACE(FSIZE("H_PHONE3"))
  hpNote3  = SPACE(FSIZE("H_P_NOTE3"))
  bd   = {}
  hr1  = 9
  mn1  = 0
  hr2  = 18
  mn2  = 0
ELSE
  fm   = FAMILY
  nm   = NAME
  s_nm = S_NAME
  n_d  = ASCAN(dep_c, DEP_NO)
  slr  = SALARY
  DO ClrPRice WITH "Пиастры!", slr
  slr  = VAL(ALLTRIM(slr))
  dt_0 = DATE_IN
  pos  = POSITION
  wp1  = W_PHONE1
  wp2  = W_PHONE2
  tmpEml = EML
  pl   = PLACE
  rm   = ROOM
  hadd = H_ADDRESS
  hc1  = H_P_CODE1
  hp1  = H_PHONE1
  hpNote1  = H_P_NOTE1
  hc2  = H_P_CODE2
  hp2  = H_PHONE2
  hpNote2  = H_P_NOTE2
  hc3  = H_P_CODE3
  hp3  = H_PHONE3
  hpNote3  = H_P_NOTE3
  bd   = BIRTHDAY
  hr1  = VAL(LEFT(TIME_BEG,2))
  mn1  = VAL(SUBSTR(TIME_BEG,4,2))
  hr2  = VAL(LEFT(TIME_END,2))
  mn2  = VAL(SUBSTR(TIME_END,4,2))
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 25, 70, "Заполните бланк"
DO Sun_Bord WITH 2, 2, 4, 67
@ 2,  4 SAY " Фамилия "
@ 2, 29 SAY " Имя "
@ 2, 46 SAY " Отчество "
@ 7,  4 SAY "Отдел"
DO Sun_Bord WITH 6, 32, 8, 65
@ 6, 34 SAY " Должность "
*DO Sun_Bord WITH 9, 3, 11, 66
*@ 9, 13 SAY " Раб. телефоны "
@ 9, 5 SAY "Раб. телефоны:"
DO Sun_Bord WITH 12, 3, 14, 66
@ 12,  5 SAY " Место работы "


DO Sun_Bord WITH 16, 2, 21, 67
@ 16,  4 SAY " Дом. адрес "
*┌──────────────────────────────────────────────────────────────────┐
*│┌─ Фамилия ──────────────── Имя ──────────── Отчество ───────────┐│
*││ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█ ││
*│└────────────────────────────────────────────────────────────────┘│
*│               Дата поступления на работу ДД.ММ.ГГ                │
*│        ┌──────────────────╖  ┌─ Должность ────────────────────┐  │
*│  Отдел │ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒ ║  │ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█ │  │
*│        ╘══════════════════╝  └────────────────────────────────┘  │
*│   Раб. телефоны: Городской ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒  Внутренний ▒▒▒▒▓▒   │
*│                                                                  │
*│E-mail▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█│
*│  Время начала работы 99ч 99м     Время окончания работы 99ч 99м  │
*│ ┌─ Место работы ───────────────────────────────────────────────┐ │
*│ │    Площадка ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒     Комната ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒    │ │
*│ └──────────────────────────────────────────────────────────────┘ │
*│┌─ Адрес ────────────────────────────────────────────────────────┐│
*││  ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█  ││
*││  Код города ▒▒▒▒▓▒▒▒▒█   Тел. ▒▒▒▒▓▒▒▒▒█   Прим. ▒▒▒▒▓▒▒▒▒█▒▒  ││
*││  Код города ▒▒▒▒▓▒▒▒▒█   Тел. ▒▒▒▒▓▒▒▒▒█   Прим. ▒▒▒▒▓▒▒▒▒█▒▒  ││
*││  Код города ▒▒▒▒▓▒▒▒▒█   Тел. ▒▒▒▒▓▒▒▒▒█   Прим. ▒▒▒▒▓▒▒▒▒█▒▒  ││
*│└────────────────────────────────────────────────────────────────┘│
*│                      Дата рождения ДД.ММ.ГГ                      │
*│                 < OK Ctrl-W > < Отказаться Esc >                 │
*└──────────────────────────────────────────────────────────────────┘

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3,  4 GET fm
@ 3, 29 GET nm
@ 3, 46 GET s_nm
@ 5, 17 SAY "Дата поступления на работу" GET dt_0
*@ 5, 49 SAY "Оклад" GET slr PICTURE "@Z 99999"
@ 6, 10 GET n_d PICTURE "@^" SIZE 1, 20 FROM dep_n
@ 7, 34 GET pos
@  9, 20 SAY "Городской" GET wp1
@  9, 48 SAY "Внутренний" GET wp2
@ 11,  2 SAY "E-mail"
@ 11,  8 GET tmpEml
@ 13,  8 SAY "Площадка" GET pl
@ 13, 38 SAY "Комната" GET rm
@ 15,  4 SAY "Время начала работы 99ч 99м     Время окончания работы 99ч 99м"
@ 15, 24 GET hr1 RANGE 0, 23 PICTURE "99"
@ 15, 28 GET mn1 RANGE 0, 59 PICTURE "99"
@ 15, 59 GET hr2 RANGE 0, 23 PICTURE "99"
@ 15, 63 GET mn2 RANGE 0, 59 PICTURE "99"
@ 17,  5 GET hadd PICTURE "@S60"
@ 18,  5 SAY "Код города" GET hc1
@ 18, 29 SAY "Тел." GET hp1
@ 18, 47 SAY "Прим." GET hpNote1
@ 19,  5 SAY "Код города" GET hc2
@ 19, 29 SAY "Тел." GET hp2
@ 19, 47 SAY "Прим." GET hpNote2
@ 20,  5 SAY "Код города" GET hc3
@ 20, 29 SAY "Тел." GET hp3
@ 20, 47 SAY "Прим." GET hpNote3
@ 22, 24 SAY "Дата рождения" GET bd
@ 23, 19 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  IF mode = "A"
    SET ORDER TO TAG CODE
    GO BOTTOM
    IF EOF() .OR. BOF()
      w = 1
    ELSE
      w = CODE+1
    ENDIF
    APPEND BLANK
    REPLACE CODE  WITH w,   ;
            WHO   WITH user,;
            WHERE WITH arm
    SET ORDER TO TAG NAME
    PRIVATE tmpNum1, tmpNum2, s_dt, r_dt

    s_dt = SET("DATE")
    SET DATE GERMAN
    r_dt = "01.01."+STR(YEAR(DATE())+1,4)
    r_dt = CTOD(r_dt)
    r_dt = r_dt-dt_0
    SET DATE &s_dt
    tmpNum1 = "    0"
    tmpNum2 = "         0"
    DO SetPrice WITH "Пиастры!", tmpNum1
    DO SetPrice WITH "Пиастры!", tmpNum2
    SELECT 0
    USE (base_path+"PS_STAT") AGAIN ALIAS PSST1514
    APPEND BLANK
    REPLACE CODE      WITH PERSONS.CODE, ;
            START     WITH tmpNum2,      ;
            REST      WITH tmpNum2,      ;
            SAV_REST  WITH tmpNum2,      ;
            SALARY    WITH tmpNum2,      ;
            PREV_SUM  WITH tmpNum1,      ;
            PREV_SUMD WITH tmpNum1,      ;
            PAY_SUM   WITH tmpNum1,      ;
            PAY_VAL   WITH "Р",          ;
            REST_ILL  WITH ROUND(7*r_dt/365,0), ;
            REST_OTP  WITH ROUND(30*r_dt/365,0),;
            S_DATE    WITH dt_0
    USE
    SELECT PERSONS
  ENDIF
  hr1  = RIGHT(STR(100+hr1,3),2)+":"+RIGHT(STR(100+mn1,3),2)+":00"
  hr2  = RIGHT(STR(100+hr2,3),2)+":"+RIGHT(STR(100+mn2,3),2)+":00"
  slr  = STR(slr, FSIZE("SALARY"))
  DO SetPRice WITH "Пиастры!", slr
  REPLACE FAMILY  WITH fm,        ;
          NAME    WITH nm,        ;
          S_NAME  WITH s_nm,      ;
          SALARY  WITH slr,       ;
          DEP_NO  WITH dep_c(n_d),;
          POSITION WITH pos,      ;
          DATE_IN WITH dt_0,      ;
          W_PHONE1 WITH wp1,      ;
          W_PHONE2 WITH wp2,      ;
          EML     WITH tmpEml,    ;
          PLACE   WITH pl,        ;
          ROOM    WITH rm,        ;
          H_ADDRESS WITH hadd,    ;
          H_P_CODE1 WITH hc1,     ;
          H_PHONE1 WITH hp1,      ;
          H_P_NOTE1 WITH hpNote1, ;
          H_P_CODE2 WITH hc2,     ;
          H_PHONE2 WITH hp2,      ;
          H_P_NOTE2 WITH hpNote2, ;
          H_P_CODE3 WITH hc3,     ;
          H_PHONE3 WITH hp3,      ;
          H_P_NOTE3 WITH hpNote3, ;
          TIME_BEG  WITH hr1,     ;
          TIME_END  WITH hr2,     ;
          BIRTHDAY WITH bd
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║             Программа проверки корректности полей.                    ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Tst_Blank

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF EMPTY(fm) .OR. EMPTY(nm) .OR. EMPTY(s_nm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ни фамилия, ни имя, ни отчество не должны быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(dt_0)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"А как-же дата поступления на работу?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF slr < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Здесь что-то не так: таких окладов не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
