*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Ord_Exe      Разработчик Андрей Васин           12.03.98 15:34:32 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Обработка заявок на складе.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Ord_Exe

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

RETURN	&&&&&&&&&&&&&&&&&&&&&&&&&&


stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
PRIVATE flt, ord
flt = .T.
ord = .T.

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

USE (base_path+"PERSONS") ORDER TAG CODE ALIAS STOCKMAN
SELECT 0

USE (base_path+"PERSONS") ORDER TAG CODE ALIAS SALEMAN AGAIN
SELECT 0

USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WORKER AGAIN
SELECT 0

USE (base_path+"ORD_T") ORDER TAG DOC_NUM_1
SET RELATION TO WHO INTO SALEMAN
SET RELATION TO STO_MARK INTO STOCKMAN ADDITIVE
SET RELATION TO VAL(ALLTRIM(CHECK_S)) INTO WORKER ADDITIVE
GO BOTTOM
DO Use_Dummy
SELECT ORD_T

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать"
ON PAD Print OF (menu_name) ACTIVATE POPUP Print

  DEFINE POPUP Print MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Print PROMPT "Формат заявки" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Print DO Sw_Mode WITH "Print_1"

  DEFINE BAR  2 OF Print PROMPT "Формат списка" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Print DO Sw_Mode WITH "Print_2"

  DEFINE BAR  3 OF Print PROMPT "Список" KEY Ctrl-A, "F4"
  ON SELECTION BAR 3 OF Print DO Sw_Mode WITH "Print_L"

  DEFINE BAR  4 OF Print PROMPT "Бронь" KEY Ctrl-A, "F5"
  ON SELECTION BAR 4 OF Print DO Sw_Mode WITH "Print_B"

  DEFINE BAR  5 OF Print PROMPT "Суммарная заявка" KEY Ctrl-A, "F7"
  ON SELECTION BAR 5 OF Print DO Sw_Mode WITH "Print_S"

DEFINE PAD Prepare OF (menu_name) PROMPT "В наборе" KEY Ctrl-A, "Tab"
ON SELECTION PAD Prepare OF (menu_name) DO Sw_Mode WITH "Prepare"

DEFINE PAD Execute OF (menu_name) PROMPT "Обработка" KEY Ctrl-A, "Enter"
ON SELECTION PAD Execute OF (menu_name) DO Sw_Mode WITH "Execute"

DEFINE PAD Order OF (menu_name) PROMPT "Хрон." KEY Ctrl-A, "F3"
ON SELECTION PAD Order OF (menu_name) DO Sw_Mode WITH "Order"

DEFINE PAD Filter OF (menu_name) PROMPT "Все заявки" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, pic_w
ln = 100   && Количество видимых строк BROWSE - во весь экран!
wd = 71
pic_w = Addr_Pic()

DO D_Wins WITH ln, wd, "Обработка заявок", 1, 3
what_do = "List"

@ 2, 2 SAY ;
"─N заявки─────Дата──────Время── Имя списка ─────────────────────Пр.─Ск."
DO WHILE .T.

  DO P_Mode

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab    DO Sw_Mode WITH "Prepare"
    ON KEY LABEL F1     DO Sw_Mode WITH "Print_1"
    ON KEY LABEL F2     DO Sw_Mode WITH "Print_2"
    ON KEY LABEL F4     DO Sw_Mode WITH "Print_L"
    ON KEY LABEL F5     DO Sw_Mode WITH "Print_B"
    ON KEY LABEL F7     DO Sw_Mode WITH "Print_S"
    ON KEY LABEL F3     DO Sw_Mode WITH "Order"
    ON KEY LABEL F6     DO Sw_Mode WITH "Filter"

*
*    BROWSE - меню
*
    BROWSE FIELDS F000 = ALLTRIM(LIST_NUM)+"-"+ALLTRIM(STR(ORD_NUM)):10:H="",   ;
                  ORD_DATE:10:H="", ORD_TIME:8:H="", ;
                  LIST_NAME:32:H="", ;
                  F001 = IIF(EMPTY(WHO), "   ", " √ "):3:H="", ;
                  F002 = IIF(EMPTY(STO_MARK), "   ", " √ "):3:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_Who() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Execute")
    ENDIF

  CASE what_do = "Prepare"    && Заявка в наборе

    DO Prepare WITH ORD_T.LIST_NUM, ORD_T.ORD_NUM
    what_do = "List"

  CASE what_do = "Execute"    && Обработка заявки

    DO Corr_Ord WITH ORD_T.LIST_NUM, ORD_T.ORD_NUM
    what_do = "List"

  CASE what_do = "Print_1"    && Печать заявки

    IF new_ord
      DO Prn_OrdN WITH VAL(ALLTRIM(ORD_T.LIST_NUM)), ORD_T.ORD_NUM
    ELSE
      DO Prn_Ord WITH VAL(ALLTRIM(ORD_T.LIST_NUM)), ORD_T.ORD_NUM
    ENDIF
    what_do = "List"

  CASE what_do = "Print_2"    && Печать списка

    DO Prn_Ord1 WITH VAL(ALLTRIM(ORD_T.LIST_NUM)), ORD_T.ORD_NUM
    what_do = "List"

  CASE what_do = "Print_L"    && Печать списка

    DO P_List WITH VAL(ALLTRIM(ORD_T.LIST_NUM))
    what_do = "List"

  CASE what_do = "Print_B"    && Печать брони

    DO P_Bron WITH VAL(ALLTRIM(ORD_T.LIST_NUM))
    what_do = "List"

  CASE what_do = "Print_S"    && Печать суммарной заявки

    DO Prn_OrdS WITH VAL(ALLTRIM(ORD_T.LIST_NUM))
    what_do = "List"

  CASE what_do = "Order"      && Переупорядочение

    ord = .NOT. ord
    DO P_Mode
    what_do = "List"

  CASE what_do = "Filter"     && Установка/снятие фильтра

    flt = .NOT. flt
    DO P_Mode
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
*║             Визуализация и установка текущего режима.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 12.03.98 16:16:52 ═╝
PROCEDURE P_Mode

DO CASE
CASE flt .AND. ord
  SET ORDER TO TAG DOC_NUM_1
CASE .NOT. flt .AND. ord
  SET ORDER TO TAG DOC_NUM
CASE flt .AND. .NOT. ord
  SET ORDER TO TAG HRON_1
CASE .NOT. flt .AND. .NOT. ord
  SET ORDER TO TAG HRON
ENDCASE

DEFINE PAD Order OF (menu_name) PROMPT IIF(ord, "Хрон.", "Номер")  ;
                BEFORE Filter KEY Ctrl-A, "F3"
ON SELECTION PAD Order OF (menu_name) DO Sw_Mode WITH "Order"

DEFINE PAD Filter OF (menu_name) PROMPT IIF(flt, "Все заявки", "Неутв.") ;
                BEFORE Exit KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

ACTIVATE WINDOW (win_name+"_M") SAME
@ 1, 5 SAY "Упоряд. по "+IIF(ord, "номеру  ", "хронол. ")+ ;
           "    Фильтр "+IIF(flt, "Неутв.", "Все   ")

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                                Footer!                                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 12.03.98 16:55:20 ═╝
PROCEDURE Show_Who
PRIVATE n_sal, n_sto, n_work, ln

ln = FSIZE("FAMILY","SALEMAN")
IF WHO = 0
  n_sal = ""
ELSE
  IF FOUND("SALEMAN")
    n_sal = ALLTRIM(SALEMAN.FAMILY)+" "+LEFT(SALEMAN.NAME,1)+". "+ ;
            LEFT(SALEMAN.S_NAME,1)+"."
  ELSE
    n_sal = "?"
  ENDIF
ENDIF
n_sal = PADR(n_sal, ln)

IF STO_MARK = 0
  n_sto = ""
ELSE
  IF FOUND("STOCKMAN")
    n_sto = ALLTRIM(STOCKMAN.FAMILY)+" "+LEFT(STOCKMAN.NAME,1)+". "+ ;
            LEFT(STOCKMAN.S_NAME,1)+"."
  ELSE
    n_sto = "?"
  ENDIF
ENDIF
n_sto = PADR(n_sto, ln)

IF CHECK_S = "   "
  n_work = ""
ELSE
  IF FOUND("WORKER")
    n_work = ALLTRIM(WORKER.FAMILY)+" "+LEFT(WORKER.NAME,1)+". "+ ;
            LEFT(WORKER.S_NAME,1)+"."
  ELSE
    n_work = ""
  ENDIF
ENDIF
n_work = PADR(n_work, ln)

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-4, 3 SAY "Продавец  "+n_sal
@ WROWS()-3, 3 SAY "Собирает  "+n_work
@ WROWS()-2, 3 SAY "Кладовщик "+n_sto

ACTIVATE WINDOW (ALIAS())

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                    Коррекция и утверждение заявки.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 12.03.98 18:29:40 ═╝
PROCEDURE Corr_Ord
PARAMETERS lst_n, ord_n

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
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

PRIVATE s_sav
s_sav = SELECT()

PRIVATE lst_n_w, mode, is_name, pic_w
pic_w = Addr_Pic()
is_name = .T.
lst_n_w = IIF(TYPE("lst_n") = "C", lst_n, STR(lst_n,6))
SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS OT_000 AGAIN
IF .NOT. SEEK(lst_n_w+STR(ord_n,3))
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет заявки с номером "+ALLTRIM(lst_n)+"/"+ALLTRIM(STR(ord_n))+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

SELECT 0
mode = .T.
IF OT_000.STO_MARK # 0
  PRIVATE mss
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Заявка "+ALLTRIM(lst_n)+"/"+ALLTRIM(STR(ord_n))+" утверждена!"
  mss(3) = CHR(0)+"Коррекция не доступна."
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  mode = .F.
ELSE
  USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LS_000 AGAIN
  IF .NOT. SEEK(VAL(ALLTRIM(lst_n_w)))
    PRIVATE mss
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Списка "+ALLTRIM(lst_n)+" не существует!"
    mss(3) = CHR(0)+"Коррекция заявки не доступна."
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
    mode = .F.
  ELSE
    IF .NOT. EMPTY(LS_000.SAL_NUM)
      PRIVATE mss
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"Список "+ALLTRIM(lst_n)+" уже продан!"
      mss(3) = CHR(0)+"Коррекция заявки не доступна."
      mss(4) = ""
      DO Out_Mess WITH 7, "mss"
      mode = .F.
    ENDIF
  ENDIF
ENDIF

USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_000 AGAIN

SELECT 0
DELETE FILE(tmpo_path+"O_TMP.DBF")
DELETE FILE(tmpo_path+"O_TMP.CDX")
CREATE DBF (tmpo_path+"O_TMP")  ;
  ( CODE     N( 7), ;
    ADDRESS  C( 8), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT_ORD  N( 7), ;
    QNT_RES  N( 7), ;
    DUMMY    C( 1)  )
USE (tmpo_path+"O_TMP") EXCLU
INDEX ON ADDRESS TAG ADDRESS
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE (base_path+"ORD_D") ORDER TAG DOC_NUM ALIAS ORD_000 AGAIN
SET RELATION TO CODE INTO ST_000

SEEK lst_n_w+STR(ord_n,3)
SCAN REST WHILE lst_n_w = LIST_NUM .AND. ord_n = ORD_NUM
  IF QNT = 0
    LOOP
  ENDIF
  SELECT O_TMP
  APPEND BLANK
  REPLACE CODE     WITH ORD_000.CODE,   ;
          ADDRESS  WITH ST_000.ADDRESS, ;
          PREFIX   WITH ST_000.PREFIX,  ;
          NAME     WITH ST_000.NAME,    ;
          PRODUCER WITH ST_000.PRODUCER,;
          QNT_ORD  WITH ORD_000.QNT,    ;
          QNT_RES  WITH ORD_000.QNT
  SELECT ORD_000
ENDSCAN

USE

SELECT O_TMP
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Заявка с номером "+ALLTRIM(lst_n)+"/"+ALLTRIM(STR(ord_n))+" пуста!"
  mss(3) = CHR(0)+"  Удалим? [ Да! ] [ Нет ]"
  mss(4) = ""
  USE
  SELECT OT_000
  IF Out_Mess(7, "mss") = 1
    DELETE
  ENDIF
  USE
  SELECT ST_000
  USE
  SELECT (s_sav)
  DELETE FILE(tmpo_path+"O_TMP.DBF")
  DELETE FILE(tmpo_path+"O_TMP.CDX")
  RETURN
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Order OF (menu_name) PROMPT "По адресам" KEY Ctrl-A, "F3"
ON SELECTION PAD Order OF (menu_name) DO Sw_Mode WITH "Order"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = RECCOUNT()   && Количество видимых строк BROWSE
wd = 64

DO D_Wins WITH ln, wd, "Заявка N "+ALLTRIM(lst_n)+"/"+ALLTRIM(STR(ord_n)), 0, 0
@ 1, 2 SAY ;
"──Адрес────────Наименование───────────────────────Заказ─Отпуск──"
what_do = "List"

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F3 DO Sw_Mode WITH "Order"

*
*    BROWSE - меню
*
    IF mode
      BROWSE FIELDS ADDRESS:H="":P=pic_w, PREFIX:H="", NAME:H="",   ;
                    PRODUCER:H="", QNT_ORD:H="", QNT_RES:H="",   ;
                    F000 = IIF(QNT_ORD=QNT_RES, " ", "√"):1:H="", ;
                    DUMMY:H="" FREEZE QNT_RES ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS ADDRESS:H="":P=pic_w, PREFIX:H="", NAME:H="",   ;
                    PRODUCER:H="", QNT_ORD:H="", QNT_RES:H="",   ;
                    F000 = IIF(QNT_ORD=QNT_RES, " ", "√"):1:H="", ;
                    DUMMY:H="" FREEZE DUMMY ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Order"    && Переупорядочение

    is_name = .NOT. is_name
    IF is_name
      SET ORDER TO TAG NAME
      DEFINE PAD Order OF (menu_name) PROMPT "По адресам"   ;
            BEFOR Exit KEY Ctrl-A, "F3"
    ELSE
      SET ORDER TO TAG ADDRESS
      DEFINE PAD Order OF (menu_name) PROMPT "По именам"   ;
            BEFOR Exit KEY Ctrl-A, "F3"
    ENDIF    
    what_do = "List"

  OTHERWISE

    IF Terminate()
      EXIT
    ELSE
      what_do = "List"
    ENDIF

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
USE
DELETE FILE(tmpo_path+"O_TMP.DBF")
DELETE FILE(tmpo_path+"O_TMP.CDX")
SELECT OT_000
USE
SELECT ST_000
USE

SELECT (s_sav)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                               В работе.                                ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.03.98 09:53:59 ═╝
PROCEDURE Prepare
PARAMETERS lst_n, ord_n
PRIVATE s_sav

s_sav = SELECT()

SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS OT_000 AGAIN
IF .NOT. SEEK(lst_n+STR(ord_n,3))
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет заявки с номером "+ALLTRIM(lst_n)+"/"+ALLTRIM(STR(ord_n))+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

IF OT_000.STO_MARK # 0 .OR. OT_000.CHECK_S # "   "
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Заявка утверждена!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

IF EMPTY(OT_000.CHECK_S)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Заявка уже в наборе! Родолжаем работу? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌─────────────────────────────────────────────┐
*│ Кто собирает (пароль) ▓▓▓▓▓▓▓▓ (невидимый!) │
*│                                             │
*│      < OK Ctrl-W > < Вернуться Esc >        │
*└─────────────────────────────────────────────┘

PRIVATE ex, s_mark, p_w, pw_clr, user_w, note_w
                     &&   Объявляем и заполняем поля бланка
pw_clr = SCHEME(13,2)
ex = SUBSTR(pw_clr,AT("/",pw_clr)+1)
pw_clr = ","+IIF(RIGHT(ex,1) # "*", ex+"/"+ex,  ;
         LEFT(ex,LEN(ex)-1)+"+/"+ex)

user_w = 0
ex   = 1
s_mark = .T.
p_w = SPACE(8)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 49, "Представьтесь..."
@ 2, 34 SAY "(невидимый!)"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
*@ 2,  7 GET s_mark PICTURE "@*C Утвердить и создать контрзаявку" VALID Clr_Pw()
@ 2,  3 SAY "Кто собирает (пароль)" GET p_w VALID Seek_User() COLOR (pw_clr)
@ 4,  8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Pw()

IF ex = 1
  REPLACE CHECK_S WITH TRANSFORM(user_w, "@Z 999")
ENDIF
USE
SELECT (s_sav)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Очистка пароля.                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.03.98 12:23:08 ═╝
PROCEDURE Clr_Pw

p_w = SPACE(8)
SHOW GET p_w

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Поиск пользователя по паролю.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.03.98 12:26:04 ═╝
PROCEDURE Seek_User

PRIVATE s_sav, w

IF ex = 2 .OR. READKEY() % 246 = 12
  RETURN .T.
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"USERS") ORDER TAG PASSWORD ALIAS PW000 AGAIN

w = PADR(p_w,FSIZE("PASSWORD"))
w = SYS(15,lwr,w)
DO SetPrice WITH "superamv", w
IF .NOT. SEEK(w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Некорректный пароль, попробуйте еще раз!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  KEYBOARD "{Home}"+REPLICATE("{Del}",8)
  RETURN .F.
ENDIF
PRIVATE mss
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  "+ALLTRIM(USER_NAME)+"? [ Да ] [ Нет ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 1
  user_w = USER_CODE
  USE
  SELECT (s_sav)
  RETURN .T.
ENDIF
KEYBOARD "{Home}"+REPLICATE("{Del}",8)
user_w = 0
USE
SELECT (s_sav)
RETURN .F.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        А был ли введен пароль?                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.03.98 13:01:06 ═╝
PROCEDURE Tst_Pw

IF ex = 2
  RETURN .T.
ENDIF

IF user_w = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Кто Вы? Вы забыли представиться!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Завершение работы с заявкой.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.03.98 09:53:59 ═╝
PROCEDURE Terminate

*
*  Переменные состояния для навигации
*
IF OT_000.STO_MARK # 0
  RETURN
ENDIF

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

*┌─────────────────────────────────────────────┐
*│ Кто утвердил (пароль) ▓▓▓▓▓▓▓▓ (невидимый!) │
*│                                             │
*│      < OK Ctrl-W > < Вернуться Esc >        │
*└─────────────────────────────────────────────┘

PRIVATE ex, s_mark, p_w, pw_clr, user_w, note_w
                     &&   Объявляем и заполняем поля бланка
pw_clr = SCHEME(13,2)
ex = SUBSTR(pw_clr,AT("/",pw_clr)+1)
pw_clr = ","+IIF(RIGHT(ex,1) # "*", ex+"/"+ex,  ;
         LEFT(ex,LEN(ex)-1)+"+/"+ex)

user_w = 0
ex   = 1
s_mark = .T.
p_w = SPACE(8)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 49, "Как завершим работу?"
@ 2, 34 SAY "(невидимый!)"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
*@ 2,  7 GET s_mark PICTURE "@*C Утвердить и создать контрзаявку" VALID Clr_Pw()
@ 2,  3 SAY "Кто утвердил (пароль)" GET p_w VALID Seek_User() COLOR (pw_clr)
@ 4,  8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Pw() .AND. Same_User()

IF ex = 1
  PRIVATE s_sav, cnt, o_n, r_sav
  s_sav = SELECT()
  SELECT OT_000
  r_sav = RECNO()
  SELECT 0
  USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS OLT_000 AGAIN
  SELECT 0
  USE (base_path+"LIST_DET") ORDER TAG L_CODE ALIAS OLD_000 AGAIN
  SELECT 0
  USE (base_path+"ORD_D") ALIAS OD_000 AGAIN
  *
  *   Выставляем пометку об утверждении
  *
  cnt = .T.
  IF .NOT. EMPTY(OT_000.STO_MARK)
    cnt = .F.
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Список уже утваржден..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    REPLACE OT_000.STO_MARK WITH user_w
  ENDIF
  *  Ищем скорректированную позицию
  SELECT O_TMP
  LOCATE FOR QNT_ORD # QNT_RES
  IF .NOT. SEEK(VAL(ALLTRIM(OT_000.LIST_NUM)), "OLT_000")
    cnt = .F.    && Нет списка - не формируем заявку!
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Список уже удален..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    IF .NOT. EMPTY(OLT_000.SAL_NUM)    && Список продан - не формируем заявку!
      cnt = .F.
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Список уже продан..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ENDIF
  ENDIF
  IF FOUND() .AND. cnt

    && Нашли, начинаем формировать заявку и корректировать список
    &&     и заявку все же надо сформировать...
*    note_w = Ord_Note("N "+ALLTRIM(STR(OLT_000.LST_NUM,6))+", "+ ;
*                      ALLTRIM(OLT_000.LST_NAME))
    o_n = OLT_000.ORD_NUM+1
    REPLACE OLT_000.ORD_NUM WITH o_n      && Номер заявки
    SELECT OT_000
    APPEND BLANK             && Заголовок заявки...
    REPLACE LIST_NUM  WITH STR(OLT_000.LST_NUM,6), ;
            LIST_NAME WITH OLT_000.LST_NAME, ;
            ORD_NUM   WITH o_n,        ;
            ORD_DATE  WITH DATE(),     ;
            ORD_TIME  WITH TIME(),     ;
            STO_MARK  WITH user_w,     ;
            WHO       WITH IIF(RIGHT(OLT_000.LST_NAME,1) = CHR(0), user_w, 0), ;
            WHERE     WITH arm
    SELECT O_TMP
    *
    *  ...и содержимое заявки...
    SCAN REST FOR QNT_ORD # QNT_RES
      SELECT OD_000
      APPEND BLANK
      REPLACE LIST_NUM WITH OT_000.LIST_NUM, ;
              ORD_NUM  WITH OT_000.ORD_NUM,  ;
              CODE     WITH O_TMP.CODE,      ;
              QNT      WITH O_TMP.QNT_RES-O_TMP.QNT_ORD
      *
      *   Количества на складе...
      = SEEK(CODE, "ST_000")
      REPLACE ST_000.QNT WITH ST_000.QNT-QNT, ;
              ST_000.QNT_SAVE WITH ST_000.QNT_SAVE+QNT
      *
      *   Количества в списке...
      = SEEK(LIST_NUM+STR(CODE,7), "OLD_000")
      REPLACE OLD_000.QNT     WITH OLD_000.QNT+QNT, ;
              OLD_000.QNT_REQ WITH OLD_000.QNT_REQ+QNT, ;
              OLD_000.QNT_DUE WITH OLD_000.QNT_DUE - ;
                  IIF(RIGHT(OLT_000.LST_NAME,1) = CHR(0), QNT, 0)
      SELECT O_TMP
      IF OD_000.QNT < 0 .AND. File_O(base_path+"ABSENT.DBF")
        DO Absent WITH OD_000.CODE
      ENDIF
      
    ENDSCAN
  ENDIF
  IF FOUND("OLT_000") .AND. EMPTY(OLT_000.SAL_NUM)
    *
    *
    *  Корректируем заголовок списка в части:
    * суммы, наличия дефицита, наличия долга, наличия скидки,
    * наличия спец. цен, неутв. заявок.

    PRIVATE ww_0, ww_1, ww_2, ww_3, ww_4, ww_5, ww_6
    STORE " " TO ww_1, ww_2, ww_3, ww_4, ww_5, ww_6
    ww_0 = 0
    SELECT OLD_000
    SEEK STR(OLT_000.LST_NUM,6)
    SCAN REST WHILE LIST_NUM = STR(OLT_000.LST_NUM,6)
      ww_0 = ww_0+QNT*IIF(OLT_000.IS_USD, USD_PRICE, SAL_PRICE)    && Сумма
      *
      *  Дефицит
      = SEEK(CODE,"ST_000")
      IF MAX(QNT-QNT_REQ, 0) > MAX(ST_000.QNT,0)
        ww_1 = "*"
      ENDIF 
      *
      *  Долг
      IF QNT_DUE # 0
        ww_2 = "*"
      ENDIF 
      *
      *  Скидка или спец. цена
      IF IIF(OLT_000.IS_USD, USD_DEF, DEF_PRICE) # IIF(OLT_000.IS_USD, USD_PRICE, SAL_PRICE)
        IF EMPTY(IS_SPEC)
          ww_3 = "*"    && Скидка
        ELSE
          ww_4 = "*"    && Спец.цена
        ENDIF
      ENDIF
    ENDSCAN
    SELECT OT_000
    SEEK STR(OLT_000.LST_NUM,6)
    SCAN REST WHILE LIST_NUM = STR(OLT_000.LST_NUM,6)
      IF EMPTY(STO_MARK)  && Не утверждена на складе
        ww_5 = "*"
      ENDIF
      IF EMPTY(WHO)       && Не утверждена на продаже
        ww_6 = "*"
      ENDIF
    ENDSCAN
    SELECT OLT_000
    REPLACE OLT_000.R_TOTAL  WITH ww_0, ;
            OLT_000.IS_DEF   WITH ww_1, ;
            OLT_000.IS_DUE   WITH ww_2, ;
            OLT_000.IS_DIS   WITH ww_3, ;
            OLT_000.IS_SPEC  WITH ww_4, ;
            OLT_000.NOTE_STO WITH ww_5, ;
            OLT_000.NOTE_SAL WITH ww_6
    SELECT OT_000
  ENDIF
  IF BETWEEN(r_sav,1,RECCOUNT())
    GO r_sav
  ENDIF
  SELECT OLT_000
  USE
  SELECT OLD_000
  USE
  SELECT OD_000
  USE
  SELECT (s_sav)

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                    А тот ли утверждает заявку?!                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.03.98 09:53:59 ═╝
PROCEDURE Same_User

PRIVATE mss, rc_sv

IF EMPTY(OT_000.CHECK_S) .OR. ex = 2 .OR. READKEY() %256 = 12
  RETURN .T.
ENDIF

IF TRANSFORM(user_w,"@Z 999") # OT_000.CHECK_S
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"А собирал заявку "+ALLTRIM(WORKER.FAMILY)+"!"
  mss(3) = CHR(0)+"  Это правильно? [ Да ] [ Нет ]"
  mss(4) = ""
  IF Out_Mess(7, "mss") = 1
    RETURN .T.
  ELSE
    RETURN .F.
  ENDIF

ENDIF

RETURN .T.
