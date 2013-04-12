*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура CheckNum     Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                "Дефектные" номера денежных документов.                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.03.2002 ──┘
PROCEDURE CheckNum
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

PRIVATE s_sav
s_sav = SELECT()
*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌──────────────────────────────────────┐
*│            Год   9999                │
*│                                      │
*│Предприятие .... ....:....!....:. (F1)│
*│                                      │
*│      ┌─ Тип документа ───────┐       │
*│      │                       │       │
*│      │  ( ) Приходный ордер  │       │
*│      │  ( ) Расходный ордер  │       │
*│      │  ( ) Индекс. платежи  │       │
*│      │  ( ) Б/Н (рубли)      │       │
*│      │  ( ) Б/Н (USD)        │       │
*│      │  ( ) Сбербанк         │       │
*│      │  ( ) Чек              │       │
*│      └───────────────────────┘       │
*│                                      │
*│    < OK Ctrl-W > < Отказаться Esc >  │
*└──────────────────────────────────────┘

PRIVATE ex, year_d, f_c000, f_n000, f_c_old, pay_t, w1, pay_doc
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
ex = 1
year_d = STR(year(DATE()),4)
f_c000 = "   "
f_n000 = SPACE(16)
f_c_old = f_c000
w1 = 1
pay_t = " "
pay_doc = "INC"
*------------------------------------------------------------------------
PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 20, 41, ""
DO Sun_Bord WITH  7,  8, 16, 32, " Тип документа "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  14 SAY "Год   " GET year_d PICTURE "9999" VALID S_Year(year_d)
DO Get_Firm WITH 5, 2, .T.
@  9, 10 GET w1 PICTURE "@*VR П/O ;Р/O ;Инд.плат. ;Б/Н (руб) ;Б/Н (USD) ;Чек ;С/Б"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_F()

IF ex = 1

  DO CASE
    CASE w1 = 1
      pay_t = " "
      pay_doc = "INC"
    CASE w1 = 2
      pay_t = " "
      pay_doc = "EXP"
    CASE w1 = 3
      pay_t = "3"
    CASE w1 = 4
      pay_t = "1"
    CASE w1 = 5
      pay_t = "4"
    CASE w1 = 6
      pay_t = "Ч"
    CASE w1 = 7
      pay_t = "Б"
  ENDCASE
  
  DO PREP_001 WITH pay_t, pay_doc
  DO DEF_NUM

ENDIF

POP KEY
RELEASE WINDOW (win_name)

SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_F        Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Корректность заполнения бланка.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.03.2002 ──┘
PROCEDURE Tst_F
PRIVATE s, mss
IF READKEY() % 256 = 12
  ex = 2
ENDIF
IF ex # 1
  RETURN .T.
ENDIF
IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали предприятие!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Year       Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Проверка ввода года.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.03.2002 ──┘
PROCEDURE S_Year
PARAMETERS t_dat

PRIVATE mss, d_001
d_001 = YEAR(DATE())

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

SELECT 0
USE (base_path+"PARMS") ALIAS P_2307 AGAIN
s_dat = YEAR(DAT_START)
USE

IF VAL(t_dat) < s_dat
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Имеем документы только с "+STR(s_dat,4)+" года!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF VAL(t_dat) > d_001
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Сегодня еще только "+STR(d_001,4)+" год!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура DEF_NUM      Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Список номеров денежных документов.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.03.2002 ──┘
PROCEDURE DEF_NUM
PRIVATE r_n, d_n, rec_n, cnt_r
cnt_r = 0

SELECT NUM_001
GO TOP
r_n = 1
SCAN
  d_n = RIGHT(DOC_N,9)
  IF r_n # ABS(VAL(d_n))
    REPLACE BAD WITH .T.
    cnt_r = cnt_r + 1
    IF .NOT.BOF()
      rec_n = RECNO()
      SKIP -1
      REPLACE BAD WITH .T.
      GO rec_n
    ENDIF
  ENDIF
  r_n = ABS(VAL(d_n)) + 1
ENDSCAN

IF cnt_r = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного дефектного документа!"
  mss(3) = ""
  DO Wt_Mess 
  DO Out_Mess WITH 5, "mss"
  USE
  RETURN
ELSE
  DO VIEW_DEF
ENDIF

SELECT NUM_001
USE

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура VIEW_DEF     Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│              Просмотр списка номеров денежных документов.              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.03.2002 ──┘
PROCEDURE VIEW_DEF
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

SELECT 0
DO Use_Dummy

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View_D OF (menu_name) PROMPT "Документ" KEY Ctrl-A, "Enter"
ON SELECTION PAD View_D OF (menu_name) DO Sw_Mode WITH "Prnt"

DEFINE PAD Filter_D OF (menu_name) PROMPT "Дефектные номера/Все" KEY Ctrl-A, "F1"
ON SELECTION PAD Filter_D OF (menu_name) DO Sw_Mode WITH "Fltr"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

SELECT NUM_001
SET ORDER TO TAG DOC_N
*
*   Расчет размеров окна
*
PRIVATE ln, wd, tit_doc, tit_firm, flt
flt = .F.
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 68 
tit_doc = "Приходный ордер"
tit_firm = f_n000
DO CASE
  CASE pay_t = " "
    IF pay_doc = "INP"
      tit_doc = "Приходный ордер"
    ENDIF
    IF pay_doc = "EXP"
      tit_doc = "Расходный ордер"
    ENDIF
  CASE pay_t = "3"
    tit_doc = "Индекс. платежи"
  CASE pay_t = "1"
    tit_doc = "Безналичные платежи (рубли)"
  CASE pay_t = "4"
    tit_doc = "Безналичные платежи (USD)"
  CASE pay_t = "Б"
    tit_doc = "Сбербанк"
  CASE pay_t = "Ч"
    tit_doc = "Чек"
ENDCASE    
    
DO D_Wins WITH ln, wd, "Предприятие:  "+tit_firm, 0, 0
@ 1, 3 SAY "Тип документа:  "+tit_doc
@ 2, 2 SAY "──N док-та──────Дата────────────Клиент────────────────────Сумма"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1 DO Sw_Mode WITH "Fltr"

*    BROWSE - меню
*
*                  (LEFT(DOC_N,1)+ALLTRIM(RIGHT(DOC_N,9))):H="":10
    BROWSE FIELDS s0001=IIF(BAD,"√"," "):H="":1,;  
                  s0002=(LEFT(DOC_N,1)+ALLTRIM(RIGHT(DOC_N,9))):H="":10,  ;
                  DOC_D:H="":10,  ;
                  ACC_N:H="":30,  ;
                  SUM_T:H="":P=IIF(pay_t="3".OR.pay_t="4","@Z 999999999.999","@Z 9999999999.99"),;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Prnt")
    ENDIF

  CASE what_do = "Prnt"    && Печать документа

    PRIVATE d_num
    d_num = DOC_N
    DO P_money WITH f_c000, pay_t, d_num, year_d
    what_do = "List"

  CASE what_do = "Fltr"    && Фильтр : дефектные номера/все

    IF .NOT. flt
      SET FILTER TO BAD
      flt = .T.
    ELSE
      SET FILTER TO
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
RELEASE MENU (menu_name) EXTENDED

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура PREP_001     Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Создание временного файла.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.03.2002 ──┘
PROCEDURE PREP_001
PARAMETERS t_001, d_001
PRIVATE key_s

DO Wt_Mess WITH "Подготовка списка..."

SELECT 0
DELETE FILE (tmpo_path+"NUM_001.DBF")
DELETE FILE (tmpo_path+"NUM_001.CDX")
CREATE DBF (tmpo_path+"NUM_001.DBF") ;
      (DOC_N C(10),  ;
       DOC_D D,      ;  
       DOC_T C( 1),  ; 
       ACC_C N( 7),  ;
       ACC_N C(50),  ;
       SUM_T N(15,3),;
       BAD   L       )
INDEX ON DOC_N TAG DOC_N

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2306 AGAIN

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS MON_2306 AGAIN
SET RELATION TO CUS_CODE INTO ACC_2306

key_s = f_c000+t_001+year_d

SELECT MON_2306
IF d_001 = "INC"
  SET FILTER TO .NOT."-" $ DOC_NUM
  SCAN FOR FIRM+DOC_TYPE+LEFT(DTOS(DOC_DATE),4)+DOC_NUM = key_s
    SELECT NUM_001
    APPEND BLANK
    REPLACE DOC_N WITH MON_2306.DOC_NUM,  ;
            DOC_D WITH MON_2306.DOC_DATE, ;  
            ACC_C WITH MON_2306.CUS_CODE, ;
            ACC_N WITH ACC_2306.CUS_NAME, ;
            SUM_T WITH MON_2306.MONEY_R+MON_2306.N_OF_SALE
    SELECT MON_2306
  ENDSCAN
ENDIF

IF d_001 = "EXP"
  SET FILTER TO "-" $ DOC_NUM
  SCAN FOR FIRM+DOC_TYPE+LEFT(DTOS(DOC_DATE),4)+DOC_NUM = key_s
    SELECT NUM_001
    APPEND BLANK
    REPLACE DOC_N WITH MON_2306.DOC_NUM,  ;
            DOC_D WITH MON_2306.DOC_DATE, ;  
            ACC_C WITH MON_2306.CUS_CODE, ;
            ACC_N WITH ACC_2306.CUS_NAME, ;
            SUM_T WITH MON_2306.MONEY_R+MON_2306.N_OF_SALE
    SELECT MON_2306
  ENDSCAN
ENDIF

IF d_001 = " "
  SCAN FOR FIRM+DOC_TYPE+LEFT(DTOS(DOC_DATE),4)+DOC_NUM = key_s
    SELECT NUM_001
    APPEND BLANK
    REPLACE DOC_N WITH MON_2306.DOC_NUM,  ;
            DOC_D WITH MON_2306.DOC_DATE, ;  
            ACC_C WITH MON_2306.CUS_CODE, ;
            ACC_N WITH ACC_2306.CUS_NAME, ;
            SUM_T WITH IIF(t_001="3".OR.t_001="4",MON_2306.MONEY_D,;
                           MON_2306.MONEY_R+MON_2306.N_OF_SALE)
    SELECT MON_2306
  ENDSCAN
ENDIF

SELECT MON_2306
USE

SELECT ACC_2306
USE

DO Wt_Mess

RETURN