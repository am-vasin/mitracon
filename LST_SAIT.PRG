*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Lst_Sait     Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Создание списка по заказу E-mail.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.08.2002 ──┘
PROCEDURE Lst_Sait

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
s_sav = SELECT()

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

PRIVATE s_file, c_code, c_name, face, n_list, cli
PRIVATE ex, shab1, shab2, mss, flg_cli, tmpCheck
PRIVATE f_c000, f_n000, f_c_old, b_c000, b_n000, b_sw000
PRIVATE tmpINN, tmpKPP, tmpAddr, tmpType, tmpFax
m.tmpINN  = ""
m.tmpKPP  = ""
m.tmpType = 1
m.tmpFax  = ""
m.tmpAddr = SPACE(60)
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
f_c000  = def_firm
b_c000  = def_bank
f_n000  = SPACE(16)
b_n000  = SPACE(16)
f_c_old = ""
b_sw000 = .F.
ex = 1
face = ""
c_name = ""
c_code = 0
cli = .F.
flg_cli = .F.

s_file = Int_File("", "DBF", "Укажите исходный DBF-файл")

IF EMPTY(s_file)
  RETURN
ENDIF

SELECT 0
USE (s_file) ALIAS S_FILE EXCLUSIVE
IF FSIZE("CUST_ID") = 0 .OR. TYPE("CUST_ID") # "N" ;
    .OR. FSIZE("CUST_CODE") = 0 .OR. TYPE("CUST_CODE") # "N" ;
      .OR. FSIZE("ORD_ID") = 0 .OR. TYPE("ORD_ID") # "N" ;
        .OR. FSIZE("CODE") = 0 .OR. TYPE("CODE") # "N" ;
          .OR. FSIZE("QTY") = 0 .OR. TYPE("QTY") # "N"
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Структура файла-заказа не соответствует установленным требованиям!"
  mss(3) = CHR(0)+"Обратитесь к программисту!"
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF
IF ORD_ID < 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Заявка без номера!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF

c_code = CUST_CODE

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2821 AGAIN

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG REQ_DOC ALIAS LT_2821 AGAIN
IF SEEK("E"+STR(S_FILE.ORD_ID), "LT_2821") 
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Заявка N "+ALLTRIM(STR(S_FILE.ORD_ID))+" обработана. Список N "+ALLTRIM(STR(LT_2821.LST_NUM))+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT ACC_2821
  USE
  SELECT LT_2821
  USE
  SELECT S_FILE
  USE
  RETURN
ENDIF

IF m.c_code # 0
  IF .NOT. SEEK(m.c_code, "ACC_2821")
    m.c_code = 0
  ENDIF
ENDIF
IF m.c_code = 0
  SELECT ACC_2821
  SET ORDER TO CUS_NAME
  IF SEEK(SYS(15, m.lwr, S_FILE.CONTACT))
    m.c_code = CUS_CODE
  ENDIF
  SET ORDER TO TAG CUS_CODE
ENDIF

m.c_name  = S_FILE.CONTACT
m.c_name  = CHRTRAN(m.c_name, shab1, shab2)
m.tmpINN  = S_FILE.INN
m.tmpAddr = IIF(EMPTY(S_FILE.POST), S_FILE.ADDRESS, S_FILE.POST)
m.tmpAddr = CHRTRAN(m.tmpAddr, shab1, shab2)
m.tmpFax  = S_FILE.FAX
DO CASE
CASE S_FILE.CUST_FACE = "P"
  m.tmpType = 3
CASE S_FILE.CUST_FACE = "F"
  m.tmpType = 2
ENDCASE

*------------------------------------------------------------------------
*    Содержательная часть программы:
*
*┌────────────────────────────────────────────────────┐
*│┌─ Клиент ─────────────────────────────────────────┐│
*││AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA││
*││ ( ) Юр. лицо  ( ) Физ. лицо  ( ) Предприниматель ││
*││Адрес ....:....!....:....!....:....!....:....!....││
*││ИНН ....:....!..  Факс ....:....!....:....!....:..││
*│└──────────────────────────────────────────────────┘│
*│                      Поиск F7                      │
*│      Предприятие 999 9999999999999999 (F1)         │
*│                                                    │
*│             Банк [ ] 9999999999999999              │
*│                                                    │
*│        < OK Ctrl-W > < Отказаться Esc >            │
*└────────────────────────────────────────────────────┘

*------------------------------------------------------------------------
PRIVATE l, wd, cn
l = 14
wd = 56
PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH l, wd, "Заказ № "+ALLTRIM(STR(S_FILE.ORD_ID))
DO Sun_Bord WITH 2, 2, 7, 53, " Клиент "+IIF(m.c_code=0, "(новый)", "")
@ 7, 24 SAY "Поиск F7"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3,  3 GET m.c_name
@ 4,  4 GET m.tmpType PICTURE "@*RH Юр. лицо ;Физ. лицо ;Предприниматель"
@ 5,  3 SAY "Адрес" GET m.tmpAddr PICTURE "@S44"
@ 6,  3 SAY "ИНН" GET m.tmpINN
@ 6, 21 SAY "Факс" GET m.tmpFax PICTURE "@S27"
DO Get_Firm WITH 8,  8, enab_firm
DO Get_Bank WITH 10, 15, enab_bank
@ 12, 11 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
ON KEY LABEL F7 DO Seek_Cus
READ CYCLE VALID Tst_F()
ON KEY LABEL F7

RELEASE WINDOW (win_name)
IF ex = 2
  SELECT S_FILE
  USE
  SELECT ACC_2821
  USE
  SELECT LT_2821
  USE
  SELECT(s_sav)
  RETURN
ENDIF

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS ST_2821 AGAIN
SEEK f_c000
tmpCheck = DEF_WCHK

USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_2821 AGAIN

SELECT 0
USE (base_path+"LIST_DET") ALIAS LD_2821 AGAIN

n_list = DOC_NUM("LISTS", "", "", "")

SELECT LT_2821
APPEND BLANK
REPLACE LST_ARM   WITH arm,           ;
        LST_NUM   WITH n_list,        ;
        CR_DAT    WITH DATE(),        ;
        LST_NAME  WITH LEFT(c_name,23)+" ИНТЕРНЕТ";
        IS_CHECK  WITH tmpCheck,      ;
        CLI_CODE  WITH c_code,        ;
        REQ_DOC   WITH STR(S_FILE.ORD_ID),;
        REQ_TYPE  WITH "E",           ;
        FIRM      WITH f_c000,        ;
        BANK      WITH b_c000,        ;
        LST_FLAGS WITH "             *",  ;
        WHO_CR    WITH user,          ;
        WHERE_CR  WITH arm

SELECT S_FILE
SCAN
  = SEEK(S_FILE.CODE, "ST_2821")
  SELECT LD_2821
  APPEND BLANK
  REPlACE LIST_NUM  WITH STR(n_list,6),   ;
          CODE      WITH S_FILE.CODE,     ;
          PREFIX    WITH ST_2821.PREFIX,  ;
          NAME      WITH ST_2821.NAME,    ;
          PRODUCER  WITH ST_2821.PRODUCER,;
          QNT       WITH S_FILE.QTY,      ;
          DEF_PRICE WITH ST_2821.PRICE_R, ;
          SAL_PRICE WITH ROUND_6(S_FILE.PRICE), ;
          IS_SPEC   WITH "*"
  SELECT S_FILE
ENDSCAN

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"Сформирован новый список N "+ALLTRIM(STR(n_list))
mss(3) = ""
DO Out_Mess WITH 5, "mss"
  
SELECT S_FILE
USE
SELECT ST_2821
USE
SELECT LT_2821
USE
SELECT LD_2821
USE
SELECT ACC_2821
USE

POP KEY
RELEASE WINDOW (win_name)

SELECT(s_sav)
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                           Выход из бланка.                            ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Tst_F

PRIVATE r_value, is_OK

IF m.ex = 2
  RETURN .T.
ENDIF
IF  EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Некорректный код предприятия"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF m.c_code = 0
  m.is_OK = Tst_New()
ELSE
  m.is_OK = Tst_Old()
ENDIF

IF m.is_OK
  DO Md_Cus
ENDIF

RETURN m.is_OK

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_New      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Проверка нового клиента                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.09.2005 ──┘
PROCEDURE Tst_New

PRIVATE s_sav, mss, res, dim

m.s_sav = SELECT()

SELECT 0

*
*  Проверяем имя клиента
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS TST_5920 ORDER TAG CUS_NAME
IF SEEK(SYS(15, m.lwr, m.c_name))
  DIMENSION mss[4]
  mss[1] = ""
  mss[2] = CHR(0)+"Клиент с таким именем уже существует!"
  mss[3] = CHR(0)+"  Возьмем существующего [ Да ] [ Нет ]"
  mss[4] = ""
  IF Out_Mess(7, "mss") = 1
    m.c_name = CUS_NAME
    SHOW GET m.c_name
    @ 2, 12 SAY "────────────────"
    m.c_code = TST_5920.CUS_CODE
  ENDIF
  USE
  SELECT (m.s_sav)
  RETURN .F.
ENDIF
USE
SELECT (m.s_sav)

RETURN Tst_INN(m.c_code, m.tmpINN)

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Old      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Проверка существующего клиента                     │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.09.2005 ──┘
PROCEDURE Tst_Old

PRIVATE s_sav, mss, err, tmpFace, nameFace, ret

m.ret = .T.
m.err = ""
DIMENSION mss[1]
mss[1] = ""

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS TST_5920 ORDER TAG CUS_CODE
SEEK m.c_code
SELECT 0
USE (m.base_path+"CUS_BIL") SHARED AGAIN ALIAS TSTB_5920 ORDER TAG CUS_CODE
SEEK m.c_code
IF SYS(15, m.lwr, TST_5920.CUS_NAME) # SYS(15, m.lwr, c_name)
  DIMENSION mss[ALEN(mss)+2]
  mss[ALEN(mss)-1] = CHR(0)+"Клиент существует под именем"
  mss[ALEN(mss)] = CHR(0)+ALLTRIM(TST_5920.CUS_NAME)+"."
  m.err = m.err+"N"
ENDIF
m.tmpFace = 1
m.nameFace = "юр. лицо"
DO CASE
CASE TST_5920.MARK1 = "1"
  m.tmpFace = 3
  m.nameFace = "предприниматель"
CASE TST_5920.MARK1 = "2"
  m.tmpFace = 2
  m.nameFace = "Физ. лицо"
ENDCASE
IF m.tmpFace # m.tmpType
  DIMENSION mss[ALEN(mss)+1]
  mss[ALEN(mss)] = CHR(0)+"В базе данных клиент зарегистрирован как "+m.nameFace+"."
  m.err = m.err+"F"
ENDIF

IF .NOT. EMPTY(TSTB_5920.INN) .AND. .NOT. EMPTY(m.tmpINN)
  IF TSTB_5920.INN # m.tmpINN
    DIMENSION mss[ALEN(mss)+1]
    mss[ALEN(mss)] = CHR(0)+"В базе данных ИНН клиента "+ALLTRIM(TSTB_5920.INN)+"."
    m.err = m.err+"I"
  ENDIF
ENDIF

IF .NOT. EMPTY(m.err)
  DIMENSION mss[ALEN(mss)+2]
  mss[ALEN(mss)-1] = CHR(0)+"  Установить атрибуты [ базы данных ] [ сайта ] [ вернуться ] [ новый клиент ]"
  mss[ALEN(mss)] = ""
  m.ret = Out_Mess(7, "mss")
  DO CASE
  CASE m.ret = 3
    m.ret = .F.
  CASE m.ret = 2
    m.ret = .T.
  CASE m.ret = 1
    IF "N" $ m.err
      m.c_name = TST_5920.CUS_NAME
      SHOW GET m.c_name
    ENDIF
    IF "F" $ m.err
      m.tmpType = m.tmpFace
      SHOW GET m.tmpType
    ENDIF
    IF "I" $ m.err
      m.tmpINN = TSTB_5920.INN
      SHOW GET m.tmpINN
    ENDIF
    m.ret = .T.
  CASE m.ret = 4
    m.c_code = 0
    @ 2, 12 SAY "(новый)"
    m.ret = .F.
  ENDCASE
ENDIF

USE IN TSTB_5920
USE IN TST_5920

RETURN m.ret

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Md_Cus       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Создание, коррекция клиента                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.09.2005 ──┘
PROCEDURE Md_Cus

PRIVATE s_sav, tmpMark

m.tmpMark = " "
DO CASE
CASE m.tmpType = 2
  m.tmpMark = "2"
CASE m.tmpType = 3
  m.tmpMark = "1"
ENDCASE

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"CLI_INFO") SHARED AGAIN ALIAS TSTI_5920 ORDER TAG CUS_CODE
SELECT 0
USE (m.base_path+"CUS_BIL") SHARED AGAIN ALIAS TSTB_5920 ORDER TAG CUS_CODE
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS TST_5920 ORDER TAG CUS_CODE
IF m.c_code = 0
  m.c_code = Doc_Num("CUS_CODE  ", "", arm, DATE())
  APPEND BLANK
  REPLACE CUS_CODE WITH m.c_code,  ;
          SUBSYS   WITH m.sys_char,;
          DATE_ON  WITH DATE(),    ;
          WHO      WITH m.user,    ;
          WHERE    WITH m.arm
ELSE
  SEEK m.c_code
ENDIF

REPLACE CUS_NAME WITH m.c_name, ;
        MARK1    WITH m.tmpMark

SELECT TSTB_5920
IF .NOT. SEEK(m.c_code)
  APPEND BLANK
ENDIF
REPLACE CUS_CODE WITH m.c_code, ;
        CUS_NAME WITH m.c_name, ;
        INN      WITH IIF(EMPTY(m.tmpINN), INN, m.tmpINN), ;
        ADDRESS  WITH IIF(EMPTY(m.tmpAddr), ADDRESS, m.tmpAddr)

SELECT TSTI_5920
IF .NOT. SEEK(m.c_code)
  APPEND BLANK
ENDIF
REPLACE CUS_CODE WITH m.c_code, ;
        NAME     WITH m.c_name, ;
        FAX      WITH IIF(EMPTY(m.tmpFax), FAX, m.tmpFax), ;
        ADDRESS  WITH IIF(EMPTY(m.tmpAddr), ADDRESS, m.tmpAddr)

USE IN TST_5920
USE IN TSTI_5920
USE IN TSTB_5920

SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_Cus     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Поиск клиента                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 29.09.2005 ──┘
PROCEDURE Seek_Cus
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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌────────────────────────────────────────────────────┐
*│┌─ Заголовок рамки ────────────────────────────────┐│
*││....:....!....:....!....:....!....:....!....:....!││
*│└──────────────────────────────────────────────────┘│
*│         < OK Ctrl-W > < Отказаться Esc >           │
*└────────────────────────────────────────────────────┘

PRIVATE ex, tmpShab, retValue, s_sav, tmpName
m.ex = 1
m.tmpShab = m.c_name
m.retValue = 0
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 56, "Поиск клиента"
DO Sun_Bord WITH  2,  2,  4, 53, " Шаблон для поиска "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3,  3 GET m.tmpShab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE

IF m.ex = 1 .AND. .NOT. EMPTY(m.tmpShab)
  *
  * Отрабатываем бланк
  m.tmpShab = SYS(15, m.lwr, ALLTRIM(m.tmpShab))
  m.tmpName = SYS(3)
  m.s_sav = SELECT()
  SELECT 0
  CREATE DBF (m.tmpo_path+m.tmpName) ( LINK N(10) )
  USE (m.tmpo_path+m.tmpName) ALIAS TMP_LINK
  SELECT 0
  USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_05929 ORDER TAG CUS_NAME
  DO Wt_Mess WITH "Ищем..."
  SCAN FOR m.tmpShab $ SYS(15, m.lwr, CUS_NAME)
    SELECT TMP_LINK
    APPEND BLANK
    REPLACE LINK WITH RECNO("CUS_05929")
    SELECT CUS_05929
  ENDSCAN
  DO Wt_Mess
  SELECT TMP_LINK
  IF RECCOUNT() = 0
    PRIVATE mss
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"Ничего не найдено..."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    DO Ch_Cust
  ENDIF
  IF m.retValue # 0
    m.c_name = CUS_05929.CUS_NAME
    m.c_code = CUS_05929.CUS_CODE
  ENDIF
  USE IN CUS_05929
  USE IN TMP_LINK
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  SELECT (m.s_sav)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

IF m.retValue # 0
  SHOW GET m.c_name
  @ 2, 12 SAY "────────────────"
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Cust      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выбор клиента                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 29.09.2005 ──┘
PROCEDURE Ch_Cust

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

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
*  └─ <Файл1 > (<Алиас>)

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT CUS_05929
SET ORDER TO
SELECT TMP_LINK
SET RELATION TO LINK INTO CUS_05929

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

m.ln = MIN(WROWS("")-11, RECCOUNT())   && Количество видимых строк BROWSE
m.wd = 50

DO D_Wins WITH m.ln, m.wd, "Выберите клиента", 0, 0
m.what_do = "List"

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS CUS_05929.CUS_NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    m.retValue = LINK
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
SELECT (m.s_sav)

RETURN
