*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл All_Docs     Разработчик Андрей Васин           13.06.97 21:08:11 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Печать первичных документов.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE All_Docs
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

*┌─────────────────────────────────────────────────┐
*│ ┌─ Интервал дат ──────────────────────────────┐ │
*│ │  Нач. дата ДД.ММ.ГГ    Кон. дата ДД.ММ.ГГ   │ │
*│ └─────────────────────────────────────────────┘ │
*│ ┌─ Типы документов ───────────────────────────┐ │
*│ │        [ ] Приход      [ ] Расход           │ │
*│ │        [ ] Деньги      [ ] Корр. нал.       │ │
*│ └─────────────────────────────────────────────┘ │
*│                  ┌──────────────────────┐       │
*│      Предприятие │                      │       │
*│                  └──────────────────────┘       │
*│ ┌─ [ ] Все сотрудники ────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!....:│ │
*│ └─────────────────────────────────────────────┘ │
*│        < OK Ctrl-W > < Отказаться Esc >         │
*└─────────────────────────────────────────────────┘

PRIVATE ex, dat_1, dat_2, d_min, d_max, sw_inc, sw_exp, sw_mon, sw_corr
PRIVATE firmN, firmC, nFirm, allP, codeP
m.allP = .T.
m.nFirm = 1
m.codeP = 0
USE (base_path+"FIRMS") SHARED AGAIN ORDER TAG B_NAME
DIMENSION firmN[1], firmC[1]
firmN[1] = "Все предприятия"
firmC[1] = ""
m.ex = 1
SCAN
  m.ex = m.ex+1
  DIMENSION firmN[m.ex], firmC[m.ex]
  firmN[m.ex] = ALLTRIM(FIRMS.B_NAME)
  firmC[m.ex] = FIRMS.FIRM_CODE
ENDSCAN

USE (base_path+"PARMS")
STORE DAT_START TO dat_1, d_min
STORE DATE() TO dat_2, d_max, dat_1
USE
STORE .F. TO sw_inc, sw_exp, sw_mon, sw_corr
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 17, 53, "Перечень документов"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO Sun_Bord WITH 2, 3, 4, 49, " Интервал дат "
@ 3,  6 SAY "Нач. дата" GET dat_1 PICTURE "@D" VALID Tst_B(dat_1)
@ 3, 28 SAY "Кон. дата" GET dat_2 PICTURE "@D" VALID Tst_B(dat_2)

DO Sun_Bord WITH 5, 3, 8, 49, " Типы документов "
@ 6, 12 GET sw_inc PICTURE "@*C Приход"
@ 6, 28 GET sw_exp PICTURE "@*C Расход"
@ 7, 12 GET sw_mon PICTURE "@*C Деньги"
@ 7, 28 GET sw_corr PICTURE "@*C Корр. нал."

@ 10, 8 SAY "Предприятие"
@ 9,20 GET nFirm PICTURE "@^" FROM firmN SIZE 3,22

DO Sun_Bord WITH 12, 3, 14, 49, " [ ] Все сотрудники "
@ 12, 6 GET allP PICTURE "@*C" VALID V_Pers()

@ 15, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_H()

POP KEY
RELEASE WINDOW (win_name)

IF ex = 1
  DO Prp_Tmp
  IF RECCOUNT() = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Не найдено ни одного документа."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RELEASE WINDOW Wt_Mess
  ELSE
    WAIT WINDOW "Всего документов: "+ALLTRIM(STR(RECCOUNT()))+CHR(13);
                                    +CHR(13)+"Нажмите любую клавишу..."    
    DO V_Docs
  ENDIF

*  DELETE FILE (tmpo_path+"ALL_DOCS.DBF")
*  DELETE FILE (tmpo_path+"ALL_DOCS.CDX")

ENDIF
*--------------------------------------------------------------------------

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Проверка правильности отдельной даты.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.06.97 21:25:41 ═╝
PROCEDURE Tst_B
PARAMETERS t_dat
PRIVATE mss

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF t_dat < d_min
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Имеем документы только с "+TRANSFORM(d_min,"@D")+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF t_dat > d_max
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Сегодня еще только "+TRANSFORM(d_max,"@D")+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Проверка бланка в целом.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.06.97 21:31:39 ═╝
PROCEDURE Tst_H
PRIVATE mss

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF dat_1 > dat_2
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Начальная дата больше конечной? Так не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF .NOT. (sw_inc .OR. sw_exp .OR. sw_mon .OR. sw_corr)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Так о каких, собственно, документах идет речь?"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Подготовка временного файла.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.06.97 21:46:00 ═╝
PROCEDURE Prp_Tmp
PRIVATE rc, fam, place, doc_n, d_nam, w

DO Wt_Mess WITH "Подборка документов."

DELETE FILE (tmpo_path+"ALL_DOCS.DBF")
DELETE FILE (tmpo_path+"ALL_DOCS.CDX")

CREATE DBF (tmpo_path+"ALL_DOCS.DBF")  ;
  (  FIRM     C( 3), ;
     V_DOC    C(11), ;
     DOC_NUM  C(10), ;
     DOC_DATE D,     ;
     DOC_TIME N( 5), ;
     FILE     C( 1), ;
     DOC_TYPE C( 1), ;
     DOC_NAM  C(21), ;
     TOTAL_R  N(12,2), ;
     TOTAL_D  N(12,2), ;
     VALUTA   C( 1), ;
     CUSTOMER C(50), ;
     WHO      C(30), ;
     WHERE    C(16)  )

INDEX ON DTOS(DOC_DATE)+STR(DOC_TIME,5) TAG HRON

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0

*
*       Приход
*
IF sw_inc .OR. sw_corr
  USE (base_path+"ACT_S_T")
  SET RELATION TO WHO_ACT   INTO PERSONS,  ;
                  WHERE_ACT INTO ARM_LST,  ;
                  SUP_CODE  INTO ACCOUNT
  SCAN FOR BETWEEN(DOC_DATE, dat_1, dat_2)
    IF SUP_CODE < 0 .AND. .NOT. sw_corr .OR. SUP_CODE > 0 .AND. .NOT. sw_inc
      LOOP
    ENDIF
    IF nFirm # 1
      IF FIRM # firmC[nFirm]
        LOOP
      ENDIF
    ENDIF
    IF .NOT. allP
      IF WHO_ACT # codeP
        LOOP
      ENDIF
    ENDIF
    doc_n = ALLTRIM(DOC_NUM)
    IF FOUND("PERSONS")
      fam   = ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+". "+ ;
              LEFT(PERSONS.S_NAME,1)+"."
    ELSE
      fam = ""
    ENDIF
    IF FOUND("ARM_LST")
      place = ARM_LST.ARM_N
    ELSE
      place = ""
    ENDIF
    SELECT ALL_DOCS
    APPEND BLANK
    REPLACE FIRM     WITH ACT_S_T.FIRM,    ;
            V_DOC    WITH doc_n,           ;
            DOC_NUM  WITH ACT_S_T.DOC_NUM, ;
            DOC_DATE WITH ACT_S_T.DOC_DATE,;
            DOC_TIME WITH 0,               ;
            FILE     WITH IIF(ACT_S_T.SUP_CODE < 0, "i", "I"), ;
            DOC_TYPE WITH ACT_S_T.INC_TYPE,;
            DOC_NAM  WITH IIF(ACT_S_T.SUP_CODE < 0, "Корр. приход ", "Приемный акт "), ;
            VALUTA   WITH ACT_S_T.VALUTA,  ;
            TOTAL_R  WITH IIF(ACT_S_T.VALUTA = "$", 0, ACT_S_T.TOTAL_H), ;
            TOTAL_D  WITH IIF(ACT_S_T.VALUTA = "$", ACT_S_T.TOTAL_H, 0), ;
            CUSTOMER WITH ACCOUNT.CUS_NAME,;
            WHO      WITH fam,             ;
            WHERE    WITH place
    SELECT ACT_S_T
  ENDSCAN
ENDIF

*
*       Расход
*
IF sw_exp .OR. sw_corr
  USE (base_path+"SALE_TIT") ORDER TAG HRON
  SET RELATION TO WHO      INTO PERSONS,  ;
                  WHERE    INTO ARM_LST,  ;
                  CUS_CODE INTO ACCOUNT
  SEEK DTOS(dat_1)
  IF .NOT. FOUND()
    rc = RECNO(0)
    IF BETWEEN(rc,1,RECCOUNT())
      GO rc
    ENDIF
  ENDIF
  SCAN REST WHILE DOC_DATE <= dat_2
    IF CUS_CODE < 0 .AND. .NOT. sw_corr .OR. CUS_CODE >= 0 .AND. .NOT. sw_exp
      LOOP
    ENDIF
    IF nFirm # 1
      IF FIRM # firmC[nFirm]
        LOOP
      ENDIF
    ENDIF
    IF .NOT. allP
      IF WHO # codeP
        LOOP
      ENDIF
    ENDIF
    doc_n = ALLTRIM(LEFT(DOC_NUM,4))+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))
    IF FOUND("PERSONS")
      fam   = ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+". "+ ;
              LEFT(PERSONS.S_NAME,1)+"."
    ELSE
      fam = ""
    ENDIF
    IF FOUND("ARM_LST")
      place = ARM_LST.ARM_N
    ELSE
      place = ""
    ENDIF
    SELECT ALL_DOCS
    APPEND BLANK
    IF SUBSTR(SALE_TIT.DOC_NUM, 2, 3) = "***"
      w = "Товарный чек"
    ELSE
      w = IIF(SALE_TIT.CUS_CODE < 0, "Корр. расх. ", "Накладная ")+ ;
          IIF(SALE_TIT.NEW_TYPE="0", "(0)", "")
    ENDIF
    REPLACE FIRM     WITH SALE_TIT.FIRM,    ;
            V_DOC    WITH doc_n, ;
            DOC_NUM  WITH SALE_TIT.DOC_NUM, ;
            DOC_DATE WITH SALE_TIT.DOC_DATE,;
            DOC_TIME WITH SALE_TIT.DOC_TIME,;
            FILE     WITH IIF(SALE_TIT.CUS_CODE < 0, "e", "E"),  ;
            DOC_TYPE WITH SALE_TIT.NEW_TYPE,;
            DOC_NAM  WITH w, ;
            TOTAL_R  WITH SALE_TIT.TOTAL_R, ;
            TOTAL_D  WITH SALE_TIT.TOTAL_D, ;
            VALUTA   WITH IIF(SALE_TIT.NEW_TYPE = "3", "$", "р"),  ;
            CUSTOMER WITH SALE_TIT.CUS_NAME,;
            WHO      WITH fam,             ;
            WHERE    WITH place
    SELECT SALE_TIT
  ENDSCAN
ENDIF

*
*       Деньги
*
IF sw_mon
  USE (base_path+"INCMONEY") ORDER TAG HRON
  SET RELATION TO WHO      INTO PERSONS,  ;
                  WHERE    INTO ARM_LST,  ;
                  CUS_CODE INTO ACCOUNT
  SEEK DTOS(dat_1)
  IF .NOT. FOUND()
    rc = RECNO(0)
    IF BETWEEN(rc,1,RECCOUNT())
      GO rc
    ENDIF
  ENDIF
  SCAN REST WHILE DOC_DATE <= dat_2
    IF nFirm # 1
      IF FIRM # firmC[nFirm]
        LOOP
      ENDIF
    ENDIF
    IF .NOT. allP
      IF WHO # codeP
        LOOP
      ENDIF
    ENDIF
    doc_n = ALLTRIM(LEFT(DOC_NUM,1))+ALLTRIM(SUBSTR(DOC_NUM,2))
    IF FOUND("PERSONS")
      fam   = ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+". "+ ;
              LEFT(PERSONS.S_NAME,1)+"."
    ELSE
      fam = ""
    ENDIF
    IF FOUND("ARM_LST")
      place = ARM_LST.ARM_N
    ELSE
      place = ""
    ENDIF
    rc = ""
    DO CASE
    CASE DOC_TYPE = " "
      IF "-" $ INCMONEY.DOC_NUM
        rc = "Расходный ордер"
      ELSE
        rc = "Приходный ордер"
      ENDIF
    CASE DOC_TYPE = "Ч"
      rc = "Чек"
    CASE DOC_TYPE = "Б"
      rc = "Через СБ"
    CASE DOC_TYPE = "1"
      rc = "Плат. поручение"
    CASE DOC_TYPE = "3"
      rc = "Индекс. платеж"
    CASE DOC_TYPE = "4"
      rc = "Б/Н USD       "
    ENDCASE

    SELECT ALL_DOCS
    APPEND BLANK
    REPLACE FIRM     WITH INCMONEY.FIRM,    ;
            V_DOC    WITH doc_n,            ;
            DOC_NUM  WITH INCMONEY.DOC_NUM, ;
            DOC_DATE WITH INCMONEY.DOC_DATE,;
            DOC_TIME WITH INCMONEY.DOC_TIME,;
            FILE     WITH "M",              ;
            DOC_TYPE WITH INCMONEY.DOC_TYPE,;
            DOC_NAM  WITH rc,               ;
            TOTAL_R  WITH INCMONEY.MONEY_R, ;
            TOTAL_D  WITH INCMONEY.MONEY_D, ;
            VALUTA   WITH IIF(INCMONEY.DOC_TYPE $ "34", "$", "р"), ;
            CUSTOMER WITH INCMONEY.CUS_NAME,;
            WHO      WITH fam,              ;
            WHERE    WITH place
    SELECT INCMONEY
  ENDSCAN
ENDIF

USE
SELECT ACCOUNT
USE
SELECT ARM_LST
USE
SELECT PERSONS
USE

SELECT ALL_DOCS
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Просмотр и печать документов.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.06.97 22:41:25 ═╝
PROCEDURE V_Docs
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
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

DO Use_Dummy
SELECT ALL_DOCS
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Dok OF (menu_name) PROMPT "Просмотр документа"
ON PAD Dok OF (menu_name) ACTIVATE POPUP Dok

  DEFINE POPUP Dok MARGIN RELATIVE SHADOW COLOR SCHEME 4
  
  DEFINE BAR  1 OF Dok PROMPT "Поиск документа" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Dok DO Sw_Mode WITH "Sea_D"
  
  DEFINE BAR  2 OF Dok PROMPT "Упорядочивание по предприятию" KEY Ctrl-A, "F1"
  ON SELECTION BAR 2 OF Dok DO Sw_Mode WITH "Ord_F"

  DEFINE BAR  3 OF Dok PROMPT "Упорядочивание по хронологии" KEY Ctrl-A, "F2"
  ON SELECTION BAR 3 OF Dok DO Sw_Mode WITH "Ord_H"

  DEFINE BAR  4 OF Dok PROMPT "Приходный ордер для чека" KEY Ctrl-A, "F3"
  ON SELECTION BAR 4 OF Dok DO Sw_Mode WITH "Md_Ord"

DEFINE PAD Prnt OF (menu_name) PROMPT "Печать документа" KEY Ctrl-A, "Enter"
ON SELECTION PAD prnt OF (menu_name) Do Sw_Mode WITH "Prnt"

DEFINE PAD NSP OF (menu_name) PROMPT "Сумма для НСП" KEY Ctrl-A, "Tab"
ON SELECTION PAD NSP OF (menu_name) Do Sw_Mode WITH "NSP"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = 65
*┌Фрм───Дата──────N док-та───Тип документа───────────────Сумма─────┐
*│ФФФ ДД.ММ.ГГГГ NNNNNNNNNNN ДДДДДДДДДДДДДДДДДДДДД 999 999 999.99 $│
*└─────────────────────────────────────────────────────────────────┘
*┌─────────────────────────────────────────────────────────────────┐
*│    Клиент ....:....!....:....!....:....!....:....!....:....!    │
*│       ....:....!....:....!....:....!     ....:....!....:.       │
*└─────────────────────────────────────────────────────────────────┘
DO D_Wins WITH ln, wd, "Документы", 0, 2
what_do = "List"
@ 3, 2 SAY "Фрм───Дата──────N док-та───Тип документа───────────────Сумма"
@ WROWS()-3, 6 SAY "Клиент"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Sw_Mode WITH "NSP"
    ON KEY LABEL F7  DO Sw_Mode WITH "Sea_D"
    ON KEY LABEL F1  DO Sw_Mode WITH "Ord_F"
    ON KEY LABEL F2  DO Sw_Mode WITH "Ord_H"
    ON KEY LABEL F3  DO Sw_Mode WITH "Md_Ord"
*
*    BROWSE - меню
*
    BROWSE FIELDS FIRM:3:H="",      ;
                  DOC_DATE:10:H="", ;
                  V_DOC:11:H="",    ;
                  DOC_NAM:21:H="",  ;
                  F000 = IIF(VALUTA = "$", TOTAL_D, TOTAL_R):H="":P="@Z 999 999 999.99", ;
                  VALUTA:1:H="",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_F()               ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Prnt")
    ENDIF

 CASE what_do = "Prnt"    && Печать документа

    DO Prnt
    what_do = "List"

 CASE what_do = "Sea_D"    && Поиск документа

    DO Sea_D

    what_do = "List"

 CASE what_do = "Ord_F"    && Упорядочивание по предприятию

    SELECT ALL_DOCS
    INDEX ON FIRM+LEFT(DTOS(DOC_DATE),4)+ALLTRIM(V_DOC) TAG V_DOC
    @ 1, 25 SAY "Порядок по предприятию"

    what_do = "List"

 CASE what_do = "Ord_H"    && Упорядочивание по хронологии

    SELECT ALL_DOCS
    SET ORDER TO TAG HRON   
    @ 1, 25 SAY "Порядок по хронологии"
    
    what_do = "List"

 CASE what_do = "Md_Ord"   && П/О для чека

    DO Set_Ord WITH ALL_DOCS.FIRM, ALL_DOCS.DOC_TYPE, ;
                    ALL_DOCS.DOC_NUM, ALL_DOCS.DOC_DATE
    what_do = "List"

 CASE what_do = "NSP"      && Сумма для НСП

    IF FILE # "E"
      PRIVATE mss0
      DIMENSION mss0(3)
      mss0(1) = ""
      mss0(2) = CHR(0)+"Это не накладная!"
      mss0(3) = ""
      DO Out_Mess WITH 7, "mss0"
    ELSE
      DO Set_NSP WITH FIRM, DOC_NUM, DOC_DATE
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO
CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                               Поиск документа.                         ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 06.04.2000 ═╝
PROCEDURE Sea_D
PRIVATE f_c000, f_n000, f_c_old, s_sav
PRIVATE shab1, shab2

shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
f_c000  = def_firm
f_c_old = def_firm
f_n000  = ""
s_sav = SELECT()

*    Содержательная часть программы:
*
*               Поиск документа
*┌─────────────────────────────────────────────┐
*│                                             │
*│           Номер документа ....:....!.       │
*│                                             │
*│    Предприятие AAA AAAAAAAAAAAAAAAA (F1)    │
*│                                             │
*│       < OK Ctrl-W > < Отказаться Esc >      │
*│                                             │
*└─────────────────────────────────────────────┘
*
PRIVATE ln, wd, ex, p_y, d_n, rec_d, r_sea, mss
ln = 9
wd = 43
ex = 1
p_s = 1
d_n = SPACE(11)
r_sea = RECNO()

DO D_Win_N WITH ln, wd, "Поиск документа"  
@ 3, 6 SAY "Номер документа" GET d_n
DO Get_Firm WITH 5, 3, enab_firm
@ 7, 5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE

IF ex = 1
  SELECT ALL_DOCS
  INDEX ON FIRM+ALLTRIM(V_DOC) TAG V_DOC
  SEEK (f_c000+ALLTRIM(d_n))
  IF (f_c000+ALLTRIM(d_n)) == ALL_DOCS.FIRM+ALLTRIM(ALL_DOCS.V_DOC)
    r_sea = RECNO()
    IF BETWEEN(0,r_sea,RECNO())
      GO r_sea  
    ENDIF
  ELSE
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Нет документа с номером "+ALLTRIM(d_n)+"/"+f_c000+" !"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    GO r_sea
  ENDIF
ENDIF
SELECT (s_sav)
RETURN
****-------------------------------
*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Вывод информационной строки.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 14.06.97 12:21:25 ═╝
PROCEDURE Show_F

ACTIVATE WINDOW (win_name+"_M") SAME

*┌─────────────────────────────────────────────────────────────────┐
*│    Клиент ....:....!....:....!....:....!....:....!....:....!    │
*│       ....:....!....:....!....:....!     ....:....!....:.       │
*└─────────────────────────────────────────────────────────────────┘
@ WROWS()-3, 13 SAY ALL_DOCS.CUSTOMER
@ WROWS()-2,  9 SAY ALL_DOCS.WHO+"     "+ALL_DOCS.WHERE

ACTIVATE WINDOW (ALIAS()) SAME
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Печать документов.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 14.06.97 13:01:37 ═╝
PROCEDURE Prnt
PRIVATE mode, s_sav, d_n00, d_d00, d_t00, w0

DO CASE
CASE ALL_DOCS.FILE = "M"      && Деньги
  DO P_Money WITH ALL_DOCS.FIRM, ALL_DOCS.DOC_TYPE, ;
                  ALL_DOCS.DOC_NUM, ALL_DOCS.DOC_DATE
CASE ALL_DOCS.FILE $ "eE"     && Продажа + Коррекция (расход)
  IF SUBSTR(ALL_DOCS.DOC_NUM, 2, 3) = "***"
    DO Prn_Inv WITH ALL_DOCS.FIRM, ALL_DOCS.DOC_NUM, ALL_DOCS.DOC_DATE
  ELSE
    w0 = Wh_Doc()
    DO CASE
    CASE w0 = 1
      DO Prn_Inv WITH ALL_DOCS.FIRM, ALL_DOCS.DOC_NUM, ALL_DOCS.DOC_DATE
    CASE w0 = 2
      s_sav = SELECT()
      SELECT 0
      USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS S_9309 AGAIN
      SEEK ALL_DOCS.FIRM+LEFT(DTOS(ALL_DOCS.DOC_DATE),4)+ALL_DOCS.DOC_NUM
      d_n00 = NEW_DOC
      d_d00 = DOC_DATE
      d_t00 = NEW_TYPE
      USE
      SELECT (s_sav)
      DO P_Bill_F WITH ALL_DOCS.FIRM, d_n00, d_d00, d_t00
    OTHERWISE
      DO Send_Sto WITH ALL_DOCS.FIRM, ALL_DOCS.DOC_NUM, ALL_DOCS.DOC_DATE
    ENDCASE
  ENDIF
CASE ALL_DOCS.FILE $ "iI"     && Приход + Коррекция (приход)

*  DO P_Act WITH ALL_DOCS.FIRM, ALL_DOCS.DOC_NUM, ALL_DOCS.DOC_DATE
*  mode = IIF(ALL_DOCS.FILE = "I", Get_Mode(), 1)
  mode = Get_Mode("!" $ ALL_DOCS.DOC_NUM)
  DO CASE
  CASE mode = 1
    DO P_Act WITH ALL_DOCS.FIRM, ALL_DOCS.DOC_NUM, ALL_DOCS.DOC_DATE
  CASE mode = 2
    DO P_Act WITH ALL_DOCS.FIRM, ALL_DOCS.DOC_NUM, ALL_DOCS.DOC_DATE, "F"
  CASE mode = 3
    DO P_Act WITH ALL_DOCS.FIRM, ALL_DOCS.DOC_NUM, ALL_DOCS.DOC_DATE, "S"
  * NEW 30.06.1999
  CASE mode = 4
    DO P_Act WITH ALL_DOCS.FIRM, ALL_DOCS.DOC_NUM, ALL_DOCS.DOC_DATE, "#"
  * END OF NEW 30.06.1999
  CASE mode = 5  && CORR 30.06.1999
    DO El_Ord WITH ALL_DOCS.FIRM, ALL_DOCS.DOC_NUM, ALL_DOCS.DOC_DATE
  ENDCASE
ENDCASE

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Установка режима для печати приемного акта.               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 02.09.97 11:47:11 ═╝
PROCEDURE Get_Mode
PARAMETER is_corr

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


PRIVATE m_list, p    &&
                     &&   Объявляем и заполняем поля бланка
p = 1
DIMENSION m_list(6)    && CORR 30.06.1999
m_list(1) = "Приемный акт по умолчанию "
m_list(2) = IIF(is_corr, "\", "")+"Полный приемный акт       "
m_list(3) = IIF(is_corr, "\", "")+"Сокр. приемный акт        "
m_list(4) = "Узкий приемный акт (С-П!) "  && NEW 30.06.1999
m_list(5) = "Заявка поставщику         "  && CORR 30.06.1999
m_list(6) = "Отказаться ( Esc )        "  && CORR 30.06.1999
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ALEN(m_list)+3, LEN(m_list(1))+4, "Тип приемного акта"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  p = MIN(p,ALEN(m_list))
  p = MAX(p,1)
  @ 1, 1 MENU m_list, ALEN(m_list)
  READ MENU TO p
  IF READKEY() % 256 = 12
    p = ALEN(m_list)
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN p

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Wh_Doc       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Выбор типа документа.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/09/1999 ──┘
PROCEDURE Wh_Doc
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

*┌───────────────────────────────────────────────────┐
*│                                                   │
*│ < Накладная > < Счет-фактура > < Файл для STORE > │
*│                                                   │
*└───────────────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 55, "Тип документа"

*------------------------------------------------------------------------
*      Ввод полей бланка
*


@ 3, 4 GET ex PICTURE "@*HT \ Накладная ;\ Счет-фактура ;\ Файл для STORE "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Pers       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выбор сотрудника                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.11.2010 ──┘
PROCEDURE V_Pers

PRIVATE tmpFIO

m.tmpFIO = ""
IF m.allP
  @ 13, 4 SAY SPACE(25)
ELSE
  DO Ch_Pers WITH m.codeP, m.tmpFio
  IF m.codeP = 0
    m.allP = .T.
    @ 13, 4 SAY SPACE(25)
    SHOW GETS
  ELSE
    @ 13, 4 SAY m.tmpFio
  ENDIF
ENDIF 

RETURN .T.
