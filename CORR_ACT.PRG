*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Corr_Act     Разработчик Андрей Васин           09.06.97 14:34:23 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Коррекция приемного акта.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Corr_Act
PARAMETER is_GTD
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
IF is_GTD
  DEFINE PAD Corr OF (menu_name) PROMPT "Установка ГТД" KEY Ctrl-A, "Enter"
*  DEFINE PAD GTD OF (menu_name) PROMPT "ГТД" KEY Ctrl-A, "Ctrl-Enter"
*  ON SELECTION PAD GTD OF (menu_name) Do Sw_Mode WITH "GTD"
ELSE
  DEFINE PAD Corr OF (menu_name) PROMPT "Коррекция" KEY Ctrl-A, "Enter"
ENDIF
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "Tab"
ON SELECTION PAD Print OF (menu_name) Do Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

PRIVATE dt0, d_names, d_codes, n, w, i000, mss
USE (base_path+"PARMS")
dt0 = DAT_START

*
*   Список разделов номенклатуры
*
USE (base_path+"DIVISION")
n = 0
SCAN
  n = n+1
  DIMENSION w(n,2)
  w(n,1) = B_NAME
  w(n,2) = CODE
ENDSCAN
= ASORT(w,1)

DIMENSION d_names(n), d_codes(n)
FOR i000 = 1 TO n
  d_codes(i000) = w(i000,2)
  d_names(i000) = w(i000,1)
ENDFOR
RELEASE w, i000

USE

DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_9128 AGAIN

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS A_9128 AGAIN

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACT_S_T")    && ORDER TAG DOC_NUM
IF is_GTD
  SET FILTER TO RIGHT(DOC_NUM,2) = "  " .AND. DOC_DATE >= dt0
ELSE
  SET FILTER TO RIGHT(DOC_NUM,2) = "  " .AND. DOC_DATE >= dt0 .AND. EMPTY(STOP_CORR)
ENDIF
SET RELATION TO SUP_CODE  INTO ACCOUNT, ;
                WHO_ACT   INTO P_9128,  ;
                WHERE_ACT INTO A_9128
*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 23, 21)+FSIZE("CUS_NAME", "ACCOUNT")

*┌Фрм───Дата─────N док-та───Поставщик──────────────────────────────────────┐
*│!!! ДД.ММ.ГГГГ NNNNNNNNNN ....:....!....:....!....:....!....:....!....:..│
*└─────────────────────────────────────────────────────────────────────────┘
*┌─────────────────────────────────────────────────────────────────────────┐
*│                   Накладная 9999999999 от ДД.ММ.ГГГГ                    │
*│                     Пр. акт 9999999999 от ДД.ММ.ГГГГ                    │
*│ Док-т создал ФФФФФФФФФФФФФФФФФФФФФФФФ_И._О. Раб. место ....:....!....:. │
*└─────────────────────────────────────────────────────────────────────────┘
DO D_Wins WITH ln, wd, "Приемные акты", 0, 2
@ 1, 2 SAY "Фрм───Дата─────N док-та───Поставщик"
@ WROWS()-2,3 SAY "Док-т создал                                Раб. место"
what_do = "List"

GO BOTTOM
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   Do Sw_Mode WITH "Print"
*    ON KEY LABEL Ctrl+Enter Do Sw_Mode WITH "GTD"

*
*    BROWSE - меню
*
    BROWSE FIELDS FIRM:3:H="",      ;
                  DOC_DATE:H="",    ;
                  DOC_NUM:10:H="",  ;
                  F000 = Left_(ACCOUNT.CUS_NAME,48):48:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && Коррекция приемного акта

    IF is_GTD
      IF .NOT. BOF() .AND. .NOT. EOF()
        DO Act_GTD WITH FIRM, DOC_NUM, DOC_DATE
      ENDIF
    ELSE
      IF SUP_CODE < 0
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Приемный акт, связанный со спецдвижением коррекции не подлежит!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
      ELSE
        IF .NOT. BOF() .AND. .NOT. EOF()
          DO Corr
        ENDIF
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "GTD"    && Создание ГТД

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO New_Gtd WITH FIRM, DOC_NUM, DOC_DATE
    ENDIF
    what_do = "List"

  CASE what_do = "Print"    && Печать приемного акта

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Act WITH FIRM, DOC_NUM, DOC_DATE
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
*║                   Непосредственно коррекция списка.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 09.06.97 14:34:53 ═╝
PROCEDURE Corr

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

DEFINE PAD Sw_Mode OF (menu_name) PROMPT "Режим" KEY Ctrl-A, "F2"
ON SELECTION PAD Sw_Mode OF (menu_name) Do Sw_Mode WITH "Sw_Mode"

DEFINE PAD Add_Pos OF (menu_name) PROMPT "Нов. поз." KEY Ctrl-A, "F3"
ON SELECTION PAD Add_Pos OF (menu_name) Do Sw_Mode WITH "Add_Pos"

DEFINE PAD Addr OF (menu_name) PROMPT "Адреса" KEY Ctrl-A, "F5"
ON SELECTION PAD Addr OF (menu_name) Do Sw_Mode WITH "Addr"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) Do Sw_Mode WITH "Seek"

DEFINE PAD GTD OF (menu_name) PROMPT "ГТД" KEY Ctrl-A, "Ctrl-Tab"
ON SELECTION PAD GTD OF (menu_name) Do Sw_Mode WITH "GTD"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"
*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DELETE FILE (tmpo_path+"CORR_ACT.DBF")
DELETE FILE (tmpo_path+"CORR_ACT.CDX")

IF ACT_S_T.VALUTA = "$"
  CREATE DBF (tmpo_path+"CORR_ACT.DBF")  ;
      (  RC_NO    N(10),  ;
         CODE     N( 7),  ;
         IDP      C(11),  ;
         PREFIX   C( 4),  ;
         NAME     C(78),  ;
         PRODUCER C( 8),  ;
         GTD_NUM  C(26),  ;
         COUNTRY  C(16),  ;
         COUNTRYC C( 3),  ;
         GTD_LINE C( 4),  ;
         B_GTD    L( 1),  ;
         QNT_DOC  N( 6),  ;
         QNT_INC  N( 6),  ;
         QNT_REST N( 6),  ;
         QNT      N( 6),  ;
         PRICE    N( 8,2) )
ELSE
  CREATE DBF (tmpo_path+"CORR_ACT.DBF")  ;
      (  RC_NO    N(10),  ;
         CODE     N( 7),  ;
         IDP      C(11),  ;
         PREFIX   C( 4),  ;
         NAME     C(78),  ;
         PRODUCER C( 8),  ;
         GTD_NUM  C(26),  ;
         COUNTRY  C(16),  ;
         COUNTRYC C(16),  ;
         GTD_LINE C( 4),  ;
         B_GTD    L( 1),  ;
         QNT_DOC  N( 6),  ;
         QNT_INC  N( 6),  ;
         QNT_REST N( 6),  ;
         QNT      N( 6),  ;
         PRICE    N(12,5) )
ENDIF
USE (tmpo_path+"CORR_ACT.DBF") EXCLUSIVE
INDEX ON CODE TAG CODE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"OKSM") ORDER TAG CODE

SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM

SEEK ACT_S_T.FIRM+LEFT(DTOS(ACT_S_T.DOC_DATE),4)+ACT_S_T.DOC_NUM
SCAN REST WHILE ACT_S_T.FIRM = FIRM .AND. DOC_NUM = ACT_S_T.DOC_NUM .AND. DOC_DATE = ACT_S_T.DOC_DATE
  = SEEK(CODE,"STOCK")
  SELECT CORR_ACT
  APPEND BLANK
  REPLACE RC_NO    WITH RECNO("ACT_S"), ;
          CODE     WITH ACT_S.CODE,     ;
          IDP      WITH ACT_S.IDP,      ;
          PREFIX   WITH STOCK.PREFIX,   ;
          NAME     WITH STOCK.NAME,     ;
          PRODUCER WITH STOCK.PRODUCER, ;
          GTD_NUM  WITH ACT_S.GTD_NUM,  ;
          COUNTRY  WITH ACT_S.COUNTRY,  ;
          COUNTRYC WITH ACT_S.COUNTRYC, ;
          GTD_LINE WITH ACT_S.GTD_LINE, ;
          QNT_DOC  WITH ACT_S.QNT_EXP,  ;
          QNT_INC  WITH ACT_S.QNT_INC,  ;
          QNT_REST WITH ACT_S.QNT_REST, ;
          QNT      WITH ACT_S.QNT_REST, ;
          PRICE    WITH IIF(ACT_S_T.VALUTA = "$", ACT_S.PRICE_D, ACT_S.PRICE_R)
  SELECT ACT_S
ENDSCAN
SELECT CORR_ACT
APPEND BLANK

*
*   Расчет размеров окна
*
PRIVATE ln, wd, view_mode, pic
ln = 100    && Количество видимых строк BROWSE
wd = IIF(ACT_S_T.VALUTA = "$", 71, 73)
pic = "@Z "+IIF(ACT_S_T.VALUTA = "$", "99999.9999", "999999.99999")

DO D_Wins WITH ln, wd, "Приемный акт N "+   ;
                        ALLTRIM(ACT_S_T.DOC_NUM)+" от "+ ;
                        TRANSFORM(ACT_S_T.DOC_DATE,"@D"), 1, 3
@ 1,  2 SAY "Tab" COLOR (SCHEME(14,2))
@ 1,  6 SAY " - поиск"
@ 2,  5 SAY "Наименование"
@ 2, 42 SAY "Приход"
@ 2, 49 SAY "Остат."
@ 2, 56 SAY "Кол-во"
@ 2, 65 SAY "Цена"

what_do = "List"

view_mode = "0"       && Только приемный акт

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Приемный акт

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F2 DO Sw_Mode WITH "Sw_Mode"
    ON KEY LABEL F3 Do Sw_Mode WITH "Add_Pos"
    ON KEY LABEL F5 Do Sw_Mode WITH "Addr"
    ON KEY LABEL F7 Do Sw_Mode WITH "Seek"
    ON KEY LABEL Ctrl+Tab Do Sw_Mode WITH "GTD"
    ON KEY LABEL Tab DO Seek_P

*
*    BROWSE - меню
*
    IF view_mode = "0"
      BROWSE FOR CODE # 0 .AND. (QNT # 0 .OR. QNT_INC # 0 .OR. QNT_REST # 0 .OR. QNT_DOC # 0) REST  ;
             FIELDS PREFIX:H="":W=.F.,   ;
                    Brw_Name=Nm_Part(NAME):25:H="":W=.F.,  ;
                    PRODUCER:H="":W=.F., ;
             QNT_INC:H="":W=.F.:P="@Z 999999", QNT_REST:H="":W=.F.:P="@Z 999999",  ;
             QNT:H="":P="@Z 999999":V=Tst_Qnt(), ;
             PRICE:H="":P=pic:W=RC_NO=0:V=Rnd_R()  ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WHEN Sh_Addr() ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS PREFIX:H="":W=.F.,   ;
             Brw_Name=Nm_Part(NAME):25:H="":W=.F.,  ;
             PRODUCER:H="":W=.F.,    ;
             CORR_ACT.QNT_INC:H="":W=.F.:P="@Z 999999",             ;
             CORR_ACT.QNT_REST:H="":W=.F.:P="@Z 999999",            ;
             CORR_ACT.QNT:H="":P="@Z 999999":V=Tst_Qnt(),  ;
             CORR_ACT.PRICE:H="":V=Rnd_R():P=pic:W=CORR_ACT.QNT#0 .AND. CORR_ACT.RC_NO = 0 ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WHEN Sh_Addr() ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Sw_Mode"    && Смена режима просмотра

    IF view_mode = "0"
      SET ORDER TO TAG CODE
      SELECT STOCK
      SEEK CORR_ACT.CODE
      SET ORDER TO TAG NAME_P
      SET RELATION TO IIF(SEEK(CODE,"CORR_ACT"), CODE, 0) INTO CORR_ACT
      view_mode = "1"
    ELSE
      SET RELATION TO
      SET ORDER TO TAG CODE
      SELECT CORR_ACT
      SET ORDER TO TAG NAME
      view_mode = "0"
    ENDIF
    what_do = "List"
      
  CASE what_do = "Add_Pos"    && Добавление позиции

    what_do = RECNO("STOCK")
    ln = Add_Pos()
    IF ln = 0
      GO what_do IN STOCK
    ELSE
      GO ln IN STOCK
    ENDIF
    what_do = "List"
    
  CASE what_do = "Addr"    && Адреса на складе

    IF view_mode = "0"
      = SEEK(CODE, "STOCK")
    ENDIF
    DO Addr
    what_do = "List"

  CASE what_do = "Seek"    && Контекстный поиск

    DO Seek1
    what_do = "List"

  CASE what_do = "GTD"     && ГТД

*    DO Get_GTD0
    IF .NOT. EMPTY(CORR_ACT.CODE)
      DO Get_GTD1
    ENDIF
    what_do = "List"

  OTHERWISE

    SELECT CORR_ACT
    LOCATE FOR QNT # QNT_REST

    IF FOUND()          && Что-то есть!
      IF .NOT. Corr_OK()
        view_mode = "0"
        SELECT STOCK
        SET RELATION TO
        SET ORDER TO TAG CODE
        SELECT CORR_ACT
        SET ORDER TO TAG NAME
        what_do = "List"
        LOOP
      ENDIF
    ENDIF
      
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
SELECT STOCK
USE
SELECT ACT_S
USE
SELECT OKSM
USE
SELECT CORR_ACT
USE
DELETE FILE (tmpo_path+"CORR_ACT.DBF")
DELETE FILE (tmpo_path+"CORR_ACT.CDX")

SELECT ACT_S_T

RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Выход из поля количества.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.06.97 09:04:56 ═╝
PROCEDURE Tst_Qnt
PRIVATE mss, q

IF CORR_ACT.QNT < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Отрицательных наличий не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF CORR_ACT.QNT-CORR_ACT.QNT_REST+STOCK.QNT < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"В вашем распоряжении только "+ALLTRIM(STR(STOCK.QNT))+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF view_mode = "1" .AND. CORR_ACT.CODE = 0
  SELECT CORR_ACT
  q = QNT
  REPLACE QNT WITH 0
  APPEND BLANK
  REPLACE CODE     WITH STOCK.CODE,    ;
          PREFIX   WITH STOCK.PREFIX,  ;
          NAME     WITH STOCK.NAME,    ;
          PRODUCER WITH STOCK.PRODUCER,;
          QNT      WITH q
  SELECT STOCK
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Поиск по начальным символам.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.06.97 10:43:18 ═╝
PROCEDURE Seek_P
PRIVATE prf, s, ln, rc_sav

ACTIVATE WINDOW (win_name+"_M") SAME

prf = ""
ln = 25

DO WHILE .T.
  ACTIVATE WINDOW (win_name+"_M") SAME
  @ 1, 15 SAY REPLICATE("▒",25) COLOR SCHEME 1
  @ 1, 15 SAY prf COLOR SCHEME 1
  s = INKEY(0)
  IF s < 0       && Функциональная клавиша
    IF s = -9
      KEYBOARD "{F10}"
      EXIT
    ENDIF
    LOOP
  ENDIF
  IF BETWEEN(s,  32,126) .OR.   ;
     BETWEEN(s, 128,175) .OR.   ;
     BETWEEN(s, 224,241)     && Символ
    IF LEN(prf) >= 25        && Все! Больше некуда!
      LOOP
    ENDIF
    rc_sav = RECNO()
    IF SEEK(prf+CHR(s))
      prf = prf+CHR(s)
      SHOW WINDOW (ALIAS()) REFRESH
    ELSE
      GO rc_sav
    ENDIF
  ENDIF
  IF s = 127       && Back Space
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      = SEEK(prf)
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
  ENDIF
  IF s = 13        && Enter
    EXIT
  ENDIF
  IF s = 24        && Down Arrow
    KEYBOARD "{DnArrow}"
    EXIT
  ENDIF
  IF s = 5         && Up Arrow
    KEYBOARD "{UpArrow}"
    EXIT
  ENDIF
  IF s = 3         && Page Down
    KEYBOARD "{PgDn}"
    EXIT
  ENDIF
  IF s = 18        && Page Up
    KEYBOARD "{PgUp}"
    EXIT
  ENDIF
  IF s = 27        && Escape
    KEYBOARD "{Esc}"
    EXIT
  ENDIF
ENDDO

@ 1, 15 SAY "                         "
  
ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                             Вывод адресов.                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.06.97 12:18:48 ═╝
PROCEDURE Sh_Addr
PRIVATE addr_w1, addr_w2, tmpC

IF view_mode = "0"
  = SEEK(CORR_ACT.CODE, "STOCK")
ENDIF

addr_w1 = STOCK.ADDRESS
addr_w2 = STOCK.MEMO_ST

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-4, 3 SAY "Адрес "
@ WROWS()-4, COL() SAY addr_w1 PICTURE Addr_Pic() COLOR SCHEME 1
@ WROWS()-4, COL() SAY "  Доп. адрес "
@ WROWS()-4, COL() SAY addr_w2 COLOR SCHEME 1
@ WROWS()-4, COL() SAY "  Приход по док-ту "
@ WROWS()-4, COL() SAY STR(CORR_ACT.QNT_DOC,6) COLOR SCHEME 1

@ WROWS()-3, 2 SAY "ГТД N "
@ WROWS()-3, COL() SAY CORR_ACT.GTD_NUM COLOR SCHEME 1
@ WROWS()-3, COL() SAY " Страна "
@ WROWS()-3, COL() SAY CORR_ACT.COUNTRYC COLOR SCHEME 1
IF EMPTY(CORR_ACT.COUNTRYC)
  m.tmpC = SPACE(FSIZE("COUNTRY", "CORR_ACT"))
ELSE
  IF SEEK(CORR_ACT.COUNTRYC, "OKSM")
    m.tmpC = LEFT(OKSM.B_NAME, FSIZE("COUNTRY", "CORR_ACT"))
  ELSE
    m.tmpC =CORR_ACT.COUNTRY
  ENDIF
ENDIF
@ WROWS()-3, COL()+1 SAY m.tmpC COLOR SCHEME 1

*@ WROWS()-2, 2 SAY "Позиция "
*@ WROWS()-2, COL() SAY PREFIX COLOR SCHEME 1
@ WROWS()-2, 2 SAY LEFT(NAME, WCOLS()-4) COLOR SCHEME 1
*@ WROWS()-2, COL()+1 SAY PRODUCER COLOR SCHEME 1


ACTIVATE WINDOW (ALIAS())

RETURN .T.


*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Коррекция адресов.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 10:48:15 ═╝
PROCEDURE Addr

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

PRIVATE ex, addr1, addr2, q_r, m_r  &&
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
addr1 = STOCK.ADDRESS
addr2 = STOCK.MEMO_ST
q_r   = STOCK.QNT_ROUND
m_r   = STOCK.QNT_PACK
*------------------------------------------------------------------------
*
*┌──────────────────────────────────────────────────────┐
*│                                                      │
*│Основной адрес ▒▒▒▒▓▒▒▒??  Доп. адрес ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒│
*│                                                      │
*│           Упаковка ▒▒▒▒▓   ▒▒▒▒▓▒▒▒           │
*│                                                      │
*│           < OK Ctrl-W > < Отказаться Esc >           │
*│                                                      │
*└─────────────── F1 - история адресов ─────────────────┘

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 58, ALLTRIM(STOCK.PREFIX)+" "+ALLTRIM(STOCK.NAME)+" "+  ;
                       ALLTRIM(STOCK.PRODUCER)
@ WROWS()-1, 17 SAY " F1 - история адресов "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  2 SAY "Основной адрес" GET addr1 PICTURE Addr_Pic()
@ 3, 29 SAY "Доп. адрес" GET addr2
@ 5, 13 SAY "Упаковка" GET q_r PICTURE "@Z 99999"
@ 5, 29 SAY " Отпуск" GET m_r PICTURE "@Z 9999999"
@ 7, 13 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

ON KEY LABEL F1 DO Addr_Hst WITH addr1, addr2, STOCK.ADDR_0, STOCK.MEMO_ST_0, ;
                          STOCK.ADDR_1, STOCK.MEMO_ST_1
READ CYCLE
ON KEY LABEL F1

IF ex = 1
  IF STOCK.ADDRESS # addr1
    REPLACE STOCK.ADDR_1  WITH STOCK.ADDR_0
    REPLACE STOCK.ADDR_0  WITH STOCK.ADDRESS
    REPLACE STOCK.ADDRESS WITH addr1
  ENDIF
  IF STOCK.MEMO_ST # addr2
    REPLACE STOCK.MEMO_ST_1 WITH STOCK.MEMO_ST_0
    REPLACE STOCK.MEMO_ST_0 WITH STOCK.MEMO_ST
    REPLACE STOCK.MEMO_ST   WITH addr2
  ENDIF
  REPLACE STOCK.QNT_ROUND WITH MAX(1, q_r),;
          STOCK.QNT_PACK  WITH m_r
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Процедура контекстного поиска.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 11:59:42 ═╝
PROCEDURE Seek1

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

PRIVATE ex, nm_w     &&
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
nm_w = SPACE(25)     &&
*------------------------------------------------------------------------
*
*┌─────────────────────────────────────────────────────┐
*│                                                     │
*│ Задайте шаблон для поиска ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓ │
*│                                                     │
*│          < OK Ctrl-W > < Отказаться Esc >           │
*│                                                     │
*└─────────────────────────────────────────────────────┘
PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 57, "Поиск в "+IIF(view_mode#"0", "номенклатуре.", "списке.")

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  3 SAY "Задайте шаблон для поиска" GET nm_w
@ 5, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
nm_w = ALLTRIM(nm_w)

IF ex = 1 .AND. .NOT. EMPTY(nm_w)
  DO Choose_P WITH nm_w
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Подборка по контексту и выбор позиции.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 12:17:29 ═╝
PROCEDURE Choose_P
PARAMETERS shab
PRIVATE sel_sav

sel_sav = SELECT()

DO Wt_Mess WITH "Подборка..."
IF view_mode = "0"
  SELECT PREFIX, NAME, PRODUCER, RECNO() AS REC_N FROM CORR_ACT  ;
         INTO CURSOR TMP_C   ;
         WHERE (shab $ NAME .OR. shab $ PREFIX .OR. shab $ PRODUCER)  ;
               .AND. QNT # 0
ELSE
  SELECT PREFIX, NAME, PRODUCER, RECNO() AS REC_N FROM STOCK  ;
         INTO CURSOR TMP_C   ;
         WHERE (shab $ NAME .OR. shab $ PREFIX .OR. shab $ PRODUCER)  ;
               .AND. EMPTY(DATE_OFF)
ENDIF
DO Wt_Mess
SELECT TMP_C
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не удалось найти..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
  RETURN
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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = 39
DO D_Wins WITH ln, wd, "Выберите позицию", 0, 0
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS F000 = PREFIX+" "+NAME+" "+PRODUCER:H="":39,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

    IF view_mode # "0"
      GO TMP_C.REC_N IN STOCK
    ELSE
      GO TMP_C.REC_N IN CORR_ACT
    ENDIF
    what_do = "exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

SELECT TMP_C
USE
SELECT (sel_sav)
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Формирование файла коррекции прихода.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.06.97 16:21:39 ═╝
PROCEDURE Corr_OK

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
*             Формируем документ?
*┌───────────────────────────────────────────┐
*│                                           │
*│           [ ] Печать документа            │
*│                                           │
*│     < OK Ctrl-W > < Отказаться Esc >      │
*│                                           │
*└───────────────────────────────────────────┘

PRIVATE ex, prn_d
ex  = 1
prn_d = .T.

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 47, "Формируем документ?"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 13 GET prn_d PICTURE "@*C Печать документа"
@ 5,  7 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc " 

READ CYCLE
*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)

IF ex = 1    && Форируем документ
  PRIVATE sum_h, sum_0, new_doc, new_dat, ref_doc, ref_dat, frm000
  PRIVATE i, qn, qn_r, is_usd, nds_w, p_0, p_r, p_d, mss, is_emp
  PRIVATE tmpParty
  PRIVATE r_brends, t_gtd, t_cnt, t_cntc, t_line, bad_g
  new_dat = DATE()
  *
  *  Проверка на наличие пустых ГТД
  SELECT STOCK
  SET ORDER TO TAG CODE
  SELECT 0
  r_brends = ""
  USE base_path+"B_GROUPS" SHARED AGAIN ALIAS BBB_2412
  SCAN
    r_brends = r_brends+DEV_LIST
  ENDSCAN
  USE
  SELECT CORR_ACT
  is_emp = .F.
  SCAN FOR QNT_REST # QNT .AND. CORR_ACT.RC_NO = 0 .AND. CODE # 0
    IF .NOT. EMPTY(GTD_NUM)
      IF .NOT. B_GTD .OR. .NOT. EMPTY(LAST_SALE)
        LOOP
      ENDIF
    ENDIF
    = SEEK(CODE, "STOCK")
    IF " "+ALLTRIM(STR(STOCK.PRO_CODE))+" " $ r_brends
      LOOP
    ENDIF
    t_gtd  = GTD_NUM
    t_cnt  = COUNTRY
    t_cntc = COUNTRYC
    t_line = GTD_LINE
    bad_g  = B_GTD
    DO Set_Gtd WITH CODE, STOCK.PREFIX, STOCK.NAME, STOCK.PRODUCER, t_gtd, t_cnt, t_cntc, t_line, bad_g
    REPLACE GTD_NUM WITH t_gtd, COUNTRY WITH t_cnt, COUNTRYC WITH t_cntc, GTD_LINE WITH t_line, B_GTD WITH bad_g
    IF EMPTY(GTD_NUM)
      is_emp = .T.
    ENDIF
  ENDSCAN
  IF is_emp
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ВНИМАНИЕ! Не везде проставлена ГТД! Продолжаем прием? [ Нет ] [ Да ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      RETURN .F.
    ENDIF
  ENDIF

  SELECT ACT_S_T
  is_usd = VALUTA = "$"
  nds_w  = NDS_
  frm000 = FIRM
  ref_dat = DOC_DATE
  ref_doc = DOC_NUM
  new_doc = Doc_Num("ACT_NUM", frm000, arm, new_dat)
  new_doc = STR(new_doc,8)+"!"
  SELECT 0
  USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS GTD_0220 AGAIN
  SELECT 0
  USE (base_path+"ACT_S") ALIAS NEW_LIST AGAIN
  SELECT CORR_ACT
  sum_h = 0
  sum_0 = 0
  SCAN FOR QNT_REST # QNT && Идем по списку и корректируем ACT_S и NEW_LIST
    qn = QNT-QNT_REST
    qn_r = IIF(ACT_S_T.FLG_BUY = "B", 0, qn)
    IF .NOT. SEEK(CORR_ACT.GTD_NUM, "GTD_0220")
      SELECT GTD_0220
      APPEND BLANK
      REPLACE GTD_NUM  WITH CORR_ACT.GTD_NUM, ;
              COUNTRY  WITH CORR_ACT.COUNTRY, ;
              COUNTRYC WITH CORR_ACT.COUNTRYC,;
              GTD_LINE WITH CORR_ACT.GTD_LINE
    ENDIF
    IF CORR_ACT.RC_NO # 0          && Приход по этой позиции был в исходном док-те
      GO CORR_ACT.RC_NO IN ACT_S
      m.tmpParty = LEFT(ACT_S.PARTY, 10)+"_"
    ELSE                  && Позиции не было в док-те - вставим!
      IF is_usd
        p_r = ROUND(CORR_ACT.PRICE*kurs_b,2)
        p_d = CORR_ACT.PRICE
        p_0 = CORR_ACT.PRICE
      ELSE
        p_r = CORR_ACT.PRICE
        p_0 = p_r*100/(100+nds_w)     && Используем процент НДС исх. акта
*        p_0 = p_r*100/(100+nds_flt)   &&    ... или текущий? Сомнительно!
        p_d = ROUND(p_r/kurs_b,2)
      ENDIF
      m.tmpParty = G_Party()
      SELECT ACT_S
      APPEND BLANK
      REPLACE FIRM     WITH ACT_S_T.FIRM,     ;
              DOC_NUM  WITH ACT_S_T.DOC_NUM,  ;
              DOC_DATE WITH ACT_S_T.DOC_DATE, ;
              INC_TYPE WITH ACT_S_T.INC_TYPE, ;
              FLG_BUY  WITH ACT_S_T.FLG_BUY,  ;
              CODE     WITH CORR_ACT.CODE,    ;
              PARTY    WITH m.tmpParty,       ;
              IDP      WITH CORR_ACT.IDP,     ;
              PRICE_0  WITH p_0,              ;
              PRICE_R  WITH p_r,              ;
              PRICE_D  WITH p_d
      m.tmpParty = LEFT(ACT_S.PARTY, 10)+"_"
    ENDIF
    REPLACE ACT_S.QNT_REST WITH ACT_S.QNT_REST+qn,  ;
            ACT_S.QNT_REP  WITH ACT_S.QNT_REP+qn_r, ;
            ACT_S.REST_REP WITH ACT_S.REST_REP+qn_r,;
            ACT_S.GTD_NUM  WITH CORR_ACT.GTD_NUM,   ;
            ACT_S.COUNTRY  WITH CORR_ACT.COUNTRY,   ;
            ACT_S.COUNTRYC WITH CORR_ACT.COUNTRYC,  ;
            ACT_S.GTD_LINE WITH CORR_ACT.GTD_LINE
    SELECT NEW_LIST
    APPEND BLANK
    REPLACE FIRM     WITH frm000,           ;
            DOC_NUM  WITH new_doc,          ;
            DOC_DATE WITH new_dat,          ;
            INC_TYPE WITH ACT_S_T.INC_TYPE, ;
            FLG_BUY  WITH ACT_S_T.FLG_BUY,  ;
            CODE     WITH CORR_ACT.CODE,    ;
            PARTY    WITH m.tmpParty,       ;
            IDP      WITH CORR_ACT.IDP,     ;
            QNT_INC  WITH qn,               ;
            PRICE_0  WITH ACT_S.PRICE_0,    ;
            PRICE_D  WITH ACT_S.PRICE_D,    ;
            PRICE_R  WITH ACT_S.PRICE_R,    ;
            GTD_NUM  WITH ACT_S.GTD_NUM,    ;
            COUNTRY  WITH ACT_S.COUNTRY,    ;
            COUNTRYC WITH ACT_S.COUNTRYC,   ;
            GTD_LINE WITH ACT_S.GTD_LINE
    sum_h = sum_h+ROUND(qn*IIF(is_usd, ACT_S.PRICE_D, ACT_S.PRICE_R),2)
    sum_0 = sum_0+qn*ACT_S.PRICE_0
    SELECT CORR_ACT
    = SEEK(NEW_LIST.CODE, "STOCK")
    REPLACE STOCK.QNT     WITH STOCK.QNT+qn,   ;
            STOCK.QNT_REP WITH STOCK.QNT_REP+qn_r
  ENDSCAN
  SELECT GTD_0220
  USE
  SELECT NEW_LIST
  USE (base_path+"ACT_S_T") ALIAS NEW_TIT AGAIN
  APPEND BLANK
  REPLACE FIRM      WITH frm000,    ;
          DOC_NUM   WITH new_doc,   ;
          DOC_DATE  WITH new_dat,   ;
          DOC_TIME  WITH SECONDS(), ;
          SUP_CODE  WITH ACT_S_T.SUP_CODE, ;
          CUS_CODE  WITH ACT_S_T.CUS_CODE, ;
          FLG_BUY   WITH ACT_S_T.FLG_BUY,  ;
          INC_TYPE  WITH ACT_S_T.INC_TYPE, ;
          INV_TYPE  WITH "9",              ;
          VALUTA    WITH ACT_S_T.VALUTA,   ;
          EXP_NUM   WITH ref_doc,   ;
          EXP_DATE  WITH ref_dat,   ;
          TOTAL_0   WITH sum_0,     ;
          NDS_      WITH nds_w,     ;
          TOTAL_H   WITH sum_h,     ;
          SEND_M    WITH 0,         ;
          REP_SUP   WITH 0,         ;
          WHO_ACT   WITH user,      ;
          WHERE_ACT WITH arm
  SELECT 0
  USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS MON_9128 AGAIN
  IF .NOT. SEEK(STR(NEW_TIT.SUP_CODE)+frm000)
    APPEND BLANK
    REPLACE FIRM WITH frm000, CUS_CODE WITH NEW_TIT.SUP_CODE
  ENDIF
  IF NEW_TIT.VALUTA = "$"
    REPLACE ACC_SUMD WITH ACC_SUMD+NEW_TIT.TOTAL_H
  ELSE
    REPLACE ACC_SUM WITH ACC_SUM+NEW_TIT.TOTAL_H
  ENDIF
  USE
  SELECT NEW_TIT
  USE
  IF prn_d
    DO P_Act WITH frm000, new_doc, new_dat
  ENDIF
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME
PRIVATE nm_w

*
*   Вот здесь, рисуем...
*
@ WROWS()-3, 21 SAY IIF("!" $ DOC_NUM, "  Пр. акт ", "Накладная ")
@ ROW(), COL() SAY EXP_NUM COLOR (SCHEME(13,6))
@ ROW(), COL() SAY " от "
@ ROW(), COL() SAY DTOC(EXP_DATE) COLOR (SCHEME(13,6))
nm_w = ALLTRIM(P_9128.FAMILY)+" "+LEFT(P_9128.NAME,1)+". "+LEFT(P_9128.S_NAME,1)+"."
nm_w = PADR(nm_w, 30)
@ WROWS()-2, 16 SAY nm_w COLOR (SCHEME(13,6))
@ WROWS()-2, 58 SAY A_9128.ARM_N COLOR (SCHEME(13,6))

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Rnd_R        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Округление (искривление) рублевой цены.                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 28.01.1999 ──┘
PROCEDURE Rnd_R

IF ACT_S_T.VALUTA = "$"
  RETURN .T.
ENDIF

REPLACE PRICE WITH Round_6(PRICE, ACT_S_T.NDS_)

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_GTD0     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Выбор ГТД.                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 16.02.2000 ──┘
PROCEDURE Get_GTD0

PRIVATE tmpGTD, point1, point2, point3, mss, s_sav

IF CORR_ACT.CODE = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Номер ГТД для пустого количества!?"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
IF CORR_ACT.RC_NO # 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Коррекция старого номера ГТД запрещена!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
IF CORR_ACT.QNT = 0 .AND. CORR_ACT.QNT_INV = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  Номер ГТД для пустого количества! Это правильно? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN
  ENDIF
ENDIF

tmpGTD = Ch_GTD(CORR_ACT.GTD_NUM)
point1 = AT(CHR(13), tmpGTD, 1)
point2 = AT(CHR(13), tmpGTD, 2)
point3 = AT(CHR(13), tmpGTD, 3)
s_sav = SELECT()
SELECT CORR_ACT
IF point1 # 0
  REPLACE GTD_NUM  WITH LEFT(tmpGTD,point1-1), ;
          COUNTRY  WITH SUBSTR(tmpGTD,point1+1, point2-point1-1),;
          GTD_LINE WITH SUBSTR(tmpGTD,point2+1, point3-point2-1),;
          COUNTRYC WITH SUBSTR(tmpGTD,point3+1)
ENDIF
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура New_GTD      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Новая ГТД                                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 08/08/2000 ──┘
PROCEDURE New_GTD
PARAMETERS doc_f, doc_n, doc_d

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

*┌──────────────────────────────────┐
*│  ┌─ Номер ГТД ────────────────┐  │
*│  │ ....:....!....:....!....:. │  │
*│  └────────────────────────────┘  │
*│     Дата ГТД ДД.ММ.ГГГГ [ ]      │
*│   [ ] Страна ....:....!....:.    │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, gtd_n, cntry_n, gtd_l, sw, gtd_d, sw0
ex = 1
sw = .F.
sw0 = .F.
gtd_n   = SPACE(26)
cntry_n = SPACE(16)
gtd_d   = {}
gtd_l   = SPACE(4)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 38, "Новая ГТД"
DO Sun_Bord WITH  2,  4,  4, 33, " Номер ГТД "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  6 GET gtd_n
@ 5,  7 SAY "Дата ГТД" GET gtd_d
@ 5, 27 GET sw0 PICTURE "@*C " VALID G_Dt()
@ 6,  5 GET sw PICTURE "@*C Страна" VALID G_Cnt()
@ 6, 16 GET cntry_n WHEN .F.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Bl()

IF ex = 1
  *
  * Отрабатываем бланк
  PRIVATE s_sav, is_doc
  s_sav = SELECT()
  SELECT 0
  is_doc = .T.
  USE (path_comm+"GTD_TIT") ORDER TAG GTD_NUM AGAIN ALIAS GTDT_0808
  IF SEEK(gtd_n)
    PRIVATE mss
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Такой документ уже существует!"
    mss(3) = CHR(0)+"  Как поступить? [ Добавить позиции ] [ Отказаться ]"
    mss(4) = ""
    IF Out_Mess(7, "mss") = 2
      USE
      SELECT (s_sav)
      is_doc = .F.
    ENDIF
  ELSE
    APPEND BLANK
    REPLACE GTD_NUM  WITH gtd_n,  ;
            GTD_DATE WITH gtd_d
  ENDIF
  IF is_doc
    USE (base_path+"STOCK") ORDER TAG CODE AGAIN ALIAS STO_0808
    SELECT 0
    USE (path_comm+"GTD") AGAIN ALIAS GTD_0808
    SELECT 0
    USE (base_path+"ACT_S") ORDER TAG DOC_NUM AGAIN ALIAS ACT_0808
    SET RELATION TO CODE INTO STO_0808
    SEEK doc_f+LEFT(DTOS(doc_d),4)+doc_n
    SCAN REST WHILE doc_d = DOC_DATE .AND. doc_n = DOC_NUM .AND. doc_f = FIRM
      REPLACE GTD_NUM WITH gtd_n, COUNTRY WITH cntry_n
      SELECT GTD_0808
      APPEND BLANK
      REPLACE GTD_NUM  WITH gtd_n,  ;
              GTD_DATE WITH gtd_d,  ;
              COUNTRY  WITH cntry_n,;
              QNT      WITH ACT_0808.QNT_INC, ;
              NAME     WITH ALLTRIM(STO_0808.PREFIX)+ALLTRIM(STO_0808.NAME)
      SELECT ACT_0808
    ENDSCAN
    USE
    SELECT STO_0808
    USE
    SELECT GTD_0808
    USE
    SELECT (s_sav)
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Cnt        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Ввод страны.                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 08.08.2000 ──┘
PROCEDURE G_Cnt

IF .NOT. sw
  RETURN .T.
ENDIF

sw = .F.
cntry_n = Country(cntry_n)

SHOW GET sw
SHOW GET cntry_n

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_dt         Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Ввод даты по календарю.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 08.08.2000 ──┘
PROCEDURE G_dt

IF .NOT. sw0
  RETURN .T.
ENDIF

sw0 = .F.
gtd_d = Get_Date(gtd_d)

SHOW GET sw0
SHOW GET gtd_d

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Bl       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка корректности заполнения бланка.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 08.08.2000 ──┘
PROCEDURE Tst_Bl
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(gtd_n)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали номер ГТД"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(gtd_d)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали дату ГТД"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(cntry_n)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали страну"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_GTD1     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Выбор ГТД.                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.11.2011 ──┘
PROCEDURE Get_GTD1
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
PRIVATE tmpGtd, tmpLine, tmpCountry, tmpCntC, tmpSw

*
*   Заполняем значениями переменные состояния...
*
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()
m.tmpGtd = CORR_ACT.GTD_NUM
m.tmpLine = CORR_ACT.GTD_LINE
m.tmpCountry = CORR_ACT.COUNTRY
m.tmpCntC    = CORR_ACT.COUNTRYC
m.tmpSw = .F.

*------------------------------------------------------------------------
*    Содержательная часть программы:
*
*┌─────────────────────────────────────────────────────┐
*│ Номер ГТД                                           │
*│                                                     │
*│ Строка ГТД             Страна                       │
*│                                                     │
*│          < OK Ctrl-W > < Отказаться Esc >           │
*└─────────────────────────────────────────────────────┘

PRIVATE ex, wd, l1, l2
m.wd = MAX(16+LEN(m.tmpGtd), 31+LEN(m.tmpLine+m.tmpCountry))
l1 = FLOAT((m.wd-10-LEN(m.tmpGtd))/2)
l2 = FLOAT((m.wd-18-LEN(m.tmpLine+m.tmpCountry))/2)
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, m.wd, "ГТД"
*DO Sun_Bord WITH  2,  2,  4, 49, " Заголовок рамки "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, l1 SAY "Номер ГТД" GET m.tmpGtd
@ 4,  2 SAY "Строка ГТД" GET m.tmpLine
@ 4, COL()+1 SAY "Страна" GET m.tmpSw PICTURE "@*C" VALID S_Cnt1()
@ 4, COL()   GET m.tmpCntC VALID S_Cnt()
@ 4, COL()+1 GET m.tmpCountry WHEN .F.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF m.ex = 1
  *
  * Отрабатываем бланк
  REPLACE	CORR_ACT.GTD_NUM	WITH m.tmpGtd,	;
			CORR_ACT.GTD_LINE	WITH m.tmpLine,	;
			CORR_ACT.COUNTRYC	WITH m.tmpCntC, ;
			CORR_ACT.COUNTRY	WITH m.tmpCountry
  IF CORR_ACT.RC_NO # 0
    GO CORR_ACT.RC_NO IN ACT_S
    REPLACE	ACT_S.GTD_NUM	WITH m.tmpGtd,	;
			ACT_S.GTD_LINE	WITH m.tmpLine,	;
			ACT_S.COUNTRYC	WITH m.tmpCntC, ;
			ACT_S.COUNTRY	WITH m.tmpCountry
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Cnt        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Установка страны.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.02.2012 ──┘
PROCEDURE S_Cnt

PRIVATE tmpV

IF EMPTY(m.tmpCntC)
  m.tmpCountry = SPACE(LEN(tmpCountry))
  SHOW GET m.tmpCountry
  RETURN .T.
ENDIF
m.tmpV = ALLTRIM(m.tmpCntC)
m.tmpV = PADL(m.tmpV, LEN(m.tmpCntC), "0")
IF .NOT. SEEK(m.tmpV, "OKSM")
  m.tmpCountry = SPACE(LEN(tmpCountry))
  m.tmpCntC    = SPACE(LEN(tmpCntC))
  SHOW GET m.tmpCountry
  SHOW GET m.tmpCntC
  RETURN .F.
ENDIF
m.tmpCntC = m.tmpV
m.tmpCountry = LEFT(OKSM.B_NAME, LEN(tmpCountry))
SHOW GET m.tmpCountry
SHOW GET m.tmpCntC

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Cnt1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                                 Выбор страны.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.02.2012 ──┘
PROCEDURE S_Cnt1

PRIVATE tmpV

m.tmpSw = .F.
m.tmpV = Ctrl_Ch(m.tmpCntC)
IF .NOT. EMPTY(m.tmpV)
  m.tmpCntC = LEFT(m.tmpV, 3)
  m.tmpCountry = SUBSTR(m.tmpV, 4, LEN(m.tmpCountry))
  SHOW GET m.tmpCntC
  SHOW GET m.tmpCountry
ENDIF
SHOW GET m.tmpSw
