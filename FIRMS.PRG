*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Firms        Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Коррекция списка предприятий.                      ║
*║ Модифицирована А.Драчевым - устан.поля DEF_WCHK и LET_WCHK в FIRMS     ║
*╚══════════════════════════════════════════════════════════ 01/12/1999 ══╝
PROCEDURE Firms

PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
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
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"FIRMS") ORDER TAG ALL_NAMES ALIAS F_9112 AGAIN

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Корр." KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "Добавл." KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "Удал./Восст." KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Pay OF (menu_name) PROMPT "Платежи" KEY Ctrl-A, "Tab"
ON SELECTION PAD Pay OF (menu_name) DO Sw_Mode WITH "Pay"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
*┌Код─Полное название────────────────────────────────────Удалено───┐
*│999 99999999999999999999999999999999999999999999999999 99.99.9999│
*└─────────────────────────────────────────────────────────────────┘
*┌─────────────────────────────────────────────────────────────────┐
*│   Короткое название 999999999999999     Телефон 999999999999    │
*│ Адрес юр.    99999999999999999999999999999999999999999999999999 │
*│ Адрес факт.  99999999999999999999999999999999999999999999999999 │
*│ ИНН 999999999999   Чеки: [ ] по умолчанию,  [ ] выбор разрешен  │ && NEW 22.12.1999
*└─────────────────────────────────────────────────────────────────┘
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 65, 63)

DO D_Wins WITH ln, wd, "Список предприятий", 0, 4
@ 1, 2 SAY "Код─Полное название────────────────────────────────────Удалено"
@ WROWS()-5,  5 SAY "Короткое название                     Телефон"
@ WROWS()-4,  3 SAY "Адрес юр."
@ WROWS()-3,  3 SAY "Адрес факт."
@ WROWS()-2,  3 SAY "ИНН 999999999999   Чеки: [ ] по умолчанию   [ ] Выбор разрешен"   && NEW 22.12.1999

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del   DO Del_Firm
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Pay"

*
*    BROWSE - меню
*
    BROWSE FIELDS FIRM_CODE:H="", ;
                  L_NAME:H="",    ;
                  DATE_OFF:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && Коррекция фирмы

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO Modi_Line
    ENDIF
    what_do = "List"

  CASE what_do = "Del"     && Удаление/восстановление

    DO Del_Firm
    what_do = "List"

  CASE what_do = "Add"     && Добавление фирмы

    DO Modi_Line WITH .T.
    what_do = "List"

  CASE what_do = "Pay"     && Допустимые типы платежей

    DO Let_Pay WITH FIRM_CODE
    what_do = "List"

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
USE

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-5, 23 SAY B_NAME
@ WROWS()-5, 51 SAY PHONE
@ WROWS()-4, 16 SAY ADDRESS
@ WROWS()-3, 16 SAY ADDR_FACT
@ WROWS()-2,  7 SAY INN                   && NEW 22.12.1999
@ WROWS()-2, 29 SAY IIF(DEF_WCHK,"X"," ") && NEW 22.12.1999
@ WROWS()-2, 48 SAY IIF(LET_WCHK,"X"," ") && NEW 22.12.1999

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_Line    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Добавление/коррекция предприятия.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 01/12/1999 ──┘
PROCEDURE Modi_Line
PARAMETERS new

*
*  Переменные состояния для навигации
*
IF .NOT. EMPTY(DATE_OFF) .AND. .NOT. new
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Фирма удалена, какой смысл ее корректировать?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
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

*┌────────────────────────────────────────────────────────────────────────┐
*│ ┌─ Предприятие ──────────────────────────────────────────────────────┐ │
*│ │            Код 999   Короткое название 9999999999999999            │ │
*│ │                                                                    │ │
*│ │ Полное название 99999999999999999999999999999999999999999999999999 │ │
*│ └────────────────────────────────────────────────────────────────────┘ │
*│      Адрес юр. 99999999999999999999999999999999999999999999999999      │
*│                                                                        │
*│     Адрес факт. 99999999999999999999999999999999999999999999999999     │
*│                                                                        │
*│      Телефон 999999999999   ИНН 9999999999   КПП ....:....!....:.      │
*│                                                                        │
*│             [ ] Есть кассовый аппарат    Номер аппарата ##             │
*│                                                                        │
*│            Чеки:   [ ] По умолчанию    [ ] Выбор разрешен              │&& NEW 22.12.1999
*│                                                                        │
*│   Налог с продаж 999.9   Максимальная сумма наличной оплаты 99999999   │
*│                                                                        │
*│ Руковод. xxxxxxxxxXxxxxxxxxxXxxxx   Гл. бухг. xxxxxxxxxXxxxxxxxxxXxxxx │
*│                                                                        │
*│                    Кассир xxxxxxxxxXxxxxxxxxxXxxxx                     │
*│                                                                        │
*│                    < OK Ctrl-W > < Отказаться Esc >                    │
*└────────────────────────────────────────────────────────────────────────┘


PRIVATE ex, cd, n0, n1, ad, ad_f, c_i, ph, nl, sum, d_chk, l_chk   && NEW 22.12.1999
PRIVATE is_kkm, kkm_no, face1, face2, face3, tmpKPP
IF new
  cd  = "   "
  n0  = SPACE(16)
  n1  = SPACE(50)
  ad  = SPACE(50)
  ad_f  = SPACE(50)
  tmpKPP = SPACE(16)
  tmpKPP = KPP
  ph  = SPACE(12)
  nl  = 0
  sum = 0
  d_chk = .F.                                         && NEW 22.12.1999
  l_chk = .F.                                         && NEW 22.12.1999
  is_kkm = .F.
  kkm_no = "  "
  face1 = SPACE(24)
  face2 = SPACE(24)
  face3 = SPACE(24)
ELSE
  cd  = FIRM_CODE
  n0  = B_NAME
  n1  = L_NAME
  ad  = ADDRESS
  ad_f  = ADDR_FACT
  c_i = INN
  tmpKPP = KPP
  ph  = PHONE
  nl  = P_NALOG
  sum = M_SUM
  d_chk = DEF_WCHK                                    && NEW 22.12.1999
  l_chk = LET_WCHK                                    && NEW 22.12.1999
  is_kkm = .NOT. EMPTY(KKM)
  kkm_no = KKM
  face1 = BOSS
  face2 = ABAK
  face3 = KASSIR
ENDIF

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 25, 76, IIF(new, "Добавление ", "Коррекция ")+"предприятия" && NEW 22.12.1999
DO Sun_Bord WITH  2,  3,  6, 72, " Предприятие "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3, 16 SAY "Код" GET cd WHEN new VALID Up_Case()
@  3, 26 SAY "Короткое название" GET n0
@  5,  5 SAY "Полное название" GET n1
@  7,  8 SAY "Адрес юр." GET ad
@  9,  8 SAY "Адрес факт." GET ad_f
@ 11,  7 SAY "Телефон" GET ph
@ 11, 30 SAY "ИНН" GET c_i
@ 11, 48 SAY "КПП" GET tmpKPP
@ 13, 15 GET is_kkm PICTURE "@*C Есть кассовый аппарат" VALID TstKKM()
@ 13, 44 SAY "Номер аппарата" GET kkm_no PICTURE "##" WHEN is_kkm
@ 15, 14 SAY "Чеки:   " GET  d_chk PICTURE "@*C По умолчанию" WHEN is_kkm    && NEW 22.12.1999
@ 15, 42 GET  l_chk PICTURE "@*C Выбор разрешен" WHEN is_kkm   && NEW 22.12.1999
@ 17,  5 SAY "Налог с продаж" GET nl PICTURE "@Z 999.9"
@ 17, 28 SAY "Максимальная сумма наличной оплаты" GET sum PICTURE "@Z 99999999"
@ 19,  3 SAY "Руковод." GET face1
@ 19, 39 SAY "Гл. бухг." GET face2
@ 21, 22 SAY "Кассир" GET face3
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * Отрабатываем бланк
  IF new
    APPEND BLANK
  ENDIF
  REPLACE FIRM_CODE WITH cd, ;
          B_NAME    WITH n0, ;
          L_NAME    WITH n1, ;
          ADDRESS   WITH ad, ;
          ADDR_FACT WITH ad_f, ;
          PHONE     WITH ph, ;
          INN       WITH c_i,;
          KPP       WITH tmpKPP,;
          P_NALOG   WITH nl, ;
          M_SUM     WITH sum,;
          KKM       WITH kkm_no, ;
          BOSS      WITH face1,;
          ABAK      WITH face2,;
          KASSIR    WITH face3,;
          DEF_WCHK  WITH d_chk,;
          LET_WCHK  WITH l_chk      && NEW 22.12.1999
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Up_Case      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Перевод кода в верхний регистр.                     │
*│                                                                        │
*└────────────────────────────────────────────────────────── 01/12/1999 ──┘
PROCEDURE Up_Case

cd = CHRTRAN(cd, shab1, shab2)
SHOW GET cd
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка корректности заполнения бланка.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 01/12/1999 ──┘
PROCEDURE Tst_Blank

PRIVATE rc_sav, mss
IF ex = 2
  RETURN .T.
ENDIF

IF new
  IF EMPTY(cd)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Код предприятия не может быть пустым!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    KEYBOARD REPLICATE("{Tab}",2)
    RETURN .F.
  ENDIF
  rc_sav = RECNO()
  SET ORDER TO TAG FIRM_CODE
  IF SEEK(cd)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Код предприятия должет быть уникальным!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SET ORDER TO TAG ALL_NAMES
    IF BETWEEN(rc_sav, 1, RECCOUNT())
      GO rc_sav
    ENDIF
    KEYBOARD REPLICATE("{Tab}",2)
    RETURN .F.
  ENDIF
  SET ORDER TO TAG ALL_NAMES
  IF BETWEEN(rc_sav, 1, RECCOUNT())
    GO rc_sav
  ENDIF
ENDIF
IF EMPTY(n0)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Короткое имя не может быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",3)
  RETURN .F.
ENDIF
IF EMPTY(n1)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Полное имя не может быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",4)
  RETURN .F.
ENDIF
IF nl < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Таких налогов не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",4)
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Del_Firm     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Удаление/восстановление предприятия.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 01/12/1999 ──┘
PROCEDURE Del_Firm

IF BETWEEN(RECNO(), 1, RECCOUNT())
  IF EMPTY(DATE_OFF)
    REPLACE DATE_OFF WITH DATE()
  ELSE
    REPLACE DATE_OFF WITH {}
  ENDIF
ENDIF
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TstKKM       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Поле наличия ККМ.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 14.09.2000 ──┘
PROCEDURE TstKKM

IF is_kkm
  kkm_no = " 0"
  SHOW GET kkm_no
ELSE
  kkm_no = "  "
  l_chk  = .F.
  d_chk  = .F.
  SHOW GET kkm_no
  SHOW GET l_chk
  SHOW GET d_chk
ENDIF

RETURN .T.

