*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Corr_Fre     Разработчик Андрей Васин           21.06.97 15:25:14 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║         Программа коррекции и формирования свободного списка.          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Corr_Fre

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

DEFINE PAD Sw_Mode OF (menu_name) PROMPT "Сп/Ном." KEY Ctrl-A, "F2"
ON SELECTION PAD Sw_Mode OF (menu_name) Do Sw_Mode WITH "Sw_Mode"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) Do Sw_Mode WITH "Seek1"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, h_list, chr_shab, int_1, int_2
DIMENSION int_1(3), int_2(3)   && Описание пунктов меню для программы поиска
int_1(1) = 27      && Esc
int_2(1) = "{Esc}"
int_1(2) = -6      && F7
int_2(2) = "{F7}"
int_1(3) = -1      && F2
int_2(3) = "{F2}"

chr_shab = ' !"'+    ;
           "#$%&'()*+,-./0123456789:;<=>?@"+  ;
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"+         ;
           "[\]^_`"+                             ;
           "abcdefghijklmnopqrstuvwxyz"+         ;
           "{|}~"+                               ;
           "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"+   ;
           "абвгдежзийклмнопрстуфхцчшщъыьэюя"
ln = 100   && Количество видимых строк BROWSE
DO CASE
CASE WK_TIT.IS_QNT .AND. WK_TIT.IS_PRICE
  wd = 65
CASE WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
  wd = 56
CASE .NOT. WK_TIT.IS_QNT .AND. WK_TIT.IS_PRICE
  wd = 58
CASE .NOT. WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
  wd = 51
ENDCASE

h_list = .T.

DO D_Wins WITH ln, wd, ALLTRIM(WK_TIT.L_NAME), 1, 0

@ 1, 3 SAY "Поиск:"
@ 1, 10 SAY PADR("Tab",FSIZE("NAME","STOCK")) COLOR (SCHEME(14,2))
IF .NOT. WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
  @ 1, 17 SAY "Отметка: "
  @ 1, COL() SAY PADR("Пробел",WCOLS()-COL()-1) COLOR (SCHEME(14,2))
ENDIF
@ 2,  4 SAY " Позиция "

what_do = "List"
SELECT STOCK
SET ORDER TO TAG NAME
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Tab DO Seek_
    ON KEY LABEL F2 DO Sw_Mode WITH "Sw_Mode"
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek1"
    ON KEY LABEL F5 DO Sw_Mode WITH "Addr"
    ON KEY LABEL F3 DO Sw_Mode WITH "New_pos"
    ON KEY LABEL F4 DO Sw_Mode WITH "Due"
    IF .NOT. WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
      ON KEY LABEL SpaceBar DO Mov_Rec
    ENDIF

*
*    BROWSE - меню
*
    IF h_list           && Вся номенклатура
      SELECT TMP_WK
      SET FILTER TO
      SET ORDER TO TAG CODE
      SELECT STOCK
      SET ORDER TO TAG NAME
      SET RELATION TO IIF(SEEK(CODE,"TMP_WK"), CODE, 0) INTO TMP_WK
    ELSE
      SELECT STOCK
      SET ORDER TO TAG CODE
      SELECT TMP_WK
      SET FILTER TO CODE # 0
      SET ORDER TO TAG NAME
      SET RELATION TO CODE INTO STOCK
    ENDIF
    DO CASE
    CASE WK_TIT.IS_QNT .AND. WK_TIT.IS_PRICE
      BROWSE FIELDS ;
             PREFIX:H="":4:W=.F.,    ;
             NAME:H="":35:W=.F.,     ;
             PRODUCER:H="":8:W=.F.,  ;
             TMP_WK.QNT:H="":P="@Z 999999":V=Mov_Rec(), ;
             TMP_WK.PRICE:H="":P="@Z 99999.99":W=TMP_WK.QNT#0 ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    CASE WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
      BROWSE FIELDS ;
             PREFIX:H="":4:W=.F.,    ;
             NAME:H="":35:W=.F.,     ;
             PRODUCER:H="":8:W=.F.,  ;
             TMP_WK.QNT:H="":P="@Z 999999":V=Mov_Rec() ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    CASE .NOT. WK_TIT.IS_QNT .AND. WK_TIT.IS_PRICE
      BROWSE FIELDS ;
             PREFIX:H="":4:W=.F.,    ;
             NAME:H="":35:W=.F.,     ;
             PRODUCER:H="":8:W=.F.,  ;
             TMP_WK.PRICE:H="":P="@Z 99999.99":V=Mov_Rec() ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    CASE .NOT. WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
      BROWSE FIELDS ;
             PREFIX:H="":4:W=.F.,    ;
             NAME:H="":35:W=.F.,     ;
             PRODUCER:H="":8:W=.F.,  ;
             F000 = IIF(TMP_WK.QNT # 0, "√", " "):H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDCASE
    SET RELATION TO
    IF what_do = "List"
      what_do = "Exit"
    ENDIF
    ON KEY

  CASE what_do = "Sw_Mode"    && Просмотр списка

    h_list = .NOT. h_list
    what_do = "List"

  CASE what_do = "Seek1"      && Контекстный поиск

    DO Seek1
    what_do = "List"

  OTHERWISE

    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Сохраним изменения? [ Да ] [ Нет ]"
    mss(3) = ""
    ln = Out_Mess(5, "mss")
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN ln = 1

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                    Добавление или улаление записи.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.06.97 16:11:21 ═╝
PROCEDURE Mov_Rec
PRIVATE q_sav, p_sav, nv

nv = 0
q_sav = 0
p_sav = 0

IF WK_TIT.IS_QNT      && Количество живет и доминирует!
  IF TMP_WK.QNT = 0 
    IF TMP_WK.CODE # 0
      nv = -1
    ENDIF
  ELSE
    IF TMP_WK.CODE = 0
      q_sav = QNT
      p_sav = PRICE
      nv = 1
    ENDIF
  ENDIF
ELSE                  && Количество не живет:
  IF .NOT. WK_TIT.IS_PRICE   &&   только пометки
    IF TMP_WK.QNT # 0 
      IF TMP_WK.CODE # 0
        nv = -1
      ENDIF
    ELSE
      IF TMP_WK.CODE = 0
        q_sav = 1
        nv = 1
      ENDIF
    ENDIF
  ELSE                       &&   только цена
    IF TMP_WK.PRICE = 0 
      IF TMP_WK.CODE # 0
        nv = -1
      ENDIF
    ELSE
      IF TMP_WK.CODE = 0
        q_sav = QNT
        p_sav = PRICE
        nv = 1
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF nv = 1
  SELECT TMP_WK
  REPLACE QNT WITH 0, PRICE WITH 0
  APPEND BLANK
  REPLACE LIST_NO  WITH RECNO("WK_TIT"),;
          CODE     WITH STOCK.CODE,     ;
          PREFIX   WITH STOCK.PREFIX,   ;
          NAME     WITH STOCK.NAME,     ;
          PRODUCER WITH STOCK.PRODUCER, ;
          PRICE    WITH p_sav,          ;
          QNT      WITH q_sav
  SELECT STOCK
  SHOW WINDOW (ALIAS()) SAME REFRESH
ENDIF
IF nv = -1
  SELECT TMP_WK
  DELETE
  IF h_list
    SELECT STOCK
  ENDIF
  SHOW WINDOW (ALIAS()) SAME REFRESH
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Программа поиска по начальным буквам.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.97 22:05:53 ═╝
PROCEDURE Seek_

PRIVATE s, prf, ln, bg, rc_s, i

ln = FSIZE("NAME", "STOCK")
ACTIVATE WINDOW (win_name+"_M") SAME
prf = ""
@ 1, 3 SAY "Поиск:"
@ 1, 10 SAY SPACE(WCOLS()-9)
bg = REPLICATE("▒", ln)
DO WHILE .T.
  @ 1, 10 SAY bg
  @ 1, 10 SAY prf
  s = INKEY(0)
  i = ASCAN(int_1, s)
  IF i # 0
    KEYBOARD int_2(i)
    EXIT
  ENDIF
  IF s < 0     && Функциональная клавиша
    IF s = -9  && F10 - запускаем меню
      KEYBOARD "{F10}"
      EXIT
    ENDIF
    LOOP
  ENDIF
  s = CHR(s)
  IF s $ chr_shab .AND. LEN(prf) < ln   && Символ
    rc_s = RECNO()
    IF SEEK(prf+s)
      prf = prf+s
    ELSE
      IF rc_s < RECCOUNT() .AND. rc_s > 0
        GO rc_s
      ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH
    LOOP
  ENDIF
  IF s = CHR(13)       && Enter
    EXIT
  ENDIF
  IF s = CHR(3)        && PgDn
    KEYBOARD "{PgDn}"
    EXIT
  ENDIF
  IF s = CHR(18)       && PgUp
    KEYBOARD "{PgUp}"
    EXIT
  ENDIF
  IF s = CHR(5)        && UpArrow
    KEYBOARD "{UpArrow}"
    EXIT
  ENDIF
  IF s = CHR(24)       && DnArrow
    KEYBOARD "{DnArrow}"
    EXIT
  ENDIF
  IF s = CHR(127)      && BackSpace
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      SEEK prf
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
    LOOP
  ENDIF
ENDDO

@ 1, 10 SAY PADR("Tab",ln) COLOR (SCHEME(14,2))
IF .NOT. WK_TIT.IS_QNT .AND. .NOT. WK_TIT.IS_PRICE
  @ 1,17 SAY "Отметка: "
  @ 1, COL() SAY PADR("Пробел",WCOLS()-COL()-1) COLOR (SCHEME(14,2))
ENDIF

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
DO D_Win_N WITH 8, 57, "Поиск в "+IIF(h_list, "номенклатуре.", "списке.")

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  3 SAY "Задайте шаблон для поиска" GET nm_w
@ 5, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
nm_w = ALLTRIM(nm_w)

IF ex = 1 .AND. .NOT. EMPTY(nm_w)
  DO Choose_P WITH nm_w, h_list
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
PARAMETERS shab, is_stock
PRIVATE sel_sav

sel_sav = SELECT()

DO Wt_Mess WITH "Подборка..."
IF is_stock
  SELECT PREFIX, NAME, PRODUCER, RECNO() AS REC_N FROM STOCK  ;
         INTO CURSOR TMP_C   ;
         WHERE (shab $ NAME .OR. shab $ PREFIX .OR. shab $ PRODUCER)  ;
               .AND. EMPTY(DATE_OFF)
ELSE
  SELECT PREFIX, NAME, PRODUCER, RECNO() AS REC_N FROM INC_LIST  ;
         INTO CURSOR TMP_C   ;
         WHERE (shab $ NAME .OR. shab $ PREFIX .OR. shab $ PRODUCER)  ;
               .AND. LIST_NO = key_v .AND. (QNT # 0 .OR. QNT_INV # 0)
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
wd = 49
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
    BROWSE FIELDS F000 = PREFIX+" "+NAME+" "+PRODUCER:H="":49,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

    IF is_stock
      GO TMP_C.REC_N IN STOCK
    ELSE
      GO TMP_C.REC_N IN INC_LIST
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
