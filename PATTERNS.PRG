*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Patterns     Разработчик Андрей Васин           31.01.97 20:12:55 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║          Программа создания и коррекции шаблонов документов.           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Patterns

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

DEFINE PAD Corr OF (menu_name) PROMPT "Коррекция" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "Добавление" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "Удаление" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) Do Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy

SELECT 0
USE PATTERN

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 100   && Количество видимых строк BROWSE
wd = FSIZE("DOC_PROMPT")+2

DO D_Wins WITH ln, wd, "Шаблоны документов", 0, 0
what_do = IIF(RECCOUNT() > 0, "List", "Add")

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "Add"
    ON KEY LABEL Del   Do Sw_Mode WITH "Del"

*
*    BROWSE - меню
*
    BROWSE FIELDS F001 = IIF(EMPTY(DATE_OFF), " ", ""):H="",  ;
           DOC_PROMPT:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && Коррекция документа

    DO Corr_Doc WITH "Corr"
    what_do = "List"

  CASE what_do = "Add"     && Добавление документа

    DO Corr_Doc WITH "Add"
    what_do = IIF(RECCOUNT() = 0, "Exit", "List")

  CASE what_do = "Del"     && Удаление/восстановление документа

    IF EMPTY(DATE_OFF)
      REPLACE DATE_OFF WITH DATE()
    ELSE
      REPLACE DATE_OFF WITH {}
    ENDIF
    what_do = IIF(RECCOUNT() = 0, "Exit", "List")

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
*║                    Коррекция добавление документа.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 31.01.97 20:29:51 ═╝
PROCEDURE Corr_Doc
PARAMETERS mode

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

PRIVATE ex, d_name, d_text, p_text, p_numb, p_len, fnt, ff_sw, ff_ln
PRIVATE i
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
IF mode = "Add"
  d_name = SPACE(40)
  d_text = ""
  p_numb = 1
  p_len  = 66
  fnt    = 1
  ff_sw  = 1
  ff_ln  = 0
  DIMENSION p_text(1)
  p_text(1) = ""
ELSE
  d_name = DOC_PROMPT
  d_text = PATTERN
  p_len  = PAGE_LEN
  fnt    = IIF(EMPTY(FONT), 1, 2)
  ff_sw  = IIF(F_FEED < 0, 1, 2)
  ff_ln  = MAX(F_FEED, 0)
  p_numb = 0
  i = AT(CHR(12),d_text)
  DO WHILE i # 0
    p_numb = p_numb+1
    DIMENSION p_text(p_numb)
    p_text(p_numb) = LEFT(d_text, i-1)
    d_text = SUBSTR(d_text, i+1)
    i = AT(CHR(12),d_text)
  ENDDO
  p_numb = p_numb+1
  DIMENSION p_text(p_numb)
  p_text(p_numb) = d_text
ENDIF
  
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 13, 57, "Параметры шаблона"
DO Sun_Bord WITH 3, 7, 5, 49
@ 3, 9 SAY " Шрифт "
DO Sun_Bord WITH 6, 7, 8, 49
@ 6, 9 SAY " Прогон в конце документа "
i = .F.

*------------------------------------------------------------------------
*      Ввод полей бланка
*

*┌─────────────────────────────────────────────────────┐
*│  Документ ....:....!....:....!....:....!....:....!  │
*│     ┌─ Шрифт ─────────────────────────────────┐     │
*│     │      ( ) Нормальный     ( ) Сжатый      │     │
*│     └─────────────────────────────────────────┘     │
*│     ┌─ Прогон в конце документа ──────────────┐     │
*│     │     ( ) Формата    ( ) Строк    999     │     │
*│     └─────────────────────────────────────────┘     │
*│   Число страниц 99    Длина страницы в строках 99   │
*│               [ ] Содержимое шаблона                │
*│          < OK Ctrl-W > < Отказаться Esc >           │
*└─────────────────────────────────────────────────────┘

@ 2,  4 SAY "Документ" GET d_name VALID Tst_F(1)
@ 4, 14 GET fnt PICTURE "@*RH Нормальный    ;Сжатый"
@ 7, 13 GET ff_sw PICTURE "@*RH Формата   ;Строк" VALID Tst_F(2)
@ 7, 41 GET ff_ln PICTURE "@Z 999" WHEN ff_sw = 2  VALID Tst_F(3)
@ 9,  5 SAY "Число страниц" GET p_numb PICTURE "@Z 99" VALID Tst_F(4)
@ 9, 25 SAY "Длина страницы в строках" GET p_len PICTURE "@Z 99" VALID Tst_F(5)
@ 10, 17 GET i PICTURE "@*C Содержимое шаблона" VALID Tst_F(6)
@ 11, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE

IF ex = 1
  IF mode = "Add"
    SET ORDER TO TAG CODE
    GO BOTTOM
    IF BOF() .OR. EOF()
      ex = 1
    ELSE
      ex = DOC_CODE+1
    ENDIF
    APPEND BLANK
    REPLACE DOC_CODE WITH ex
  ENDIF
  d_text = ""
  FOR ex = 1 TO ALEN(p_text)-1
    d_text = d_text+p_text(ex)+CHR(12)
  ENDFOR
  d_text = d_text+p_text(ALEN(p_text))
  REPLACE DOC_PROMPT WITH d_name,  ;
          PAGE_LEN   WITH p_len,   ;
          FONT       WITH IIF(fnt = 1, " ", "1"), ;
          F_FEED     WITH IIF(ff_sw = 1, -1, ff_ln),  ;
          PATTERN    WITH d_text
ENDIF

*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║               Программа проверки выхода из полей бланка.               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 31.01.97 22:33:49 ═╝
PROCEDURE Tst_F
PARAMETER fld

PRIVATE mss, m, w

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

DO CASE
CASE fld = 1        && Название документа
  IF EMPTY(d_name)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Пустое название?... Ох не надо бы..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
CASE fld = 2        && Переключатель прогона
  IF ff_sw = 1
    ff_ln = 0
    SHOW GET ff_ln
  ENDIF
CASE fld = 3        && Число строк прогона
  IF ff_ln < 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Так сколько строк прогнать в конце документа?"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
CASE fld = 4        && Число страниц документа
  IF p_numb <= 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Так сколько страниц должно быть в документе?"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF p_numb < ALEN(p_text)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Вы потеряете одну или несколько страниц. Это правильно? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      RETURN .F.
    ENDIF
    DIMENSION p_text(p_numb)
  ENDIF
  IF p_numb > ALEN(p_text)
    DIMENSION p_text(p_numb)
    FOR m = 1 TO ALEN(p_text)
      w = p_text(m)
      IF TYPE("w") # "C"
        p_text(m) = ""
      ENDIF
    ENDFOR
  ENDIF
CASE fld = 5        && Длина страницы
  IF p_len <= 3
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Не коротка ли страница?"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
CASE fld = 6        && Содержимое шаблона
  IF i
    DO Modi_Shab
    i = .F.
    SHOW GET i
*    KEYBOARD "{Tab}"
  ENDIF
ENDCASE

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Коррекция шаблона.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 31.01.97 23:05:20 ═╝
PROCEDURE Modi_Shab

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

PRIVATE ex, p_flt, s, mss
ex    = 0
p_flt = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH WROWS("")-4, WCOLS("")-2, ALLTRIM(d_name)
@ 1, 1 CLEAR
DO Sun_Bord WITH 1, 1, WROWS()-2, WCOLS()-2
@ WROWS()-1, 0 SAY PADC("PgUp - пред. страница,  PgDn - след. страница, Esc - выход", WCOLS())

*------------------------------------------------------------------------
*      Ввод полей бланка
*
ON KEY LABEL Escape KEYBOARD CHR(23)
ON KEY LABEL PgUp   DO Mov_P WITH -1
ON KEY LABEL PgDn   DO Mov_P WITH  1
DO WHILE .T.
  ex = 0
  s = PADL("Стр. "+ALLTRIM(STR(p_flt))+" ",9)
  @ 0, WCOLS()-10 SAY s COLOR SCHEME 5
  @ 2,2 EDIT p_text(p_flt) SIZE WROWS()-4, WCOLS()-4
  READ CYCLE
  s = 1
  DO WHILE AT(CHR(13)+CHR(10), p_text(p_flt), s) # 0
    s = s+1
  ENDDO
  IF s > p_len
    DIMENSION mss(5)
    mss(1) = ""
    mss(2) = CHR(0)+"ВНИМАНИЕ!  Число  строк на странице превышает указанную  Вами же"
    mss(3) = CHR(0)+"длину страницы! Вы достаточно хорошо представляете как это будет"
    mss(4) = CHR(0)+"в документе?                                                    "
    mss(5) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
  IF ex = 0
    EXIT
  ENDIF
  p_flt = p_flt+ex
  p_flt = MAX(p_flt,1)
  p_flt = MIN(p_flt,ALEN(p_text))
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                   Обработчик клавиши смены страницы.                   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 31.01.97 23:24:07 ═╝
PROCEDURE Mov_p
PARAMETER n

ex = n
KEYBOARD CHR(23)

RETURN
