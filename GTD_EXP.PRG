*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла GTD_Exp      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Расширение ГТД на все партии позиций.                  ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 04.04.2000 ══╝
PROCEDURE GTD_Exp
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
* Расстановка ГТД
*┌───────────────────────────────────┐
*│   Просмотрено 999999 позиций      │
*│   Найдено ГТД для:                │
*│   999999 позиций, 999999 партий   │
*│                                   │
*│ < OK Ctrl-W > < Отказаться Esc >  │
*└───────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 39, "Расстановка ГТД"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2,  5 SAY "Просмотрено        позиций"
@ 3,  5 SAY "Найдено ГТД для:"
@ 4, 12 SAY "позиций,        партий"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  PRIVATE cnt1, cnt2, cnt3, g0, c0, cc0, l0, sw0, s_sav, r_sav
  s_sav = SELECT()
  STORE 0 TO cnt1, cnt2, cnt3
  DELETE FILE (tmpo_path+"GTD_EXP.DBF")
  SELECT 0
  CREATE DBF (tmpo_path+"GTD_EXP") ;
          (   MARK     L,     ;
              CODE     N( 7), ;
              PREFIX   C( 4), ;
              NAME     C(39), ;
              PRODUCER C( 8), ;
              GTD_NUM  C(26), ;
              GTD_LINE C( 4), ;
              COUNTRY  C(16), ;
              COUNTRYC C( 3)  )
  SELECT 0
  USE (base_path+"ACT_S") ORDER TAG CODE DESCENDING ALIAS P_0404 AGAIN
  SELECT 0
  USE (base_path+"STOCK") ORDER TAG NAME ALIAS ST_0404 AGAIN
  SET RELATION TO CODE INTO P_0404
  SCAN
    cnt1 = cnt1+1
    g0 = ""
    c0 = ""
    l0 = ""
    sw0 = .F.
    IF FOUND("P_0404")
      SELECT P_0404
      r_sav = RECNO()
      SCAN REST WHILE CODE = ST_0404.CODE
        IF EMPTY(g0) .AND. .NOT. EMPTY(GTD_NUM)
          g0 = GTD_NUM
          c0 = COUNTRY
          cc0 = COUNTRYC
          l0 = GTD_LINE
        ENDIF
        IF EMPTY(GTD_NUM)
          sw0 = .T.
          cnt3 = cnt3+1
        ENDIF
      ENDSCAN
      IF .NOT. EMPTY(g0) .AND. sw0
        cnt2 = cnt2+1
        SELECT GTD_EXP
        APPEND BLANK
        REPLACE MARK     WITH .T.,              ;
                CODE     WITH ST_0404.CODE,     ;
                PREFIX   WITH ST_0404.PREFIX,   ;
                NAME     WITH ST_0404.NAME,     ;
                PRODUCER WITH ST_0404.PRODUCER, ;
                GTD_NUM  WITH g0, ;
                GTD_LINE WITH l0, ;
                COUNTRY  WITH c0, ;
                COUNTRYC WITH cc0
      ENDIF
      SELECT ST_0404
    ENDIF
    @ 2, 17 SAY cnt1 PICTURE "@Z 999999"
    @ 4,  5 SAY cnt2 PICTURE "@Z 999999"
    @ 4, 21 SAY cnt3 PICTURE "@Z 999999"
  ENDSCAN
  @ WROWS()-2, 2 SAY PADC("Нажмите что-нибудь...", WCOLS()-4)
  = INKEY(0, "HM")
  USE
  SELECT GTD_EXP
  IF RECCOUNT() = 0
    USE
    DELETE FILE (tmpo_path+"GTD_EXP.DBF")
  ENDIF
  SELECT P_0404
  USE
  SELECT (s_sav)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF USED("GTD_EXP")
  DO Set_M
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  Расставляем ГТД? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 1
    DO Wt_Mess WITH "Расставляем..."
    SELECT 0
    USE (base_path+"ACT_S") ORDER TAG CODE ALIAS P_0405
    SELECT GTD_EXP
    SCAN FOR MARK
      SELECT P_0405
      SEEK GTD_EXP.CODE
      REPLACE REST WHILE CODE = GTD_EXP.CODE FOR EMPTY(GTD_NUM) ;
              GTD_NUM  WITH GTD_EXP.GTD_NUM, ;
              GTD_LINE WITH GTD_EXP.GTD_LINE,;
              COUNTRY  WITH GTD_EXP.COUNTRY, ;
              COUNTRYC WITH GTD_EXP.COUNTRYC
      SELECT GTD_EXP
    ENDSCAN
    SELECT P_0405
    USE
    DO Wt_Mess
  ENDIF
  SELECT GTD_EXP
  USE
  DELETE FILE (tmpo_path+"GTD_EXP.DBF")
ENDIF
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Marks    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│            Просмотр обобранных позиций и установка отметок.            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.04.2000 ──┘
PROCEDURE Set_M

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
SELECT GTD_EXP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "Отметить" KEY Ctrl-A, "Пробел"
ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 51

DO D_Wins WITH ln, wd, "Найдены позиции...", 0, 1
what_do = "List"
*
*┌─────────────────────────────────────────┐
*│M AAAA 0000000000111111111122222 PPPPPPPP│
*└─────────────────────────────────────────┘
*┌──ГТД N────────────────Страна────────────┐
*│....:....!....:....!....:. ....:....!....│
*└─────────────────────────────────────────┘
@ WROWS()-3, 2 SAY "──ГТД N────────────────Страна"
GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL SpaceBar DO Sw_Mode WITH "Mark"

*
*    BROWSE - меню
*
    BROWSE FIELDS M000 = IIF(MARK, "√", " "):1:H="", ;
                  PREFIX:4:H="",   ;
                  NAME:35:H="",    ;
                  PRODUCER:8:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "Mark"    && Отметка

    REPLACE MARK WITH .NOT. MARK
    SKIP
    IF EOF()
      GO BOTTOM
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
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
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
@ WROWS()-2,  4 SAY GTD_NUM
@ WROWS()-2, 29 SAY LEFT(COUNTRY,14)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
