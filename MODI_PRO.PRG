*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Modi_Pro     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Коррекция списка брендов                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 24.12.2005 ══╝
PROCEDURE Modi_Pro

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
m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"DEVELOP") SHARED AGAIN ALIAS DV_5C24 ORDER TAG B_NAME


*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "Корр." KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Delete OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Seek OF (menu_name) PROMPT "Конт. поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, seekTxt, seekMode, r_sav, mss, new_rec
m.seekTxt  = ""
m.seekMode = 1
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 69

DO D_Wins WITH m.ln, m.wd, "Фирмы-производители", 1, 1
@ 2, 2 SAY "Назв.────Полное название"
@ 1, 2 SAY "Поиск"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 8 SAY PADR("Enter", 8) COLOR (SCHEME(14,2))
    DO Prp_Nav_1

    ON KEY LABEL Tab   DO Sw_Mode WITH "Modify"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Delete"
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL Enter DO Locator WITH 1, 8, 8

*
*    BROWSE - меню
*
    BROWSE FIELDS F000 = LEFT(B_NAME, 8):H="", ;
                  L_NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Modify"    && Коррекция

    IF .NOT. BOF() .AND. .NOT. EOF() .AND. CODE # 1
      DO Prod1 WITH CODE
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Add"       && Добавление

    m.new_rec = 0
    DO Prod1
    IF BETWEEN(m.new_rec, 1, RECCOUNT())
      GO m.new_rec
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Delete"    && Удаление

    IF .NOT. BOF() .AND. .NOT. EOF() .AND. CODE # 1
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Действительно удаляем? [ Да ] [ Нет ]"
      mss[3] = ""
      IF Out_Mess(5, "mss") = 1
        REPLACE DATE_OFF WITH DATE(), ;
                WHO_DEL  WITH m.user
      ENDIF
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Seek"      && Контекстный поиск

    IF Get_Shab()
      IF .NOT. EMPTY(m.seekTxt)
        m.r_sav = RECNO()
        IF m.seekMode = 1
          GO TOP
        ELSE
          IF .NOT. EOF()
            SKIP
          ENDIF
        ENDIF
        LOCATE REST FOR m.seekTxt $ B_NAME .OR. m.seekTxt $ L_NAME
        IF .NOT. FOUND()
          DIMENSION mss[3]
          mss[1] = ""
          mss[2] = CHR(0)+""
          mss[3] = ""
          DO Out_Mess WITH 7, "mas"
          IF BETWEEN(m.r_sav, 1, RECCOUNT())
            GO m.r_sav
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    m.what_do = "List"

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
SELECT (m.s_sav)

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
GO RECNO()		&& Дабы обновить значение поля PERIOD (ЭМПИРИКА!)
@ WROWS()-2, 3     SAY "Срок поставки "
@ WROWS()-2, COL() SAY PERIOD COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Locator      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Посимвольный поиск                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 24.12.2005 ──┘
PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t
PRIVATE rc_near, flt_sav, p_sav

is_ins = INSMODE()   && Режим вставка/замена

*
*   "*" - коды, соответствующие печатным символам
*
chr_type = SPACE(31)+     REPLICATE("*",126-31 )+ ;
           SPACE(127-126)+REPLICATE("*",175-127)+ ;
           SPACE(223-175)+REPLICATE("*",241-223)+ ;
           SPACE(255-223)
*
*   "I" - коды, соответствующие символам внутренней навигации
*
chr_type = STUFF(chr_type,   1, 1, "I")  &&  Home
chr_type = STUFF(chr_type,   4, 1, "I")  &&  Стрелка вправо
chr_type = STUFF(chr_type,   6, 1, "I")  &&  End
chr_type = STUFF(chr_type,   7, 1, "I")  &&  Del
chr_type = STUFF(chr_type,  13, 1, "I")  &&  Enter
chr_type = STUFF(chr_type,  19, 1, "I")  &&  Стрелка влево
chr_type = STUFF(chr_type,  22, 1, "I")  &&  Ins
chr_type = STUFF(chr_type, 127, 1, "I")  &&  BackSpace

*
*   "E" - коды, соответствующие символам внешней навигации ( по базе )
*

chr_type = STUFF(chr_type,  3, 1, "E")  &&  PgDn
chr_type = STUFF(chr_type,  5, 1, "E")  &&  Стрелка вверх
chr_type = STUFF(chr_type, 18, 1, "E")  &&  PgUp
chr_type = STUFF(chr_type, 24, 1, "E")  &&  Стрелка вниз

prf_flt = ""    && Префикс
flt_sav = prf_flt
c_flt   = 0     && Текущая позиция курсора

DO WHILE .T.
  IF .NOT. flt_sav == prf_flt
    rc_near = RECNO()
    IF .NOT. SEEK(prf_flt)
      prf_flt = flt_sav
      c_flt   = p_sav
      IF BETWEEN(rc_near, 1, RECCOUNT())
        GO rc_near
      ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH
  ENDIF
  @ l001, c001 SAY PADR(prf_flt, ln, "▒")
  @ l001, c001+c_flt SAY ""
  flt_sav = prf_flt
  p_sav   = c_flt
  c_c = INKEY(0)                && Код символа
  IF c_c >= 0
    c = CHR(c_c)                && Символ
    c_t = SYS(15, chr_type, c)  && Тип символа
  ENDIF
  DO CASE
  CASE c_c < 0                  && Функциональная клавиша
  CASE c_t = "*"                && Печатный символ
    prf_flt = LEFT(prf_flt, c_flt)+c+  ;
              SUBSTR(prf_flt, c_flt+IIF(is_ins,1,2))
    prf_flt = LEFT(prf_flt, ln)
    c_flt = c_flt+1
    c_flt = MIN(c_flt, ln)
    LOOP
  CASE c_t = "E"                && Навигация по базе
  CASE c_t = "I"                && Внутренняя навигация
    DO CASE
    CASE c_c = 7                && Del
      prf_flt = LEFT(prf_flt, c_flt)+SUBSTR(prf_flt, c_flt+2)
    CASE c_c = 127              && BackSpace
      prf_flt = LEFT(prf_flt, c_flt-1)+SUBSTR(prf_flt, c_flt+1)
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 19               && Стрелка влево
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 4                && Стрелка вправо
      c_flt = MIN(c_flt+1,LEN(prf_flt))
    CASE c_c = 22               && Ins
      is_ins = .NOT. is_ins
      = INSMODE(is_ins)
    CASE c_c = 13
      EXIT
    ENDCASE
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

ACTIVATE WINDOW (win_name+"_M") SAME
@ 1, 8 SAY PADR("Enter", 8) COLOR (SCHEME(14,2))
ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Shab     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Ввод шаблона для поиска                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 24.12.2005 ──┘
PROCEDURE Get_Shab
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

*┌────────────────────────────────────┐
*│ ┌─ Шаблон для поиска ────────────┐ │
*│ │                                │ │
*│ └────────────────────────────────┘ │
*│( ) Искать сначала  ( ) Искать далее│
*│  < OK Ctrl-W > < Отказаться Esc >  │
*└────────────────────────────────────┘

PRIVATE ex, ret, tmpTxt, tmpMode
m.ex = 1
m.ret = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 40, "Контекстный поиск"
DO Sun_Bord WITH  2,  3,  4, 36, " Шаблон для поиска "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
m.tmpTxt  = m.seekTxt
m.tmpMode = m.seekMode
@ 3, 4 EDIT m.tmpTxt SIZE 1, 32 COLOR (","+SCHEME(1,1))
@ 5, 2 GET  m.tmpMode PICTURE "@*RH Искать сначала; Искать далее"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF m.ex = 1
  *
  * Отрабатываем бланк
  m.seekTxt  = m.tmpTxt
  m.seekMode = m.tmpMode
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN m.ex = 1
