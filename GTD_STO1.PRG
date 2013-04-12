*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Gtd_Sto1     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Выбор ГТД по позиции.                            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 26.02.2000 ══╝
PROCEDURE Gtd_Sto1
PARAMETER pName

PRIVATE chr_shab, int_1, int_2, tmpValue
DIMENSION int_1(2), int_2(2)   && Описание пунктов меню для программы поиска
int_1(1) = 27      && Esc
int_2(1) = "{Esc}"
int_1(2) = -6      && F7
int_2(2) = "{F7}"

chr_shab = ' !"'+    ;
           "#$%&'()*+,-./0123456789:;<=>?@"+  ;
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"+         ;
           "[\]^_`"+                             ;
           "abcdefghijklmnopqrstuvwxyz"+         ;
           "{|}~"+                               ;
           "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"+   ;
           "абвгдежзийклмнопрстуфхцчшщъыьэюя"
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
USE (path_comm+"GTD_TIT") ORDER TAG GTD_NUM ALIAS GTD3_0226 AGAIN

SELECT 0
USE (path_comm+"GTD") ORDER TAG NAME ALIAS GTD0_0226 AGAIN
SET RELATION TO GTD_NUM INTO GTD3_0226

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "Контекстный поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE tmpName, seek_shab, retValue
*
*  Встаем на ближайшую
GO TOP
IF TYPE("pName") = "C"
  tmpName = ALLTRIM(pName)
  retValue = RECNO()
  seek_shab = 3
  DO WHILE(seek_shab) <= LEN(tmpName)
    IF .NOT. SEEK(LEFT(tmpName, seek_shab))
      IF BETWEEN(retValue, 1, RECCOUNT())
        GO retValue
      ENDIF
     EXIT
    ENDIF
    retValue = RECNO()
    seek_shab = seek_shab+1
  ENDDO
ENDIF
IF TYPE("pName") = "N"
  GO pName
ENDIF
retValue = ""
tmpName  = ""
seek_shab = ""
seek_shab = PADR(seek_shab, 25)
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 65
*┌─────────────────────────────────────────────────────────────────┐
*│                                                                 │
*└─────────────────────────────────────────────────────────────────┘
*┌──Дата─────Номер ГТД──────────────────Страна─────────────Кол-во──┐
*│ДД.ММ.ГГГГ GTL----------------------- -----Страна----- 99999999 *│
*└─────────────────────────────────────────────────────────────────┘

DO D_Wins WITH ln, wd, "Выбор позиции из номенклатуры ГТД", 1, 1
@ WROWS()-3, 2 SAY "──Дата─────Номер ГТД──────────────────Страна─────────────Кол-во"
@ 1, 3 SAY "Поиск:"
@ 1, 10 SAY PADR("Tab",WCOLS()-10) COLOR (SCHEME(14,2))

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Seek_
    ON KEY LABEL F7  DO Sw_Mode WITH "Seek"

*
*    BROWSE - меню
*
    SELECT GTD0_0226
    BROWSE FIELDS NAME:65:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    retValue = RECNO()
    EXIT

  CASE what_do = "Seek"      && Контекстный поиск

    DO Seek_C
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
SELECT GTD3_0226
USE
SELECT (s_sav)

RETURN retValue

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Программа поиска по начальным буквам.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.97 22:05:53 ═╝
PROCEDURE Seek_

PRIVATE s, prf, ln, bg, rc_s, i

ln = 29
ACTIVATE WINDOW (win_name+"_M") SAME
prf = ""
@ 1, 3 SAY "Поиск:"
@ 1, 10 SAY PADR("Tab",ln) COLOR (SCHEME(14,2))
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
    KEYBOARD CHR(23)
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

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_C       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Контекстный поиск.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.02.2000 ──┘
PROCEDURE Seek_C

IF .NOT. Get_Shab()
  RETURN ""
ENDIF
IF EMPTY(seek_shab)
  RETURN ""
ENDIF

PRIVATE s_sav, tmpShab, r_sav
tmpShab = ALLTRIM(seek_shab)
s_sav = SELECT()
SELECT 0
DO Use_Link
ZAP
SELECT GTD0_0226
r_sav = RECNO()
DO Wt_Mess WITH "Поиск..."
SCAN
  IF tmpShab $ NAME
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("GTD0_0226")
    SELECT GTD0_0226
  ENDIF
ENDSCAN
DO Wt_Mess

SET ORDER TO
tmpShab = Ch_P()

SELECT (s_sav)

SET ORDER TO NAME
RETURN tmpShab

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Shab     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Шаблон для поиска.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.02.2000 ──┘
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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌─────────────────────────────────┐
*│  ┌─ Шаблон для поиска ───────┐  │
*│  │ ....:....!....:....!....: │  │
*│  └───────────────────────────┘  │
*│< OK Ctrl-W > < Отказаться Esc > │
*└─────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 37, "Контекстный поиск"
DO Sun_Bord WITH  2,  4,  4, 32, " Шаблон для поиска "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 6 GET seek_shab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Choose_P     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│               Выбор позиции из отобранных по контексту.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.02.2000 ──┘
PROCEDURE Ch_P

SET ORDER TO
SELECT LINK
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
  RETURN
ENDIF

SET RELATION TO LNK INTO GTD0_0226
GO TOP

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
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 64

DO D_Wins WITH ln, wd, "Позиции ГТД", 0, 1
@ WROWS()-3, 2 SAY "──Дата─────Номер ГТД────────────Стр──Страна─────────────Кол-во──"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS GTD0_0226.NAME:64:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    EXIT

  OTHERWISE

    IF BETWEEN(r_sav, 1, RECCOUNT())
      GO r_sav
    ENDIF
    EXIT

  ENDCASE

ENDDO

SET RELATION TO
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*

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
*│ДД.ММ.ГГГГ GTL----------------------- -----Страна----- 99999999 *│
@ WROWS()-2, 2 SAY DTOC(GTD0_0226.GTD_DATE)+" "+GTD0_0226.GTD_NUM+" "+ ;
                   GTD0_0226.GTD_LINE+" "+GTD0_0226.COUNTRY+" "+STR(GTD0_0226.QNT,8)+ ;
                   GTD3_0226.GTD_TYPE

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
