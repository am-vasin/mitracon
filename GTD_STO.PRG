*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Gtd_Sto      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Номенклатура ГТД.                            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 26.02.2000 ══╝
PROCEDURE Gtd_Sto
PARAMETER pPref, pName, pProd

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
USE (path_comm+"GTD") ORDER TAG NAME DESCENDING ALIAS GTD1_0226 AGAIN
SET RELATION TO GTD_NUM INTO GTD3_0226

SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_STO ALIAS GTD0_0226 AGAIN
SET RELATION TO NAME INTO GTD1_0226

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
tmpName = ALLTRIM(pPref)+ALLTRIM(pName)
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

retValue = ""
tmpName  = IIF(EMPTY(pPref), "", ALLTRIM(pPref)+" ")+ALLTRIM(pName)+ ;
           IIF(EMPTY(pProd), "", " /"+ALLTRIM(pProd)+"/")
seek_shab = ALLTRIM(pName)
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

DO D_Wins WITH ln, wd, tmpName, 1, 5
@ WROWS()-7, 2 SAY "──Дата─────Номер ГТД──────────────────Страна─────────────Кол-во──"
@ 1, 3 SAY "Поиск:"
@ 1, 10 SAY PADR("Tab",WCOLS()-10) COLOR (SCHEME(14,2))

PRIVATE l, c, ln1
l   = WLROW(win_name+"_M")+WROWS(win_name+"_M")-6
c   = WLCOL(win_name+"_M")+2
ln1 = 5

DEFINE WINDOW (win_name+"_E1") FROM l, c TO l+ln1-1, c+wd-1 NONE COLOR SCHEME 13
ACTIVATE WINDOW (win_name+"_E1")
DEFINE WINDOW (win_name+"_I1") FROM l-3, c-2 TO l+ln1, c+wd+20   ;
                           NONE COLOR SCHEME 13

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_E1")
    SELECT GTD1_0226
    
    BROWSE FIELDS GTD_DATE:10:H="",  ;
                  GTD_NUM:26:H="",   ;
                  COUNTRY:16:H="",   ;
                  QNT:H="":P="@Z 99999999", ;
                  GTD3_0226.GTD_TYPE:1:H="" ;
           NOAPPEND NODELETE NOLGRID NOMODIFY NOMENU NOCLEAR NOWAIT ;
           WINDOW (win_name+"_I1") IN WINDOW (win_name+"_E1")

    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Seek_
    ON KEY LABEL F7  DO Sw_Mode WITH "Seek"

*
*    BROWSE - меню
*
    SELECT GTD0_0226
    BROWSE FIELDS NAME:59:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    retValue = NAME
    EXIT

  CASE what_do = "Seek"      && Контекстный поиск

    tmpValue = Seek_C()
    IF .NOT. EMPTY(tmpValue)
      SEEK tmpValue
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_E1")
RELEASE WINDOW (win_name+"_I1")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
USE
SELECT GTD1_0226
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

PRIVATE s_sav, tmpShab
tmpShab = ALLTRIM(seek_shab)
s_sav = SELECT()
SELECT 0
DO Use_Link
ZAP
SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_STO ALIAS GSTO_0226 AGAIN
DO Wt_Mess WITH "Поиск..."
SCAN
  IF tmpShab $ NAME
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("GSTO_0226")
    SELECT GSTO_0226
  ENDIF
ENDSCAN
DO Wt_Mess

tmpShab = Ch_P()

SELECT GSTO_0226
USE
SELECT (s_sav)

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
  RETURN ""
ENDIF

SET RELATION TO LNK INTO GSTO_0226
GO TOP

PRIVATE retVal
retVal = ""
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
wd = 29

DO D_Wins WITH ln, wd, "Позиции ГТД", 0, 0
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
    BROWSE FIELDS GSTO_0226.NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    retVal = GSTO_0226.NAME
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

RETURN retVal
