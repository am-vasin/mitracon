*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Autor        Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       ProductManager для позиции                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 12/18/2001 ══╝
PROCEDURE Autor
PARAMETERS pos_code

PRIVATE s_sav
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
*            PPPP nnnnnnnnnNnnnnnnnnnNnnnnn ffffFfff
*┌─────────────────────────────────────────────────────────────┐
*│                                                             │
*│ Product manager: ....:....!....:....!....:....!....:....!.. │
*│                              ..:....!....:....!..           │
*│ Закупка ....:....!....:....                                 │
*│ Дата поставки ....:....!                                    │
*│ Позиция снята с производства                                │
*│         < Резерв > < Наличие в филиалах >  < Esc >          │
*│                                                             │
*└─────────────────────────────────────────────────────────────┘

PRIVATE ex, ttl, tmpName, tmpSname, pur, obs, d_wait
ex = 3
pur = ""
obs = ""
d_wait = {}
*------------------------------------------------------------------------

s_sav = SELECT()

IF File_O(path_comm+"PURCHASE.DBF")
  SELECT 0
  USE (path_comm+"PURCHASE") ALIAS PUR_2702 ORDER TAG NAME_DATE AGAIN
  SET FILTER TO PURFLAG = "K"
ENDIF

SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ALIAS ST_2712 ORDER TAG CODE
SEEK pos_code
ttl = ALLTRIM(ST_2712.PRODUCER)
ttl = ALLTRIM(ST_2712.PREFIX)+" "+ ;
      ALLTRIM(ST_2712.NAME)+" "+ ;
      IIF(EMPTY(ttl), "", " /"+ttl+"/")

IF USED("PUR_2702")
  SELECT PUR_2702
  IF SEEK(ST_2712.NAME+ST_2712.PREFIX+ST_2712.PRODUCER, "PUR_2702")
    d_wait = PUR_2702.DATE
  ENDIF
ENDIF

SELECT 0
IF File_O(path_comm+"SUBJECT.DBF")
  USE (path_comm+"SUBJECT") SHARED AGAIN ALIAS SJ_2712 ORDER TAG STO_CODE
ELSE
  RETURN .F.
ENDIF  
SEEK pos_code
IF NOPUR = "X"
  pur = "Не закупать никогда"
ENDIF
IF NOPUR = "x"
  pur = "Не закупать сейчас"
ENDIF
IF NOPUR = " "
  pur = "Нет запрета"
ENDIF
IF OBSOLETE
  obs = "Снята с производства"
ENDIF
tmpName = SJ_2712.AUTHOR
*IF .NOT. EMPTY(tmpName)
*  SELECT 0
*  USE (base_path+"PERSONS") SHARED AGAIN ALIAS P_2712 ORDER TAG ALIAS
*  IF SEEK(tmpName)
*    tmpName = ALLTRIM(P_2712.FAMILY)+" "+ALLTRIM(P_2712.NAME)+" "+ALLTRIM(P_2712.S_NAME)
*  ELSE
*    tmpName = ""
*  ENDIF
*  USE
*ENDIF

SELECT ST_2712

IF EMPTY(tmpName)
  tmpName = "Нет!"
ENDIF
PUSH KEY CLEAR       && На всякий пожарный случай!
DO D_Win_N WITH 11, 68, ttl

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3,  4 SAY "Product manager:"
@ 3, 21 SAY tmpName COLOR (SCHEME(14,2))
@ 5,  4 SAY "Закупка  "
@ 5, 14 SAY pur COLOR (SCHEME(14,2))
IF .NOT. EMPTY(d_wait)
  @ 7,  4 SAY "Дата поставки "
  @ 7, 19 SAY DTOC(d_wait) COLOR (SCHEME(14,2))
ELSE
  @ 7,  4 SAY "Поставка"
  @ 7, 14 SAY "В ближайшее время не ожидается" COLOR (SCHEME(14,2))
ENDIF
IF .NOT. EMPTY(obs)
  @ 8,  3 SAY obs COLOR (SCHEME(14,2))
ENDIF
DO WHILE  .T.
  @ WROWS()-2, 13 GET ex PICTURE "@*HT \! Резерв ;\? Наличие в филиалах ;\? Выход ESC"
 
  READ CYCLE
  IF LASTKEY() = 27
    EXIT
  ENDIF
  IF ex = 3
    EXIT
  ENDIF
  IF ex = 1
    DO Rep_Pos WITH 0
  ENDIF
  IF ex = 2
    DO Rep_Pos WITH 1
  ENDIF
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

SELECT ST_2712
USE
SELECT SJ_2712
USE
IF USED("PUR_2702")
  SELECT PUR_2702
  USE
ENDIF
SELECT (s_sav)
RETURN
