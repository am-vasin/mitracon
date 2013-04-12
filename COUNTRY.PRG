*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Country      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                                Страна.                                 ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 14.02.2000 ══╝
PROCEDURE Country
PARAMETERS startVal

PRIVATE tmpPoint, tmpList, tmpLen, s_sav

s_sav = SELECT()
SELECT 0
USE (path_comm+"COUNTRY") ORDER TAG B_NAME ALIAS C_0214 AGAIN
tmpLen = 0
SCAN
  tmpLen = tmpLen+1
  DIMENSION tmpList(tmpLen)
  tmpList(tmpLen) = B_NAME
ENDSCAN
USE
SELECT (s_sav)
IF tmpLen = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Список стран пуст!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN startVal
ENDIF

tmpPoint = MAX(ASCAN(tmpList, startVal), 1)

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

PRIVATE ln
ln = MIN(tmpLen+3, 20)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln, LEN(tmpList)+4, "Страны"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  tmpPoint = MAX(1, tmpPoint)
  tmpPoint = MIN(tmpLen, tmpPoint)
  @ 1, 1 MENU tmpList, tmpLen, ln-3
  READ MENU TO tmpPoint
  IF READKEY() % 256 = 12
    tmpPoint = 0
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN IIF(tmpPoint = 0, startVal, tmpList(tmpPoint))
