*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла New_Info     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Функциональное назначение.                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 04.04.2004 ══╝
PROCEDURE New_Info
PARAMETER c_code

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

PRIVATE ex, tmpFunc, tmp_seg
PRIVATE sel_sav
sel_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") ALIAS STO_4404 AGAIN ORDER TAG CODE
SEEK c_code
SELECT 0
USE (path_comm+"SUBJECT") ALIAS SJ_4404 AGAIN ORDER TAG STO_CODE
tmpFunc = PADR("Функциональное назначение не задано", FSIZE("FUNCTION", "SJ_4404"))
tmp_seg = SPACE(FSIZE("SEGMENT", "SJ_4404"))
IF SEEK(c_code)
  IF .NOT. EMPTY(FUNCTION)
    tmpFunc = FUNCTION
  ENDIF
  tmp_seg = SEGMENT
ENDIF

                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&

*
*┌──────────────────────────────────────────────────────────────────┐
*│         Позиция ▒▒▒▒ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓ ▒▒▒▒▓▒▒▒          │
*│                                                                  │
*│ ....:....!....:....!....:....!....:....!....:....!....:....!.... │
*│                                                                  │
*│                          Сегмент ....:.                          │
*│                              < OK >                              │
*└──────────────────────────────────────────────────────────────────┘

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 70, "Функциональное назначение"

*      Ввод полей бланка
*
@  2, 11 SAY "Позиция"
@  2, COL()+1 GET STO_4404.PREFIX
@  2, COL()+1 GET STO_4404.NAME
@  2, COL()+1 GET STO_4404.PRODUCER
@  4, 3 GET tmpFunc
@  6, 28 SAY "Сегмент" GET tmp_seg
CLEAR GETS
@  7, 32 GET ex PICTURE "@*HT \! OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

USE IN STO_4404
USE IN SJ_4404
SELECT (sel_sav)

RETURN
