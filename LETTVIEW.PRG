*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла LettView     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Просмотр текста сообщения                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 22.10.2005 ══╝
PROCEDURE LettView
PARAMETERS tmpID		&& код сообщения

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

*┌───────────────────────────────────────────────────────────┐
*│          Тема ....:....!....:....!....:....!..            │
*│┌─ Текст сообщения ───────────────────────────────────────┐│
*││                                                         ││
*││                                                         ││
*││                                                         ││
*││                                                         ││
*││                                                         ││
*││                                                         ││
*││                                                         ││
*││                                                         ││
*│└─────────────────────────────────────────────────────────┘│
*│ Создано ....:....!....:....!....:....!.. ДД.ММ.ГГГГ 99:99 │
*│                         < OK Esc >                        │
*└───────────────────────────────────────────────────────────┘

PRIVATE ex, s_sav, tmpWho, tmpText, tmpTime
m.ex = 1
m.s_sav = SELECT()
SELECT 0
USE base_path+"PERSONS" SHARED AGAIN ORDER TAG CODE ALIAS PRS_5A22
SELECT 0
USE base_path+"LETT_OUT" SHARED AGAIN ORDER TAG MESS_ID ALIAS MSS_5A22
SEEK m.tmpID
= SEEK(MSS_5A22.WHO, "PRS_5A22")
m.tmpWho  = ALLTRIM(PRS_5A22.FAMILY)+" "+LEFT(PRS_5A22.NAME, 1)+" "+LEFT(PRS_5A22.S_NAME, 1)
m.tmpWho  = PADR(m.tmpWho, 32)
m.tmpText = MSS_5A22.MESS_TEXT
m.tmpTime = LEFT(MSS_5A22.CR_TIME, 5)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 16, 63, "Просмотр сообщения"
DO Sun_Bord WITH  3,  2, 12, 60, " Текст сообщения "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2, 12 SAY "Тема" GET MSS_5A22.TITLE WHEN .F.
@  4,  3 EDIT m.tmpText NOMODIFY SIZE 8, 57 COLOR (SCHEME(1, 1))
@ WROWS()-3, 3 SAY "Создано" GET m.tmpWho WHEN .F.
@ WROWS()-3, COL()+1 GET MSS_5A22.CR_DATE WHEN .F.
@ WROWS()-3, COL()+1 GET m.tmpTime WHEN .F.
@ WROWS()-2, FLOOR(WCOLS()/2-5) GET m.ex PICTURE "@*HT \? OK Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

USE IN MSS_5A22
USE IN PRS_5A22

SELECT (m.s_sav)

RETURN
