********************************************************************************
*    Файл BankName       Разработчик Андрей Васин                14.01.2012    *
********************************************************************************
*                                                                              *
*                             Ввод названия банка                              *
*                                                                              *
********************************************************************************
PROCEDURE BankName
PARAMETERS prmTit, prmLen, prmStart
*
* prmTit   - Загодовок формы
* prmLen   - Длина названия
* prmStart - Начальное значение
*
*
*  Переменные состояния для навигации
*
PRIVATE tmpTit, tmpLen, tmpWid, tmpShab

m.tmpTit  = IIF(TYPE("m.prmTit") = "C", m.prmTit, "Задайте шаблон для поиска")
m.tmpLen  = IIF(TYPE("m.prmLen") = "N", m.prmLen, 20)
m.tmpWid  = MIN(m.tmpLen, WCOLS("")-8)
m.tmpShab = IIF(TYPE("m.prmStart") = "C", m.prmStart, "")

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

*+-------------------------------------------------+
*!                                                 !
*!        < OK Ctrl-W > < Отказаться Esc >         !
*+-------------------------------------------------+

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 5, MAX(m.tmpWid+6, 38), m.tmpTit

*------------------------------------------------------------------------
*      Ввод полей бланка
*


@ WROWS()-3, FLOOR(WCOLS()/2-m.tmpWid/2) EDIT m.tmpShab SIZE 1, m.tmpWid, m.tmpLen COLOR (SCHEME(1,1))
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF m.ex # 1
  *
  * Отрабатываем бланк
  m.tmpShab = ""
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.tmpShab

