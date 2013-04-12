*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Rep_Prc                                                           ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Цены оптовые, мелкооптовые, розничные ...            ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 08.08.2001 ═╝
PROCEDURE Rep_Prc
PARAMETERS p_code
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

PRIVATE sel_sav, w_name
PRIVATE prc_o, prc_o_r, prc_mo, prc_mo_r, prc_r, prc_r_r, qtr

PUSH KEY CLEAR
sel_sav = SELECT()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_1808 AGAIN

SEEK p_code
w_name = ALLTRIM(PREFIX+" "+NAME+" "+PRODUCER)

prc_o    = PRICE_O    && оптовая цена в у.е.
prc_o_r  = PRICE_O_R  && оптовая цена в руб. 
prc_mo   = PRICE_O1   && мелкооптовая цена в у.е.
prc_mo_r = PRICE_O1_R && мелкооптовая цена в руб. 
prc_r    = PRICE      && розничная цена в у.е. 
prc_r_r  = PRICE_R    && розничная цена в руб. 
qtr      = QNT_OPT    && мелкооптовое количество
*qtr     = QNT_ROUND  && норма упаковки

PRIVATE wd, l0, c0
wd = 50
l0 = FLOOR((WROWS("")-8)/2)
c0 = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW W_PRC FROM l0, c0 TO IIF(qtr=< 1,l0+9,l0+7), c0+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW W_PRC
DO Sun_Bord WITH 0, 1, WROWS()-1, WCOLS()-2
@ 1, 2 SAY "Позиция "+ w_name
@ 3, 8 SAY "Цена:  Оптовая  Мелкоопт.  Розничая"
@ 4, 8 SAY "USD "+STR(price_o,10,2)+STR(prc_mo,11,2)+STR(prc_r,10,2)
@ 5, 8 SAY "Руб."+STR(price_o_r,10,2)+STR(prc_mo_r,11,2)+STR(prc_r_r,10,2)
IF qtr =< 1
  @ 7, 10 SAY "Норма упаковки не установлена !"
  @ 9, 15 SAY "Нажмите что-нибудь..."
ELSE
  @ 7, 15 SAY "Нажмите что-нибудь..."
ENDIF

= INKEY(0, "MH")
RELEASE WINDOW W_PRC

SELECT S_1808
USE

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

SELECT(sel_sav)
POP KEY

RETURN