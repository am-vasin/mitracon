*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║           Программа создания окон для списка позиций.                 ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE D_Win
PARAMETERS tit, tp, is_rub
*
*   tit  - заголовок;
*   tp   - режим: 1 - с локатором, 2 - без локатора
*
PRIVATE l, c, wd, ln, c_w
ln = WROWS("")-3
wd = 75 && 69  &&74
IF is_rub 
  wd = 77
ENDIF
l  = CEILING((WROWS("")-ln)/2)
c  = FLOOR((WCOLS("")-wd)/2)
*c_w = IIF(tp=1, 2, 1)
c_w = 1
IF .NOT. WEXIST("L_Det_M")
  DEFINE WINDOW L_Det_M FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
  ACTIVATE WINDOW L_Det_M
  @ c_w, 1 TO WROWS()-IIF(sale_mode = "Mitracon", 5, 4), WCOLS()-2
  IF is_rub
    @ c_w,   0 SAY    " ┌────Маркировка─────────────────Нал─Кол-во───────Пр.цена─────Долг────О─З─П┐"
    @ WROWS()-IIF(sale_mode = "Mitracon", 5, 4), 0 SAY ;
    " └──────────────────────────────────────────────────────────────────────┘"
*    @ WROWS()-3,0 SAY " ┌────────────── Фирма ── Нал.──Покуп.┐  Бронь (част.▒, полн.█)───┘││ "
*    @ WROWS()-2,0 SAY " │              ....:... ....:. 999999│  Дефицит(█), снят(▒)───────┘│ "
*    @ WROWS()-1,0 SAY " └────────────────────────────────────┘  Скидка(▒), спеццена (█)────┘ "

    @ WROWS()-3,0 SAY " ┌────────────── Фирма ── Нал.──Покуп.─Доп.н.┐   Бронь────────────┘││ "
    @ WROWS()-2,0 SAY " │              ....:... ....:. 999999 999999│   Дефицит───────────┘│ "
    @ WROWS()-1,0 SAY " └───────────────────────────────────────────┘   Скид.(▒), спец.(█)─┘ "

  ELSE
    @ c_w,   0 SAY    " ┌────Маркировка─────────────────Нал─Кол-во────Пр.цена───Долг───────О─З─П┐"
    @ WROWS()-IIF(sale_mode = "Mitracon", 5, 4), 0 SAY ;
    " └────────────────────────────────────────────────────────────────────┘"
    @ WROWS()-3,0 SAY " ┌────────────── Фирма ── Нал.──Покуп.─Доп.н.┐ Бронь────────────┘││ "
    @ WROWS()-2,0 SAY " │              ....:... ....:. 999999 999999│ Дефицит───────────┘│ "
    @ WROWS()-1,0 SAY " └───────────────────────────────────────────┘ Скид.(▒), спец.(█)─┘ "
*    @ WROWS()-3,0 SAY " ┌────────────── Фирма ── Нал.──Покуп.┐ Бронь (част.▒, полн.█)┘││ "
*    @ WROWS()-2,0 SAY " │              ....:... ....:. 999999│ Дефицит(█), снят(▒)────┘│ "
*    @ WROWS()-1,0 SAY " └────────────────────────────────────┘ Скидка(▒), спеццена (█)─┘ "
  ENDIF
ENDIF
ACTIVATE WINDOW L_Det_M
@ 0, 0 SAY PADC(tit,WCOLS()) COLOR SCHEME 5
*@ WROWS()-2, 3 SAY STR(sum_list,12) COLOR SCHEME 1
ln = ln-IIF(sale_mode = "Mitracon", 6, 5)-c_w
wd = wd-4
l  = l+c_w+1
c  = c+2
IF .NOT. WEXIST("L_Det_E")
  DEFINE WINDOW L_Det_E FROM l, c TO l+ln-1, c+wd-1 NONE COLOR SCHEME 13
ELSE
  ZOOM WINDOW L_Det_E NORM AT l, c SIZE ln, wd
ENDIF

ln = ln+4
wd = wd+20
l  = l-3
c  = c-2
IF .NOT. WEXIST("L_Det_I")
  DEFINE WINDOW L_Det_I FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR (cl_w)
ELSE
  ZOOM WINDOW L_Det_I NORM AT l, c SIZE ln, wd
ENDIF

RETURN
