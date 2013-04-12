           WHEN Draw_Foot()    ;
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
@ WROWS()-2, 2 SAY PREFIX COLOR SCHEME 1
@ ROW(), COL()+1 SAY NAME COLOR SCHEME 1
@ ROW(), COL()+1 SAY PRODUCER COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
