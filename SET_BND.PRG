*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Программа установки границ.                        ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Set_Bnd

*        Установка ганиц для прих. ордеров
*┌────────────────────────────────────────────────────┐
*│                                                    │
*│                    [ ] Общая                       │
*│                                                    │
*│ Левая граница ($) 99999   Правая граница ($) 99999 │
*│                                                    │
*│             ( ) Создать ( ) Уничтожить             │
*│                                                    │
*│          < OK Ctrl-W > < Отказаться Esc >          │
*│                                                    │
*└────────────────────────────────────────────────────┘
PRIVATE exi, is_new, is_com, bound_l, bound_r, ex, crea, w
crea    = 1
bound_l = 0
bound_r = 0
ex      = 1
is_com  = .F.
w       = .F.
IF File_O(prmo_path+"BND.MEM")
  RESTORE FROM (prmo_path+"BND.MEM") ADDITIVE
ELSE
  bound_l = 0
  bound_r = 0
ENDIF

DO D_W WITH "Set_Bnd", 12, 56, "Установка ганиц для прих. ордеров", 1

@ 3, 22 GET is_com PICTURE "@*C Общая" WHEN Sav_W() VALID Do_Bnd()
@ 5,  3 SAY "Левая граница ($)"  GET bound_l PICTURE "@Z 99999"
@ 5, 29 SAY "Правая граница ($)" GET bound_r PICTURE "@Z 99999"
@ 7, 15 GET crea PICTURE "@*RH Создать ;Уничтожить"
@ 9, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE VALID Sav_Val()

RELEASE WINDOW Set_Bnd
RETURN

**************************************************************************
*
*   Сохраняем входное значение
*
PROCEDURE Sav_W

w = is_com
RETURN .T.

**************************************************************************
*
*   Восстанавливаем границы
*
PROCEDURE Do_Bnd

IF w = is_com
  RETURN .T.
ENDIF

IF is_com
  IF File_O(base_path+"BND.MEM")
    RESTORE FROM (base_path+"BND.MEM") ADDITIVE
  ELSE
    bound_l = 0
    bound_r = 0
  ENDIF
ELSE
  IF File_O(prmo_path+"BND.MEM")
    RESTORE FROM (prmo_path+"BND.MEM") ADDITIVE
  ELSE
    bound_l = 0
    bound_r = 0
  ENDIF
ENDIF
SHOW GET bound_l
SHOW GET bound_r
RETURN .T.

**************************************************************************
*
*   Сохраняем то, что понавводили
*
PROCEDURE Sav_Val

IF ex = 2
  RETURN .T.
ENDIF

IF crea = 2
  IF is_com
    IF File_O(base_path+"BND.MEM")
      DELETE FILE (base_path+"BND.MEM")
    ENDIF
  ELSE
    IF File_O(prmo_path+"BND.MEM")
      DELETE FILE (prmo_path+"BND.MEM")
    ENDIF
  ENDIF
ELSE
  IF is_com
    SAVE TO (base_path+"BND.MEM") ALL LIKE bound_*
  ELSE
    SAVE TO (prmo_path+"BND.MEM") ALL LIKE bound_*
  ENDIF
ENDIF
RETURN .T.
