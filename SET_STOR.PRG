*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа установки доп. наличия.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Set_Stor

PRIVATE typ, ex, acc_c

*     Установить доп. наличие равным:
*┌───────────────────────────────────────┐
*│   ( ) Суммарному наличию в филиалах   │
*│   ( ) Суммарной брони                 │
*│   ( ) Наличию в одном из филиалов     │
*│   < OK Ctrl-W > < Отказаться Esc >    │
*└───────────────────────────────────────┘

HIDE MENU Main_M
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
typ = 1
ex  = 1

DO D_W WITH "Set_Stor", 7, 43, "Установить доп. наличие равным:", 1

@ 2, 5 GET typ PICTURE   ;
"@*R Суммарному наличию в филиалах; Суммарной брони; Наличию в одном из филиалов"
@ 5, 5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE

RELEASE WINDOW Set_Stor
IF ex = 2
  RETURN
ENDIF

USE (base_path+"STOCK")
DO CASE
CASE typ = 1  && ВСЕ ФИЛИАЛЫ
  DO Wt_Mess WITH "Установка доп. наличия..."
  SCAN
    REPLACE QNT_STORE WITH QNT_REAL
  ENDSCAN
  DO Wt_Mess
CASE typ = 2  && ВСЯ БРОНЬ
  DO Wt_Mess WITH "Установка доп. наличия..."
  SCAN
    REPLACE QNT_STORE WITH QNT_SAVE+QNT_REZ
  ENDSCAN
  DO Wt_Mess
CASE typ = 3  && Конкретный филиал
  DO Use_Dummy
  DO Use_Link
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
  IF Custs("", .F., .F.)
    acc_c = ACCOUNT.CUS_CODE
    DO Wt_Mess WITH "Установка доп. наличия..."
    USE (base_path+"RYN_L") ORDER TAG CODE
    SET FILTER TO CUS_CODE = acc_c
    SELECT STOCK
    SCAN
      REPLACE QNT_STORE WITH IIF(SEEK(CODE,"RYN_L"), RYN_L.QNT, 0)
    ENDSCAN
    DO Wt_Mess
  ENDIF
ENDCASE

CLOSE DATABASES

RETURN
