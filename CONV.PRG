*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                     Программа конвертации денег.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Conv

PRIVATE ln, wd, ex, kurs_w, s_usd, s_rub, nt, mode, n_d

*             Конвертация
*┌──────────────────────────────────────┐
*│ ┌─ Конвертируем ───────────────────┐ │
*│ │ ( ) Рубли в USD  ( ) USD в рубли │ │
*│ └──────────────────────────────────┘ │
*│ Рубли 9999999.99    USD 9999999.9999 │
*│        ( F1 - Счетчик купюр )        │
*│           По курсу 99.999            │
*│ ┌─ Примечание ─────────────────────┐ │
*│ │ ....:....1....:....2....:....3.. │ │
*│ └──────────────────────────────────┘ │
*│   < OK Ctrl-W > < Отказаться Esc >   │
*└──────────────────────────────────────┘

ln     = 13
wd     = 42
ex     =  1
kurs_w = kurs_b
s_usd  = 0
s_rub  = 0
nt     = SPACE(FSIZE("NOTE"))
mode   = 1

DO D_W WITH "Conv", ln, wd, "Конвертация", 1

@ 2, 3 TO  4, 38
@ 8, 3 TO 10, 38

@ 2, 5 SAY " Конвертируем "
@ 6,10 SAY "( F1 - Счетчик купюр )"
@ 8, 5 SAY " Примечание "

@ 3, 5 GET mode PICTURE "@*RH Рубли в USD ;USD в рубли"
@ 5, 4 SAY "Рубли" GET s_rub PICTURE "@Z 9999999.99" WHEN mode = 1 VALID Clc_1()
@ 5,23 SAY "USD" GET s_usd PICTURE "@Z 9999999.9999" WHEN mode = 2 VALID Clc_2()
@ 7,13 SAY "По курсу" GET kurs_w PICTURE "99.999" VALID Clc_0()
@ 9, 5 GET nt
@ 11,5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
ON KEY LABEL F1 DO Clc_C
READ CYCLE
ON KEY LABEL F1 DO Clc_C

RELEASE WINDOW Conv

IF ex = 2 .OR. s_rub = 0 .OR. s_usd = 0
  RETURN
ENDIF
IF mode = 1
  s_rub = -s_rub
ELSE
  s_usd = -s_usd
ENDIF

SELECT KASSA_S
=LOCK()
*n_d = LAST_DOC+1
n_d = Doc_Num("KASSA_M", "", arm, DATE())
REPLACE SUM_R    WITH SUM_R+s_rub, ;
        SUM_D    WITH SUM_D+s_usd
UNLOCK

SELECT VIEW
APPEND BLANK
REPLACE DOC_NUM  WITH sys_char+STR(n_d,6), ;
        DOC_DATE WITH DATE(),              ;
        DOC_TIME WITH SECONDS(),           ;
        CODE     WITH 2,                   ;
        SUM_R    WITH s_rub,               ;
        SUM_D    WITH s_usd,               ;
        KURS_FL  WITH kurs_w,              ;
        NOTE     WITH nt,                  ;
        WHO      WITH user,                ;
        WHERE    WITH arm
RETURN

************  Программы пересчета по курсу *****************************
PROCEDURE Clc_1
s_usd = ROUND(s_rub/kurs_w, 4)
SHOW GETS
RETURN .T.
**************************
PROCEDURE Clc_2
s_rub = ROUND(s_usd*kurs_w,2)
SHOW GETS
RETURN .T.
**************************
PROCEDURE Clc_0
PRIVATE mss

IF READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF kurs_w <= 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Таких курсов доллара не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF .NOT. BETWEEN(kurs_w/kurs_b, 0.8, 1.2)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  Очень большой скачок курса. Это правильно? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN .F.
  ENDIF
ENDIF
IF mode = 1
  s_usd = ROUND(s_rub/kurs_w,4)
ELSE
  s_rub = ROUND(s_usd*kurs_w,2)
ENDIF

SHOW GETS
RETURN .T.

**********************************************************
PROCEDURE Clc_C
PRIVATE v

v = VARREAD()

IF v = "S_USD"
  s_usd = Cupury("$", q_usd, 0, -23)
ENDIF

IF v = "S_RUB"
  s_rub = Cupury(" ", q_rub, 0, 23)
ENDIF

RETURN
