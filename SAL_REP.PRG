*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║          Программа приема денег с рабочего места продавца.            ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Sal_Rep

PRIVATE sel_sav, rc_sav, mss, what_do
sel_sav = SELECT()

IF user = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Андрюха, не надо тебе этого делать!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

DO Use_Dummy


SELECT 0
USE KASSA ORDER TAG HRON1
SET RELATION TO WHO_REP INTO USERS,   ;
                WHERE   INTO ARM_LST
SET FILTER TO EMPTY(WHO_GET)
GO TOP

PRIVATE ln, wd, tt, ft, pic1, pic2, pic3
DIMENSION tt(1), ft(2)

pic1 = "@Z 999 999.9999"
pic2 = "@S"+ALLTRIM(STR(FSIZE("DOC_NUM")+1))
pic3 = "@S16"
tt(1) = PADC("Дата", IIF(SET("CENTURY") = "ON", 10, 8), "─")+"─"+  ;
        PADC("N док-та", FSIZE("DOC_NUM")+1, "─")+"─"+  ;
        PADC("Сумма", 12, "─")+"─"+  ;
        PADC("Раб. место", FSIZE("ARM_N", "ARM_LST"), "─")+"─"+  ;
        PADC("Продавец", 16, "─")

ft(1) = ""
ft(2) = ""

ln = WROWS("")-8
wd = LEN(tt(1))

DEFINE MENU Sal_Rep IN SCREEN COLOR SCHEME 4
DEFINE PAD Choose OF Sal_Rep PROMPT "Оприходовать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF Sal_Rep DO Ch_Mode WITH "Sal_Rep", "Input"
DEFINE PAD Exit OF Sal_Rep PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF Sal_Rep DO Ch_Mode WITH "M_Kassa", "Exit"

DO Def_Win WITH "Rep_M", "Rep_E", "Rep_I", ln, wd, "Отчеты продавцов", tt, ft

what_do = "List"

DO WHILE .T.
  DO CASE
  CASE what_do = "List"
    ACTIVATE WINDOW Rep_E
    DO Prp_Menu WITH "Sal_Rep"
    ON KEY LABEL F10 ACTIVATE MENU Sal_Rep
    ON KEY LABEL Enter KEYBOARD CHR(23)
    BROWSE FIELDS DOC_DATE:H="",   ;
                  F001 = LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5)):H="":P=pic2,  ;
                  VALUTA:H="",     ;
                  TOTAL:H="":P=pic1,   ;
                  ARM_LST.ARM_N:H="",    ;
                  F002 = IIF(FOUND("USERS"), USERS.USER_NAME, "Сам!"):H="":P=pic3,  ;
                  DUMMY.F:H=""           ;
            FREEZE DUMMY.F               ;
            NOMENU NOCLEAR NOAPPEN NODELETE WINDOW Rep_I IN WINDOW Rep_E
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Input")
    ENDIF
    ON KEY LABEL F10
    ON KEY LABEL Enter
    HIDE MENU Sal_Rep
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
  CASE what_do = "Input"
    IF .NOT. EOF() .AND. .NOT. BOF()
      DO Input
    ENDIF
    what_do = "List"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW Rep_M
RELEASE WINDOW Rep_E
RELEASE WINDOW Rep_I
HIDE MENU Sal_Rep
RELEASE MENU Sal_Rep EXTENDED

USE
SELECT DUMMY
USE

SELECT (sel_sav)

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Программа утверждения отчета.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Input

PRIVATE ln, wd, ex, s_usd, s_rub, kurs_w, n_d, note1, note2
PRIVATE m_rub, m_usd, money_r, money_d, v_sav

* money_r   Общая сумма в руб.  по отчету
* money_d   Общая сумма в долл. по отчету
* s_rub     Принято рублей по долларовому отчету
* s_usd     Принято долларов по рублевому отчету
* m_rub     Эквивалент долларов в рублях (рубл.отчет)
* m_usd     Эквивалент рублей в долларах (долл.отчет)

IF VALUTA = "$"
  money_d = TOTAL
*  money_r = 0
ELSE
  money_r = TOTAL
*  money_d = 0
ENDIF
s_rub  = 0
s_usd  = 0
m_rub  = 0
m_usd  = 0
kurs_w = kurs_b
note1  = SPACE(32)
note2  = SPACE(32)

*          Прием денег от продавца
*┌─────────────────────────────────────────┐
*│                                         │
*│   Общая сумма в рублях 9999999.99 (F1)  │
*│                                         │
*│ Принято USD 999999.9999 по курсу 99.999 │
*│                                         │
*│   В рублях это составляет 9999999.99    │
*│                                         │
*│         Принято 9999999.99 руб.         │
*│                                         │
*│  ┌ Примечание 1 ( пост. ) ───────────┐  │
*│  │ ....:....1....:....2....:....3..  │  │
*│  └───────────────────────────────────┘  │
*│  ┌ Примечание 2 ( конв. ) ───────────┐  │
*│  │ ....:....1....:....2....:....3..  │  │
*│  └───────────────────────────────────┘  │
*│                                         │
*│    < OK Ctrl-W > < Отказаться Esc >     │
*└─────────────────────────────────────────┘

ln = 20
wd = 45
ex =  1
DO D_W WITH "Input", ln, wd, "Прием денег от продавца", 1

PUSH KEY CLEAR
@ 11,  4 TO 13, 40
@ 11,  5 SAY " Примечание 1 ( пост. ) "
@ 14,  4 TO 16, 40
@ 14,  5 SAY " Примечание 2 ( конв. ) "

IF VALUTA = "$"
  @ 3,  4 SAY "Общая сумма в USD "+STR(TOTAL,11,4)+" (F1)"
  @ 5,  3 SAY "Принято руб." GET s_rub PICTURE "@Z 9999999.99" ;
                   WHEN Save_Val(s_rub) VALID Draw_RD(1)
  @ 5, 27 SAY "по курсу" GET kurs_w PICTURE "99.999" ;
                   WHEN Save_Val(kurs_w) VALID Draw_RD(2)
  @ 7,  5 SAY "В USD это составляет "+TRANSFORM(s_rub/kurs_w, "@Z 999999.9999")
  @ 9, 11 SAY "Принято "+TRANSFORM(s_usd, "@Z 999999.9999")
  @ 9, 30 SAY " USD"
  @ 12, 6 GET note1
  @ 15, 6 GET note2 WHEN s_rub # 0
  @ 18, 6 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
ELSE
  @ 3,  5 SAY "Общая сумма в рублях "+STR(TOTAL,10,2)+" (F1)"
  @ 5,  3 SAY "Принято USD" GET s_usd PICTURE "@Z 999999.9999" ;
                   WHEN Save_Val(s_usd) VALID Draw_DR(1)
  @ 5, 27 SAY "по курсу" GET kurs_w PICTURE "99.999" ;
                   WHEN Save_Val(kurs_w) VALID Draw_DR(2)
  @ 7,  5 SAY "В рублях это составляет "+TRANSFORM(s_usd*kurs_w, "@Z 9999999.99")
  @ 9, 11 SAY "Принято "+TRANSFORM(s_rub, "@Z 9999999.99")
  @ 9, 30 SAY "руб."
  @ 12, 6 GET note1
  @ 15, 6 GET note2 WHEN s_usd # 0
  @ 18, 6 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
ENDIF
ON KEY LABEL F1 DO Clc_C WITH "R"

READ CYCLE

ON KEY LABEL F1

RELEASE WINDOW Input
IF ex = 2
  RETURN
ENDIF

*
*    Формируем прием отчета

SELECT KASSA_S
n_d = Doc_Num("KASSA_M", "", arm, DATE())
IF KASSA.VALUTA = "$"
  REPLACE SUM_D WITH SUM_D+KASSA.TOTAL
ELSE
  REPLACE SUM_R WITH SUM_R+KASSA.TOTAL
ENDIF  

SELECT KASSA
REPLACE WHO_GET WITH user

*    Списываем деньги с АРМа...
*
SELECT ARM_LST
IF KASSA.VALUTA = "$"
  REPLACE KASSA_USD WITH KASSA_USD-KASSA.TOTAL
ELSE
  REPLACE KASSA WITH KASSA-KASSA.TOTAL
ENDIF

*    ... и приходуем их в кассу предприятия.
*
SELECT VIEW
APPEND BLANK
REPLACE DOC_NUM  WITH sys_char+STR(n_d,6), ;
        DOC_DATE WITH DATE(),              ;
        DOC_TIME WITH SECONDS(),           ;
        CODE     WITH oper_c,              ;
        REF_NUM  WITH KASSA.DOC_NUM,       ;
        REF_DATE WITH KASSA.DOC_DATE,      ;
        C_BILL   WITH wk_bill,             ;
        NOTE     WITH note1,               ;
        WHO      WITH user,                ;
        WHERE    WITH arm
IF KASSA.VALUTA = "$"
  REPLACE SUM_D WITH KASSA.TOTAL
ELSE
  REPLACE SUM_R WITH KASSA.TOTAL
ENDIF

IF KASSA.VALUTA # "$"
  IF s_usd # 0    && С конвертацией ! (рубли в доллары)
    SELECT KASSA_S
    n_d = Doc_Num("KASSA_M", "", arm, DATE())
    REPLACE SUM_R    WITH SUM_R-s_usd*kurs_w, ;
            SUM_D    WITH SUM_D+s_usd

    SELECT VIEW
    APPEND BLANK
    REPLACE DOC_NUM  WITH sys_char+STR(n_d,6),     ;
            DOC_DATE WITH DATE(),                  ;
            DOC_TIME WITH SECONDS(),               ;
            CODE     WITH 2,                       ;
            SUM_R    WITH -s_usd*kurs_w,           ;
            SUM_D    WITH s_usd,                   ;
            KURS_FL  WITH kurs_w,                  ;
            NOTE     WITH note2,                   ;
            WHO      WITH user,                    ;
            WHERE    WITH arm
  ENDIF            
ELSE
  IF s_rub # 0    && С конвертацией ! (рубли в доллары)
    SELECT KASSA_S
    n_d = Doc_Num("KASSA_M", "", arm, DATE())
    REPLACE SUM_D    WITH SUM_D-s_rub/kurs_w, ;
            SUM_R    WITH SUM_R+s_rub

    SELECT VIEW
    APPEND BLANK
    REPLACE DOC_NUM  WITH sys_char+STR(n_d,6),  ;
            DOC_DATE WITH DATE(),               ;
            DOC_TIME WITH SECONDS(),            ;
            CODE     WITH 2,                    ;
            SUM_R    WITH s_rub,                ;
            SUM_D    WITH -s_rub/kurs_w,        ;
            KURS_FL  WITH kurs_w,               ;
            NOTE     WITH note2,                ;
            WHO      WITH user,                 ;
            WHERE    WITH arm
  ENDIF
ENDIF
SELECT KASSA

POP KEY
*┌────────────────────────────────────────────────────────────────────────┐
*│                                                                        │
*│                 Сохранение значения при входе в поле.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.02.1999 ──┘
PROCEDURE Save_Val
PARAMETERS v
v_sav = v
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│                                                                        │
*│           Варианты выхода из полей конвертации РУБЛИ -> USD.           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 17.12.2001 ──┘
PROCEDURE Draw_RD
PARAMETERS mode
PRIVATE mss
DIMENSION mss(3)

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

DO CASE
CASE mode = 1                       && Принять рубли (отчет в долларах)
  IF s_rub < 0 .AND. money_d > 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Отдаем рубли?!! Ни за что!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF money_d < 0 .AND. s_rub > 0 
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Доллары и рубли должны быть одного знака!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  
  m_usd = ROUND(s_rub/kurs_w,4)     && В USD это составляет
  s_usd = money_d-m_usd             && Принято USD
  @ 7,  5 SAY "В USD это составляет "+TRANSFORM(m_usd, "@Z 999999.9999")
  @ 9, 11 SAY "Принято "+TRANSFORM(s_usd, "@Z 999999.9999")

CASE mode = 2      && Курс для пересчета рублей в доллары
  IF kurs_w <= 0
    mss(1) = ""
    mss(2) = CHR(0)+"Таких курсов долларов не бывает!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
 
  m_usd = ROUND(s_rub/kurs_w,4)    && В USD это составляет
  s_usd = money_d-m_usd            && Принято USD
  @ 7,  5 SAY "В USD это составляет "+TRANSFORM(m_usd, "@Z 999999.9999")
  @ 9, 11 SAY "Принято "+TRANSFORM(s_usd, "@Z 999999.9999")

ENDCASE    

SHOW GETS

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│                                                                        │
*│           Варианты выхода из полей конвертации USD -> РУБЛИ.           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 17.12.2001 ──┘
PROCEDURE Draw_DR
PARAMETERS mode

PRIVATE mss
DIMENSION mss(3)

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

DO CASE
CASE mode = 1                      && Принять доллары (отчет в рублях)
  IF s_usd < 0 .AND. money_r > 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Отдаем валюту?!! Ни за что!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF money_r < 0 .AND. s_usd > 0 
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Рубли и доллары должны быть одного знака!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  m_rub = ROUND(s_usd*kurs_w,2)    && В рублях это составляет
  s_rub = money_r-m_rub            && Принято руб.
  @ 7,  5 SAY "В рублях это составляет "+TRANSFORM(m_rub, "@Z 9999999.99")
  @ 9, 11 SAY "Принято "+TRANSFORM(s_rub, "@Z 9999999.99")

CASE mode = 2                      && Курс для пересчета долларов в рубли
  IF kurs_w <= 0
    mss(1) = ""
    mss(2) = CHR(0)+"Таких курсов долларов не бывает!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF

  m_rub = ROUND(s_usd*kurs_w,2)    && В рублях это составляет
  s_rub = money_r-m_rub            && Принято руб.
  @ 7,  5 SAY "В рублях это составляет "+TRANSFORM(m_rub, "@Z 9999999.99")
  @ 9, 11 SAY "Принято "+TRANSFORM(s_rub, "@Z 9999999.99")


ENDCASE    

SHOW GETS

RETURN .T.

************************************************************************
PROCEDURE Clc_C
PARAMETERS vl

IF vl = "R"
  = Cupury(" ", s_rub, 0, -23)
ELSE
  = Cupury("$", s_usd, 0, -23)
ENDIF
RETURN