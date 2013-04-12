*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║            Программа создания/коррекции информации о клиенте.         ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Cli_Info
PARAMETERS ext_menu, let_corr, parmCode

PRIVATE is_new, ln, wd, sel_sav, mss, ex, crsr, manager_n, k_sw, k_code, k_name
PRIVATE tmpName, tmpCode

sel_sav = SELECT()
manager_n = ""
IF EMPTY(parmCode)
  tmpCode = ACCOUNT.CUS_CODE
ELSE
  tmpCode = parmCode
ENDIF
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_1507 AGAIN
IF .NOT. SEEK(tmpCode)
  USE
  SELECT (sel_sav)
  RETURN
ENDIF
tmpName = CUS_NAME
IF .NOT. EMPTY(CUS_1507.WHO_NEW)
  SELECT 0
  USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PRS_0C29
  SEEK CUS_1507.WHO_NEW
  manager_n = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
  USE
ENDIF
manager_n = PADR(manager_n, 28)
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    DEACTIVATE MENU (ext_menu)
  ENDIF
ENDIF

DEFINE WINDOW H_Wind FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW H_Wind
SELECT 0
USE (base_path+"CLI_KIND") ORDER TAG CODE ALIAS KIND_TMP AGAIN
SELECT 0
USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE ALIAS INFO_TMP AGAIN
k_sw   = .F.
k_code = SPACE(FSIZE("KIND_CODE", "KIND_TMP"))
k_name = SPACE(FSIZE("KIND_NAME", "KIND_TMP"))
is_new = .NOT. SEEK(tmpCode)
IF .NOT. is_new .AND. .NOT. EMPTY(INFO_TMP.KIND_CODE)
  IF SEEK(INFO_TMP.KIND_CODE, "KIND_TMP")
    k_code = KIND_TMP.KIND_CODE
    k_name = KIND_TMP.KIND_NAME
  ENDIF
ENDIF

IF is_new .AND. .NOT. let_corr
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"По этому покупателю нет данных..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RELEASE WINDOW H_Wind
  USE
  SELECT KIND_TMP
  USE
  SELECT CUS_1507
  USE
  SELECT (sel_sav)
  RETURN
ENDIF

PUSH KEY CLEAR
IF is_new
  SCATTER MEMVAR BLANK
ELSE
  SCATTER MEMVAR
ENDIF
ln = IIF(let_corr, 24, 23)
wd = 78

DO D_W WITH "Cli_Info", ln, wd, "Покупатель: "+ALLTRIM(tmpName), 0
@ 0, 0 SAY ""
*       Покупатель: < Имя клиента >
? "┌───────────────────────────────────────────────────────────────────────────┐"
? "│    ┌──── Страна ────────── Регион ────────────────── Город ─────────┐     │"
? "│    │  ....:....1....:.  ....:....1....:....1....  ....:....1....:.  │     │"
? "│    └────────────────────────────────────────────────────────────────┘     │"
? "│┌── Адрес ────────────────────────────────────────────────────────────────┐│"
? "││....:.  ....:....1....:....1....:....1....:....1....:....1....:....1.... ││"
? "││              Телефон ....:....1....       Факс ....:....1               ││"
? "││               Эл. почта ....:....1....:....1....:....1..                ││"
? "│└─────────────────────────────────────────────────────────────────────────┘│"
? "│ Ф.И.О. ....:....1....:....1....:....1..   Должность ....:....1....:....1  │"
? "│┌─[ ] Сфера деят. ────┐┌── Специализация ─────────────────────────────────┐│"
? "││                     ││[ ] Аудио & Мультимедиа  [ ] Мед. оборудование    ││"
? "││                     ││[ ] Видео & Быт. техика  [ ] Авто-техника         ││"
? "││                     ││[ ] Средства связи       [ ] Телекоммуникация     ││"
? "││                     ││[ ] Изм. техника         [ ] Оборуд.радиовещ.и TV ││"
? "││                     ││[ ] Источники питания    [ ] Навигац.оборудование ││"
? "││                     ││[ ] Компьютерная техника     ....:....1....:.     ││"
? "││                     ││[ ] Охранные системы                              ││"
? "│└─────────────────────┘└──────────────────────────────────────────────────┘│"
? "│Примечание ....:....1....:....1....:....1....:....1....:....1....:....1....│"
? "│                   Менеджер ....:....!....:....!....:...                   │"
IF let_corr
  ? "│                     < OK Ctrl-W > < Отказаться Esc >                      │"
  ? "└───────────────────────────────────────────────────────────────────────────┘"
ELSE
  ? "└───────────────────────── Нажмите любую клавишу ───────────────────────────┘"
ENDIF

ex = 1

@  3,  8 GET m.country
@  3, 26 GET m.region
@  3, 52 GET m.city

@  6,  2 GET m.index
@  6, 10 GET m.address PICTURE "@S64"
@  7, 24 GET m.phone
@  7, 50 GET m.fax
@  8, 27 GET m.eml

@ 10,  9 GET m.contact
@ 10, 54 GET m.job

*!*@ 11, 5  GET k_sw PICTURE "@*C Сфера деят. " VALID Ch_Kind()
*!*@ 14, 7  GET k_name WHEN .F.

@ 11, 3  SAY " Сфера деят. "
@ 12, 3 GET m.production PICTURE "@*C Производство"
@ 13, 3 GET m.work       PICTURE "@*C Разработка"
@ 14, 3 GET m.sale       PICTURE "@*C Продажа"
@ 15, 3 GET m.repair     PICTURE "@*C Ремонт"
@ 16, 7 GET m.f_other

@ 12, 25 GET m.audio      PICTURE "@*C Аудио & Мультимедиа"
@ 13, 25 GET m.video      PICTURE "@*C Видео & Бытовая тех."
@ 14, 25 GET m.connect    PICTURE "@*C Средства связи"
@ 15, 25 GET m.meastech   PICTURE "@*C Изм. техника"
@ 16, 25 GET m.power      PICTURE "@*C Источники питания"
@ 17, 25 GET m.computer   PICTURE "@*C Компьютерная техника"
@ 18, 25 GET m.quardsys   PICTURE "@*C Охранные системы"

@ 12, 50 GET m.medequip   PICTURE "@*C Мед. оборудование"
@ 13, 50 GET m.auto       PICTURE "@*C Авто-техника"
@ 14, 50 GET m.telecom    PICTURE "@*C Телекоммуникация"
@ 15, 50 GET m.radio      PICTURE "@*C Оборуд.радиовещ.и TV"
@ 16, 50 GET m.navig      PICTURE "@*C Навигац.оборудование"
@ 17, 54 GET m.p_other

@ 20, 12 GET m.note
@ 21, 20 SAY "Менеджер " GET manager_n WHEN .F.
IF let_corr
  crsr = SET("CURSOR") = "OFF"
  SET CURSOR ON
  @ 22, 22 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
  READ CYCLE VALID Tst_Blank()
  IF ex = 1
    m.who = user
    m.where = arm
    m.cus_code = tmpCode
    m.name     = tmpName
    IF is_new
      APPEND BLANK
    ENDIF
    m.kind_code = k_code
    GATHER MEMVAR
  ENDIF
  IF crsr
    SET CURSOR OFF
  ENDIF
ELSE
  = INKEY(0,"HM")
  READ WHEN .F.
ENDIF

USE
SELECT KIND_TMP
USE
SELECT CUS_1507
USE
RELEASE WINDOW H_Wind
RELEASE WINDOW Cli_Info
SELECT (sel_sav)
POP KEY

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Kind      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Выбор типа клиента.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.03.2001 ──┘
PROCEDURE Ch_Kind

IF .NOT. k_sw
  RETURN .T.
ENDIF

k_sw = .F.
SHOW GET k_sw

DO Cli_Kind WITH k_code, k_name

SHOW GET k_name

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│                                                                        │
*│                           Выход из бланка.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.03.2003 ──┘
PROCEDURE Tst_Blank
PRIVATE mss

IF EMPTY(m.country) .AND. let_corr .AND. ex = 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Название страны должно быть заполнено!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
