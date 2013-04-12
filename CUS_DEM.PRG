                        ║
*╚════════════════════════════════════════════════════ 07.05.97 12:17:29 ═╝
PROCEDURE Choose_P
PARAMETERS shab
PRIVATE sel_sav

sel_sav = SELECT()

DO Wt_Mess WITH "Подборка..."
IF view_mode = "0"
  SELECT PREFIX, NAME, PRODUCER, RECNO() AS REC_N FROM CORR_ACT  ;
         INTO CURSOR TMP_C   ;
         WHERE (shab $ NAME .OR. shab $ PREFIX .OR. shab $ PRODUCER)  ;
               .AND. QNT # 0
ELSE
  SELECT PREFIX, NAME, PRODUCER, RECNO() AS REC_N FROM STOCK  ;
     ════════════════════════╝
PROCEDURE Cus_Dem

PRIVATE f_c000, f_n000, b_c000, b_n000, b_sw000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
PRIVATE arr_w, mss, s, sd, c_c, c_n, sw
PRIVATE n1, n2, n3, adr, c_cd, c0, q, pr, prd
PRIVATE refr, let_pass, dat_0, dat_mss
f_c000 = def_firm
f_n000 = def_firm
b_c000 = def_bank
f_c_old = def_firm

*
*     Формируем список клиентов из спроса
*

DO Use_Dummy

refr     = .T.
let_pass = File_O(tmpo_path+"T_DEM.DBF") .AND. File_O(tmpo_path+"T_DEM.CDX") .AND.  ;
           File_O(tmpo_path+"L_DEM.DBF") .AND. File_O(tmpo_path+"L_DEM.CDX")
IF let_pass
  = ADIR(c_c, tmpo_path+"T_DEM.DBF")
  dat_mss = "Спрос подготовлен "+DTOC(c_c(1,3))
  RELEASE c_c
ELSE
  dat_mss = "Спрос не подготовлен "
ENDIF

dat_0    = DATE()-30

IF .NOT. Set_Bl()
  CLOSE DATABASES
  RETURN
ENDIF

IF refr

  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

  SELECT 0
  USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_COD

  SELECT 0
  USE (base_path+"STOCK") ORDER TAG NAME_P ALIAS STO_NAM AGAIN

  DELETE FILE (tmpo_path+"T_DEM.DBF")
  DELETE FILE (tmpo_path+"T_DEM.CDX")

  DELETE FILE (tmpo_path+"L_DEM.DBF")
  DELETE FILE (tmpo_path+"L_DEM.CDX")

  SELECT 0
  SET EXCLUSIVE ON
  CREATE DBF (tmpo_path+"T_DEM.DBF")  ;
     ( CUS_CODE N(FSIZE("CUS_CODE","ACCOUNT")),   ;
       CUS_NAME C(FSIZE("CUS_NAME","ACCOUNT")),   ;
       L_SUM_D  N(10,4),                          ;
       L_SUM    N(10,2)  )
  INDEX ON SYS(15, lwr, CUS_NAME) TAG NAME

  SELECT 0
  CREATE DBF (tmpo_path+"L_DEM.DBF")  ;
     ( CUS_CODE N(FSIZE("CUS_CODE","ACCOUNT")),   ;
       CODE     N(FSIZE("CODE","STO_COD")),       ;
       PREFIX   C(FSIZE("PREFIX","STO_COD")),     ;
       NAME     C(FSIZE("NAME","STO_COD")),       ;
       PRO_CODE N(FSIZE("PRO_CODE","STO_COD")),   ;
       PRODUCER C(FSIZE("PRODUCER","STO_COD")),   ;
       ADDRESS  C(FSIZE("ADDRESS","STO_COD")),    ;
       QNT      N( 6),                            ;
       PRICE_D  N(10,4),                          ;
       PRICE_R  N( 8,2)   )
  INDEX ON STR(CUS_CODE, 8)+NAME+PREFIX TAG NAME
  INDEX ON STR(CUS_CODE, 8)+STR(CODE,7) TAG CODE ADDITIVE

  SET EXCLUSIVE OFF

  SELECT 0
  USE (base_path+"DEMAND") ORDER TAG CLI_CODE
  SET DELETED OFF

  DO Wt_Mess WITH "Подготовка списка клиентов..."
  c_c = 0
  c_n = ""
  s   = 0
  sd  = 0
  sw  = .F.
  SCAN FOR CLI_CODE # 0 .AND. DATE >= dat_0
    IF CLI_CODE # c_c   && Сменился клиент - разбираемся...

      IF s # 0          && Что-то для него нашли
        SELECT T_DEM
        APPEND BLANK
        REPLACE CUS_CODE WITH c_c,   ;
                CUS_NAME WITH c_n,   ;
                L_SUM_D  WITH sd,    ;
                L_SUM    WITH s
        SELECT DEMAND
      ENDIF

      s   = 0
      sd  = 0
      c_c = CLI_CODE
      sw = .T.
      IF SEEK(CLI_CODE, "ACCOUNT")
        IF .NOT. EMPTY(ACCOUNT.DATE_OFF)
          sw = .F.
        ENDIF
        c_n = ACCOUNT.CUS_NAME
      ELSE
        sw = .F.
      ENDIF
    ENDIF

    *    Разбираемся с позицией
    *
    IF .NOT. sw    && Нет клиента
      LOOP
    ENDIF
    c0 = 0
    IF STO_CODE # 0  && Складская...
      IF SEEK(STO_CODE, "STO_COD")
        IF EMPTY(STO_COD.DATE_OFF) .AND. STO_COD.QNT > 0
          c0 = STO_COD.CODE
          n1 = STO_COD.PREFIX
          n2 = STO_COD.NAME
          n3 = STO_COD.PRODUCER
          adr= STO_COD.ADDRESS
          p_cod=STO_COD.PRO_CODE
          q  = STO_COD.QNT
          pr = STO_COD.PRICE_R
          prd= STO_COD.PRICE
        ENDIF
      ENDIF
    ELSE
      IF SEEK(NAME+PREFIX+PRODUCER, "STO_NAM")
        IF EMPTY(STO_NAM.DATE_OFF) .AND. STO_NAM.QNT > 0
          c0 = STO_NAM.CODE
          n1 = STO_NAM.PREFIX
          n2 = STO_NAM.NAME
          n3 = STO_NAM.PRODUCER
          adr= STO_NAM.ADDRESS
          p_cod=STO_NAM.PRO_CODE
          q  = STO_NAM.QNT
          pr = STO_NAM.PRICE_R
          prd= STO_NAM.PRICE
        ENDIF
      ENDIF
    ENDIF
    IF c0 = 0
      LOOP
    ENDIF
    SELECT L_DEM
    IF .NOT. SEEK(STR(c_c, 8)+STR(c0,7))
      APPEND BLANK
      REPLACE CUS_CODE WITH c_c,   ;
              CODE     WITH c0,    ;
              PREFIX   WITH n1,    ;
              NAME     WITH n2,    ;
              PRODUCER WITH n3,    ;
              PRICE_D  WITH prd,   ;
              PRICE_R  WITH pr
    ENDIF
    c0 = QNT
    REPLACE  QNT WITH MIN(QNT+DEMAND.QTY, q)
    sd = sd+(QNT-c0)*PRICE_D
    s = s+(QNT-c0)*PRICE_R
    SELECT DEMAND
  ENDSCAN

  IF s # 0          && И последний клиент
    SELECT T_DEM
    APPEND BLANK
    REPLACE CUS_CODE WITH c_c,   ;
            CUS_NAME WITH c_n,   ;
            L_SUM_D  WITH sd,    ;
            L_SUM    WITH s
    SELECT DEMAND
  ENDIF

  SET DELETED ON
  USE
  SELECT STO_NAM
  USE
  SELECT STO_COD
  USE
  SELECT ACCOUNT
  USE

  DO Wt_Mess
ELSE

  SELECT 0
  USE (tmpo_path+"T_DEM.DBF") ORDER TAG NAME

  SELECT 0
  USE (tmpo_path+"L_DEM.DBF") ORDER TAG CODE

ENDIF

SELECT L_DEM
SET ORDER TO TAG NAME

SELECT T_DEM

IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ни для кого ничего не появилось!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  DO V_Custs
ENDIF

CLOSE DATABASES
*DELETE FILE (tmpo_path+"T_DEM.DBF")
*DELETE FILE (tmpo_path+"T_DEM.CDX")
*DELETE FILE (tmpo_path+"L_DEM.DBF")
*DELETE FILE (tmpo_path+"L_DEM.CDX")

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Просмотр списка клиентов.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 04.02.97 14:39:29 ═╝
PROCEDURE V_Custs
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

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN && COLOR SCHEME 4

DEFINE PAD View OF (menu_name) PROMPT "Просмотр" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) Do Sw_Mode WITH "View"

DEFINE PAD ToList OF (menu_name) PROMPT "Список" KEY Ctrl-A, "Tab"
ON SELECTION PAD ToList OF (menu_name) Do Sw_Mode WITH "ToList"

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) Do Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

SELECT 0
USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE
SELECT T_DEM
SET RELATION TO CUS_CODE INTO CLI_INFO
*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = FSIZE("CUS_NAME")+FSIZE("L_SUM_D")+3

DO D_Wins WITH ln, wd, "Список клиентов", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр перечня списков

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter  KEYBOARD CHR(23)
    ON KEY LABEL Tab    Do Sw_Mode WITH "ToList"
    ON KEY LABEL Ctrl+P Do Sw_Mode WITH "Print"

*
*    BROWSE - меню
*
    BROWSE FIELDS CUS_NAME:H="", L_SUM_D:H="",  ;
                  EML000 = IIF(EMPTY(CLI_INFO.EML), " ", "√"):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "exit", "View")
    ENDIF

  CASE what_do = "View"    && Просмотр списка

    DO View_L
    what_do = "List"

  CASE what_do = "Print"   && Печать списка

    DO P_C_Dem
    what_do = "List"

  CASE what_do = "ToList"  && В заготовку!

    DO Mk_List
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Просмотр списка позиций.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 04.02.97 14:39:29 ═╝
PROCEDURE View_L
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

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN && COLOR SCHEME 4

DEFINE PAD Report OF (menu_name) PROMPT "Справка" KEY Ctrl-A, "Enter"
ON SELECTION PAD Report OF (menu_name) Do Sw_Mode WITH "Report"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
SELECT L_DEM
PRIVATE ln, wd, tt
ln = 100   && Количество видимых строк BROWSE
wd = FSIZE("PREFIX")+FSIZE("NAME")+FSIZE("PRODUCER")+FSIZE("QNT")+FSIZE("PRICE_D")+20

DO D_Wins WITH ln, wd, ALLTRIM("T_DEM"), 1, 0
@ 1, 2 SAY PADR("    Позиция",FSIZE("PREFIX")+FSIZE("NAME")+FSIZE("PRODUCER")+2)+" "+ ;
           PADL("Кол-во",FSIZE("QNT"))+" "+ ;
           PADL("Цена",FSIZE("PRICE_R"))+" "+ ;
           PADL("Сумма",10)
what_do = "List"

DO WHILE .T.
  DO CASE
  CASE what_do = "List"
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
*
*    BROWSE - меню
*
    BROWSE KEY STR(T_DEM.CUS_CODE,8)  ;
           FIELDS PREFIX:H="",   ;
                  NAME:H="",     ;
                  PRODUCER:H="", ;
                  QNT:H="",      ;
                  PRICE_D:H="",  ;
                  F000 = QNT*PRICE_D:H="":P="99 999 999.9999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Report")
    ENDIF
    ON KEY
  CASE what_do = "Report"
    DO Move_S WITH L_DEM.CODE
    what_do = "List"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
SELECT T_DEM

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                             Печать списка.                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.02.97 21:44:49 ═╝
PROCEDURE P_C_Dem

PRIVATE p_ln, fnt, n_cp, lft, ffeed, sb, n, t_d, t_p, f_d, f_p, p_drctry

DIMENSION sb(4,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Customer}"
sb( 2,2) = ""

sb( 3,1) = "{Pg_Sum  }"
sb( 3,2) = ""

sb( 4,1) = "{Tot_Sum }"
sb( 4,2) = ""

n = ALEN(sb,1)
FOR i = 1 TO n
  sb(i,2) =PADR(sb(i,2), LEN(sb(i,1)))
ENDFOR
sb(2,2) = ALLTRIM(T_DEM.CUS_NAME)

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "CUS_DEM" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR


n = MEMLINES(DOC_H)
DIMENSION t_d(n)
FOR i = 1 TO n
  t_d(i) = MLINE(DOC_H,i)
ENDFOR

n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

n = MEMLINES(PAGE_F)
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(PAGE_F,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

SELECT L_DEM
SEEK STR(T_DEM.CUS_CODE,8)
COUNT REST WHILE CUS_CODE = T_DEM.CUS_CODE TO nn
SEEK STR(T_DEM.CUS_CODE,8)
DELETE FILE (tmpo_path+"P_C_DEM.DBF")
COPY REST WHILE CUS_CODE = T_DEM.CUS_CODE TO (tmpo_path+"P_C_DEM.DBF") ;
  FIELDS PREFIX, NAME, PRODUCER, QNT, PRICE_D
SEEK STR(T_DEM.CUS_CODE,8)
DO Ini_Prn WITH "Список спроса для клиента", p_ln, lft, n_cp, fnt, ffeed, p_drctry

PRIVATE n_pg, n_p_det, nn, sum_w, sum_p, f_pg, str_w
n_p_det  = p_ln-MAX(ALEN(t_d),ALEN(t_p))-MAX(ALEN(f_p),ALEN(f_d))
n_pg = 1

IF n_p_det <= 0  && Забили всю страницу под заголовки... ни каких страниц!
  n_p_det = nn
ELSE
  nn = MAX(nn-n_p_det, 0)
  n_pg = 1+CEILING(nn/n_p_det)
ENDIF

nn = 0

STORE 0 TO sum_w
FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1   && Заголовок НЕ первой страницы
    EJECT
    FOR i = 1 TO ALEN(t_p)
      str_w = t_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ELSE        && Заголовок первой страницы
    FOR i = 1 TO ALEN(t_d)
      str_w = t_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF
  STORE 0 TO sum_p
*
*   Детальная часть страницы
* 
  FOR i = 1 TO n_p_det
    nn = nn+1
    @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+STR(QNT,7)+  ;
                     STR(PRICE_R,9,2)+STR(QNT*PRICE_R,11,2)
    sum_p = sum_p+QNT*PRICE_D
    SKIP
    IF EOF() .OR. CUS_CODE # T_DEM.CUS_CODE
      EXIT
    ENDIF
  ENDFOR
*
*    Окончание страницы
*
  sum_w = sum_w+sum_p

  sb( 3,2) = PADR(STR(sum_p,10,2), LEN(sb(3,1)))
  IF f_pg = n_pg
    sb(4,2) =PADR(STR(sum_w,10,2), LEN(sb(4,1)))
    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR

DO Term_Prn WITH "", tmpo_path+"P_C_DEM.DBF", T_DEM.CUS_CODE
DELETE FILE (tmpo_path+"P_C_DEM.DBF")

SELECT T_DEM

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Создание списка.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.02.97 22:53:01 ═╝
PROCEDURE Mk_List
PRIVATE l_num

DO G_Firm

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM
l_num = Doc_Num("LISTS", f_c000, arm, DATE())
APPEND BLANK
REPLACE FIRM      WITH f_c000,  ;
        LST_ARM   WITH arm,     ;
        LST_NUM   WITH l_num,   ;
        CR_DAT    WITH DATE(),  ;
        LST_NAME  WITH ALLTRIM(T_DEM.CUS_NAME), ;
        CLI_CODE  WITH T_DEM.CUS_CODE,  ;
        R_TOTAL   WITH T_DEM.L_SUM,     ;
        ORD_QUEUE WITH queuetype,       ;
        WHO_CR    WITH user,            ;
        WHERE_CR  WITH arm
l_num = STR(l_num,6)
USE (base_path+"LIST_DET")
SELECT L_DEM
SEEK STR(T_DEM.CUS_CODE,8)
SCAN REST WHILE CUS_CODE = T_DEM.CUS_CODE
  SELECT LIST_DET
  APPEND BLANK
  REPLACE LIST_NUM WITH l_num,   ;
          CODE     WITH L_DEM.CODE, ;
          PREFIX   WITH L_DEM.PREFIX, ;
          NAME     WITH L_DEM.NAME,   ;
          PRO_CODE WITH L_DEM.PRO_CODE, ;
          PRODUCER WITH L_DEM.PRODUCER, ;
          ADDRESS  WITH L_DEM.ADDRESS,  ;
          QNT      WITH L_DEM.QNT,      ;
          DEF_PRICE WITH L_DEM.PRICE_R, ;
          SAL_PRICE WITH L_DEM.PRICE_R
  SELECT L_DEM
ENDSCAN

SELECT LIST_DET
USE
SELECT T_DEM

PRIVATE mss
DIMENSION mss(3)

mss(1) = ""
mss(2) = CHR(0)+"Создан список N "+ALLTRIM(l_num)+"."
mss(3) = ""

DO Out_Mess WITH 5, "mss"

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                    Установа режима подбора данных.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 16.02.97 18:42:16 ═╝
PROCEDURE Set_Bl
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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

PRIVATE ex
ex   = 1
*------------------------------------------------------------------------
*
*┌──────────────────────────────────────────────┐
*│                                              │
*│                                              │
*│                                              │
*│    [ ] Обновить список спроса с ДД.ММ.ГГ     │
*│                                              │
*│         < OK Ctrl-W > < Отказаться >         │
*│                                              │
*└──────────────────────────────────────────────┘
*
PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 50, "Режим просмотра спроса"
@ 3, 2 SAY PADC(dat_mss, WCOLS()-4)

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 5,  6 GET refr PICTURE "@*C Обновить список спроса с" WHEN let_pass VALID Clr_Dat()
@ 5, 35 GET dat_0 PICTURE "@D" WHEN refr
@ 7, 11 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                Программа выхода из поля обновления.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 16.02.97 18:42:16 ═╝
PROCEDURE Clr_Dat

IF .NOT. refr
  dat_0 = {}
  SHOW GET dat_0
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Firm       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Получаем предприятие.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11/02/1999 ──┘
PROCEDURE G_Firm

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌─────────────────────────────────────────┐
*│                                         │
*│  Предприятие AAA AAAAAAAAAAAAAAAA (F1)  │
*│                                         │
*│                 < OK >                  │
*└─────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 45, "Укажите предприятие"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

DO Get_Firm WITH 3, 4, enab_firm
@ WROWS()-2, 19 GET ex PICTURE "@*HT \ OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
