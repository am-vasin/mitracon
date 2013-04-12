*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Sal_Book     Разработчик Андрей Васин           05.02.97 23:23:52 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                             Книга продаж.                              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Sal_Book

PRIVATE dat_0, dat_1, dat_v0, dat_v1, mss, s000

USE (base_path+"PARMS")
dat_0  = DATE()           && DAT_START
dat_v0 = DAT_START
USE

dat_1  = DATE()
dat_v1 = DATE()
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
f_c000 = "   "
f_n000 = SPACE(16)
f_c_old = f_c000
IF .NOT. Fill_Bl()
  RETURN
ENDIF

DO Wt_Mess WITH "Подборка данных..."

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"CUS_BIL") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS MON_1214

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON
SET RELATION TO CUS_CODE INTO ACCOUNT, ;
                CUS_CODE INTO CUS_BIL, ;
                FIRM+PAY_TYPE+LEFT(DTOS(PAY_DATE),4)+(PAY_DOC) INTO MON_1214


DELETE FILE (tmpo_path+"SAL_BOOK.DBF")
SELECT 0
SET EXCLUSIVE ON
CREATE DBF (tmpo_path+"SAL_BOOK.DBF")  ;
   ( DOC_NUM  C(10), ;
     DOC_DATE D,     ;
     CUS_NAME C(FSIZE("CUS_NAME","ACCOUNT")),  ;
     INN      C(12), ;
     PAY_DATE D,     ; 
     SUM_TOV  N(10,2), ;
     SUM_NDS  N(10,2), ;
     SUM_H    N(10,2)   )   

SET EXCLUSIVE OFF
PRIVATE c_nam, i_cod
SELECT SALE_TIT

IF .NOT. SEEK(DTOS(dat_0))
  i_cod = RECNO(0)
  IF i_cod > 0 .AND. i_cod < RECCOUNT()
    GO i_cod
  ELSE
    GO BOTTOM
  ENDIF
ENDIF
PRIVATE s_check, scheck0, p_dat
s_check = 0
s_check0 = 0

SCAN REST WHILE DOC_DATE <= dat_1 FOR FIRM = f_c000
*  IF NEW_TYPE # " " .OR. .NOT. FOUND("ACCOUNT")
*    LOOP
*  ENDIF

  IF CUS_CODE < 0
    LOOP
  ENDIF
  IF SUBSTR(DOC_NUM, 2, 3) = "***"
    s_check = s_check+TOTAL_R
    s_check0 = s_check0+ROUND(TOTAL_R*100/(100+nds_flt),2)
    LOOP
  ENDIF
  c_nam = IIF(ACCOUNT.CUS_CODE = 0, SALE_TIT.CUS_NAME, ACCOUNT.CUS_NAME)
  i_cod = ""
  IF FOUND("CUS_BIL")
    c_nam = CUS_BIL.CUS_NAME
    i_cod = CUS_BIL.INN
  ENDIF
*-----------------------------------------
  p_dat = {}
  IF .NOT. EMPTY(SALE_TIT.PAY_DOC)
    IF FOUND ("MON_1214")
      IF .NOT. EMPTY(MON_1214.DOC_NUM)
        p_dat = IIF(EMPTY(MON_1214.ENTER_DAT),MON_1214.DOC_DATE,MON_1214.ENTER_DAT)
      ENDIF
    ENDIF
  ENDIF
*-----------------------------------------    
  SELECT SAL_BOOK
  APPEND BLANK
*  s000 = ROUND(SALE_TIT.TOTAL_R*100/(100+SALE_TIT.NDS_),2)
  REPLACE DOC_NUM  WITH SALE_TIT.NEW_DOC,  ;
          DOC_DATE WITH SALE_TIT.DOC_DATE, ;
          CUS_NAME WITH c_nam,             ;
          INN      WITH i_cod,             ;
          PAY_DATE WITH p_dat,             ; 
          SUM_TOV  WITH SALE_TIT.TOTAL_0,  ;
          SUM_NDS  WITH SALE_TIT.TOTAL_R-SALE_TIT.TOTAL_0, ;
          SUM_H    WITH SALE_TIT.TOTAL_R   
  SELECT SALE_TIT
  
ENDSCAN

SELECT SAL_BOOK
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"За указанный период не найдено ни одного счета-фактуры!"
  mss(3) = ""
  DO Wt_Mess 
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

PRIVATE sb, p_ln, fnt, n_cp, lft, t_p, f_p, f_d, n, i, ffeed
DIMENSION sb(17,2)

*{Pgs}        - Страница/всего страниц
*{Dat1  }     - Начальная дата
*{Dat2  }     - Конечная дата
*{Firm_Name}  - Предприятие
*{T001      } - Сумма продаж без НДС на странице
*{T002      } - Сумма НДС на странице
*{T003      } - Сумма продаж с НДС на странице
*{T011      } - Сумма продаж без НДС по документу
*{T012      } - Сумма НДС по документу
*{T013      } - Сумма продаж с НДС по документу
*{TCheck0   } - Сумма по чекам без НДС по документу
*{TCheckN   } - Сумма по чекам НДС по документу
*{TCheck    } - Сумма по чекам с НДС по документу
*{P_INN     } - ИНН

sb(1,1) = "{Pgs}"
sb(1,2) = ""

sb(2,1) = "{Dat1  }"
sb(2,2) = TRANSFORM(dat_0,"@D ")

sb(3,1) = "{Dat2  }"
sb(3,2) = TRANSFORM(dat_1,"@D ")

sb(4,1) = "{T001      }"
sb(4,2) = "            "

sb(5,1) = "{T002      }"
sb(5,2) = "            "

sb(6,1) = "{T003      }"
sb(6,2) = "            "

sb(7,1) = "{T011      }"
sb(7,2) = "            "

sb(8,1) = "{T012      }"
sb(8,2) = "            "

sb(9,1) = "{T013      }"
sb(9,2) = "            "

sb(10,1) = "{Firm_Name}"

sb(11,1) = "{TCheck0   }"
sb(11,2) = STR(s_check0, 12, 2)

sb(12,1) = "{TCheckN   }"
sb(12,2) = STR(s_check-s_check0, 12, 2)

sb(13,1) = "{TCheck    }"
sb(13,2) = STR(s_check, 12, 2)

sb(14,1) = "{P_INN     }"

SELECT ACCOUNT
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE
SEEK f_c000

sb(15, 1) = "{Руководитель          }"
sb(15, 2) = BOSS
sb(16, 1) = "{Гл. бухгалтер         }"
sb(16, 2) = ABAK
sb(17, 1) = "{Кассир                }"
sb(17, 2) = KASSIR

sb(10,2) = L_NAME
sb(14,2) = INN

USE

SELECT CUS_BIL
USE

SELECT MON_1214
USE

SELECT SALE_TIT

USE (base_path+"DOC_FORM")
LOCATE FOR "SAL_BOOK" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

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
SELECT SAL_BOOK
GO TOP
DO Wt_Mess

PRIVATE s001, s002, s003, s101, s102, s103
PRIVATE n_pg, f_pg, n_det, j, stw, p_drctry

n_det = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
n_pg  = CEILING(RECCOUNT()/n_det)

STORE 0 TO s101, s102, s103
DO Ini_Prn WITH "Книга продаж", p_ln, lft, n_cp, fnt, ffeed, p_drctry
FOR f_pg = 1 TO n_pg
  IF f_pg # 1
    EJECT
  ENDIF
  STORE 0 TO s001, s002, s003
  sb(1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  FOR i = 1 TO ALEN(t_p)
    stw = t_p(i)
    FOR j = 1 TO ALEN(sb,1)
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  FOR i = 1 TO n_det
    stw = PADR(DTOC(DOC_DATE),11)+ ;
          PADR(LEFT(DOC_NUM,1)+"-"+ALLTRIM(SUBSTR(DOC_NUM,2)),11)+ ;
          PADL(CUS_NAME,38)+" "+INN+DTOC(PAY_DATE)+STR(SUM_H,13,2)+STR(SUM_TOV,13,2)+" 18.00"+STR(SUM_NDS,13,2)
    s001 = s001+SUM_TOV
    s002 = s002+SUM_NDS
    s003 = s003+SUM_H
    @ PROW()+1, 0 SAY stw
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  sb(4,2) = STR(s001, 12, 2)
  sb(5,2) = STR(s002, 12, 2)
  sb(6,2) = STR(s003, 12, 2)

  s101 = s101+s001
  s102 = s102+s002
  s103 = s103+s003

  IF f_pg # n_pg
    FOR i = 1 TO ALEN(f_p)
      stw = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        stw = STRTRAN(stw, sb(j,1), sb(j,2))
      ENDFOR
      @ PROW()+1, 0 SAY stw
    ENDFOR
  ENDIF
  IF EOF()
    EXIT
  ENDIF
ENDFOR

sb(7,2) = STR(s101, 12, 2)
sb(8,2) = STR(s102, 12, 2)
sb(9,2) = STR(s103, 12, 2)

FOR i = 1 TO ALEN(f_d)
  stw = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  @ PROW()+1, 0 SAY stw
ENDFOR

SELECT SAL_BOOK
USE
DO Term_Prn WITH "", tmpo_path+"SAL_BOOK.DBF"
CLOSE DATABASES

DELETE FILE (tmpo_path+"SAL_BOOK.DBF")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Fill_Bl      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Заполнение бланка.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/15/1999 ──┘
PROCEDURE Fill_Bl
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

*┌──────────────────────────────────────┐
*│   ┌─ Интервал дат ───────────────┐   │
*│   │  С ДД.ММ.ГГГГ по ДД.ММ.ГГГГ  │   │
*│   └──────────────────────────────┘   │
*│Предприятие .... ....:....!....:. (F1)│
*│   < OK Ctrl-W > < Отказаться Esc >   │
*└──────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 42, "Заполните бланк"
DO Sun_Bord WITH  2,  5,  4, 36, " Интервал дат "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  8 SAY "С" GET dat_0
@  3, 21 SAY "по" GET dat_1
DO Get_Firm WITH 5, 2, .T.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID T_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура T_Blank      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка корректности заполнения бланка.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/15/1999 ──┘
PROCEDURE T_Blank
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы забыли указать предприятие!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_0 < dat_v0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Имеем данные только с "+DTOC(dat_v0)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_1 > dat_v1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Сегодня только "+DTOC(dat_v1)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_1 < dat_0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы указали начальную дату больше конечной!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
