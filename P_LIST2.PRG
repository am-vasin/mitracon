*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                   Программа печати списка.                            ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE P_List2
PARAMETERS l_num, is_alt, ord_part
*PARAMETERS ext_menu
PRIVATE p_ln, fnt, n_cp, lft, n, i, t_p, f_p, f_d, p_drctry
PRIVATE sel_rem, f_name, nn, ffeed, is_cor, sp_pr, to_calc, bound
*PRIVATE is_usd
is_cor = .F.
*is_usd = Ask_Vlt()
bound = Get_Bound()
IF bound < 0
  RETURN
ENDIF
sel_rem = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM AGAIN ALIAS LT9831
SEEK l_num
to_calc = .NOT. IS_USD

f_name = SYS(3)
DO Wt_Mess WITH "Подготовка списка"
DO Prep_Lst WITH f_name
DO Wt_Mess

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "LIST_B" == ALLTRIM(UPPER(DOC_NAME))
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

SELECT T_LST0
PRIVATE n_p_det, sum_d, sum_p, cur_p, hol_p
PRIVATE sub_1, sub_2, sub_3, sub_4, sub_5, sub_6, sub_7, sub_8, sub_9, j
sum_d = 0
n_p_det = p_ln-ALEN(t_p)-ALEN(f_p)-ALEN(f_d)-1
hol_p = CEILING(RECCOUNT()/n_p_det)

sub_1 = ALLTRIM(STR(LT9831.LST_NUM,6))
sub_2 = TRANSFORM(DATE(),"@D")
sub_5 = ALLTRIM(LT9831.LST_NAME)

DO Ini_Prn WITH "Список N "+sub_1, p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
nn = 0
*-------------------------------
PRIVATE str_w, s_sav
s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF user = 0
  sub_9 = "САМ!"
ELSE
  IF SEEK (user)
    sub_9 = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
  ELSE
    sub_9 = "?!!"
  ENDIF
ENDIF         
*sb(8,2) = PADR(sb(8,2),LEN(sb(8,1)))
USE
SELECT (s_sav)
*-------------------------------

FOR cur_p = 1 TO hol_p
  sub_3 = ALLTRIM(STR(cur_p))+"/"+ALLTRIM(STR(hol_p))
  @ PROW(), PCOL() SAY Subst_H(t_p(1))
  FOR i = 2 TO ALEN(t_p)
    @ PROW()+1, 0 SAY Subst_H(t_p(i))
  ENDFOR
  sum_p = 0
  FOR i = 1 TO n_p_det
    sp_pr = ""
    IF FSIZE("NOTE") # 0
      sp_pr = IIF(EMPTY(NOTE), "", "  √")
    ENDIF
    nn = nn+1
    @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+  ;
         IIF(FSIZE("S_ROUND")=0, "", " "+STR(S_ROUND,5))+" "+ ;
         STR(QNT,6)+MARK+" "+STR(PRC,10,4)+" "+STR(SUM_T,10,4)+sp_pr
    sum_p = sum_p+T_LST0.SUM_T
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
*  USE
  sum_d = sum_d+sum_p
  sub_4 = STR(sum_p,10,4)
  FOR i = 1 TO ALEN(f_p)
    @ PROW()+1, 0 SAY Subst_F1(f_p(i))
  ENDFOR
  IF cur_p # hol_p
    EJECT
  ENDIF
ENDFOR
sub_5 = STR(sum_d,10,4)
sub_7 = STR(GetNalog(LT9831.FIRM),5,1)
sub_6 = ROUND(sum_d*(100+GetNalog(LT9831.FIRM))/100, 4)
sub_8 = STR(sub_6-sum_d,10,4)
sub_6 = STR(sub_6,10,4)
FOR i = 1 TO ALEN(f_d)
  @ PROW()+1, 0 SAY Subst_F2(f_d(i))
ENDFOR
USE
DO Term_Prn WITH "", tmpo_path+f_name+".DBF", LT9831.CLI_CODE
DELETE FILE (tmpo_path+f_name+".DBF")
SELECT LT9831
USE
SELECT (sel_rem)
RETURN
**************************************************************
PROCEDURE Subst_H
PARAMETER st
PRIVATE str_w

str_w = st

str_w = STRTRAN(str_w,"{Dc_N}", sub_1)
str_w = STRTRAN(str_w,"{Doc_Date}", sub_2)
str_w = STRTRAN(str_w,"{Page1}", sub_3)
str_w = STRTRAN(str_w,"{List_Name}", sub_5)
str_w = STRTRAN(str_w,"{Valuta}", ;
IIF(LT9831.IS_USD .AND. .NOT. is_alt .OR.  ;
     .NOT. LT9831.IS_USD .AND. is_alt, "У.Е.", "Руб."))

RETURN str_w

**************************************************************
PROCEDURE Subst_F1
PARAMETER st
PRIVATE str_w

str_w = st

str_w = STRTRAN(str_w,"{Sum_2   }", sub_4)

RETURN str_w

**************************************************************
PROCEDURE Subst_F2
PARAMETER st
str_w = st

str_w = STRTRAN(str_w,"{Sum_H   }", sub_5)
str_w = STRTRAN(str_w,"{Sum_H_N }", sub_6)
str_w = STRTRAN(str_w,"{Sum_NSP }", sub_8)
str_w = STRTRAN(str_w,"{%NS}", sub_7)
str_w = STRTRAN(str_w,"{Autor                       }",sub_9)

RETURN str_w

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа подготовки содержимого списка.                 ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Prep_Lst
PARAMETERS f_nam

PRIVATE sel_rem, n001, prc0

sel_rem = SELECT()
SELECT 0
IF sys_char = "A"
  CREATE DBF (tmpo_path+f_nam) (PREFIX   C( 4), ;
                                NAME     C(35), ;
                                PRODUCER C( 8), ;
                                QNT      N( 6), ;
                                MARK     C( 1), ;
                                PACK     N( 6), ;
                                S_ROUND  N( 6), ;
                                CORPUS   C(16), ;
                                PRC      N(10,4), ;
                                SUM_T    N(10,4), ;
                                NOTE     C(10))
ELSE
  CREATE DBF (tmpo_path+f_nam) (PREFIX   C( 4), ;
                                NAME     C(35), ;
                                PRODUCER C( 8), ;
                                QNT      N( 6), ;
                                MARK     C( 1), ;
                                CORPUS   C(16), ;
                                PRC      N(10,4), ;
                                SUM_T    N(10,4), ;
                                NOTE     C(10))
ENDIF
USE (tmpo_path+f_nam) ALIAS T_LST0 EXCLUSIVE

n001 = STR(LT9831.LST_NUM,6)
SELECT 0
is_cor = File_O(path_comm+"CORPUS.DBF") .AND. File_O(path_comm+"SUBJECT.DBF")
IF is_cor
  USE (path_comm+"CORPUS") ORDER TAG CODE ALIAS C2_0309 AGAIN
  SELECT 0
  USE (path_comm+"SUBJECT") ORDER TAG STO_CODE ALIAS C1_0309 AGAIN
  SET RELATION TO COR_CODE INTO C2_0309
  SELECT 0
ENDIF
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S000 AGAIN
IF is_cor
  SET RELATION TO CODE INTO C1_0309
ENDIF
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_NAME ALIAS L000 AGAIN
SET RELATION TO CODE INTO S000
IF to_calc
  SET FILTER TO SAL_PRICE*MIN(QNT, S000.QNT)/kurs > bound
ELSE
  SET FILTER TO USD_PRICE*MIN(QNT, S000.QNT) > bound
ENDIF
IF TYPE("ord_part") = "N"
  SET FILTER TO QNT > S000.QNT*ord_part
ENDIF
= SEEK(n001)
SCAN REST WHILE LIST_NUM = n001 FOR .NOT. EMPTY(MAX(QNT, S000.QNT))
  SELECT T_LST0
  IF LT9831.IS_USD
    prc0 = L000.USD_PRICE
    IF is_alt
      prc0 = ROUND(Clc_Rub(prc0),2)
    ENDIF
  ELSE
    prc0 = L000.SAL_PRICE
    IF is_alt
      prc0 = ROUND(prc0/kurs,4)
    ENDIF
  ENDIF
  APPEND BLANK
  REPLACE PREFIX   WITH L000.PREFIX, ;
          NAME     WITH L000.NAME, ;
          PRODUCER WITH L000.PRODUCER, ;
          QNT      WITH MIN(L000.QNT, S000.QNT),  ;
          MARK     WITH IIF(L000.QNT >= S000.QNT, "√", " "), ;
          PRC      WITH prc0,      ;
          CORPUS   WITH IIF(is_cor, C2_0309.NAME, ""), ;
          SUM_T    WITH prc0*L000.QNT
  IF FSIZE("PACK") # 0
    REPLACE PACK    WITH MAX(S000.QNT_ROUND,1), ;
            S_ROUND WITH MAX(S000.QNT_PACK,1)
  ENDIF
    IF is_cor .AND. FSIZE("NOTE") # 0
    IF .NOT. EMPTY(C1_0309.NODISC)
      REPLACE NOTE WITH "Спец. цена"
    ENDIF
  ENDIF
SELECT L000
ENDSCAN
SELECT L000
USE
SELECT S000
USE
IF is_cor
  SELECT C1_0309
  USE
  SELECT C2_0309
  USE
ENDIF
SELECT (sel_rem)

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                  Интерактивный выбор валюты.                          ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Ask_Vlt

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

*┌─────────────────────┐
*│                     │
*│  < Рубли > < USD >  │
*│                     │
*└─────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 25, "Укажите валюту"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 4 GET ex PICTURE "@*HT \ Рубли ;\ USD "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 2

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Bound    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                              Воод границы                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.09.2001 ──┘
PROCEDURE Get_Bound

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
* Укажите сумму
*┌──────────────────────────────────┐
*│        Сумма 99 999.9999         │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, s000
s000 = 0
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 5, 38, "Укажите сумму"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 10 SAY "Сумма" GET s000 PICTURE "99 999.9999"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex # 1
  s000 = -1  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN s000
