*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ฏฅ็ โจ แฏจแช .                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE P_Lst
PARAMETERS l_num, is_alt
*PARAMETERS ext_menu

PRIVATE p_ln, fnt, n_cp, lft, n, i, t_p, f_p, f_d, p_drctry
PRIVATE sel_rem, f_name, nn, ffeed
*PRIVATE is_usd

*is_usd = Ask_Vlt()
sel_rem = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM AGAIN ALIAS LT9831
f_name = SYS(3)
DO Wt_Mess WITH "ฎคฃฎโฎขช  แฏจแช "
DO Prep_Lst WITH f_name
DO Wt_Mess

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "LIST" == ALLTRIM(UPPER(DOC_NAME))
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
PRIVATE sub_1, sub_2, sub_3, sub_4, sub_5, sub_6, sub_7, sub_8, j

sum_d = 0
n_p_det = p_ln-ALEN(t_p)-ALEN(f_p)-ALEN(f_d)-1
hol_p = CEILING(RECCOUNT()/n_p_det)

sub_1 = ALLTRIM(STR(LT9831.LST_NUM,6))
sub_2 = TRANSFORM(DATE(),"@D")
sub_5 = ALLTRIM(LT9831.LST_NAME)

DO Ini_Prn WITH "‘ฏจแฎช N "+sub_1, p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
nn = 0

FOR cur_p = 1 TO hol_p
  sub_3 = ALLTRIM(STR(cur_p))+"/"+ALLTRIM(STR(hol_p))
  @ PROW(), PCOL() SAY Subst_H(t_p(1))
  FOR i = 2 TO ALEN(t_p)
    @ PROW()+1, 0 SAY Subst_H(t_p(i))
  ENDFOR
  sum_p = 0
  FOR i = 1 TO n_p_det
    nn = nn+1
    @ PROW()+1,0 SAY STR(nn,4)+" "+PREFIX+" "+NAME+" "+PRODUCER+" "+STR(QNT,6)+" "+STR(PRC,10,4)+" "+STR(SUM_T,10,4)
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
     .NOT. LT9831.IS_USD .AND. is_alt, "“.….", "ใก."))

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
PRIVATE str_w

str_w = st

str_w = STRTRAN(str_w,"{Sum_H   }", sub_5)
str_w = STRTRAN(str_w,"{Sum_H_N }", sub_6)
str_w = STRTRAN(str_w,"{Sum_NSP }", sub_8)
str_w = STRTRAN(str_w,"{%NS}", sub_7)

RETURN str_w


*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ แฎคฅเฆจฌฎฃฎ แฏจแช .                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prep_Lst
PARAMETERS f_nam

PRIVATE sel_rem, n001, prc0

sel_rem = SELECT()
SELECT 0
CREATE DBF (tmpo_path+f_nam) (PREFIX   C( 4), ;
                              NAME     C(35), ;
                              PRODUCER C( 8), ;
                              QNT      N( 6), ;
                              PRC      N(10,4), ;
                              SUM_T    N(10,4))
USE (tmpo_path+f_nam) ALIAS T_LST0 EXCLUSIVE

n001 = STR(LT9831.LST_NUM,6)
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_NAME ALIAS L000 AGAIN
= SEEK(n001)
SCAN REST WHILE LIST_NUM = n001 FOR .NOT. EMPTY(QNT)
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
          QNT      WITH L000.QNT,  ;
          PRC      WITH prc0,      ;
          SUM_T    WITH prc0*L000.QNT
  SELECT L000
ENDSCAN
SELECT L000
USE

SELECT (sel_rem)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  ญโฅเ ชโจขญ๋ฉ ข๋กฎเ ข ซ๎โ๋.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Ask_Vlt

*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*

PRIVATE stat_type     && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
                                        1 - กซ ญช;
                                        2 - BROWSE - โ กซจๆ ;
                                        3 - BROWSE - แฏจแฎช.
PRIVATE what_do       && ฌ๏ เฅฆจฌ .
PRIVATE menu_name     && ฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎.
PRIVATE last_mouse    && เฅฌ๏ ฏฎแซฅคญฅฃฎ ญ ฆ โจ๏ งขฅเจญฎฉ ชญฎฏชจ.
PRIVATE win_name      && ฌ๏ ฎชญ  ( ฎชฎญ คซ๏ BROWSE ).

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ                     ณ
*ณ  < ใกซจ > < USD >  ณ
*ณ                     ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 6, 25, "“ช ฆจโฅ ข ซ๎โใ"

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 3, 4 GET ex PICTURE "@*HT \ ใกซจ ;\ USD "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 2
