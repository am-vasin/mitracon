*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º                à®£à ¬¬  ¯¥à¥¤ ç¨ ¤ ­­ëå ¢ ä¨«¨ «.                    º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Send1
PARAMETER is_old

PRIVATE s_date, t_num, pthw, arr_w, n, i, m, fil_id, fil_cod
PRIVATE n_over, fil_nam, own_cod, w_pr, w_pr_r, mss

*IF .NOT. G_Exclu()      && ëâ ¥¬áï § å¢ â¨âì ¬®­®¯®«ì­ë© ¤®áâã¯ ª ¡ §¥
*  RETURN                &&   € € ”ˆƒ€?!!!
*ENDIF
n_over = 4

fil_id  = " "
fil_cod = 0
own_cod = 0
fil_nam = " "

DO Choose_F

IF EMPTY(fil_id)
  CLOSE DATABASES
  RETURN
ENDIF

IF is_old
  DO Repe_Rep WITH "S"+fil_id, "F_STAT"
  RETURN
ENDIF

DO Prep_TD    && ‘®§¤ ¥¬ ¨«¨ ®ç¨é ¥¬ âà ­§¨â­ãî ¤¨à¥ªâ®à¨î
pthw = tmpo_path+"transmit\"

s_date = {}
t_num  = 0
DO Parm_TD WITH "SF"+STR(fil_cod,8)

DO Wt_Mess WITH "¥à¥¤ ç  ¯« â¥¦¥ª ¨ ®âç¥â®¢"
DO S_Money WITH pthw, s_date   && ¥à¥¤ ç  ¤¥­¥£
DO Wt_Mess

DO Wt_Mess WITH "¥à¥¤ ç  á¯¨áª  ª«¨¥­â®¢"
DO S_Custs WITH pthw, s_date
DO Wt_Mess

DO Wt_Mess WITH "¥à¥¤ ç  á¯¨áª  à ¡®ç¨å ¬¥áâ"
USE (base_path+"ARM_LST")
COPY TO (pthw+"ARM_LST")
DO Wt_Mess

DO Wt_Mess WITH "¥à¥¤ ç  á¯¨áª  á®âàã¤­¨ª®¢"
USE (base_path+"PERSONS")
COPY TO (pthw+"PERSONS")
USE (pthw+"PERSONS")
REPLACE ALL SALARY WITH " "
DO Wt_Mess

IF TYPE("path_comm") = "C"
IF .NOT. EMPTY(path_comm)
  DO Wt_Mess WITH "” ©«ë ¤«ï ¯à®£à ¬¬ë MANAGER"
*  USE (path_comm+"SUBJECT")
*  COPY TO (pthw+"SUBJECT")
  COPY FILE (path_comm+"SUBJECT.DBF") TO (pthw+"SUBJECT.DBF")
*  USE (path_comm+"CORPUS")
*  COPY TO (pthw+"CORPUS")
  COPY FILE (path_comm+"CORPUS.DBF") TO (pthw+"CORPUS.DBF")
  DO Wt_Mess
ENDIF
ENDIF

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0) + "  ¥à¥¤ ¢ âì ­®¬¥­ª« âãàã? [ ¥â ][ „  ]"
mss(3) = ""

IF Out_Mess(5,"mss") = 2 &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

DO Wt_Mess WITH "¥à¥¤ ç  ­®¬¥­ª« âãàë"
USE (base_path+"DIVISION")
COPY TO (pthw+"DIVISION")
USE (base_path+"CLASSES")
COPY TO (pthw+"CLASSES")
USE (base_path+"DEVELOP")
COPY TO (pthw+"DEVELOP")
USE (base_path+"ANALOG")
COPY TO (pthw+"ANALOG")
USE (base_path+"STOCK")
COPY TO (pthw+"STOCK")
USE (base_path+"RYN_L") ORDER TAG C_CODE
SELECT 0
USE (pthw+"STOCK")
SET RELATION TO STR(fil_cod, 5)+STR(CODE,7) INTO RYN_L
SCAN
  w_pr = RetPrice(NAME, PRICE, EMPTY(STOP_PUR), IIF(EMPTY(CLASS), DIV_CODE, CLASS))
  w_pr_r = Clc_Rub(w_pr)
  IF FOUND("RYN_L")
    REPLACE RYN_L.PRICE_D WITH w_pr, ;
            RYN_L.PRICE_R WITH w_pr_r
    REPLACE QNT_STORE WITH QNT, ;
            QNT       WITH RYN_L.QNT, ;
            PRICE_B   WITH RYN_L.PRICE_D,  ;
            PRICE_B_R WITH RYN_L.PRICE_R
  ELSE
    REPLACE QNT_STORE WITH QNT,  ;
            QNT       WITH 0,    ;
            PRICE_B   WITH w_pr, ;
            PRICE_B_R WITH Clc_Rub(w_pr)
  ENDIF
ENDSCAN
USE
SELE RYN_L
USE
DO Wt_Mess

ENDIF &&&&&&&&&&&&&&&&&&&&&&&&

DO Wt_Mess WITH "¥à¥¤ ç  áç¥â®¢"
USE (base_path+"BILLS")
COPY FOR ALLTRIM(STATUS) = "à¥¤¢." .AND. LEFT(DOC_NUM,1) = fil_id TO (pthw+"BILLS")
DO Wt_Mess
USE

USE (base_path+"TR_LOG")
APPEND BLANK
REPLACE TR_CODE  WITH "SF",   ;
        TR_NUM   WITH t_num,  ;
        TR_DATE  WITH DATE(), ;
        TR_TIME  WITH TIME(), ;
        TR_START WITH s_date, ;
        ID_CORR  WITH STR(fil_cod,8), ;
        ID_SELF  WITH STR(own_cod,8)

COPY STRUCTURE TO (pthw+"ID_SEND")
SCATTER TO arr_w
USE (pthw+"ID_SEND")

*arr_w(6) = sys_char
APPEND FROM ARRAY arr_w
USE

DO Arch WITH pthw, "F_STAT"

PRIVATE out_path, nm_f, nm_f1
nm_f = "S"+fil_id+RIGHT(STR(100000+t_num,6),5)
n = ADIR(arr_w,pthw+"*.*")
FOR i = 1 TO n
  IF LEFT(arr_w(i,1),10) # "F_STAT.A")
    DELETE FILE (pthw+arr_w(i,1))
  ELSE
    nm_f1 = log_path+nm_f+RIGHT(arr_w(i,1),4)
    DELETE FILE (nm_f1)
    COPY FILE (pthw+arr_w(i,1)) TO (nm_f1)
  ENDIF
ENDFOR

out_path = "\"
out_path = Int_Dir(out_path, "Šã¤  ¯¨è¥¬ ¤ ­­ë¥?", 0)

IF .NOT. EMPTY(out_path)
  DO Copy_A WITH pthw+"F_STAT", out_path+"F_STAT"  && ‘®åà ­¥­¨¥  àå¨¢ 
ENDIF

*DO C_Exclu      && Žá¢®¡®¦¤ ¥¬ ¡ §ã

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º                à®£à ¬¬  ¯¥à¥¤ ç¨ ¤¥­¥£ ¢ ä¨«¨ «.                     º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE S_Money
PARAMETERS pth, sdt   && ¯ãâì ¤«ï à¥§ã«ìâ â  ¨ ­ ç. ¤ â 
PRIVATE arr_t, rc_w, i

USE (base_path+"KASSA") ORDER TAG DOC_NUM

COPY STRUCTURE TO (pth+"KASSA")
SELECT 0
USE (pth+"KASSA") ALIAS DETAIL

SELECT KASSA

FOR i = YEAR(DATE())-1 TO YEAR(DATE())

  SEEK STR(i,4)+fil_id

  SCAN REST WHILE YEAR(DOC_DATE) = i .AND. LEFT(DOC_NUM,1) = fil_id

    SCATTER TO arr_t
    SELECT DETAIL
    APPEND FROM ARRAY arr_t
    SELECT KASSA

  ENDSCAN

ENDFOR

USE (base_path+"INCMONEY") ORDER TAG HRON

COPY STRUCTURE TO (pth+"INCMONEY")
SELECT DETAIL
USE (pth+"INCMONEY") ALIAS DETAIL

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT INCMONEY
SET RELATION TO CUS_CODE INTO ACCOUNT

SEEK DTOS(sdt)
IF .NOT. FOUND()
  rc_w = RECNO(0)
ELSE
  rc_w = RECNO()
ENDIF

IF rc_w > 0 .AND. rc_w <= RECCOUNT()
  GO rc_w
  SCAN REST
    IF .NOT. FOUND("ACCOUNT")       &&
      LOOP                          &&
    ENDIF                           &&   â¨ ¯« â¥¦¨ ä¨«¨ «  
                                    &&
    IF ACCOUNT.SUBSYS # fil_id      &&         ­¥ ª á îâáï!
      LOOP                          &&
    ENDIF                           &&

    SCATTER TO arr_t
    SELECT DETAIL
    APPEND FROM ARRAY arr_t
    SELECT INCMONEY

  ENDSCAN

ENDIF

USE
SELECT DETAIL
USE
SELECT ACCOUNT
USE

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º                  à®£à ¬¬  ¯¥à¥¤ ç¨ á¯¨áª  ª«¨¥­â®¢.                  º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE S_Custs
PARAMETERS pth, sdt   && ¯ãâì ¤«ï à¥§ã«ìâ â  ¨ ­ ç. ¤ â 
PRIVATE arr_t, rc_w, i

USE (base_path+"ACCOUNT")
COPY FOR SUBSYS = fil_id TO (pth+"ACCOUNT")
SET ORDER TO TAG CUS_CODE
SELECT 0
USE (base_path+"CUS_BIL")
SET RELATION TO CUS_CODE INTO ACCOUNT
COPY FOR ACCOUNT.SUBSYS = fil_id TO (pth+"CUS_BIL")
USE
SELECT ACCOUNT
USE

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º                à®£à ¬¬  ¢ë¡®à  ä¨«¨ « .                              º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Choose_F
PRIVATE f_list, f_codes, f_chars, f_numb, n, mss

USE (base_path+"CUSTOMER")

f_numb = 0
SCAN FOR ISALPHA(RYN)
  f_numb = f_numb+1
  DIMENSION f_list(f_numb), f_codes(f_numb), f_chars(f_numb)
  f_list(f_numb)  = B_NAME
  f_codes(f_numb) = CODE
  f_chars(f_numb) = RYN
ENDSCAN
LOCATE FOR .NOT. EMPTY(SELF)
own_cod = CODE
USE

IF f_numb = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"‚ë ­¥ § ¢¥«¨ ­¨ ®¤­®£® ä¨«¨ « !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF f_numb = 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"”¨«¨ « "+ALLTRIM(f_list(1))+", ¯¥à¥¤ ¥¬? [ „  ] [ ¥â ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 1
    fil_id  = f_chars(1)
    fil_cod = f_codes(1)
    fil_nam = f_list(1)
  ENDIF
  RETURN
ENDIF

PRIVATE ln, wd, i, s, win_name

win_name = PROGRAM()
ln = MIN(WROWS("")-8, f_numb)
wd = LEN(f_list(1))+4

DO D_Win_N WITH ln+3, wd, "”¨«¨ «ë"

n = 1
DO WHILE .T.
  n = MIN(n, f_numb)
  n = MAX(n, 1)
  @ 1, 1 MENU f_list, f_numb, ln
  READ MENU TO n
  s = READKEY() % 256
  IF s = 12
    EXIT
  ENDIF
  IF s = 15
    fil_id  = f_chars(n)
    fil_cod = f_codes(n)
    fil_nam = f_list(n)
    EXIT
  ENDIF
ENDDO

RELEASE WINDOW (win_name)

RETURN
