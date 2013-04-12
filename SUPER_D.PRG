*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  ก๋แโเฎฃฎ เ แ็ฅโ  คฅซ์โ ฏฎ ญ ซจ็จ๎.               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Super_D
PARAMETERS dt_0, dt_1

PRIVATE stru_, dt_0w, dt_1w, win_name, cnt

*
*   ฏเฅคฅซ๏ฅฌ จญโฅเข ซ ค โ คซ๏ เ แ็ฅโ  คขจฆฅญจ๏
*
USE PARMS
dt_0w = IIF(TYPE("dt_0") = "D", dt_0, DAT_START)
dt_1w = IIF(TYPE("dt_1") = "D", dt_1, DATE())

*
*  ฏเฅคฅซ๏ฅฌ ฎชฎ่ชฎ คซ๏ ฏเฎโฎชฎซ 
*
win_name = PROGRAM()
DO D_Win_N WITH 20, 56, "เฎโฎชฎซ"
DEFINE WINDOW Internal FROM WLROW()+2,         WLCOL()+2  ;
                       TO   WLROW()+WROWS()-2, WLCOL()+WCOLS()-3  ;
                       NONE COLOR SCHEME 13
ACTIVATE WINDOW Internal

*
*  ‘ฎงค ฅฌ ขเฅฌฅญญ๋ฉ ไ ฉซ คฅซ์โ
*
USE STOCK ORDER TAG CODE

DIMENSION stru_(7,4)

stru_( 1, 1) = "CODE"
stru_( 1, 2) = "N"
stru_( 1, 3) = FSIZE("CODE","STOCK")
stru_( 1, 4) = 0

stru_( 2, 1) = "DLT_Q"
stru_( 2, 2) = "N"
stru_( 2, 3) = 10
stru_( 2, 4) = 0

stru_( 3, 1) = "DLT_R"
stru_( 3, 2) = "N"
stru_( 3, 3) = 10
stru_( 3, 4) = 0

stru_( 4, 1) = "DLT_F"
stru_( 4, 2) = "N"
stru_( 4, 3) = 10
stru_( 4, 4) = 0

stru_( 5, 1) = "REST_Q"
stru_( 5, 2) = "N"
stru_( 5, 3) = 10
stru_( 5, 4) = 0

stru_( 6, 1) = "REST_R"
stru_( 6, 2) = "N"
stru_( 6, 3) = 10
stru_( 6, 4) = 0

stru_( 7, 1) = "REST_F"
stru_( 7, 2) = "N"
stru_( 7, 3) = 10
stru_( 7, 4) = 0

SELECT 0
SET EXCLU ON
DELETE FILE (tmpo_path+"TMP_STO.DBF")
CREATE DBF (tmpo_path+"TMP_STO") FROM ARRAY stru_
USE (tmpo_path+"TMP_STO")
SET EXCLUSIVE OFF

cnt = 0
?? " ็ ซจ ไฎเฌจเฎข ญจฅ กซ ญช . เฅฌ๏ "+TIME()
? "ฎงจๆจ๏ "+STR(cnt,6)
SELECT STOCK
SCAN
  SELECT TMP_STO
  APPEND BLANK
  REPLACE CODE WITH STOCK.CODE
  SELECT STOCK
  cnt = cnt+1
  @ ROW(), COL()-6 SAY STR(cnt,6)
ENDSCAN
USE

DO Super_N

DO Super_F

DO Super_I

DO Clc_Rest

? "       ‘…!   เฅฌ๏ "+TIME()
= INKEY(15,"HM")

RELEASE WINDOW (win_name)
RELEASE WINDOW Internal
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  ใ็ฅโ  ญ ชซ คญ๋ๅ.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Super_N
PRIVATE q1, q2, q3, cd, rc_sv

*
*     โชเ๋ข ฅฌ ก ง๋ ค ญญ๋ๅ
*
SELECT 0
USE SALE_TIT ORDER TAG DOC_NUM

SELECT 0
USE SALE ORDER TAG CODE
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO SALE_TIT

SELECT TMP_STO
GO TOP

SELECT SALE

cnt = 0
? " ็ ซจ ฏเฎแฌฎโเ ญ ชซ คญ๋ๅ. เฅฌ๏ "+TIME()
? "ฎงจๆจ๏ "+STR(cnt,6)

STORE 0 TO q1, q2, q3, cd
SCAN FOR BETWEEN(DOC_DATE, dt_0w, dt_1w)
  IF cd # CODE
    SELECT TMP_STO
    IF cd # 0
      REPLACE DLT_Q WITH q1,  ;
              DLT_R WITH q2,  ;
              DLT_F WITH q3
      STORE 0 TO q1, q2, q3
    ENDIF
    SCAN REST WHILE CODE < SALE.CODE
    ENDSCAN
    rc_sw = RECNO()
    cnt = cnt+1
    IF CODE # SALE.CODE
      ? "่จกช !"
      ? "ฎงจๆจ๏ "+STR(cnt,6)
      GO rc_sw
      SELECT SALE
      LOOP
    ENDIF
    @ ROW(), COL()-6 SAY STR(cnt,6)
    SELECT SALE
    cd = CODE
  ENDIF
  IF SALE_TIT.CUS_TYPE = 4 .AND. EMPTY(SALE_TIT.LIST_NUM)
    q3 = q3+QNT
  ELSE
    q1 = q1+QNT
  ENDIF
  q2 = q2+QNT_REP

ENDSCAN

USE
SELECT SALE_TIT
USE

RETURN      

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎฃเ ฌฌ  ฏฅเฅค ็ ข ไจซจ ซ๋.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Super_F
PRIVATE q1, cd, rc_sv

SELECT TMP_STO
GO TOP

SELECT 0
USE RYN_M ORDER TAG CODE

cnt = 0

STORE 0 TO q1, cd
? " ็ ซจ ฏเฎแฌฎโเ ไจซจ ซฎข. เฅฌ๏ "+TIME()
? "ฎงจๆจ๏ "+STR(cnt,6)

SCAN FOR BETWEEN(DOC_DATE, dt_0w, dt_1w)
  IF cd # CODE
    SELECT TMP_STO
    IF cd # 0
      REPLACE DLT_Q WITH DLT_Q+q1,  ;
              DLT_F WITH DLT_F-q1
      STORE 0 TO q1
    ENDIF
    SCAN REST WHILE CODE < RYN_M.CODE
    ENDSCAN
    rc_sw = RECNO()
    cnt = cnt+1
    IF CODE # RYN_M.CODE
      ? "่จกช !"
      ? "ฎงจๆจ๏ "+STR(cnt,6)
      GO rc_sw
      SELECT RYN_M
      LOOP
    ENDIF
    @ ROW(), COL()-6 SAY STR(cnt,6)
    SELECT RYN_M
    cd = CODE
  ENDIF
  q1 = q1+QNT

ENDSCAN

USE

RETURN      

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎฃเ ฌฌ  ฏฅเฅค ็ ข ไจซจ ซ๋.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Super_I
PRIVATE q1, cd, rc_sv

SELECT TMP_STO
GO TOP

SELECT 0
USE ACT_S ORDER TAG CODE

cnt = 0

STORE 0 TO q1, cd
? " ็ ซจ ฏเฎแฌฎโเ ฏเจๅฎค . เฅฌ๏ "+TIME()
? "ฎงจๆจ๏ "+STR(cnt,6)

SCAN FOR BETWEEN(DOC_DATE, dt_0w, dt_1w)
  IF cd # CODE
    SELECT TMP_STO
    IF cd # 0
      REPLACE DLT_Q WITH DLT_Q-q1
      STORE 0 TO q1
    ENDIF
    SCAN REST WHILE CODE < ACT_S.CODE
    ENDSCAN
    rc_sw = RECNO()
    cnt = cnt+1
    IF CODE # ACT_S.CODE
      ? "่จกช !"
      ? "ฎงจๆจ๏ "+STR(cnt,6)
      GO rc_sw
      SELECT ACT_S
      LOOP
    ENDIF
    @ ROW(), COL()-6 SAY STR(cnt,6)
    SELECT ACT_S
    cd = CODE
  ENDIF
  q1 = q1+QNT_INC

ENDSCAN

USE

RETURN      

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎฃเ ฌฌ  เ แ็ฅโ  ฎแโ โชฎข.                           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Clc_Rest

SELECT 0
USE STOCK ORDER TAG CODE

SELECT TMP_STO
SET RELATION TO CODE INTO STOCK

? " แ็ฅโ ฎแโ โชฎข. เฅฌ๏ "+TIME()

REPLACE ALL REST_Q WITH STOCK.QNT_START-DLT_Q,  ;
            REST_R WITH STOCK.QNT_S_REP-DLT_R,  ;
            REST_F WITH STOCK.QNT_S_RYN-DLT_F

? "ฎคแ็ฅโ คฅไฅชโญ๋ๅ ฏฎงจๆจฉ. เฅฌ๏ "+TIME()
COUNT FOR REST_Q # STOCK.QNT+STOCK.SAVE .OR. ;
          REST_R # STOCK.QNT_REP .OR. ;
          REST_F # STOCK.QNT_REAL     TO cnt

? "ฅไฅชโญ๋ๅ ฏฎงจๆจฉ "+STR(cnt,6)+" เฅฌ๏ "+TIME()
SELECT STOCK 
USE
RETURN
