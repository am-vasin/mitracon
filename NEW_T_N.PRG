*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                ฎข ๏ ฏเฎขฅเช  ก ซ ญแ  ฏฎ ญ ซจ็จ๎.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE New_T_N
PARAMETER vis

PRIVATE cnt, d_s, err, ab, sch, q_s_all, q_s_fil, q_s_rep, q_i_all, ;
        q_i_rep, q_e_fil, q_b, q_f, qs_n, qs_m, is_data, q0, q0_r, log_rec, err_cnt

log_rec = Use_Log("เฎขฅเช  ญ ซจ็จ๏")

USE (base_path+"PARMS")
sch = SCHEME(13,1)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,4)+","+ ;
      SCHEME(13,5)+","+ ;
      SCHEME(13,6)+","+ ;
      SCHEME(13,7)+","+ ;
      SCHEME(13,8)+","+ ;
      SCHEME(13,9)
d_s = DAT_START

IF FILE_O(log_path+"N_T_N0.DBF")   && เฎขฅเช  ก๋ซ  ฏเฅเข ญ , ฏเฎคฎซฆ ฅฌ...
  USE (log_path+"N_T_N0") EXCLUSIVE
  GO BOTTOM
  cnt = NUMB
ELSE                               &&  ็จญ ฅฌ แ ญ ็ ซ ...
  CREATE DBF (log_path+"N_T_N0")  ;
     (  CODE    N( 7),  ;
        NUMB    N( 7),  ;
        PREFIX  C( 4),  ;
        NAME    C(35),  ;
        NAL_S   C( 4),  ;
        NAL_R   C( 4),  ;
        MOV_F   C( 4),  ;
        NAL_F   C( 4),  ;
        NAL_B   C( 4),  ;
        NAL_SP  C( 4),  ;
        MOV_SP  C( 4)   )
  USE (log_path+"N_T_N0") EXCLUSIVE
  cnt = 0
  APPEND BLANK
ENDIF

*
*   โชเ๋ข ฅฌ ไ ฉซ๋, ชฎโฎเ๋ฅ ญใฆญ๋ ฏเจ ฏเฎขฅเชฅ ญ ซจ็จ๏
*

SELECT 0
USE (base_path+"SALE") ORDER TAG CODE
SET FILTER TO DOC_DATE >= d_s .AND. LEFT(DOC_NUM,1) = sys_char

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM
SET FILTER TO LEFT(DOC_NUM,1) = sys_char .AND. DOC_DATE >= d_s

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE

SELECT 0
USE (base_path+"RYN_M") ORDER TAG CODE
SET FILTER TO DOC_DATE >= d_s

SELECT 0
USE (base_path+"RYN_L") ORDER TAG CODE

SELECT 0
USE (base_path+"LET_SPEC") ORDER TAG CODE

SELECT 0
USE (base_path+"SPEC_SUM") ORDER TAG POS_CUS

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG CODE

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM

SELECT 0
USE (base_path+"STOCK")
IF cnt # 0
  GO cnt
ENDIF

SELECT N_T_N0
GO TOP

PRIVATE ln, wd, tt, ft, w1, w2, w3, w4, w5, w6, w7
IF vis
  DIMENSION tt(3), ft(1)
  tt(1) = ;
  "ฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤยฤฤฤฤยฤฤฤฤยฤฤฤฤยฤฤฤฤยฤฤฤฤยฤฤฤฤ"
  tt(2) = ;
  " N/ฏฎเ ณ   ฎงจๆจ๏                    ณ ซ.ณฅ ซณ”ซ.ณ”ซ.ณเ. ณ‘ฏ.ณ‘ฏ."
  tt(3) = ;
  "ฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤ"
  ft(1) = ;
  "ฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤมฤฤฤฤมฤฤฤฤมฤฤฤฤมฤฤฤฤมฤฤฤฤมฤฤฤฤ"

  ln = WROWS("")-10
  wd = LEN(ft(1))
  DO Def_Win WITH "Tst_N_M", "Tst_N_E", "Tst_N_I", ln, wd, ;
                  "เฎขฅเช  ญ ซจ็จ๏", tt, ft

  @ 1, 1 SAY "ฺ"
  @ 2, 1 SAY "ณ"
  @ 3, 1 SAY "ร"
  @ WROWS()-1, 1 SAY "ภ"

  @ 1, WCOLS()-2 SAY "ฟ"
  @ 2, WCOLS()-2 SAY "ณ"
  @ 3, WCOLS()-2 SAY "ด"
  @ WROWS()-1, WCOLS()-2 SAY "ู"

  ACTIVATE WINDOW Tst_N_E
  BROWSE FIELDS NUMB:H="",   ;
                f001 = PREFIX+" "+Nm_Part(NAME):30:H="",   ;
                NAL_S:H="",  ;
                NAL_R:H="",  ;
                MOV_F:H="",  ;
                NAL_F:H="",  ;
                NAL_B:H="",  ;
                NAL_SP:H="", ;
                MOV_SP:H=""  ;
                NOWAIT WINDOW Tst_N_I IN WINDOW Tst_N_E COLOR (sch)

ENDIF
ON KEY ab = .T.
ab = .F.
GO BOTTOM
DO WHILE .NOT. EOF("STOCK")
  err = .F.
  q_s_all  = 0    && ก้จฅ ฏเฎค ฆจ
  q_s_fil  = 0    && เฎค ฆจ ไจซจ ซใ
  q_s_rep  = 0    && เฎค ฆจ ฏฎงจๆจฉ, ขง๏โ๋ๅ ญ  เฅ ซจง ๆจ๎
  q_i_all  = 0    && ก้จฅ ฏเจๅฎค
  q_i_rep  = 0    && เจๅฎค ฏฎงจๆจฉ, ขง๏โ๋ๅ ญ  เฅ ซจง ๆจ๎
  q_e_fil  = 0    && โฏใแช ข ไจซจ ซ
  q_b      = 0    &&  ซจ็จฅ ข ไจซจ ซ ๅ
  q_f      = 0    && เฎญ์
  q0       = 0    &&  ซจ็จฅ ฏฎ ฏ เโจ๏ฌ
  q0_r     = 0    &&   เฅ ซจง ๆจจ ฏฎ ฏ เโจ๏ฌ
  qs_n     = 0    && ‘ฏฅๆคขจฆฅญจฅ (ญ ซจ็จฅ)
  qs_m     = 0    && ‘ฏฅๆคขจฆฅญจฅ (คขจฆฅญจฅ)

  IF vis
    REPLACE CODE    WITH STOCK.CODE,    ;
            NUMB    WITH RECNO("STOCK"),;
            PREFIX  WITH STOCK.PREFIX,  ;
            NAME    WITH STOCK.NAME,    ;
            NAL_S   WITH " ",  ;
            NAL_R   WITH " ",  ;
            MOV_F   WITH " ",  ;
            NAL_F   WITH " ",  ;
            NAL_SP  WITH " ",  ;
            MOV_SP  WITH " ",  ;
            NAL_B   WITH " "
  ENDIF
  IF ab
    EXIT
  ENDIF
  IF vis
    SHOW WINDOW N_T_N0 IN WINDOW Tst_N_E SAME REFRESH
  ENDIF
  DO Cnt_Sale     && เฎค ฆจ ( ฏเฎแโฎ จ ไจซจ ซใ )
  DO Cnt_Inc      && เจๅฎค
  DO Cnt_Spec     && “โขฅเฆคฅญจ๏
  IF STOCK.QNT_START+STOCK.QNT_S_RYN+q_i_all-q_s_all = ;
     STOCK.QNT+STOCK.QNT_REAL+STOCK.QNT_SAVE+STOCK.QNT_REZ .AND.     ;
     STOCK.QNT+STOCK.QNT_REAL+STOCK.QNT_SAVE+STOCK.QNT_REZ = q0
    w1 = " OK "
  ELSE
    err = .T.
    w1 = " ๛  "
  ENDIF
  IF STOCK.QNT_S_REP+q_i_rep-q_s_rep = STOCK.QNT_REP .AND.  ;
     STOCK.QNT_REP = q0_r
    w2 = " OK "
  ELSE
    err = .T.
    w2 = " ๛  "
  ENDIF
  IF STOCK.QNT_S_SPEC+qs_m = STOCK.QNT_SPEC
    w7 = " OK "
  ELSE
    err = .T.
    w7 = " ๛  "
  ENDIF
  SELECT N_T_N0
  IF vis
    REPLACE NAL_S WITH w1,  ;
            NAL_R WITH w2
    SHOW WINDOW N_T_N0 IN WINDOW Tst_N_E SAME REFRESH
  ENDIF

  DO Cnt_Fil      && ขจฆฅญจฅ ฏฎ ไจซจ ซใ
  IF STOCK.QNT_S_RYN+q_e_fil-q_s_fil = ;
     STOCK.QNT_REAL
    w3 = " OK "
  ELSE
    err = .T.
    w3 = " ๛  "
  ENDIF
  IF vis
    SELECT N_T_N0
    REPLACE MOV_F WITH w3
    SHOW WINDOW N_T_N0 IN WINDOW Tst_N_E SAME REFRESH
  ENDIF
  DO Cnt_B_O      && ”จซจ ซ๋ จ กเฎญ์ แโ เฎฃฎ โจฏ 
  DO Cnt_B_N      && เฎญ์ ญฎขฎฃฎ โจฏ 
  DO Cnt_Sp_N     && ‘ฏฅๆญ ซจ็จฅ
  IF STOCK.QNT_REAL = q_f
    w4 = " OK "
  ELSE
    err = .T.
    w4 = " ๛  "
  ENDIF
  IF STOCK.QNT_SAVE = q_b
    w5 = " OK "
  ELSE
    err = .T.
    w5 = " ๛  "
  ENDIF
  IF STOCK.QNT_SPEC = qs_n
    w6 = " OK "
  ELSE
    err = .T.
    w6 = " ๛  "
  ENDIF
  SELECT N_T_N0
  IF vis
    REPLACE NAL_F  WITH w4,  ;
            NAL_B  WITH w5,  ;
            NAL_SP WITH w6,  ;
            MOV_SP WITH w7
    SHOW WINDOW N_T_N0 IN WINDOW Tst_N_E SAME REFRESH
  ENDIF
  IF err
    IF .NOT. vis
      REPLACE CODE    WITH STOCK.CODE,    ;
              NUMB    WITH RECNO("STOCK"),;
              PREFIX  WITH STOCK.PREFIX,  ;
              NAME    WITH STOCK.NAME,    ;
              NAL_S   WITH w1,  ;
              NAL_R   WITH w2,  ;
              MOV_F   WITH w3,  ;
              NAL_F   WITH w4,  ;
              NAL_B   WITH w5,  ;
              NAL_SP  WITH w6,  ;
              MOV_SP  WITH w7
    ENDIF
    APPEND BLANK
  ENDIF
  SKIP IN STOCK
ENDDO
ON KEY
IF vis
  = INKEY("M")
ENDIF
is_data = .F.
IF EOF("STOCK")
  is_data = .T.
  DELETE
  DELETE FILE (log_path+"N_T_N.DBF")
  COPY TO (log_path+"N_T_N.DBF") FOR .NOT. DELETED()
  USE (log_path+"N_T_N.DBF")
  err_cnt = RECCOUNT()
  USE
  DO Term_Log WITH log_rec, err_cnt
  DELETE FILE (log_path+"N_T_N0.DBF")
ENDIF
CLOSE DATABASES
IF vis
  RELEASE WINDOW Tst_N_M
  RELEASE WINDOW Tst_N_E
  RELEASE WINDOW Tst_N_I
ENDIF

RETURN is_data

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                          ฎคแ็ฅโ ฏเฎค ฆ.                              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Sale

SELECT SALE
SEEK STOCK.CODE
SCAN REST WHILE CODE = STOCK.CODE FOR DOC_DATE >= d_s
  q_s_all = q_s_all+QNT        && ก้จฅ ฏเฎค ฆจ
  q_s_rep = q_s_rep+QNT_REP    && เฎค ฆจ ฏฎงจๆจฉ, ขง๏โ๋ๅ ญ  เฅ ซจง ๆจ๎
  IF SEEK(FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM,"SALE_TIT")
    IF EMPTY(SALE_TIT.LIST_NUM)
      q_s_fil = q_s_fil+QNT    && เฎค ฆจ ไจซจ ซใ
    ENDIF
    IF SALE_TIT.CUS_CODE < 0
      qs_m = qs_m + QNT
    ENDIF
  ENDIF
ENDSCAN
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                       ฎคแ็ฅโ ใโขฅเฆคฅญจฉ.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Spec

SELECT Let_Spec
SEEK STOCK.CODE
SCAN REST WHILE CODE = STOCK.CODE FOR DOC_DATE >= d_s
  qs_m = qs_m-QNT
ENDSCAN
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  ฎคแ็ฅโ ฏฎแโใฏซฅญจ๏ โฎข เ .                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Inc

SELECT ACT_S
SEEK STOCK.CODE
SCAN REST WHILE CODE = STOCK.CODE
  IF DOC_DATE >= d_s
    q_i_all  = q_i_all+QNT_INC    && ก้จฅ ฏเจๅฎค
    q_i_rep  = q_i_rep+QNT_REP    && เจๅฎค ฏฎงจๆจฉ, ขง๏โ๋ๅ ญ  เฅ ซจง ๆจ๎
    IF SEEK(FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM,"ACT_S_T")
      IF ACT_S_T.SUP_CODE < 0
        qs_m = qs_m - QNT_INC
      ENDIF
    ENDIF
  ENDIF
  q0       = q0+QNT_REST        &&  ซจ็จฅ ฏฎ ฏ เโจ๏ฌ
  q0_r     = q0_r+REST_REP      &&   เฅ ซจง ๆจจ ฏฎ ฏ เโจ๏ฌ
ENDSCAN
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  ฎคแ็ฅโ ฎโฏใแช  ข ไจซจ ซ.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Fil

SELECT Ryn_M
SEEK STOCK.CODE
SCAN REST WHILE CODE = STOCK.CODE
  q_e_fil  = q_e_fil+QNT
ENDSCAN
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             ฎคแ็ฅโ ญ ซจ็จ๏ ข ไจซจ ซ ๅ จ แโ เฎฉ กเฎญจ.                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_B_O
PRIVATE is_f

SELECT Ryn_L
SEEK STOCK.CODE
SCAN REST WHILE CODE = STOCK.CODE
*  is_f = .F.
  is_f = .T.
*  IF CUS_CODE >= 0
*    IF SEEK(CUS_CODE,"ACCOUNT")
*      IF ACCOUNT.ACC_TYPE = 4
*        is_f = .T.
*      ENDIF
*    ENDIF
*  ENDIF
  IF is_f
    q_f = q_f+QNT
  ELSE
    q_b = q_b+QNT
  ENDIF
ENDSCAN

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                         ฎคแ็ฅโ ญฎขฎฉ กเฎญจ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_B_N
PRIVATE is_f

SELECT LIST_DET
SEEK STOCK.CODE
SCAN REST WHILE CODE = STOCK.CODE
  IF SEEK(VAL(ALLTRIM(LIST_NUM)),"LIST_TIT")
    IF EMPTY(LIST_TIT.SAL_NUM)
      q_b = q_b+QNT_REQ
    ENDIF
  ENDIF
ENDSCAN

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                         ฎคแ็ฅโ แฏฅๆญ ซจ็จ๏.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Sp_N

SELECT SPEC_SUM
SEEK STR(STOCK.CODE,7)
SCAN REST WHILE CODE = STOCK.CODE
  qs_n = qs_n+QNT
ENDSCAN

RETURN
