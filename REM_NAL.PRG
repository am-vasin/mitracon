*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Rem_nal       งเ กฎโ็จช €ญคเฅฉ  แจญ           14.01.98 14:55:04 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                         ฅเฅแโ ญฎขช  ญ ซจ็จ๏.                          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rem_Nal
PARAMETER vis

PRIVATE cnt, d_s, err, ab, sch, q_s_all, q_s_fil, q_s_rep, q_i_all, ;
        q_i_rep, q_e_fil, q_b, q_f, is_data, q0, q0_r, log_rec, err_cnt

USE (base_path+"PARMS")
d_s = DAT_START

DELETE FILE TST_NAL.DBF
DELETE FILE TST_NAL.CDX

CREATE DBF TST_NAL ;
  (  CODE  N( 7),  ;
     NEW_N N( 6),  ;
     NEW_F N( 6),  ;
     NEW_R N( 6)   )
INDEX ON CODE TAG CODE

SELECT 0
USE (base_path+"SALE") ORDER TAG CODE
SET FILTER TO DOC_DATE >= d_s .AND. LEFT(DOC_NUM,1) = sys_char

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM
SET FILTER TO LEFT(DOC_NUM,1) = sys_char .AND. DOC_DATE >= d_s

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE

SELECT 0
USE (base_path+"RYN_M") ORDER TAG CODE
SET FILTER TO DOC_DATE >= d_s

SELECT 0
USE (base_path+"STOCK")
cnt = 0
SCAN
  cnt= cnt+1
  @ 10, 10 SAY STR(cnt)
  SELECT TST_NAL
  APPEND BLANK
  REPLACE CODE  WITH STOCK.CODE,  ;
          NEW_N WITH STOCK.QNT+STOCK.QNT_SAVE,  ;
          NEW_F WITH STOCK.QNT_REAL,  ;
          NEW_R WITH STOCK.QNT_REP
  SELECT STOCK
ENDSCAN

USE
SELECT TST_NAL
PRIVATE w1, w2, w3

cnt = 0
SCAN
  cnt = cnt+1
  @ 10, 10 SAY STR(cnt)
  STORE 0 TO w1, w2, w3

  DO Cnt_Sale     && เฎค ฆจ ( ฏเฎแโฎ จ ไจซจ ซใ )
  DO Cnt_Inc      && เจๅฎค
  DO Cnt_Fil      && ขจฆฅญจฅ ฏฎ ไจซจ ซใ

  SELECT TST_NAL
  REPLACE NEW_N WITH NEW_N+w1,  ;
          NEW_F WITH NEW_F+w2,  ;
          NEW_R WITH NEW_R+w3

ENDSCAN

CLOSE DATABASES

USE TST_NAL ORDER TAG CODE
SELECT 0
USE (base_path+"STOCK")
SET RELATION TO CODE INTO TST_NAL
SET FILTER TO FOUND("TST_NAL")

REPLACE ALL QNT_START WITH TST_NAL.NEW_N, ;
            QNT_S_RYN WITH TST_NAL.NEW_F, ;
            QNT_S_REP WITH TST_NAL.NEW_R

CLOSE DATABASES

CLEAR

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                          ฎคแ็ฅโ ฏเฎค ฆ.                              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Sale

SELECT SALE
SEEK TST_NAL.CODE
SCAN REST WHILE CODE = TST_NAL.CODE
  w1 = w1+QNT     && ก้จฅ ฏเฎค ฆจ
  w3 = w3+QNT_REP && เฎค ฆจ ฏฎงจๆจฉ, ขง๏โ๋ๅ ญ  เฅ ซจง ๆจ๎
  IF SEEK(LEFT(DTOS(DOC_DATE),4)+DOC_NUM,"SALE_TIT")
    IF EMPTY(SALE_TIT.LIST_NUM)
      w2 = w2+QNT    && เฎค ฆจ ไจซจ ซใ
      w1 = w1-QNT    && เฎแโฎ ฏเฎค ฆจ ฎโช โ๋ข ฅฌ!
    ENDIF
  ENDIF
ENDSCAN
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  ฎคแ็ฅโ ฏฎแโใฏซฅญจ๏ โฎข เ .                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Inc

SELECT ACT_S
SEEK TST_NAL.CODE
SCAN REST WHILE CODE = TST_NAL.CODE
  IF DOC_DATE >= d_s
    w1 = w1-QNT_INC    && ก้จฅ ฏเจๅฎค
    w3 = w3-QNT_REP    && เจๅฎค ฏฎงจๆจฉ, ขง๏โ๋ๅ ญ  เฅ ซจง ๆจ๎
  ENDIF
ENDSCAN
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  ฎคแ็ฅโ ฎโฏใแช  ข ไจซจ ซ.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Fil

SELECT Ryn_M
SEEK TST_NAL.CODE
SCAN REST WHILE CODE = TST_NAL.CODE
  w2 = w2-QNT
  w1 = w1+QNT
ENDSCAN
RETURN
