*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  Diff_Lst      §à ¡®âç¨ª ƒ «¨­  „¬¨âà¨¥­ª®                 º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º            ‚ëç¨â ­¨¥ á¯¨áª®¢ ¨ á®§¤ ­¨¥ á¯¨áª  " §­®áâì".             º
*º           ( §­®áâì á¯¨áª®¢ ­¥ § ­®á¨âáï ¢ ®âª §ë ¯® æ¥­¥).            º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 14.06.2002 ÍÍ¼
PROCEDURE Diff_Lst
PARAMETERS old_lst, new_lst

PRIVATE s_sav, c_code, c_name, c_type, vlt
PRIVATE chck, l_name, l_fl, ml, frm, bnk

WAIT WINDOW "Ž¯¥à æ¨ï ®â¬¥­¥­ " NOWAIT
RETURN

s_sav = SELECT() 

DELETE FILE(tmpo_path+"TMP_DIFF.DBF")
CREATE DBF (tmpo_path+"TMP_DIFF");
          ( CODE      N( 7),  ;
            PREFIX    C( 4),  ;
            NAME      C(35),  ;
            PRODUCER  C( 8),  ;
            QNT       N( 6),  ;
            QNT_ORD   N( 6),  ;
            IS_SPEC   C( 1),  ;
            IS_OPT    C( 1),  ;
            USD_DEF   N(10,4),;
            DEF_PRICE N(12,5),;
            USD_PRICE N(10,4),;
            SAL_PRICE N(12,5))

SELECT 0
DELETE FILE(tmpo_path+"TMP_TIFF.DBF")
CREATE DBF (tmpo_path+"TMP_TIFF");
           (LST_ARM   C( 3),;
            CR_DAT    D,    ;
            LST_NAME  C(32),;
            LST_FLAGS C(25),;
            REQ_DOC   C( 8),;
            REQ_DATE  D,    ;
            IS_USD    L,    ;
            IS_CHECK  L,    ;
            CLI_CODE  N( 8),;
            CLI_TYPE  N( 1),;
            MAIL      C( 1),;
            FIRM      C( 3),;
            BANK      C( 3),;
            WHO_CR    N( 3),;
            WHERE_CR  C( 3))

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS TIT_2614 AGAIN
SEEK VAL(ALLTRIM(old_lst))
c_code = CLI_CODE
c_type = CLI_TYPE
vlt    = IS_USD
chck   = IS_CHECK
l_name = LST_NAME
l_flg  = LST_FLAGS
ml     = MAIL
frm    = FIRM
bnk    = BANK

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2614 AGAIN
SEEK c_code
c_name = CUS_NAME
USE

SELECT TMP_TIFF
APPEND BLANK
REPLACE LST_ARM   WITH arm,     ;
        CR_DAT    WITH DATE(),  ;
        LST_NAME  WITH LEFT(l_name,23)+" ®âª §ë",;
        LST_FLAGS WITH l_flg,   ;
        IS_USD    WITH vlt,     ;
        IS_CHECK  WITH chck,    ;
        CLI_CODE  WITH c_code,  ;
        CLI_TYPE  WITH c_type,  ;
        MAIL      WITH ml,      ;
        FIRM      WITH frm,     ;
        BANK      WITH bnk,     ;
        WHO_CR    WITH user,    ;
        WHERE_CR  WITH arm

SELECT 0
USE (base_path+"STOCK") ALIAS ST_2614 ORDER TAG CODE AGAIN

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_CODE ALIAS NEW_2614 AGAIN

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_CODE ALIAS OLD_2614 AGAIN
SET RELATION TO CODE INTO ST_2614
SEEK old_lst
SCAN REST WHILE old_lst = LIST_NUM .AND. CODE > 0
  IF ST_2614.QNT >= QNT - QNT_REQ
    IF .NOT. SEEK(new_lst+STR(CODE,7), "NEW_2614")
      IF OLD_2614.QNT > NEW_2614.QNT
        SELECT TMP_DIFF
        APPEND BLANK
        REPLACE CODE      WITH OLD_2614.CODE,     ;
                PREFIX    WITH OLD_2614.PREFIX,   ;
                NAME      WITH OLD_2614.NAME,     ;
                PRODUCER  WITH OLD_2614.PRODUCER, ;
                QNT       WITH OLD_2614.QNT,      ;
                QNT_ORD   WITH OLD_2614.QNT,      ;
                IS_SPEC   WITH OLD_2614.IS_SPEC,  ;
                IS_OPT    WITH OLD_2614.IS_OPT,   ;
                USD_DEF   WITH OLD_2614.USD_DEF,  ;
                DEF_PRICE WITH OLD_2614.DEF_PRICE,;
                USD_PRICE WITH OLD_2614.USD_PRICE,;
                SAL_PRICE WITH OLD_2614.SAL_PRICE
      ENDIF
    ELSE
      IF OLD_2614.QNT > NEW_2614.QNT
        SELECT TMP_DIFF
        APPEND BLANK
        REPLACE CODE      WITH OLD_2614.CODE,     ;
                PREFIX    WITH OLD_2614.PREFIX,   ;
                NAME      WITH OLD_2614.NAME,     ;
                PRODUCER  WITH OLD_2614.PRODUCER, ;
                QNT       WITH OLD_2614.QNT-NEW_2614.QNT,;
                QNT_ORD   WITH OLD_2614.QNT,      ;
                IS_SPEC   WITH OLD_2614.IS_SPEC,  ;
                IS_OPT    WITH OLD_2614.IS_OPT,   ;
                USD_DEF   WITH OLD_2614.USD_DEF,  ;
                DEF_PRICE WITH OLD_2614.DEF_PRICE,;
                USD_PRICE WITH OLD_2614.USD_PRICE,;
                SAL_PRICE WITH OLD_2614.SAL_PRICE
      ENDIF
    ENDIF
  ENDIF
  SELECT OLD_2614
ENDSCAN
USE

n_list = DOC_NUM("LISTS", "", "", "")

SELECT TIT_2614
APPEND BLANK
REPLACE LST_ARM  WITH arm,              ;
        LST_NUM  WITH n_list,           ;
        IS_USD   WITH TMP_TIFF.IS_USD,  ;
        IS_CHECK WITH TMP_TIFF.IS_CHECK,;
        CR_DAT   WITH DATE(),           ;
        MAIL     WITH TMP_TIFF.MAIL,    ;
        LST_NAME WITH TMP_TIFF.LST_NAME,;
        CLI_CODE WITH TMP_TIFF.CLI_CODE,;
        CLI_TYPE WITH TMP_TIFF.CLI_TYPE,;
        REQ_DOC  WITH TMP_TIFF.REQ_DOC, ;
        REQ_DATE WITH TMP_TIFF.REQ_DATE,;
        FIRM     WITH TMP_TIFF.FIRM,    ;
        BANK     WITH TMP_TIFF.BANK,    ;
        WHO_CR   WITH user,             ;
        WHERE_CR WITH arm
USE

SELECT TMP_DIFF
SET RELATION TO CODE INTO ST_2614
SCAN
  SELECT NEW_2614
  APPEND BLANK
  REPLACE LIST_NUM  WITH STR(n_list,6),     ;
          CODE      WITH TMP_DIFF.CODE,     ;
          PREFIX    WITH ST_2614.PREFIX,    ;
          NAME      WITH ST_2614.NAME,      ;
          PRODUCER  WITH ST_2614.PRODUCER,  ;
          QNT       WITH TMP_DIFF.QNT,      ;
          QNT_ORD   WITH TMP_DIFF.QNT_ORD,  ;
          IS_SPEC   WITH TMP_DIFF.IS_SPEC,  ;
          USD_DEF   WITH TMP_DIFF.USD_DEF,  ;
          DEF_PRICE WITH TMP_DIFF.DEF_PRICE,;
          USD_PRICE WITH TMP_DIFF.USD_PRICE,;
          SAL_PRICE WITH TMP_DIFF.SAL_PRICE
  SELECT TMP_DIFF
ENDSCAN

SELECT ST_2614
USE
SELECT NEW_2614
USE
SELECT TMP_DIFF
USE
SELECT TMP_TIFF
USE

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+'‘ä®à¬¨à®¢ ­ ­®¢ë© á¯¨á®ª "®âª §ë", N '+ALLTRIM(STR(n_list))
mss(3) = ""
DO Out_Mess WITH 5, "mss"

DELETE FILE(tmpo_path+"TMP_TIFF.DBF")
DELETE FILE(tmpo_path+"TMP_DIFF.DBF")

SELECT(s_sav)
RETURN