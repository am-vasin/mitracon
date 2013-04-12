*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  Dlt_Lst       §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º            ‚ëç¨â ­¨¥ á¯¨áª®¢ ¨ ¢ë­¥á¥­¨¥ à §­®áâ¨ ¢ ®âª §.             º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 28.12.1998 ÍÍ¼
PROCEDURE Dlt_Lst
PARAMETERS old_lst, new_lst

PRIVATE s_sav, c_code, c_name, vlt, dt

s_sav = SELECT() 

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS TIT_8C28 AGAIN
SEEK VAL(ALLTRIM(old_lst))
c_code = CLI_CODE
vlt    = IS_USD
dt     = DATE()

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_8C28 AGAIN
SEEK c_code
c_name = CUS_NAME

USE (base_path+"LIST_DET") ORDER TAG L_CODE ALIAS NEW_8C28 AGAIN

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_2701 AGAIN

SELECT 0
USE (base_path+"REFUSAL") ALIAS REF_8C28 AGAIN

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_CODE ALIAS OLD_8C28 AGAIN
SET RELATION TO CODE INTO S_2701
SEEK old_lst
SCAN REST WHILE old_lst = LIST_NUM .AND. CODE > 0
  IF S_2701.QNT >= QNT - QNT_REQ
    IF .NOT. SEEK(new_lst+STR(CODE,7), "NEW_8C28")
      SELECT REF_8C28
      APPEND BLANK
      REPLACE STO_CODE WITH OLD_8C28.CODE,     ;
              PREFIX   WITH OLD_8C28.PREFIX,   ;
              NAME     WITH OLD_8C28.NAME,     ;
              PRODUCER WITH OLD_8C28.PRODUCER, ;
              PRO_CODE WITH OLD_8C28.PRO_CODE, ;
              FUNCTION WITH OLD_8C28.FUNCTION, ;
              AMODEL   WITH OLD_8C28.AMODEL,   ;
              DATE     WITH DATE(),            ;
              QTY      WITH OLD_8C28.QNT,      ;
              PRICE    WITH IIF(vlt, 0, OLD_8C28.SAL_PRICE), ;
              PRICE_D  WITH IIF(vlt, OLD_8C28.USD_PRICE, 0), ;
              CUS_CODE WITH c_code,  ;
              CLIENT   WITH c_name,  ;
              WHO      WITH user,    ;
              WHERE    WITH arm
    ELSE
      IF OLD_8C28.QNT > NEW_8C28.QNT
        SELECT REF_8C28
        APPEND BLANK
        REPLACE STO_CODE WITH OLD_8C28.CODE,     ;
                PREFIX   WITH OLD_8C28.PREFIX,   ;
                NAME     WITH OLD_8C28.NAME,     ;
                PRODUCER WITH OLD_8C28.PRODUCER, ;
                PRO_CODE WITH OLD_8C28.PRO_CODE, ;
                FUNCTION WITH OLD_8C28.FUNCTION, ;
                AMODEL   WITH OLD_8C28.AMODEL,   ;
                DATE     WITH DATE(),  ;
                QTY      WITH OLD_8C28.QNT-NEW_8C28.QNT,       ;
                PRICE    WITH IIF(vlt, 0, OLD_8C28.SAL_PRICE), ;
                PRICE_D  WITH IIF(vlt, OLD_8C28.USD_PRICE, 0), ;
                CUS_CODE WITH c_code,  ;
                CLIENT   WITH c_name,  ;
                WHO      WITH user,    ;
                WHERE    WITH arm
      ENDIF                
    ENDIF
  ENDIF
  SELECT OLD_8C28
ENDSCAN
USE
SELECT S_2701
USE
SELECT NEW_8C28
USE
SELECT REF_8C28
USE

SELECT (s_sav)

RETURN
