*
*  Очистка ВСЕХ остатков на депозите клиента
*
PROCEDURE Clr_Dep
PARAMETERS f_code, c_code

PRIVATE s_sav, n_d

s_sav = SELECT()
SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE SHARED AGAIN
IF .NOT. SEEK(STR(c_code)+f_code)
  USE
  SELECT (s_sav)
  RETURN
ENDIF
REPLACE ACC_SUM WITH ACC_SUM+ACC_S_H
REPLACE ACC_S_H WITH 0
REPLACE ACC_SUMD WITH ACC_SUMD+ACC_SD_H
REPLACE ACC_SD_H WITH 0
REPLACE CR_SUM WITH 0, CRD_SUM WITH 0
IF ABS(ACC_SUMD)+ABS(ACC_SUM) = 0
  USE
  SELECT (s_sav)
  RETURN
ENDIF
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
SEEK m.c_code
SELECT 0
USE (base_path+"INCMONEY")
IF CUS_REST.ACC_SUM # 0
  n_d = Doc_Num("MONEY_BN", f_code, arm, DATE())
  n_d = sys_char+STR(n_d, 9)
  APPEND BLANK
  REPLACE FIRM     WITH f_code,    ;
          DOC_TYPE WITH "1",       ;
          DOC_DATE WITH DATE(),    ;
          DOC_NUM  WITH n_d,       ;
          DOC_TIME WITH SECONDS(), ;
          CUS_CODE WITH c_code,    ;
          CUS_NAME WITH ACCOUNT.CUS_NAME, ;
          MONEY_0  WITH -(CUS_REST.ACC_SUM)*5/6, ;
          NDS_     WITH 20,               ;
          MONEY_R  WITH -CUS_REST.ACC_SUM, ;
          MONEY_D  WITH -CUS_REST.ACC_SUM/kurs, ;
          WHO      WITH user,      ;
          WHERE    WITH arm
ENDIF

IF CUS_REST.ACC_SUMD # 0
  n_d = Doc_Num("USD_BN", f_code, arm, DATE())
  n_d = sys_char+STR(n_d, 9)
  APPEND BLANK
  REPLACE FIRM     WITH f_code,    ;
          DOC_TYPE WITH "4",       ;
          DOC_NUM  WITH n_d,       ;
          DOC_DATE WITH DATE(),    ;
          DOC_TIME WITH SECONDS(), ;
          CUS_CODE WITH c_code,    ;
          CUS_NAME WITH ACCOUNT.CUS_NAME, ;
          MONEY_0  WITH -CUS_REST.ACC_SUMD, ;
          MONEY_R  WITH -CUS_REST.ACC_SUMD*kurs,;
          MONEY_D  WITH -CUS_REST.ACC_SUMD, ;
          WHO      WITH user,      ;
          WHERE    WITH arm
ENDIF

USE
USE IN ACCOUNT
SELECT CUS_REST
REPLACE ACC_SUM WITH 0, ACC_SUMD WITH 0
USE
SELECT (s_sav)

RETURN
