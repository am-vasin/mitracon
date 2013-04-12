pth_old = "z:\store\OLD\base\"
pth_new = "z:\store\base\"

USE (pth_new+"DOC_CNT") ORDER TAG CNT_KEY
SELECT 0

USE (pth_old+"PARMS")

USE (pth_old+"ARM_LST")
SCAN FOR ARM_S = "A"
  DO Set_N1 WITH "BILLS     ", "CMP", ARM_C, ARM_LST.LAST_BILL
  DO Set_N1 WITH "SALE      ", "CMP", ARM_C, ARM_LST.LAST_INV
  DO Set_N1 WITH "KASSA     ", "   ", ARM_C, ARM_LST.LAST_REP
ENDSCAN


PROCEDURE Set_N1
PARAMETERS c_name, f_c, a_c, n
IF n = 0
  RETURN
ENDIF
PRIVATE s_sav
s_sav = SELECT()
SELECT DOC_CNT
IF .NOT. SEEK(c_name+f_c+a_c+"1999")
  APPEND BLANK
  REPLACE FIRM_C   WITH f_c, ;
          ARM_C    WITH a_c, ;
          YEAR     WITH "1999", ;
          DOC_CODE WITH c_name, ;
          LAST_DOC WITH n
ELSE
  REPLACE LAST_DOC WITH n
ENDIF
SELECT (s_sav)

RETURN
