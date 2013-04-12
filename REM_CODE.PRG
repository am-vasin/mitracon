*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º ” ©« R_Index       §à ¡®âç¨ª €­¤à¥© ‚ á¨­           16.09.98 17:28:31 º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                        Š®àà¥ªæ¨ï ª®¤®¢ ¯®§¨æ¨©.                        º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Rem_Code

SET PRINTER TO (log_path+"NIGHT.TXT") ADDITIVE
SET DEVICE TO PRINT

PRIVATE f_list, i, err_lst, err_cod, s, s0, delta
DIMENSION f_list(24,3)
delta = 8984903

f_list( 1,1) = "ABSENT"
f_list( 1,2) = "CODE"
f_list( 1,3) = "CODE"

f_list( 2,1) = "ACT_S"
f_list( 2,2) = "CODE"
f_list( 2,3) = "CODE"

f_list( 3,1) = "CORR_LST"
f_list( 3,2) = ""
f_list( 3,3) = "CODE"

f_list( 4,1) = "DEMAND"
f_list( 4,2) = "STO_CODE"
f_list( 4,3) = "STO_CODE"

f_list( 5,1) = "INC_DUE"
f_list( 5,2) = ""
f_list( 5,3) = "CODE"

f_list( 6,1) = "INC_LIST"
f_list( 6,2) = ""
f_list( 6,3) = "CODE"

f_list( 7,1) = "LET_SPEC"
f_list( 7,2) = "CODE"
f_list( 7,3) = "CODE"

f_list( 8,1) = "LIST_DET"
f_list( 8,2) = "CODE"
f_list( 8,3) = "CODE"

f_list( 9,1) = "MAN_REP"
f_list( 9,2) = ""
f_list( 9,3) = "STO_CODE"

f_list(10,1) = "OFFER"
f_list(10,2) = "CODE"
f_list(10,3) = "CODE"

f_list(11,1) = "ORDER"
f_list(11,2) = ""
f_list(11,3) = "CODE"

f_list(12,1) = "ORD_D"
f_list(12,2) = ""
f_list(12,3) = "CODE"

f_list(13,1) = "ORD_L"
f_list(13,2) = ""
f_list(13,3) = "CODE"

f_list(14,1) = "REFUSAL"
f_list(14,2) = ""
f_list(14,3) = "STO_CODE"

f_list(15,1) = "REPORT"
f_list(15,2) = ""
f_list(15,3) = "CODE"

f_list(16,1) = "RYN_1_L"
f_list(16,2) = "CODE"
f_list(16,3) = "CODE"

f_list(17,1) = "RYN_L"
f_list(17,2) = "CODE"
f_list(17,3) = "CODE"

f_list(18,1) = "RYN_M"
f_list(18,2) = "CODE"
f_list(18,3) = "CODE"

f_list(19,1) = "SALE"
f_list(19,2) = "CODE"
f_list(19,3) = "CODE"

f_list(20,1) = "SPEC_SUM"
f_list(20,2) = ""
f_list(20,3) = "CODE"

f_list(21,1) = "STOCK"
f_list(21,2) = "CODE"
f_list(21,3) = "CODE"

f_list(22,1) = "STO_DEM"
f_list(22,2) = "CODE"
f_list(22,3) = "CODE"

f_list(23,1) = "STO_LOG"
f_list(23,2) = ""
f_list(23,3) = "CODE"

f_list(24,1) = "WK_LIST"
f_list(24,2) = ""
f_list(24,3) = "CODE"

FOR i = 1 TO 24
  err_cod = 0
  IF .NOT. FILE(base_path+f_list(i,1)+".DBF")
    wait wind f_list(i,1)
    LOOP
  ENDIF
  @ PROW()+1, 0 SAY f_list(i,1)+"   "+TIME()
  IF EMPTY(f_list(i,2))
    USE (base_path+f_list(i,1))
    s = f_list(i,3)
    SET FILTER TO &s > 9000000
    REPLACE ALL &s WITH &s - delta
  ELSE
    s = f_list(i,2)
    USE (base_path+f_list(i,1)) ORDER TAG &s
    s = f_list(i,3)
    GO BOTT
    DO WHILE &s > 9000000
      REPLACE &s WITH &s - delta
      GO BOTT
    ENDDO
  ENDIF
  USE
ENDFOR

SET PRINTER TO PRN:
SET DEVICE TO SCREEN

RETURN
