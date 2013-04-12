*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  Do_Check      §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                        ‡ ï¢ª  ­  ¯à®¡¨¢ªã ç¥ª .                        º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 09/01/2000 ÍÍ¼
PROCEDURE Do_Check
PARAMETERS c_sum0, n_sum0, n_sum1, c_num, c_date, c_time, c_err

PRIVATE s_sav
PRIVATE r_no, new, time_sav

s_sav = SELECT()
SELECT 0
USE (base_path+"TO_KKM") ORDER TAG HRON1
GO TOP
new = .T.
IF .NOT. BOF() .AND. .NOT. EOF()
  IF DATE() - DOC_DATE > 3
    new = .F.
  ENDIF
ENDIF
IF new
  APPEND BLANK
ENDIF
r_no = RECNO()
REPLACE DOC_DATE WITH {},     ;
        CHK_SUM  WITH c_sum0, ;
        NDS_SUM  WITH n_sum0, ;
        NSP_SUM  WITH n_sum1, ;
        WHO      WITH user,   ;
        ERR_CODE WITH 0,      ;
        WHERE    WITH VAL(ALLTRIM(arm))
FLUSH
SET ORDER TO
time_sav = SECONDS()
GO r_no
DO WHILE time_sav+120 > SECONDS() .AND. EMPTY(DOC_DATE)
  GO r_no
ENDDO
IF EMPTY(DOC_DATE)
  REPLACE DOC_NUM  WITH "**********", ;
          DOC_DATE WITH DATE(),       ;
          DOC_TIME WITH TIME(),       ;
          ERR_CODE WITH 99999,        ;
          WHO      WITH USER,         ;
          WHERE    WITH VAL(ALLTRIM(arm))
  FLUSH
ENDIF
c_num  = DOC_NUM
c_date = DOC_DATE
c_time = VAL(ALLTRIM(SUBSTR(DOC_TIME,1,2)))*3600+ ;
         VAL(ALLTRIM(SUBSTR(DOC_TIME,4,2)))*60+ ;
         VAL(ALLTRIM(SUBSTR(DOC_TIME,7,2)))
c_err  = ERR_CODE

USE
SELECT (s_sav)

RETURN
