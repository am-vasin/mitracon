*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º ” ©« P_Ctrl        §à ¡®âç¨ª €­¤à¥© ‚ á¨­           24.02.98 18:24:41 º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º      à®£à ¬¬  ä®à¬¨à®¢ ­¨ï ã¯à ¢«ïîé¨å ESC-æ¥¯®ç¥ª ¤«ï ¯à¨­â¥à .      º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE P_Ctrl
PARAMETER p_len, fnt, p_type_n, p_end

*
*  p_len - ¤«¨­  áâà ­¨æë
*  fnt - â¨¯ èà¨äâ , 0 - ®¡ëç­ë©, 1 - condensed
*  p_type_n - ­®¬¥à § ¯¨á¨ (â¨¯  ¯à¨­â¥à ) ¢ PRNTYPES.DBF
*  p_end - ª®­¥æ ¢ë¢®¤  ¤®ªã¬¥­â ?

PRIVATE s, start, nf, res, i, s_sav, tmpName

s_sav = SELECT()
res = ""

IF File_O(base_path+"PRNSETUP.DBF")
  SELECT 0
  USE (base_path+"PRNSETUP.DBF")
ELSE
  RETURN res  && Žâáãâáâ¢ã¥â â ¡«¨æ , ®¯¨áë¢ îé ï ã¯à.á¨¬¢®«ë ¯à¨­â¥à 
ENDIF

GO p_type_n
IF EOF()
  USE
  SELECT (s_sav)
  RETURN res && ‘«¨èª®¬ ¡®«ìè®© ­®¬¥à â¨¯  ¯à¨­â¥à 
ENDIF

tmpName = UPPER(ALLTRIM(PR_NAME))

DO CASE
CASE tmpName = "HP LASERJET 2100"
  IF p_end
    res = CHR(27)+"E"								&& ‘¡à®á
  ELSE
    res = CHR(27)+"E"								&& ‘¡à®á
    res = res+CHR(27)+IIF(EMPTY(LEFT(fnt,1)), ;
                          "&k0S", "&k2S")			&& ˜à¨äâ
    res = res+CHR(27)+IIF(EMPTY(SUBSTR(fnt,2,1)), ;
                          "&l0O", "&l1O")			&& Žà¨¥­â æ¨ï
  ENDIF
CASE tmpName = "EPSON"
  IF p_end
    IF Sale_Mode = 'MEGA'
      res = RESET
    ELSE
      res = CHR(27)+"@"								&& ‘¡à®á
    ENDIF
  ELSE
    IF Sale_Mode = 'MEGA'
      IF !EMPTY(Set_len) AND !EMPTY(p_len)
        res=Start+Set_len+CHR(p_len)+IIF(EMPTY(fnt), Set_Norm, Set_Cond)
      ELSE
        res=Start+IIF(EMPTY(fnt), Set_Norm, Set_Cond)
      ENDIF
    ELSE
      res = CHR(27)+"@"								&& ‘¡à®á
      res = res+IIF(EMPTY(LEFT(fnt,1)),   ;
                        CHR(18), CHR(15))			&& ˜à¨äâ
      IF p_len > 0
        res = res+CHR(27)+"C"+CHR(p_len)			&& „«¨­  áâà ­¨æë
      ENDIF
    ENDIF
  ENDIF
OTHERWISE
ENDCASE

USE
SELECT (s_sav)
RETURN res
