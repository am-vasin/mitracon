*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º ” ©« IniTrans      §à ¡®âç¨ª €­¤à¥© ‚ á¨­           06.03.98 11:03:05 º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                  ˆ­¨æ¨ «¨§ æ¨ï ä ©«  à áå®¤  ¤«ï ‚Š.                   º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE IniTrans
PARAMETER n_word
PRIVATE nf, bf, i, p

IF n_word > 1
  IF .NOT. File_O(path_comm+"STO_LET.DBF")
    RETURN ""
  ELSE
    RETURN path_comm+"STO_LET.DBF"
  ENDIF
ELSE
  IF .NOT. EMPTY(Get_Sys("COM_SALE"))
    IF .NOT. File_O(path_comm+"SALE.DBF")
      RETURN ""
    ELSE
      RETURN path_comm+"SALE.DBF"
    ENDIF
  ELSE
    RETURN ""
  ENDIF
ENDIF
