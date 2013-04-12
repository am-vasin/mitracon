*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º ” ©« Ad_Month       §à ¡®âç¨ª €­¤à¥© + €««          08.04.97 21:18:48 º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                     „®¡ ¢«¥­¨¥ § ¯¨á¨ ¢ „ è ¡«®­®¢.                   º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Ad_Month
GO BOTTOM IN MOULD
IF BOF() .OR. EOF()
  t_month = MONTH(DATE())
  t_year  = YEAR(DATE())
ELSE
  t_month = MOULD.MONTH+1
  t_year  = MOULD.YEAR
ENDIF
IF t_month >12
  t_year = Year +1
  t_month = 1
ENDIF
DO Mould WITH t_month,t_year 

RETURN
