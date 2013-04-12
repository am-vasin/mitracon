PROCEDURE Err_Proc
PARAMETERS e_numb, e_mess, e_proc, e_line
EXTERNAL ARRAY err_lst
*PUBLIC base_path

IF TYPE("err_111") = "N"  && Пытаемся достучаться при 111-й ошибке
  IF e_numb = 111
    err_111 = err_111+1
    IF err_111 < 10
      RETRY
    ENDIF
  ENDIF
  err_111 = 0
ENDIF

ON ERROR

PRIVATE _mess, prg

IF INLIST(e_numb, 108, 109)   && Конфликт мультидоступа
  DIMENSION _mess(7)
  _mess(1) = ""
  _mess(2) = CHR(0)+"Нас не допускают к базе данных!"
  _mess(3) = CHR(0)+"Разберитесь в чем дело и нажмите ANY KEY!"
  _mess(4) = CHR(0)+"Для А Васина: "+ALLTRIM(STR(e_numb))+"/"+e_proc+"/"+ALLTRIM(STR(e_line))+"."
  _mess(5) = CHR(0)+DBF()+" "+ALLTRIM(STR(RECNO()))
  _mess(6) = CHR(0)+"ВНИМАНИЕ! Ctrl-Alt-Del, Reset не нажимать ни при каких обстоятельствах!!!"
  _mess(7) = ""
  DO Out_Mess WITH 7, "_mess"
  ON ERROR DO Err_Proc WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()
  RETRY
ENDIF
IF e_numb = 125    && неприятности при печати
  SET DEVICE TO SCREEN
  DIMENSION _mess(4)
  _mess(1) = ""
  _mess(2) = "                          Устройство печати не готово."
  _mess(3) = "                   [ Продолжить печать ]   [ Прекратить печать ]"
  _mess(4) = ""
  IF Out_Mess(7,"_mess") = 1
    SET DEVICE TO PRINTER
    ON ERROR DO Err_Proc WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()
    RETRY
  ELSE
    IF TYPE("n_file") = "N"
      = FCLOSE(n_file)
    ENDIF
  ENDIF
ELSE
  IF TYPE("err_lst") = "N"
    IF TYPE("err_lst(1)") = "N"
      PRIVATE i
      FOR i = 1 TO ALEN(err_lst)
        IF err_lst(i) = e_numb
          IF TYPE("err_cod") = "N"
            err_cod = e_numb
          ENDIF
          ON ERROR DO Err_Proc WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()
          RETURN
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
ENDIF
CLOSE DATABASES
SELECT 1
CLEAR WINDOWS
SET DEVICE TO SCREEN
ON KEY LABEL Enter
ON KEY LABEL LeftMouse
*ON KEY LABEL F1
PRIVATE f_n
f_n = IIF(TYPE("base_path") = "C", base_path, "")+"ERR_LOG"
IF FILE(f_n+".DBF")
  PRIVATE user_w, arm_w
  user_w = 0
  arm_w  = CHR(0)
  IF TYPE("user") = "N"
    user_w = user
  ENDIF
  IF TYPE("arm") = "C"
    arm_w = arm
  ENDIF
  USE (f_n)
  SET ORDER TO TAG ERR_DAT
  GO TOP
  REPLACE ER_DAT WITH DATE(), ;
          ER_TIM WITH TIME(), ;
          ER_NUM WITH e_numb, ;
          ER_MES WITH e_mess, ;
          ER_PRO WITH e_proc, ;
          ER_LIN WITH e_line, ;
          ER_USE WITH user_w, ;
          ER_ARM WITH arm_w
  USE
ENDIF
IF e_numb # 125
*  DIMENSION _mess(6)
  DIMENSION _mess(3)
  _mess(1) = ""
*  _mess(2) = CHR(0)+"Моих грехов разбор      "
*  _mess(3) = CHR(0)+"Оставьте до поры,       "
*  _mess(4) = CHR(0)+"Вы оцените красоту игры!"
*  _mess(5) = CHR(0)+"Ошибка "+ALLTRIM(STR(e_numb,5))+" в модуле "+e_proc+"."
*  _mess(6) = ""
  _mess(2) = CHR(0)+"Ошибка "+ALLTRIM(STR(e_numb,5))+" в модуле "+e_proc+"."
  _mess(3) = ""
  DO Out_Mess WITH 7, "_mess"
ENDIF
CLOSE DATABASES
ON ERROR DO Err_Proc WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()
IF PROGRAM(3) = "START_P"
  RETURN TO START_P
ENDIF
