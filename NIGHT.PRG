*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Night        Разработчик Андрей Васин           11.09.98 11:06:58 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                          Монитор регламентов.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Night
*
*     Общие праметры
*
PUBLIC base_path   && путь к файлам базы данных
PUBLIC tmp_path    && путь к общим рабочим файлам
PUBLIC log_path    && путь к общим файлам журнала
PUBLIC tmpo_path   && путь к собственным рабочим файлам
PUBLIC prmo_path   && путь к собственным файлам параметров
PUBLIC lwr         && таблица перекодировки в нижний регистр
PUBLIC arm         && код рабочего места
PUBLIC arm_nam     && Название рабочего места
PUBLIC arm_rec     && номер записи в файле ARM_LST
PUBLIC user        && код пользователя
PUBLIC kurs        && текущий курс доллара для расчета цен
PUBLIC kurs_b      && текущий курс доллара для расчетов с клиентом
PUBLIC kurs_3      && текущий курс доллара 3
PUBLIC kurs_mmvb   && текущий курс ММВБ
PUBLIC p_file      && имя файла печати
PUBLIC sup_own     && собственный код в списке поставщиков
PUBLIC own_val     && собственная валюта
PUBLIC popup_n     && имя POPUP
PUBLIC pad_sav     && имя пункта BAR MENU
PUBLIC bar_        && номер пункта
PUBLIC mode        && режим: main, filial + C - для Компэл
PUBLIC menu_name   && имя программы основного меню
PUBLIC sys_char    && литера - идентификатор системы
PUBLIC acc_level   && уровень доступа
PUBLIC f_l_bg      && первая строка фона
PUBLIC prn_mode    && режим печати
PUBLIC vk_arch     && имя файла архива для ВК
PUBLIC vk_spec     && имя файла архива для ВК
PUBLIC new_ord     && признак нового формата заявки
PUBLIC sav_tit, mod_tit && Заставка и ее режим
PRIVATE vrs, rg_nom, f_ser, demo, sav_dir, do_arm, mode, m_term, r_nam
sav_tit = ""
mod_tit = ""
*
*
*
demo = .F.
SET DATE GERMAN

DO Set_Lwr          && заполнение таблицы перекодировки в нижний регистр

ON READERROR = 1

*
*   Установка директорий
*

DO Set_Proc         && установка параметров среды FoxPro

sav_dir = SET("DEFAULT")+CURDIR()  && текущая директория ( плюс драйв! )

SET DEFAULT TO (SYS(2004)) && Текущая директория - директория FoxPro и программ
PRIVATE s
s = "! "+SET("DEFAULT")
&s
s = "! CD "+CURDIR()
s = LEFT(s,LEN(s)-1)
&s

prmo_path = sav_dir+"PRMS\"
tmpo_path = sav_dir+"TMP\"
base_path = SYS(2004)+"BASE\"
tmp_path  = SYS(2004)+"BASE\TMP\"
log_path  = SYS(2004)+"LOG\"

s = base_path+";"+tmpo_path+";"+prmo_path+";"+tmp_path+";"+log_path
SET PATH TO (s)

m.kurs   = Get_Curs("CURS")
m.kurs_b = Get_Curs("CURS_B")
m.kurs_3 = Get_Curs("CURS_N")
m.kurs_mmvb = Get_Curs("CURS_MMVB")

USE (base_path+"PARMS")
m.mode = PARMS.MODE
sys_char = SYS_ID
USE

arm = " 13"
user = 0

ON ERROR DO Err_Proc WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()

PRIVATE rc_987, dt_w, dt_w, str_w
rc_987 = 1
SET PRINTER TO (log_path+"NIGHT.TXT") ADDITIVE
SET DEVICE TO PRINT
@ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+" Запуск регламентов ____________________________"
SET DEVICE TO SCREEN
SET PRINTER TO PRN:
IF File_O(base_path+"NIGHT.DBF")
  DO WHILE .T.
    USE (base_path+"NIGHT")
    IF RECCOUNT() < rc_987
      USE
      EXIT
    ENDIF
    GO rc_987
    IF STOP              && Кто-то замазал...
      rc_987 = rc_987+1
      USE
      LOOP
    ENDIF
    dt_w = DATE()
    IF TIME() < "10:00:00"
      dt_w = dt_w-1
    ENDIF
    IF TYPE_LIST = 1   && Месячное расписание
      str_w = DAY(dt_w)
    ELSE
      str_w = DOW(dt_w)
    ENDIF
    IF SUBSTR(LOG_LIST, str_w, 1) = " "   && Не время...
      rc_987 = rc_987+1
      USE
      LOOP
    ENDIF
    REPLACE S_DATE WITH DATE(), S_TIME WITH TIME()
    str_w = ALLTRIM(COMM_STR)
    r_nam = ALLTRIM(NAME)
    USE
    SET PRINTER TO (log_path+"NIGHT.TXT") ADDITIVE
    SET DEVICE TO PRINT
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+" Регламент "+r_nam
    SET DEVICE TO SCREEN
    SET PRINTER TO PRN:
    &str_w
    CLOSE DATABASES
    SET PRINTER TO (log_path+"NIGHT.TXT") ADDITIVE
    SET DEVICE TO PRINT
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+" Регламент завершен"
    SET DEVICE TO SCREEN
    SET PRINTER TO PRN:
    USE (base_path+"NIGHT")
    GO rc_987
    REPLACE T_DATE WITH DATE(), T_TIME WITH TIME()
    USE
    rc_987 = rc_987+1
  ENDDO
ENDIF

SET DEFAULT TO &sav_dir

QUIT

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║            Программа установки параметров среды FoxPro.               ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Set_Proc

SET HELP      OFF
SET CONFIRM   OFF
SET ESCAPE    OFF
SET TALK      OFF
SET EXACT     OFF
SET DEBUG     OFF
SET STEP      OFF
SET SYSMENU   OFF
SET DELETED   ON
SET SAFETY    OFF
SET EXCLUSIVE OFF
SET MEMOWIDTH TO 150
SET REPROCESS TO 200
SET REFRESH   TO 5
SET DECIMAL   TO 3
SET UDFPARMS  TO REFERENCE
SET FUNCTION  1 TO "f"
SET FUNCTION  2 TO "f"
SET FUNCTION  3 TO "f"
SET FUNCTION  4 TO "f"
SET FUNCTION  5 TO "f"
SET FUNCTION  6 TO "f"
SET FUNCTION  7 TO "f"
SET FUNCTION  8 TO "f"
SET FUNCTION  9 TO "f"
SET FUNCTION 10 TO "f"
SET FUNCTION 11 TO "f"
SET FUNCTION 12 TO "f"
ON READERROR =0
*
*
*       На время отладки!!!
*ON KEY LABEL F5 SUSPEND
*ON KEY LABEL F6 ON KEY LABEL Enter
*ON KEY LABEL F7 QUIT

RETURN

PROCEDURE Del_Com
ON KEY LABEL Esc
RELEASE WINDOW COMMAND
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Обработчик ошибок.                                     ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Err_Proc
PARAMETERS e_numb, e_mess, e_proc, e_line
EXTERNAL ARRAY err_lst
*PUBLIC base_path
ON ERROR

PRIVATE _mess, prg

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
  arm_w  = "  0"
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
CLOSE DATABASES
ON ERROR DO Err_Proc WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()
IF PROGRAM(3) = "START_P"
  RETURN TO START_P
ENDIF
RETURN TO MASTER

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║             Программа проверки наличия файла.                         ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE File_O
PARAMETERS f_nam

PRIVATE arr_w
RETURN adir(arr_w, f_nam) > 0

PROCEDURE Out_Mess
PARAMETERS c_sch, mss_arr
RETURN

PROCEDURE Wt_Mess
PARAMETER txt
RETURN
