*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Night        ��о�｀砒┴ ��むォ ��瓱�           11.09.98 11:06:58 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                          ����皰� 爛����キ皰�.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Night
*
*     ｡薑� ����モ琺
*
PUBLIC base_path   && �竄� � ������ ��щ ����諷
PUBLIC tmp_path    && �竄� � �♂━ ��｀腮� ������
PUBLIC log_path    && �竄� � �♂━ ������ �窶����
PUBLIC tmpo_path   && �竄� � 甌＝癶キ�覓 ��｀腮� ������
PUBLIC prmo_path   && �竄� � 甌＝癶キ�覓 ������ �����モ牀�
PUBLIC lwr         && ��゛��� �ムオ�え牀→� � �└�┤ 爛��痰�
PUBLIC arm         && ��� ��｀腑�� �メ��
PUBLIC arm_nam     && ��Б��┘ ��｀腑�� �メ��
PUBLIC arm_rec     && ���ム ����瓱 � ����� ARM_LST
PUBLIC user        && ��� ���讌���皀��
PUBLIC kurs        && 皀�竕┤ �窶� ぎ����� か� ��瘍モ� 罐�
PUBLIC kurs_b      && 皀�竕┤ �窶� ぎ����� か� ��瘍モ�� � ��┘�皰�
PUBLIC kurs_3      && 皀�竕┤ �窶� ぎ����� 3
PUBLIC kurs_mmvb   && 皀�竕┤ �窶� ����
PUBLIC p_file      && ━� ����� �ョ�皋
PUBLIC sup_own     && 甌＝癶キ�覃 ��� � 甎�瓷� ��痰�∫┴��
PUBLIC own_val     && 甌＝癶キ��� ���鈞�
PUBLIC popup_n     && ━� POPUP
PUBLIC pad_sav     && ━� �祗��� BAR MENU
PUBLIC bar_        && ���ム �祗���
PUBLIC mode        && 爛Θ�: main, filial + C - か� ����蹕
PUBLIC menu_name   && ━� �牀������ �甅�↓��� �キ�
PUBLIC sys_char    && ��皀�� - ┐キ皋筥��皰� 瓱痰ガ�
PUBLIC acc_level   && 窶�▲�� ぎ痰祚�
PUBLIC f_l_bg      && �ム��� 痰牀�� 筮��
PUBLIC prn_mode    && 爛Θ� �ョ�皋
PUBLIC vk_arch     && ━� ����� �璢│� か� ��
PUBLIC vk_spec     && ━� ����� �璢│� か� ��
PUBLIC new_ord     && �爬Л�� ��〓�� 筮爼��� ��錢��
PUBLIC sav_tit, mod_tit && ��痰�→� � ゥ 爛Θ�
PRIVATE vrs, rg_nom, f_ser, demo, sav_dir, do_arm, mode, m_term, r_nam
sav_tit = ""
mod_tit = ""
*
*
*
demo = .F.
SET DATE GERMAN

DO Set_Lwr          && ������キ┘ ��゛�肓 �ムオ�え牀→� � �└�┤ 爛��痰�

ON READERROR = 1

*
*   �痰���→� え爛�皰爬�
*

DO Set_Proc         && 竅����→� �����モ牀� 痼イ� FoxPro

sav_dir = SET("DEFAULT")+CURDIR()  && 皀�竕�� え爛�皰爬� ( ��釶 む���! )

SET DEFAULT TO (SYS(2004)) && �オ竕�� え爛�皰爬� - え爛�皰爬� FoxPro � �牀�����
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
@ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+" ���竅� 爛����キ皰� ____________________________"
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
    IF STOP              && �皰-皰 �������...
      rc_987 = rc_987+1
      USE
      LOOP
    ENDIF
    dt_w = DATE()
    IF TIME() < "10:00:00"
      dt_w = dt_w-1
    ENDIF
    IF TYPE_LIST = 1   && �メ閾��� ��甎����┘
      str_w = DAY(dt_w)
    ELSE
      str_w = DOW(dt_w)
    ENDIF
    IF SUBSTR(LOG_LIST, str_w, 1) = " "   && �� ∇ガ�...
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
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+" �ィ���キ� "+r_nam
    SET DEVICE TO SCREEN
    SET PRINTER TO PRN:
    &str_w
    CLOSE DATABASES
    SET PRINTER TO (log_path+"NIGHT.TXT") ADDITIVE
    SET DEVICE TO PRINT
    @ PROW()+1, 0 SAY DTOC(DATE())+" "+TIME()+" �ィ���キ� ��▲琥キ"
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

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�            蹍������ 竅����→� �����モ牀� 痼イ� FoxPro.               �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
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
*       �� ∇ガ� �皓�お�!!!
*ON KEY LABEL F5 SUSPEND
*ON KEY LABEL F6 ON KEY LABEL Enter
*ON KEY LABEL F7 QUIT

RETURN

PROCEDURE Del_Com
ON KEY LABEL Esc
RELEASE WINDOW COMMAND
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                ｡��｀砒┴ �荐｀�.                                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
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

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ �牀▲爲� ����腮� �����.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
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
