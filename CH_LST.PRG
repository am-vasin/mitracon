*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Ch_Lst       맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                       귣‘� 飡젺쩆設��． 召ⓤ첓.                       �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 07.09.1999 컴�
PROCEDURE Ch_Lst
PARAMETERS flgs, c_code, f00, b00, nm, usd, chc

PRIVATE s_sav, mss

flgs = SPACE(16)
s_sav = SELECT()
IF .NOT. File_O(prmo_path+"LST_TYPE.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"뜢� �� �ㄽ�� 쭬．獸˚�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  flgs = ""
  RETURN
ENDIF
SELECT 0
USE (prmo_path+"LST_TYPE") ALIAS L___0 AGAIN
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"뜢� �� �ㄽ�� 쭬．獸˚�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  flgs = ""
  USE
  SELECT (s_sav)
  RETURN
ENDIF
IF RECCOUNT() # 1
  SELECT 0
  DO Use_Dummy
  SELECT L___0

  *
  *  룯誓Д��瑜 貰飡�輾⑨ ㄻ� 췅˘짛與�
  *

  PRIVATE stat_type     && 뮜� 췅˘짛與�: 0 - �α�젺쩆設�硫;
                                          1 - ∥젺�;
                                          2 - BROWSE - �젩エ��;
                                          3 - BROWSE - 召ⓤ��.
  PRIVATE what_do       && 닾� 誓┬쵟.
  PRIVATE menu_name     && 닾� 졹Þ魚����． Д��.
  PRIVATE last_mouse    && 귖�э ��笹ⅳ�ⅲ� 췅쬊殊� ㎖�黍��� き��え.
  PRIVATE win_name      && 닾� �き� ( �ぎ� ㄻ� BROWSE ).
  PRIVATE s_sav         && 뜮Д� �젩�曄� �∥졹殊 ㄻ� 貰魚젺��⑨ � ¡遜�젺�˙��⑨!

  *
  *   뇿��キ畑� ㎛좂��⑨Ж ��誓Д��瑜 貰飡�輾⑨...
  *
  stat_type  = 3
  menu_name  = PROGRAM()
  last_mouse = 0
  win_name   = PROGRAM()

  *
  *   렞誓ㄵワ�� 졹Þ魚����� Д��
  *
  DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

  DEFINE PAD Choose OF (menu_name) PROMPT "귣□졻�" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

  DEFINE PAD Exit OF (menu_name) PROMPT "롡첓쭬筍碎" KEY Ctrl-A, "Esc"
  ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

  *
  *   맆淞β �젳Д昔� �き�
  *
  PRIVATE ln, wd
  ln = MIN(RECCOUNT(), WROWS("")-11)   && 뒶エ曄飡¡ ˘ㄸщ� 飡昔� BROWSE
  wd = FSIZE("NAME")

  DO D_Wins WITH ln, wd, "귣‥黍收 쭬．獸˚�", 0, 0
  what_do = "List"

  DO WHILE .T.

    DO CASE

    CASE what_do = "List"    && 뤲�細�循 召ⓤ첓

      statys_type = 3
      DO Prp_Nav_1
      ON KEY LABEL Enter KEYBOARD CHR(23)

  *
  *    BROWSE - Д��
  *
     BROWSE FIELDS NAME,     ;
            DUMMY.F:H="" FREEZE DUMMY.F ;
            NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
            WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

    CASE what_do = "Choose"    && 뤲�細�循 召ⓤ첓

      EXIT

    OTHERWISE

      flgs = ""
      EXIT

    ENDCASE

  ENDDO

  RELEASE WINDOW (win_name+"_I")
  RELEASE WINDOW (win_name+"_E")
  RELEASE WINDOW (win_name+"_M")
  RELEASE MENU (menu_name) EXTENDED
ENDIF
IF LEN(flgs) # 0
  flgs   = FLAGS
  c_code = CUS_CODE
  f00    = FIRM
  b00    = BANK
  nm     = VarLName(LST_NAME)  && NEW 22.12.1999 룼ㅱ�젺�˚� ��誓Д��音
  usd    = IS_USD
  chc    = IS_CHECK
  IF c_code # 0
    USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS L___0 AGAIN
    SEEK c_code
    nm = ALLTRIM(CUS_NAME)+" "+ALLTRIM(nm)
  ENDIF
ENDIF
*
*   �お侁졻���履� (젶ゃ�졻���履�!) 쭬む猶젰� DBF-�젵ル.
*
USE
SELECT (s_sav)

RETURN

* && NEW 22.12.1999
* 뜝�ⓤ젺� �. 꾝좂ⅱ臾 ㄻ� 獸．, 譽�〓 � ��誓嶺� 召ⓤぎ�,
* 貰ℓ젺�音 췅 �說�´ �젩ギ���, 〓ギ �젳���□젳��.
* 뮔∥ⓩ� ��ㅱ�젺�¡�:
* \� - �졹, \� - Ж�呻�, \� - 醒ゃ�ㅻ,
* \깕� - ．� (4 與菴�), \� - ．� (2 與菴�), \� - Д碎� (2 與菴�)
*
FUNCTION VarLName  && 룼ㅱ�젺�˚� 收ゃ蟯� 쩆瞬 � №�Д�� � º� 召ⓤ첓
PARAMETER LName
PRIVATE s_date, s_day, s_month, s_year4, s_year2, s_hour, s_min, s_sec, s_lname
s_dtos = DTOS(DATE())
s_day = SUBSTR(s_dtos,7,2)
s_month = SUBSTR(s_dtos,5,2)
s_year2 = SUBSTR(s_dtos,3,4)
s_year4 = SUBSTR(s_dtos,1,4)
s_Time = TIME()
s_hour = SUBSTR(s_time,1,2)
s_min = SUBSTR(s_time,4,2)
s_sec = SUBSTR(s_time,7,2)
s_Lname = STRTRAN(LName, '\�', s_day)
s_Lname = STRTRAN(s_LName, '\�', s_month)
s_Lname = STRTRAN(s_LName, '\깕�', s_year4)
s_Lname = STRTRAN(s_LName, '\�', s_year2)
s_Lname = STRTRAN(s_LName, '\�', s_hour)
s_Lname = STRTRAN(s_LName, '\�', s_min)
s_Lname = STRTRAN(s_LName, '\�', s_sec)

RETURN s_LName
