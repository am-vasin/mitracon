*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  แญฎขญ ๏ ฏเฎฃเ ฌฌ  - ฌฎญจโฎเ                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Monitor
PARAMETERS wk_dir
*
*     ก้จฅ ฏเ ฌฅโเ๋
*
PUBLIC base_path   && ฏใโ์ ช ไ ฉซ ฌ ก ง๋ ค ญญ๋ๅ
PUBLIC tmp_path    && ฏใโ์ ช ฎก้จฌ เ กฎ็จฌ ไ ฉซ ฌ
PUBLIC log_path    && ฏใโ์ ช ฎก้จฌ ไ ฉซ ฌ ฆใเญ ซ 
PUBLIC tmpo_path   && ฏใโ์ ช แฎกแโขฅญญ๋ฌ เ กฎ็จฌ ไ ฉซ ฌ
PUBLIC path_comm   && ฏใโ์ ช ฎก้ฅฉ คจเฅชโฎเจจ
PUBLIC send_log    && ฏใโ์ ช ฆใเญ ซใ ฎโแ๋ซชจ
PUBLIC send_path   && ฏใโ์ ช ไ ฉซ ฌ ฎโแ๋ซชจ
PUBLIC prmo_path   && ฏใโ์ ช แฎกแโขฅญญ๋ฌ ไ ฉซ ฌ ฏ เ ฌฅโเฎข
PUBLIC lwr         && โ กซจๆ  ฏฅเฅชฎคจเฎขชจ ข ญจฆญจฉ เฅฃจแโเ
PUBLIC arm         && ชฎค เ กฎ็ฅฃฎ ฌฅแโ 
PUBLIC arm_nam     &&  งข ญจฅ เ กฎ็ฅฃฎ ฌฅแโ 
PUBLIC arm_rec     && ญฎฌฅเ ง ฏจแจ ข ไ ฉซฅ ARM_LST
PUBLIC user        && ชฎค ฏฎซ์งฎข โฅซ๏
PUBLIC user_Name   && จฌ๏ ฏฎซ์งฎข โฅซ๏
PUBLIC kurs        && โฅชใ้จฉ ชใเแ คฎซซ เ  คซ๏ เ แ็ฅโ  ๆฅญ
PUBLIC kurs_b      && โฅชใ้จฉ ชใเแ คฎซซ เ  คซ๏ เ แ็ฅโฎข แ ชซจฅญโฎฌ
PUBLIC kurs_3      && โฅชใ้จฉ ชใเแ คฎซซ เ  3
PUBLIC kurs_mmvb   && โฅชใ้จฉ ชใเแ 
PUBLIC p_file      && จฌ๏ ไ ฉซ  ฏฅ็ โจ
PUBLIC sup_own     && แฎกแโขฅญญ๋ฉ ชฎค ข แฏจแชฅ ฏฎแโ ข้จชฎข
PUBLIC own_val     && แฎกแโขฅญญ ๏ ข ซ๎โ 
PUBLIC popup_n     && จฌ๏ POPUP
PUBLIC pad_sav     && จฌ๏ ฏใญชโ  BAR MENU
PUBLIC bar_        && ญฎฌฅเ ฏใญชโ 
PUBLIC mode        && เฅฆจฌ: main, filial + C - คซ๏ ฎฌฏํซ
PUBLIC menu_name   && จฌ๏ ฏเฎฃเ ฌฌ๋ ฎแญฎขญฎฃฎ ฌฅญ๎
PUBLIC sys_char    && ซจโฅเ  - จคฅญโจไจช โฎเ แจแโฅฌ๋
PUBLIC acc_level   && ใเฎขฅญ์ คฎแโใฏ 
PUBLIC f_l_bg      && ฏฅเข ๏ แโเฎช  ไฎญ 
PUBLIC prn_mode    && เฅฆจฌ ฏฅ็ โจ
PUBLIC vk_arch     && จฌ๏ ไ ฉซ   เๅจข  คซ๏ 
PUBLIC vk_spec     && จฌ๏ ไ ฉซ   เๅจข  คซ๏ 
PUBLIC new_ord     && ฏเจงญ ช ญฎขฎฃฎ ไฎเฌ โ  ง ๏ขชจ
PUBLIC def_firm    && เฅคฏเจ๏โจฅ ฏฎ ใฌฎซ็ ญจ๎
PUBLIC def_bank    &&  ญช ฏฎ ใฌฎซ็ ญจ๎
PUBLIC df_wchk     && เจงญ ช ็ฅช  ฏฎ ใฌฎซ็ ญจ๎ (คซ๏ ค ญญฎฃฎ ฏเฅคฏเจ๏โจ๏)
PUBLIC lt_wchk     && ๋กฎเ ฏเจงญ ช  ็ฅช  (คซ๏ ค ญญฎฃฎ ฏเฅคฏเจ๏โจ๏) เ งเฅ่ฅญ
PUBLIC enab_firm   && ๋กฎเ ฏเฅคฏเจ๏โจ๏ เ งเฅ่ฅญ
PUBLIC enab_bank   && ๋กฎเ ก ญช  เ งเฅ่ฅญ
PUBLIC nds_flt     && ’ฅชใ้จฉ ฏเฎๆฅญโ(!) ‘
PUBLIC own_mail    && เจงญ ช ญ ซจ็จ๏ แฎกแโขฅญญฎฉ ํซฅชโเฎญญฎฉ ฏฎ็โ๋
PUBLIC choose_inv  && ๋กฎเ ไฎเฌ โ  ฏเจ ฏฅ็ โจ ญ ชซ คญฎฉ
PUBLIC pay_prc     && ’ฅชใ้จฉ ฏเฎๆฅญโ(!) ญ ซฎฃ  แ ฏเฎค ฆ
PUBLIC prc_firm    && ฎค ฏเฅคฏเจ๏โจ๏, ชฎโฎเฎฌใ แฎฎโข. โฅชใ้ฅฅ งญ ็ฅญจฅ pay_prc
PUBLIC kkm_exist   &&  ซจ็จฅ ช แแฎขฎฃฎ  ฏฏ เ โ  ญ  ฏเฅคฏเจ๏โจจ
PUBLIC b_stock     && เฅฌ๏ ญ ็ ซ  เ กฎโ๋ แชซ ค 
PUBLIC e_stock     && เฅฌ๏ ฎชฎญ็ ญจ๏ เ กฎโ๋ แชซ ค 
PUBLIC t_stock     && จญจฌ ซ์ญฎฅ ขเฅฌ๏ ฏฎค ็จ ง ๏ขชจ
PUBLIC hard_sale   && ฅฆจฌ ฎกเ กฎโชจ ง ๏ขฎช
PUBLIC sale_mode   && ฅฆจฌ เ กฎโ๋ ฏเฎฃเ ฌฌ๋
PUBLIC err_111     && ‘็ฅโ็จช คซ๏ 111-ฉ ฎ่จกชจ
PUBLIC user_dep    && ฎค ฎโคฅซ  ฏฎซ์งฎข โฅซ๏
PUBLIC queuetype   && ’จฏ ฎ็ฅเฅคจ คซ๏ ง ๏ขฎช ญ  แชซ ค
PUBLIC showLett	   && ฎช ง๋ข โ์ แฎฎก้ฅญจ๏
PUBLIC n_digit

ON ERROR DO Err_Start WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()
DO Set_Proc        && ใแโ ญฎขช  ฏ เ ฌฅโเฎข แเฅค๋ FoxPro
DO N_Digit
err_111 = 0
SET TALK OFF
PUBLIC sav_tit, mod_tit &&  แโ ขช  จ ฅฅ เฅฆจฌ
PRIVATE vrs_b, vrs_p, rg_nom, f_ser, demo, sav_dir, do_arm, mode, m_term
sav_tit = ""
mod_tit = ""
def_firm = "   "
def_bank = "   "
df_wchk = .F.
lt_wchk = .T.
showLett = .T.
*
*
*
demo = .F.
SET DATE GERMAN
sys_char = SYS(16)
sys_char = SUBSTR(sys_char, RAT(" ",sys_char)+1)
= ADIR(vrs_p, sys_char)
sys_char = DTOC(vrs_p(1, 3))

RELEASE vrs_p

vrs_b = "006"    && ฅเแจ๏ ก ง๋ ค ญญ๋ๅ
vrs_p = "001"    && ฅเแจ๏ ฏเฎฃเ ฌฌ๋
user_name = ""

vrs_p = vrs_b+"."+vrs_p+" ("+sys_char+")"     && ฅเแจ๏
RELEASE sys_char

DO Set_Lwr          && ง ฏฎซญฅญจฅ โ กซจๆ๋ ฏฅเฅชฎคจเฎขชจ ข ญจฆญจฉ เฅฃจแโเ

rg_nom = "78919173"                           && ฎคแโ ญฎขช  1
acc_level = 0
f_ser  = ;
CHR(185)+CHR( 28)+CHR(182)+CHR(223)+CHR(177)+CHR(211)+CHR(209)+CHR(218)+CHR(198)     && ฎคแโ ญฎขช  2
ON READERROR = 1

*
*   “แโ ญฎขช  คจเฅชโฎเจฉ
*
IF FIle_O("RUS.BAT")
  PRIVATE nf, tmpB
  m.nf = FOPEN("RUS.BAT")
  DO WHILE .NOT. FEOF(m.nf)
    m.tmpB = FGETS(m.nf)
    IF .NOT. EMPTY(m.tmpB)
      m.tmpB = "! "+ALLTRIM(m.tmpB)
      &tmpB
    ENDIF
  ENDDO
  = FCLOSE(m.nf)
  RELEASE nf, tmpB
ENDIF

sav_dir = SET("DEFAULT")+CURDIR()  && โฅชใ้ ๏ คจเฅชโฎเจ๏ ( ฏซ๎แ คเ ฉข! )

SET DEFAULT TO (SYS(2004)) && ’ฅชใ้ ๏ คจเฅชโฎเจ๏ - คจเฅชโฎเจ๏ FoxPro จ ฏเฎฃเ ฌฌ
PRIVATE s
s = "! "+SET("DEFAULT")
&s
s = "! CD "+CURDIR()
s = LEFT(s,LEN(s)-1)
&s

prmo_path = sav_dir+"PRMS\"
tmpo_path = sav_dir+"TMP\"
base_path = SYS(2004)+"BASE\"
send_path = SYS(2004)+"SEND\"
tmp_path  = SYS(2004)+"BASE\TMP\"
log_path  = SYS(2004)+"LOG\"

*send_log  = "Z:\SENDMESS\"
*path_comm = "Z:\COMM\"
*send_log  = "Y:\SENDMESS\"

s = base_path+";"+tmpo_path+";"+prmo_path+";"+tmp_path+";"+log_path
SET PATH TO (s)
LOAD ISDISK         &&  ฃเใฆ ฅฌ ฏเฎฃเ ฌฌใ ฏเฎขฅเชจ ญ ซจ็จ๏ คจแชฅโ๋

*
*  ชฎก๋ ใแโ ญฎขช  ชใเแ  คฎซซ เ 
*

prn_mode = .T.
own_mail = .F.
choose_inv = " "
IF File_O(prmo_path+"OWN_PARM.DBF")
  USE (prmo_path+"OWN_PARM")
  m.showLett = IIF(FSIZE("SHOW_LETT") = 0, m.showLett, OWN_PARM.SHOW_LETT)
  m.prn_mode = IIF(FSIZE("PRN_MODE") = 0, m.prn_mode, OWN_PARM.PRN_MODE)
  m.own_mail = IIF(FSIZE("OWN_MAIL") = 0, m.own_mail, OWN_PARM.OWN_MAIL)
  m.choose_inv = IIF(FSIZE("CHOOSE_INV") = 0, m.choose_inv, OWN_PARM.CHOOSE_INV)
  *
  *   prn_mode - เฅฆจฌ ฏฅ็ โจ - .T. - แ ง แโ ขชฎฉ, .F. - กฅง ง แโ ขชจ.
  *
  USE
ENDIF
USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CURS
m.kurs   = Get_Curs("CURS","CURS")
m.kurs_b = Get_Curs("CURS_B","CURS")
m.kurs_3 = Get_Curs("CURS_N","CURS")
m.kurs_mmvb = Get_Curs("CURS_MMVB","CURS")

DO Set_Color        && ใแโ ญฎขช  ๆขฅโฎข
DO Draw_Tit         && ง แโ ขช 
queuetype = Get_Own("QUEUE")
IF EMPTY(queuetype)
  queuetype = " "
ELSE
  queuetype = LEFT(queuetype,1)
ENDIF

b_stock = "00:00"
e_stock = "23:59"
USE (base_path+"PARMS")
m.mode = PARMS.MODE
*new_ord = "NEW_ORD" $ UPPER(PARMS.PARMS_LINE)
new_ord = .NOT. EMPTY(Get_Sys("NEW ORD"))
*kkm_exist = "KKM" $ UPPER(PARMS.PARMS_LINE)
kkm_exist = .NOT. EMPTY(Get_Sys("KKM"))
sale_mode = Get_Sys("SALE_MODE")
sale_mode = ALLTRIM(sale_mode)
IF EMPTY(sale_mode)
  sale_mode = "Compel"
ENDIF
DO Get_Sto WITH queuetype   &&  เ ฌฅโเ๋ แชซ ค 
*IF FSIZE("PARMS_LINE") = 0
*  hard_sale = .F.
*ELSE
*  hard_sale = " HS " $ " "+UPPER(PARMS_LINE)
*ENDIF
hard_sale = .NOT. EMPTY(Get_Sys("HARD SALE"))

sys_char = SYS_ID
send_log  = ALLTRIM(SEND_DIR)
path_comm = ALLTRIM(COMM_DIR)
def_firm  = SYS_FIRM
def_bank  = SYS_BANK
IF vrs_b # VERSION
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅเแจ๏ ก ง๋ คฅญญ๋ๅ ("+VERSION+") ญฅ แฎฎโขฅโแโขใฅโ ขฅเแจจ ฏเฎฃเ ฌฌ๋ ("+vrs_b+")!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SET DEFAULT TO (sav_dir)
  s = "! "+LEFT(sav_dir,2)
  &s
  s = "! CD "+SUBSTR(sav_dir,3)
  s = LEFT(s,LEN(s)-1)
  &s
  QUIT
ENDIF
SELECT 0
IF .NOT. File_O(prmo_path+"O_PARMS.DBF")
  IF .NOT. New_Arm()
    SET DEFAULT TO (sav_dir)
    s = "! "+LEFT(sav_dir,2)
    &s
    s = "! CD "+SUBSTR(sav_dir,3)
    s = LEFT(s,LEN(s)-1)
    &s
    QUIT
  ENDIF
ENDIF
USE (prmo_path+"O_PARMS")
IF LEN(ARM_C) = 1
  arm = STR(ASC(ARM_C),3)
ELSE  
  arm = ARM_C
ENDIF

*
*    ฅฃจแโเจเใฅฌแ๏
USE (base_path+"ARM_LST")
IF LEN(arm) = 1
  LOCATE FOR ARM_C = STR(ASC(arm),3)
ELSE
  LOCATE FOR ARM_C = arm
ENDIF

IF .NOT. EMPTY(ARM_FIRM)
  def_firm  = ARM_FIRM
  def_bank  = ARM_BANK
ENDIF
enab_firm = LET_FIRM
enab_bank = LET_BANK

IF .NOT. FOUND()
  USE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ่ฅ เ กฎ็ฅฅ ฌฅแโฎ ญฅ ง เฅฃจแโเจเฎข ญฎ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  QUIT
ENDIF
arm_nam = ARM_N
arm_rec = RECNO()
IF FSIZE("SYS_NAM") # 0
  IF .NOT. EMPTY(SYS_NAM)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+" ่ฅ เ กฎ็ฅฅ ฌฅแโฎ ใฆฅ ง ญ๏โฎ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    IF LASTKEY() # 22
      SET DEFAULT TO (sav_dir)
      QUIT
    ENDIF
  ENDIF
  IF .NOT. Tst_Date()
    SET DEFAULT TO (sav_dir)
    QUIT
  ENDIF
  SELECT PARMS
  IF PARMS.EXCLU < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+" กฎโ โ์ ญฅซ์ง๏! คฅโ เฅฃซ ฌฅญโ..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    IF LASTKEY() # 22
      QUIT
    ENDIF
    REPLACE PARS.EXCLU WITH 0
  ENDIF

  IF EMPTY(ARM_LST.SYS_NAM)
    REPLACE EXCLU WITH EXCLU+1
  ENDIF
  SELECT ARM_LST
  REPLACE SYS_NAM WITH "เฎค ฆจ", ;
          SYS_DAT WITH DATE(),  ;
          SYS_TIM WITH TIME()
ENDIF
USE (base_path+"ARM_INFO") ORDER TAG ARM_C
PRIVATE m999, m777
m999 = SYS(0)
m999 = LEFT(m999, AT("#",m999)-1)
PRIVATE arm_inf
arm_inf = ""
IF LEN(arm) = 1
  arm_inf = STR(ASC(arm),3)
ELSE
  arm_inf = arm
ENDIF
IF .NOT. SEEK(arm_inf)
  APPEND BLANK
  REPLACE ARM_C WITH arm_inf, NET_NAME WITH m999
  DIMENSION m777(6)
  m777(1) = ""
  m777(2) = CHR(0)+"€…!  งเ กฎโ็จช ขญฎข์ ฎกเ ้ ฅโแ๏ ช  ฌ แ ฏเฎแ์กฎฉ:"
  m777(3) = CHR(0)+"คซ๏ โฎฃฎ, ็โฎ ก๋ ฏเฅฎคฎซฅโ์  ญฅเ งกฅเจๅใ ข ญ งข ญจ๏ๅ เ กฎ็จๅ ฌฅแโ,"
  m777(4) = CHR(0)+"ง ฏฎซญจโฅ, ฏฎฆ ซใฉแโ , กซ ญช, ชฎโฎเ๋ฉ  ฌ กใคฅโ แฅฉ็ แ ฏเฅค๊๏ขซฅญ."
  m777(5) = CHR(0)+"                    เ ญฅฅ กซ ฃฎค เฅญ,  €  แจญ"
  m777(6) = ""
  DO Out_Mess WITH 5, "m777"
  DO Arm_Info WITH arm_inf, .T.
ELSE
  IF EMPTY(NET_NAME)
    REPLACE NET_NAME WITH m999
  ENDIF
ENDIF

IF NET_NAME # m999
  DIMENSION m999(3)
  m999(1) = ""
  m999(2) = CHR(0)+"ฅชฎเเฅชโญฎฅ แจแโฅฌญฎฅ จฌ๏ เ กฎ็ฅฃฎ ฌฅแโ !"
  m999(3) = ""
  DO Out_Mess WITH 5, "m999"
  SET DEFAULT TO (sav_dir)
  CLOSE DATABASES
  QUIT
ENDIF
RELEASE m777, m999
USE
SELECT PARMS
USE

ON ERROR DO Err_Proc WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()

*IF TYPE("wk_dir") # "C"
*  IF FILE(base_path+"bas_list.dbf") .AND. FILE(base_path+"bas_list.fpt")
*    DO Index_Ba WITH base_path, 1
*  ENDIF
*ENDIF

DO Del_Tmp WITH ""
DO Del_Tmp WITH base_path
DO Clr_Tmp
DO Del_Tmp WITH tmp_path

*DO New_Year

p_file    = ""

IF .NOT. FILE(base_path+"bas_list.dbf") .OR. .NOT. FILE(base_path+"bas_list.fpt")
  DO Bas_List WITH base_path
ENDIF

USE (base_path+"CUSTOMER")
LOCATE FOR .NOT. EMPTY(SELF)
IF FOUND()
  sup_own =  CODE
ELSE
  RELEASE sup_own
ENDIF
USE

*
*    ็จแโช  ใแโ เฅข่จๅ ค ญญ๋ๅ - ฏใโ์ ฅ๎ ง ญจฌ ฅโแ๏ แชซ ค!
*
USE (base_path+"PARMS")
*PRIVATE d_min, d_max
*d_min = DAYS_MIN
*d_max = DAYS_MAX
own_val = VALUTA
nds_flt = NDS_
pay_prc = 0
prc_firm = CHR(0)+CHR(0)+CHR(0)
USE
*IF demo
*  d_min = 1
*  d_max = 2
*ENDIF
*DO Del_Mov WITH d_min, d_max
*RELEASE d_min, d_max

vk_arch = IniTrans(1)
vk_spec = IniTrans(2)

*
*     ‘ฎงค ฅฌ ไ ฉซ ฎฏจแ ญจ๏ ญ ฏเ ขซฅญจฉ ฏฅ็ โจ
PRIVATE _stru
DIMENSION _stru(3,4)
_stru(1,1) = "PORT_NAME"
_stru(1,2) = "C"
_stru(1,3) = 8
_stru(1,4) = 0

_stru(2,1) = "NOTE"
_stru(2,2) = "C"
_stru(2,3) = 24
_stru(2,4) = 0

_stru(3,1) = "PRNTYPE"
_stru(3,2) = "N"
_stru(3,3) = 3
_stru(3,4) = 0

IF File_O(prmo_path+"OWN_DIRS.DBF")
  USE (prmo_path+"OWN_DIRS.DBF")
  IF FSIZE("PRNTYPE") = 0
    USE
	ERASE (tmpo_path+'OWN_NEW.DBF')
	CREATE DBF (tmpo_path+'OWN_NEW') FROM ARRAY _stru
	*  ฏฎซญ๏ฅฌ ค ญญ๋ฌจ จง แโ เฎฉ OWN_DIRS
	APPEND FROM (prmo_path+'OWN_DIRS')
	* "ฅเฅจฌฅญฎข๋ข ฅฌ"
	ERASE (prmo_path+'OWN_DIRS.DBF')
	COPY TO (prmo_path+'OWN_DIRS')
	USE (prmo_path+'OWN_DIRS')
	REPLACE ALL PRNTYPE WITH 1 && ฎ ใฌฎซ็ ญจ๎ ขแฅ ฏเจญโฅเ๋ โจฏ  1
  ENDIF
  USE
ELSE
  CREATE DBF (prmo_path+'OWN_DIRS') FROM ARRAY _stru
  *  ฏฎซญ๏ฅฌ ค ญญ๋ฌจ จง แโ เฎฉ OWN_DIRS
  APPEND BLANK
  REPLACE PORT_NAME WITH "PRN:", PRNTYPE WITH 1
  USE
ENDIF

DO WHILE .T.

  *
  *    ็จ้ ฅฌ ฏฎฌฅโชใ ฎ ฏฎซ์งฎข โฅซฅ
  USE (base_path+"ARM_LST")
  GO arm_rec
  IF FSIZE("US_CODE") # 0
    REPLACE US_CODE WITH 0,  ;
            US_DAT  WITH {}, ;
            US_TIM  WITH " "
  ENDIF
  USE
  user_name = ""
  DO Set_Color        && ใแโ ญฎขช  ๆขฅโฎข
  DO Draw_Tit         && ง ฃฎซฎขฎช ญ  ํชเ ญฅ
  DO Draw_Info

  m_term = .F.
  ACTIVATE SCREEN
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
  IF .NOT. Password() && ขขฎค ฏ เฎซ๏ จ จคฅญโจไจช ๆจ๏ ฏฎซ์งฎข โฅซ๏
*   EXIT
    CLEAR
    SET DEFAULT TO (sav_dir)
    s = "! "+LEFT(sav_dir,2)
    &s
    s = "! CD "+SUBSTR(sav_dir,3)
    s = LEFT(s,LEN(s)-1)
    &s
    *
    *    ็จ้ ฅฌ ฏฎฌฅโชใ ฎ เฅฃจแโเ ๆจจ
    USE (base_path+"ARM_LST")
    GO arm_rec
    IF FSIZE("SYS_NAM") # 0
      REPLACE SYS_NAM WITH " ", ;
              SYS_TIM WITH " "
    ENDIF
    USE PARMS
    REPLACE EXCLU WITH MAX(EXCLU-1, 0)
    QUIT
  ELSE
    *
    *    “แโ ญ ขซจข ฅฌ ฏฎฌฅโชใ ฎ ฏฎซ์งฎข โฅซฅ
    USE (base_path+"ARM_LST")
    GO arm_rec
    IF FSIZE("US_CODE") # 0
      REPLACE US_CODE WITH user,   ;
              US_DAT  WITH DATE(), ;
              US_TIM  WITH TIME()
    ENDIF
    USE base_path+"PERSONS" ORDER TAG CODE
    IF SEEK(user)
      user_dep = DEP_NO
      user_name = ALLTRIM(FAMILY)+" "+LEFT(NAME, 1)+" "+LEFT(S_NAME, 1)
    ELSE
      user_dep = 0
      user_name = ""
    ENDIF
    USE
    IF user = 0
      user_name = "‘€ เ กฎโ ฅโ!"
    ENDIF

    IF Sale_Mode = 'MEGA'
      * ฅฃจแโเ ๆจ๏ ข OfficeCom
      * do OCReg  && ฎก ขซฅญฎ €.เ ็ฅข๋ฌ 10.03.2001
    ENDIF

    DO Work_Log WITH "ๅฎค ข แจแโฅฌใ €", user, arm
    DEACTIVATE MENU Main_M
    RELEASE MENU Main_M EXTENDED
    DO NY_Mail   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    DO Main_M WITH base_path+"MENU_LST", menu_name, 4
    bar_    = 0
    popup_n = ""
    pad_sav = ""
    DO WHILE .NOT. m_term

      DO Draw_Tit         && ง แโ ขช 
      DO Draw_Info
      IF showLett
        DO Lett_Get
        DO Draw_Info
      ENDIF
      m_term = .T.
      @ 1, FLOOR((WCOLS("")-15)/2) SAY " แญฎขญฎฅ ฌฅญ๎ "

      IF EMPTY(bar_) .AND. EMPTY(popup_n) .AND. EMPTY(pad_sav)
        ACTIVATE MENU Main_M
      ELSE
        IF EMPTY(bar_)
          ACTIVATE MENU Main_M PAD (pad_sav)
        ELSE
          ACTIVATE POPUP (popup_n) BAR bar_
        ENDIF
      ENDIF

*
*     คฅแ์ ฎกเ กฎโช  ข๋กเ ญญฎฃฎ ฏใญชโ  ฌฅญ๎
*
      CLOSE DATABASES
      CLEAR WINDOWS
      SET DEVICE TO SCREEN
    ENDDO
  ENDIF
  DO Work_Log WITH "๋ๅฎค จง แจแโฅฌ๋ €", user, arm
  DEACTIVATE MENU Main_M
  HIDE MENU Main_M
  RELEASE MENU Main_M
ENDDO

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  ใแโ ญฎขชจ ฏ เ ฌฅโเฎข แเฅค๋ FoxPro.               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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
SET CENTURY   ON
SET SAFETY    OFF
SET EXCLUSIVE OFF
SET MEMOWIDTH TO 250
SET REPROCESS TO 200
SET REFRESH   TO 5
SET DECIMAL   TO 7
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
*         ขเฅฌ๏ ฎโซ คชจ!!!
*ON KEY LABEL F5 SUSPEND
*ON KEY LABEL F6 ON KEY LABEL Enter
*ON KEY LABEL F7 QUIT

RETURN

PROCEDURE Del_Com
ON KEY LABEL Esc
RELEASE WINDOW COMMAND
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  ฏเฎเจแฎขชจ โจโเฎข.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Draw_Tit
PRIVATE new_mod

new_mod = STR(WROWS(""),3)+STR(WCOLS(""),3)+SCHEME(13)

IF new_mod == mod_tit
  RESTORE SCREEN FROM sav_tit
ELSE
  DO Bg_Image
  SAVE SCREEN TO sav_tit
  mod_tit = new_mod
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  ฏเฎเจแฎขชจ จญไฎเฌ ๆจฎญญฎฉ แโเฎชจ.                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Draw_Info

PRIVATE dw, mnth, i, s, d, nLett
DIMENSION dw(7), mnth(12)

dw(1) = "ฎแชเฅแฅญ์ฅ"
dw(2) = "ฎญฅคฅซ์ญจช"
dw(3) = "โฎเญจช"
dw(4) = "‘เฅค "
dw(5) = "—ฅโขฅเฃ"
dw(6) = "๏โญจๆ "
dw(7) = "‘ใกกฎโ "

mnth( 1) = "ญข เ๏"
mnth( 2) = "”ฅขเ ซ๏"
mnth( 3) = " เโ "
mnth( 4) = "€ฏเฅซ๏"
mnth( 5) = " ๏"
mnth( 6) = "๎ญ๏"
mnth( 7) = "๎ซ๏"
mnth( 8) = "€ขฃใแโ "
mnth( 9) = "‘ฅญโ๏กเ๏"
mnth(10) = "ชโ๏กเ๏"
mnth(11) = "ฎ๏กเ๏"
mnth(12) = "ฅช กเ๏"

nLett = Lett_Cnt()

s = STR(DAY(DATE()),2)+" "+mnth(MONTH(DATE()))+   ;
    STR(YEAR(DATE()),5)+", "+dw(DOW(DATE()))+" บ ‘ฎฎก้.: "

@ WROWS("")-1, 0 SAY s COLOR SCHEME 13
IF nLett = 0
  @ WROWS("")-1, COL() SAY "ญฅโ" COLOR SCHEME 13
ELSE
  @ WROWS("")-1, COL() SAY STR(nLett, 3) COLOR SCHEME 7
ENDIF    
s = " บ €/‘ฎโเ.: "+    ;
     IIF(TYPE("arm_nam") = "C", ALLTRIM(arm_nam), "")+"/"+ALLTRIM(user_name)
s = PADR(s, WCOLS("")-COL())
@ WROWS("")-1, COL() SAY s COLOR SCHEME 13
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ        เฎฃเ ฌฌ  ง ฏเฎแ  ฏ เฎซ๏ จ จคฅญโจไจช ๆจจ ฏฎซ์งฎข โฅซ๏.         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Password

PUBLIC base_path   && ฏใโ์ ช ไ ฉซ ฌ ก ง๋ ค ญญ๋ๅ
PUBLIC lwr         && โ กซจๆ  ฏฅเฅชฎคจเฎขชจ ข ญจฆญจฉ เฅฃจแโเ
PUBLIC user        && ชฎค ฏฎซ์งฎข โฅซ๏
PUBLIC menu_name   && จฌ๏ ฏเฎฃเ ฌฌ๋ ฎแญฎขญฎฃฎ ฌฅญ๎

PRIVATE f_name     && ฏฎซญฎฅ จฌ๏ ไ ฉซ 
PRIVATE rghts      && ชฎค ใเฎขญ๏ คฎแโใฏ 
PRIVATE cnt        && แ็ฅโ็จช ฏฎฏ๋โฎช
PRIVATE next_chr   && แซฅคใ๎้จฉ ขฅคฅญญ๋ฉ แจฌขฎซ
PRIVATE pw         && ขขฅคฅญญ๋ฉ ฏ เฎซ์
PRIVATE l1, c1, l2, c2 && ฏฎงจๆจ๏ ฎชญ 
PRIVATE ret        && ขฎงขเ ้ ฅฌฎฅ งญ ็ฅญจฅ
PRIVATE hlp_idx    && จญคฅชแ ฏฎคแช งชจ
PRIVATE hpass, epass && แฎฎก้ฅญจ๏
PRIVATE col1, col2
hlp_idx = "passw"

DIMENSION epass(3), no_acc(3)

epass(1) = ""
epass(2) = CHR(0)+"ฅชฎเเฅชโญ๋ฉ ฏ เฎซ์"
epass(3) = ""

no_acc(1) = ""
no_acc(2) = CHR(0)+" ฌ ฎโช ง ญฎ ข คฎแโใฏฅ ช ฏเฎฃเ ฌฌฅ ฏเฎค ฆ!"
no_acc(3) = ""

ret = .T.
f_name = base_path+"USERS.DBF" && ไ ฉซ ฏฎซ์งฎข โฅซฅฉ

rghts = 0                      && ฅแซจ ไ ฉซ  ญฅโ, โฎ ฌฎฆญฎ ขแฅ
user  = 0
IF FILE(f_name)
  USE (f_name)
  SET ORDER TO TAG PASSWORD
  GO TOP
  IF .NOT. BOF() .AND. .NOT. EOF()  && ฅแซจ ไ ฉซ ฏใแโ, โฎ ฌฎฆญฎ ขแฅ

    *
    *   ฅซ ฅฌ ฎชฎ่ชฎ คซ๏ ขขฎค  ฏ เฎซ๏
    *
*    c1 = PADR(" ฎคแช งช (F1)  ๋ๅฎค(Esc)  เฎคฎซฆจโ์ ",WCOLS(""))
    c1 = PADR(" ๋ๅฎค(Esc) ",WCOLS(""))
    col1 = SCHEME(3,2)
    col2 = SCHEME(3,6)
    @ 0,0 SAY c1 COLOR (col1)
    c2 = 18+FSIZE("PASSWORD")
    c1 = FLOOR((WCOLS()-c2)/2)
    c2 = c1+c2-1
    l1 = FLOOR((WROWS()-5)/2)
    l2 = l1+4
    DEFINE WINDOW Get_Pass FROM l1, c1 TO l2, c2 COLOR SCHEME 13 SHADOW NONE
    ACTIVATE WINDOW Get_Pass
    @ 0, 1 TO WROWS()-1, WCOLS()-2 DOUBLE
    @ 2,5 SAY " เฎซ์?"
    c1 = FLOOR((WCOLS()-13)/2)
    @ 4, c1 SAY " Esc - ข๋ๅฎค "

    *
    *    ฅฌ ชซจฅญโใ โเจ ฏฎฏ๋โชจ...
    *
    c2 = SET("CURSOR") = "OFF"
    SET CURSOR ON
    cnt = 0
    DO WHILE .T.
      pw = ""
      DO WHILE LEN(pw) < FSIZE("PASSWORD")

        *
        *  ชฎก๋ ํๅฎฏฅ็ โ์...
        *
        @ 2, 13 SAY SPACE(FSIZE("PASSWORD")) COLOR SCHEME 1
        @ 2, 13 SAY REPLICATE("ฑ",LEN(pw))
        DO WHILE .T.
          IF SET("MOUSE") = "ON"
            next_chr = INKEY(60,"M")
          ELSE
            next_chr = INKEY(60)
          ENDIF
          IF next_chr = 0
            DO S_Saver WITH " เฎค ฆจ ", 0.1
          ELSE
            EXIT
          ENDIF
        ENDDO
        IF next_chr = -9  && F10
          next_chr = Pseudo_M()
        ENDIF
        IF SET("MOUSE") = "ON" .AND. next_chr = 151 .AND. LASTKEY() # 151
          next_chr = M_Menu()
        ENDIF
        IF next_chr < 0         && จฃญฎเจเใฅฌ ไใญชๆจฎญ ซ์ญ๋ฅ ชซ ขจ่จ
          LOOP
        ENDIF
        DO CASE          && แฌฎโเจฌ,   ็โฎ, แฎกแโขฅญญฎ, ญ ฆ ซจ?
        CASE next_chr = 28  && F1
*          DO Hlp_Mess WITH "passw"
        CASE next_chr = 13  && Enter
          EXIT
        CASE next_chr = 127 && Back Space
          IF LEN(pw) > 0
            pw = LEFT(pw,LEN(pw)-1)
          ENDIF
        CASE next_chr = 27  && Esc
          ret = .F.
          EXIT
        OTHERWISE
          pw = pw+CHR(next_chr)
        ENDCASE
        LOOP
      ENDDO
      IF ret       && ฅแซจ ญฅ Esc, โฎ จ้ฅฌ
        pw = PADR(pw,FSIZE("PASSWORD"))
        pw = SYS(15,lwr,pw)
        IF pw == CHR(118)+CHR(106)+CHR(121)+CHR(99)+CHR(110)+CHR(104)+CHR(32)+CHR(32)
          user  = 0
          rghts = 0
          EXIT
        ELSE
          DO SetPrice WITH "superamv", pw
          IF .NOT. SEEK(pw)
            cnt = cnt+1
            DO Err_Mess WITH "epass"
            IF cnt >= 3
              ret = .F.
              EXIT
            ENDIF
          ELSE
            user  = USER_CODE
            rghts = S_RIGHTS
            IF EMPTY(rghts)
              DO Out_Mess WITH 7, "no_acc"
              ret = .F.
            ELSE
              IF USER_SYS # sys_char
                PRIVATE wr_sys
                DIMENSION wr_sys(3)
                wr_sys(1) = ""
                wr_sys(2) = CHR(0)+"ฎแโใฏ ช แจแโฅฌฅ แ ค ญญฎฃฎ เ กฎ็ฅฃฎ ฌฅแโ   ฌ ง ฏเฅ้ฅญ!"
                wr_sys(3) = ""
                DO Out_Mess WITH 7, "wr_sys"
                *
                *    ็จ้ ฅฌ ฏฎฌฅโชใ ฎ เฅฃจแโเ ๆจจ
                USE (base_path+"ARM_LST")
                GO arm_rec
                IF FSIZE("SYS_NAM") # 0
                  REPLACE SYS_NAM WITH " ", ;
                          SYS_TIM WITH " "
                ENDIF
                USE (base_path+"PARMS")
                REPLACE EXCLU WITH MAX(EXCLU-1, 0)
                QUIT
              ELSE
                USE (base_path+"S_RIGHTS") ORDER TAG RIGHT
                SEEK m.rghts
                IF FOUND()
                  acc_level = VAL(LEFT(S_RIGHTS.R_FLAGS,1))
                ENDIF
                USE
              ENDIF
            ENDIF
            EXIT
          ENDIF
        ENDIF
      ELSE
        EXIT
      ENDIF
    ENDDO
  ENDIF
ENDIF
IF ret
  m.menu_name = rghts
ENDIF
RELEASE WINDOW Get_Pass
CLOSE DATABASES
@ 0,0 CLEAR TO 0, WCOLS()
RETURN ret

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ฏแฅขคฎฌฅญ๎.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Pseudo_M
PRIVATE p1, p2, p3, n_p, s, i1, i2, ret

ACTIVATE SCREEN
SET CURSOR OFF
n_p = 1
*p1 = 14
*p2 = 26
*p3 = 38
p1 = 12
p2 = 24
DO WHILE .T.
  @ 0,0 FILL TO 0, WCOLS() COLOR (col1)
  DO CASE
  CASE n_p = 1
    i1 = 0
    i2 = p1
*  CASE n_p = 2
*    i1 = p1+1
*    i2 = p2
*  CASE n_p = 3
*    i1 = p2+1
*    i2 = p3
  ENDCASE
  @ 0,i1 FILL TO 0, i2 COLOR (col2)
  IF SET("MOUSE") = "ON"
    s = INKEY(0,"M")
  ELSE
    s = INKEY(0)
  ENDIF
  DO CASE
  CASE SET("MOUSE") = "ON" .AND. s = 151 .AND. LASTKEY() # 151
    IF MROW() = 0
      i1 = MCOL()
      IF BETWEEN(i1, 0, p1)
        ret = 27
        EXIT
      ENDIF
*      IF BETWEEN(i1, p1+1, p2)
*        ret = next_chr
*        EXIT
*      ENDIF
      LOOP
    ENDIF
    LOOP
*  CASE s = 19
*    n_p = n_p-1
*    n_p = IIF(n_p<1, 2, n_p)
*    LOOP
*  CASE s = 4
*    n_p = n_p+1
*    n_p = IIF(n_p>2, 1, n_p)
*    LOOP
  CASE s = 13
    DO CASE
    CASE n_p = 1
      ret = 27
*    CASE n_p = 2
*      ret = next_chr
    ENDCASE
    EXIT
  ENDCASE
ENDDO
@ 0,0 FILL TO 0, WCOLS() COLOR (col1)
ACTIVATE WINDOW Get_Pass
SET CURSOR ON
RETURN ret

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  "ฌ๋่จญฎฃฎ" ฏแฅขคฎฌฅญ๎.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_Menu
PRIVATE p1, p2, p3, i1

IF MROW("") # 0
  RETURN next_chr
ENDIF
p1 = 12
p2 = 24
p3 = 38
i1 = MCOL("")
IF BETWEEN(i1, 0, p1)
  RETURN 27
ENDIF
RETURN next_chr

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎฃเ ฌฌ  ข๋ค ็จ แฎฎก้ฅญจ๏ ฎก ฎ่จกชฅ.                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Err_Mess
PARAMETERS ind
DO Out_Mess WITH 7,ind
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ใแโ ญฎขชจ ๆขฅโฎข.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Color
PRIVATE c_f1, c_f2, c_s1, c_s2, c_b1, c_b2, c_m1, c_m2, c_e1, c_e2

IF FILE (prmo_path+"COLORS_N.DBF")
  USE (prmo_path+"COLORS_N")
  c_f1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_f2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_s1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_s2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_b1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_b2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_m1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_m2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_e1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_e2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_x1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_x2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  SKIP
  c_d1 = ALLTRIM(CL)+IIF(BRGHT,"+","")
  c_d2 = ALLTRIM(CL)+IIF(BRGHT,"*","")
  USE
ELSE
  c_f1 = "W"
  c_f2 = "W"
  c_s1 = "N"
  c_s2 = "N"
  c_b1 = "GR+"
  c_b2 = "GR*"
  c_m1 = "B"
  c_m2 = "B"
  c_e1 = "R"
  c_e2 = "R"
  c_x1 = "N+"
  c_x2 = "N*"
  c_d1 = "N"
  c_d2 = "N"
ENDIF
c_main   = c_f1+"/"+c_s2
c_inv    = c_s1+"/"+c_f2
c_bold   = c_b1+"/"+c_s2
c_bold_i = c_b1+"/"+c_f2
c_hlp    = c_b1+"/"+c_m2
c_hlp_i  = c_b1+"/"+c_s2
c_err    = c_b1+"/"+c_e2
c_err_i  = c_b1+"/"+c_s2
c_brws   = c_b1+"/"+c_m2
c_shad   = c_x1+"/"+c_d2

SET COLOR OF SCHEME 1 TO ;
 (c_main+","+c_inv+","+c_main+","+c_main+","+c_main+",,,,,"+c_x1+"/"+c_s2)
*SET COLOR OF SCHEME 13 TO ;
* (c_inv +","+c_main+","+c_inv +","+c_inv+","+c_inv+","+c_bold+","+ ;
*  c_main+",          ,"+c_main+","+c_inv)
SET COLOR OF SCHEME 13 TO ;
 (c_inv +","+c_main+","+c_inv +","+c_inv+","+c_inv+","+c_bold_i+","+ ;
  c_main+",          ,"+c_inv+","++c_x1+"/"+c_f2)
SET COLOR OF SCHEME 14 TO ;
 (c_bold+","+c_bold_i+","+c_bold+",,,,,,,"+c_x1+"/"+c_s2)
SET COLOR OF SCHEME 3 TO (","+c_inv+",,,,"+c_main)
SET COLOR OF SCHEME 4 TO (","+c_inv+","+c_inv+",,,"+c_main+","+c_bold_i)
SET COLOR OF SCHEME 8 TO ;
 (c_inv+","+c_main+","+c_brws+","+c_brws+","+c_brws+",,,,,"+c_x1+"/"+c_f2)
SET COLOR OF SCHEME 2 TO ;
 (","+c_inv+","+c_inv+","+c_inv+",,"+c_main)
SET COLOR OF SCHEME 5 TO ;
 (c_hlp+","+c_hlp_i+","+c_hlp)
SET COLOR OF SCHEME 7 TO ;
 (c_err+","+c_err_i+","+c_err)
FOR i = 1 TO 24
  SET COLOR OF SCHEME i TO (",,,,,,,"+c_shad)
ENDFOR
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                กเ กฎโ็จช ฎ่จกฎช.                                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Err_Proc
PARAMETERS e_numb, e_mess, e_proc, e_line
EXTERNAL ARRAY err_lst
*PUBLIC base_path

IF TYPE("err_111") = "N"  && ๋โ ฅฌแ๏ คฎแโใ็ โ์แ๏ ฏเจ 111-ฉ ฎ่จกชฅ
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

IF INLIST(e_numb, 108, 109)   && ฎญไซจชโ ฌใซ์โจคฎแโใฏ 
  DIMENSION _mess(7)
  _mess(1) = ""
  _mess(2) = CHR(0)+" แ ญฅ คฎฏใแช ๎โ ช ก งฅ ค ญญ๋ๅ!"
  _mess(3) = CHR(0)+" งกฅเจโฅแ์ ข ็ฅฌ คฅซฎ จ ญ ฆฌจโฅ ANY KEY!"
  _mess(4) = CHR(0)+"ซ๏ €  แจญ : "+ALLTRIM(STR(e_numb))+"/"+e_proc+"/"+ALLTRIM(STR(e_line))+"."
  _mess(5) = CHR(0)+DBF()+" "+ALLTRIM(STR(RECNO()))
  _mess(6) = CHR(0)+"€…! Ctrl-Alt-Del, Reset ญฅ ญ ฆจฌ โ์ ญจ ฏเจ ช ชจๅ ฎกแโฎ๏โฅซ์แโข ๅ!!!"
  _mess(7) = ""
  DO Out_Mess WITH 7, "_mess"
  ON ERROR DO Err_Proc WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()
  RETRY
ENDIF
IF e_numb = 125    && ญฅฏเจ๏โญฎแโจ ฏเจ ฏฅ็ โจ
  SET DEVICE TO SCREEN
  DIMENSION _mess(4)
  _mess(1) = ""
  _mess(2) = "                          “แโเฎฉแโขฎ ฏฅ็ โจ ญฅ ฃฎโฎขฎ."
  _mess(3) = "                   [ เฎคฎซฆจโ์ ฏฅ็ โ์ ]   [ เฅชเ โจโ์ ฏฅ็ โ์ ]"
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
*  _mess(2) = CHR(0)+"ฎจๅ ฃเฅๅฎข เ งกฎเ      "
*  _mess(3) = CHR(0)+"แโ ข์โฅ คฎ ฏฎเ๋,       "
*  _mess(4) = CHR(0)+"๋ ฎๆฅญจโฅ ชเ แฎโใ จฃเ๋!"
*  _mess(5) = CHR(0)+"่จกช  "+ALLTRIM(STR(e_numb,5))+" ข ฌฎคใซฅ "+e_proc+"."
*  _mess(6) = ""
  _mess(2) = CHR(0)+"่จกช  "+ALLTRIM(STR(e_numb,5))+" ข ฌฎคใซฅ "+e_proc+"."
  _mess(3) = ""
  DO Out_Mess WITH 7, "_mess"
ENDIF
CLOSE DATABASES
ON ERROR DO Err_Proc WITH ERROR(), MESSAGE(), PROGRAM(), LINENO()
IF PROGRAM(3) = "START_P"
  RETURN TO START_P
ENDIF
RETURN TO MASTER

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                                                                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rght
PRIVATE mss, s, _w, mem_f

mem_f = "SHAB.LB"
IF .NOT. FILE (base_path+mem_f)
  _w = "****-****"
ELSE
  RESTORE FROM (base_path+mem_f) ADDITIVE
ENDIF
*s = Fdd_Ser(SET("DEFAULT"))
DO ClrPrice WITH rg_nom, s

RETURN s = _w

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ง ฏใแช  ข๋กเ ญญฎฃฎ ฏใญชโ  ฌฅญ๎.                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Start_P
PARAMETERS com_str
*
*   com_str - ชฎฌ ญค ;
*
PRIVATE s, p

bar_    = BAR()
popup_n = POPUP()
pad_sav = PAD()

s = LASTKEY()

m_term = .F.

IF s = 13
  *
  *  ชฎก๋ ใแโ ญฎขช  ชใเแ  คฎซซ เ 
  *
  IF TYPE("kurs") = "N"
    USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CURS
    m.kurs   = Get_Curs("CURS","CURS")
    m.kurs_b = Get_Curs("CURS_B","CURS")
    m.kurs_3 = Get_Curs("CURS_N","CURS")
    m.kurs_mmvb = Get_Curs("CURS_MMVB","CURS")
    USE
  ENDIF

  *
  *    “แโ ญ ขซจข ฅฌ ฏฎฌฅโชใ ฎ ไใญชๆจจ
  USE (base_path+"ARM_LST")
  GO arm_rec
  IF FSIZE("FUN_NAM") # 0
    REPLACE FUN_NAM  WITH PROMPT(), ;
            FUN_DAT  WITH DATE(),   ;
            FUN_TIM  WITH TIME()
  ENDIF
  USE
  DO Work_Log WITH PROMPT()+" (€)", user, arm

  s =" "+ ALLTRIM(PROMPT())+" "
  DO Draw_Tit
  DO Draw_Info
  p = FLOOR((WCOLS("")-LEN(s))/2)

  @ 1,p SAY s

*  DEACTIVATE MENU
*  DEACTIVATE POPUP
  HIDE MENU Main_M
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

  IF showLett
    DO Lett_Get
    DO Draw_Info
  ENDIF
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
  &com_str

  *
  *    ็จ้ ฅฌ ฏฎฌฅโชใ ฎ ไใญชๆจจ
  USE (base_path+"ARM_LST")
  GO arm_rec
  IF FSIZE("FUN_NAM") # 0
    REPLACE FUN_NAM  WITH " ", ;
            FUN_DAT  WITH {},  ;
            FUN_TIM  WITH " "
  ENDIF
  USE
  RETURN
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ฏฎแ๋ซชจ ญ  ฏเจญโฅเ ง ค ญญฎฉ แโเฎชจ.                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Prn
PARAMETERS c

PRIVATE mss
DIMENSION mss(3)

mss(1) = ""
mss(2) = PADC("€ ชโฎ กใคฅโ ขชซ๎็ โ์ ฏเจญโฅเ? [ “ฆฅ ขชซ๎็จซจ ] [ จชโฎ ญฅ กใคฅโ ]", WCOLS(""))
mss(3) = ""

DO WHILE .T.
  IF SYS(13) = 'OFFLINE'
    IF Out_Mess(7,"mss") = 1
      LOOP
    ELSE
      RETURN
    ENDIF
  ENDIF
  ??? c
  EXIT
ENDDO

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ฎชฎญ.                                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Def_Win
PARAMETERS w_nam_1, w_nam_2, w_nam_3, ln, wd, m_tit, tit, foot
PRIVATE ln1, l, c, n, l_arr_t, l_arr_f

IF TYPE("tit(1)") # "C"
  l_arr_t = 0
ELSE
  l_arr_t = ALEN(tit)
  DIMENSION tit(l_arr_t)
ENDIF

IF TYPE("foot(1)") # "C"
  l_arr_f = 0
ELSE
  l_arr_f = ALEN(foot)
  DIMENSION foot(l_arr_f)
ENDIF

ln1 = ln+MAX(l_arr_t,1)+MAX(l_arr_f,1)+1

l = CEILING((WROWS("")-ln1)/2)
c = FLOOR((WCOLS("")-wd-4)/2)

DEFINE WINDOW (w_nam_1) FROM l, c TO l+ln1-1, c+wd+3 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (w_nam_1)
@ 0, 0 SAY PADC(m_tit,WCOLS()) COLOR SCHEME 5

IF l_arr_t > 1
  FOR i = 1 TO l_arr_t-1
    @ i, 2 SAY PADC(tit(i), wd)
  ENDFOR
ENDIF

i = MAX(l_arr_t,1)
@ i, 1 TO i+ln+1, WCOLS()-2
IF l_arr_t # 0
  @ l_arr_t, 2 SAY PADC(tit(l_arr_t), wd,"ฤ")
ENDIF

n = WROWS()-l_arr_f-1
IF l_arr_f # 0
  @ n+1, 2 SAY PADC(foot(1), wd,"ฤ")
  FOR i = 2 TO l_arr_f
    @ i+n, 2 SAY PADC(foot(i), wd)
  ENDFOR
ENDIF

l = l+MAX(l_arr_t,1)+1
c = c+2
DEFINE WINDOW (w_nam_2) FROM l, c TO l+ln-1, c+wd-1 NONE COLOR SCHEME 13
DEFINE WINDOW (w_nam_3) FROM l-3, c-2 TO l+ln, c+wd+20   ;
                           NONE COLOR SCHEME 13
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ฎโชเ๋โจ๏ ( แฎงค ญจ๏ ) ไ ฉซ  - ฏใแโ๋่ชจ.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Use_Dummy

IF USED("DUMMY")
  SELECT DUMMY
  RETURN
ENDIF

SELECT 0
IF .NOT. FILE_O(tmpo_path+"DUMMY.DBF")
  CREATE DBF (tmpo_path+"DUMMY") ( F C(1))
  APPEND BLANK
ENDIF
USE (tmpo_path+"DUMMY") EXCLUSIVE
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ฎโชเ๋โจ๏ ( แฎงค ญจ๏ ) ไ ฉซ  - แข๏งชจ.              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Use_Link

IF USED("LINK")
  SELECT LINK
  RETURN
ENDIF

SELECT 0
SET EXCLUSIVE ON
IF .NOT. FILE_O(tmpo_path+"LINK.DBF")
  CREATE DBF (tmpo_path+"LINK") ( LNK N(10))
ELSE
  USE (tmpo_path+"LINK")
ENDIF
SET EXCLUSIVE OFF

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎๅเ ญฅญจฉ ฏเจ ขๅฎคฅ ข ฏเฎฃเ ฌฌใ.                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Entry_P
PARAMETERS ext_menu, own_menu
*
*    ext_menu - จฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎ ข๋ง๋ข ๎้ฅฉ ฏเฎฃเ ฌฌ๋;
*    own_menu - จฌ๏ แฎกแโขฅญญฎฃฎ  แจญๅเฎญญฎฃฎ ฌฅญ๎.
*
PRIVATE e_m, o_m, s

e_m = ""
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    e_m = ext_menu
  ENDIF
ENDIF

o_m = ""
IF TYPE("own_menu") = "C"
  IF .NOT. EMPTY(own_menu)
    o_m = own_menu
  ENDIF
ENDIF

ACTIVATE SCREEN

IF .NOT. EMPTY(e_m)
  HIDE MENU (e_m)
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ENDIF

PUSH KEY CLEAR
IF .NOT. EMPTY(o_m)
  @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
  SHOW MENU (o_m)
  s = "'"+o_m+"'"
  ON KEY LABEL F10 DO F10_Proc WITH &s
ENDIF

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ   ชโจขจง ๆจจ  แจญๅเฎญญฎฃฎ ฌฅญ๎.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE F10_Proc
PARAMETER m_name

PUSH KEY CLEAR
ACTIVATE MENU (m_name)
DEACTIVATE MENU (m_name)
POP KEY

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  ขฎแแโ ญฎขซฅญจ๏ ฏเจ ข๋ๅฎคฅ จง ฏเฎฃเ ฌฌ๋.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Exit_P
PARAMETERS ext_menu, own_menu
*
*    ext_menu - จฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎ ข๋ง๋ข ๎้ฅฉ ฏเฎฃเ ฌฌ๋;
*    own_menu - จฌ๏ แฎกแโขฅญญฎฃฎ  แจญๅเฎญญฎฃฎ ฌฅญ๎.
*
PRIVATE e_m, o_m

e_m = ""
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    e_m = ext_menu
  ENDIF
ENDIF

o_m = ""
IF TYPE("own_menu") = "C"
  IF .NOT. EMPTY(own_menu)
    o_m = own_menu
  ENDIF
ENDIF

ACTIVATE SCREEN

IF .NOT. EMPTY(o_m)
  DEACTIVATE MENU (o_m)
  RELEASE MENU (o_m) EXTENDED
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ENDIF

POP KEY

IF .NOT. EMPTY(e_m)
  @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
  DEACTIVATE MENU (e_m)
  SHOW MENU (e_m)
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฏฅเฅชซ๎็ฅญจ๏ เฅฆจฌ .                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Ch_Mode
PARAMETERS menu_nm, mode_

KEYBOARD CHR(27)
what_do = mode_

IF .NOT. EMPTY(menu_nm)
  DEACTIVATE MENU (menu_nm)
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  - ฎกเ กฎโ็จช คซ๏ ข๋ๅฎค  จง เฅฆจฌ .              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Ex_Proc
PARAMETERS m_name, chr_code
PRIVATE c

IF TYPE("chr_code") = "N" .OR. TYPE("chr_code") = "C"
  c = chr_code
ELSE
  c = 27
ENDIF

KEYBOARD IIF(TYPE("c")="N", CHR(c), c)
IF .NOT. EMPTY(m_name)
  DEACTIVATE MENU (m_name)
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  แฎงค ญจ๏ ฎชญ .                                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE D_W
PARAMETERS w_nam, ln, wd, tit, board
*
*  w_nam - จฌ๏ ฎชญ ;
*  ln    - คซจญ  ฎชญ ;
*  wd    - ่จเจญ  ฎชญ ;
*  tit   - ง ฃฎซฎขฎช;
*  board - เ ฌช : 0 - ญฅ เจแฎข โ์, 1 - ฎคจญ เญ ๏, 2 - คขฎฉญ ๏
*
PRIVATE b_w, c, l
IF TYPE("board") = "N"
  b_w = board
ELSE
  b_w = 1
ENDIF
l = CEILING((WROWS("")-ln)/2)
c = FLOOR  ((WCOLS("")-wd)/2)

DEFINE WINDOW (w_nam) FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (w_nam)
@ 0, 0 SAY PADC(tit,WCOLS()) COLOR SCHEME 5
DO CASE
CASE b_w = 1
  @ 1, 1 TO WROWS()-1, WCOLS()-2
CASE b_w = 2
  @ 1, 1 TO WROWS()-1, WCOLS()-2 DOUBLE
ENDCASE
RETURN

*=========================================================================
PROCEDURE Break_Str	&& P งกจขช  แโpฎชจ ญ  คขฅ
*-------------------------------------------------------------------------
PARAMETERS sr, s1, s2, ln

*   sr - จแๅฎคญ ๏ แโเฎช 
*   s1 - ฏฅเข ๏ แโเฎช 
*   s2 - ขโฎเ ๏ แโเฎช  (ฎแโ โฎช)
*   ln - ฎฃเ ญจ็ฅญจฅ ฏฎ คซจญฅ คซ๏ ฏฅเขฎฉ แโเฎชจ
PRIVATE n

m.s1 = m.sr
IF EMPTY (SUBSTR (m.s1, m.ln, 1)) OR EMPTY (SUBSTR (m.s1, m.ln+1, 1))
  m.s2 = ALLTRIM (SUBSTR (m.s1, m.ln+1))
  m.s1 = ALLTRIM (LEFT (m.s1,m.ln))
ELSE
  m.n = m.ln - 1
  DO WHILE m.n > 0 AND NOT EMPTY (SUBSTR (m.s1, m.n, 1))
    m.n = m.n - 1
  ENDDO
  IF m.n = 0
    m.s2 = ALLTRIM (m.s1)
    m.s1 = ""
  ELSE
    m.s2 = ALLTRIM (SUBSTR (m.s1, m.n))
    m.s1 = ALLTRIM (LEFT (m.s1, m.n))
  ENDIF
ENDIF

m.s1 = PADR (m.s1, m.ln)
RETURN

*=========================================================================
FUNCTION Num2RStr	&& —จแซฎ ฏเฎฏจแ์๎
*-------------------------------------------------------------------------
PARAMETERS num_p
*
*  s   - แโเฎช  ข ชฎโฎเฎฉ ขฎงขเ ้ ฅโแ๏ เฅงใซ์โ โ;
*  t   - ฏฅเฅค ฅโแ๏ เฎค - ( 0 - ฎคจญ, 1 - ฎคญ ), ขฎงขเ ้ ฅโแ๏
*        ขฅซจ็จญ  คซ๏ แข๏งชจ: 0 - โ๋แ๏็, 1 - โ๋แ๏็ , 2 - โ๋แ๏็จ;
*  num_p - ็จแซฎ.
*
PRIVATE s, num, t, t_w, s_tmp
PRIVATE n1, n2, n3

t = 0
num = ABS(num_p)

IF num = 0
  s = "ญฎซ์"
  t = 0
  RETURN s
ENDIF

n3 = num

s_tmp  = FLOOR (n3/1000)		&& 1 - 999
n1 = n3 - s_tmp*1000

n3 = s_tmp
s_tmp  = FLOOR (n3/1000)		&& 1,001 - 999,999
n2 = n3 - s_tmp*1000

n3 = s_tmp				&& 1,000,001 - 999,999,999

s_tmp = ""
IF n1 # 0
  DO Pr_3 WITH s_tmp, t, n1
  s_tmp = s_tmp
ELSE
  t = 0
ENDIF
s = ALLTRIM (Millions (n3)) + IIF (n2 = 0, "", " " +;
             Thousands (n2)) + " " + s_tmp

RETURN IIF(num_p<0, "ฌจญใแ ", "")+ALLTRIM (s)

*-------------------------------------------------------------------------
PROCEDURE Pr_3
*-------------------------------------------------------------------------
PARAMETERS s, t, num
PRIVATE s_tmp, n1, n2, n3

t_w   = 0
n3    = num
s_tmp = FLOOR (n3/10)			&& ฅคจญจๆ๋
n1    = n3 - s_tmp*10

n3    = s_tmp
s_tmp = FLOOR (n3/10)			&& คฅแ๏โชจ
n2    = n3 - s_tmp*10

n3    = s_tmp				&& แฎโญจ

****	‘ฎโญจ

DO CASE
  CASE n3 = 0
    s_tmp = ""
  CASE n3 = 1
    s_tmp = "แโฎ"
  CASE n3 = 2
    s_tmp = "คขฅแโจ"
  CASE n3 = 3
    s_tmp = "โเจแโ "
  CASE n3 = 4
    s_tmp = "็ฅโ๋เฅแโ "
  CASE n3 = 5
    s_tmp = "ฏ๏โ์แฎโ"
  CASE n3 = 6
    s_tmp = "่ฅแโ์แฎโ"
  CASE n3 = 7
    s_tmp = "แฅฌ์แฎโ"
  CASE n3 = 8
    s_tmp = "ขฎแฅฌ์แฎโ"
  CASE n3 = 9
    s_tmp = "คฅข๏โ์แฎโ"
ENDCASE

IF n2 = 1		&& โฎเฎฉ คฅแ๏โฎช
  s_tmp = s_tmp + " "
  DO CASE
    CASE n1 = 0
      s_tmp = s_tmp + "คฅแ๏โ์"
    CASE n1 = 1
      s_tmp = s_tmp + "ฎคจญญ คๆ โ์"
    CASE n1 = 2
      s_tmp = s_tmp + "คขฅญ คๆ โ์"
    CASE n1 = 3
      s_tmp = s_tmp + "โเจญ คๆ โ์"
    CASE n1 = 4
      s_tmp = s_tmp + "็ฅโ๋เญ คๆ โ์"
    CASE n1 = 5
      s_tmp = s_tmp + "ฏ๏โญ คๆ โ์"
    CASE n1 = 6
      s_tmp = s_tmp + "่ฅแโญ คๆ โ์"
    CASE n1 = 7
      s_tmp = s_tmp + "แฅฌญ คๆ โ์"
    CASE n1 = 8
      s_tmp = s_tmp + "ขฎแฅฌญ คๆ โ์"
    CASE n1 = 9
      s_tmp = s_tmp + "คฅข๏โญ คๆ โ์"
  ENDCASE
  t = t_w
  s = s_tmp
  RETURN
ELSE

  ****	ฅแ๏โชจ

  IF n2 > 1
    s_tmp = s_tmp + " "
    DO CASE
      CASE n2 = 2
        s_tmp = s_tmp + "คข คๆ โ์"
      CASE n2 = 3
        s_tmp = s_tmp + "โเจคๆ โ์"
      CASE n2 = 4
        s_tmp = s_tmp + "แฎเฎช"
      CASE n2 = 5
        s_tmp = s_tmp + "ฏ๏โ์คฅแ๏โ"
      CASE n2 = 6
        s_tmp = s_tmp + "่ฅแโ์คฅแ๏โ"
      CASE n2 = 7
        s_tmp = s_tmp + "แฅฌ์คฅแ๏โ"
      CASE n2 = 8
        s_tmp = s_tmp + "ขฎแฅฌ์คฅแ๏โ"
      CASE n2 = 9
        s_tmp = s_tmp + "คฅข๏ญฎแโฎ"
    ENDCASE
  ENDIF

  ****	…คจญจๆ๋

  IF n2 # 1 AND n1 # 0
    s_tmp = IIF (EMPTY (s_tmp), "", s_tmp + " ")
    DO CASE
      CASE n1 = 1
        s_tmp = s_tmp + IIF (t = 0, "ฎคจญ", "ฎคญ ")
        t_w = 1
      CASE n1 = 2
        s_tmp = s_tmp + IIF (t = 0, "คข ",  "คขฅ")
        t_w = 2
      CASE n1 = 3
        s_tmp = s_tmp + "โเจ"
        t_w = 2
      CASE n1 = 4
        s_tmp = s_tmp + "็ฅโ๋เฅ"
        t_w = 2
      CASE n1 = 5
        s_tmp = s_tmp + "ฏ๏โ์"
      CASE n1 = 6
        s_tmp = s_tmp + "่ฅแโ์"
      CASE n1 = 7
        s_tmp = s_tmp + "แฅฌ์"
      CASE n1 = 8
        s_tmp = s_tmp + "ขฎแฅฌ์"
      CASE n1 = 9
        s_tmp = s_tmp + "คฅข๏โ์"
    ENDCASE
  ENDIF
ENDIF
t = t_w
s = s_tmp
RETURN

*-------------------------------------------------------------------------
PROCEDURE Millions
*-------------------------------------------------------------------------
PARAMETERS num
PRIVATE t, s_tmp

t     = 0
s_tmp = ""
DO Pr_3 WITH s_tmp, t, num

IF NOT EMPTY (s_tmp)
  DO CASE
    CASE t = 0
      s_tmp = s_tmp + " ฌจซซจฎญฎข"
    CASE t = 1
      s_tmp = s_tmp + " ฌจซซจฎญ"
    OTHERWISE
      s_tmp = s_tmp + " ฌจซซจฎญ "
  ENDCASE
ENDIF
RETURN s_tmp

*-------------------------------------------------------------------------
PROCEDURE Thousands
*-------------------------------------------------------------------------
PARAMETERS num
PRIVATE t, s_tmp

t     = 1
s_tmp = ""
DO Pr_3 WITH s_tmp, t, num

IF NOT EMPTY (s_tmp)
  DO CASE
    CASE t = 0
      s_tmp = s_tmp + " โ๋แ๏็"
    CASE t = 1
      s_tmp = s_tmp + " โ๋แ๏็ "
    OTHERWISE
      s_tmp = s_tmp + " โ๋แ๏็จ"
  ENDCASE
ENDIF
RETURN s_tmp

*=========================================================================
FUNCTION  Capital	&&  ฏจโ ซจง ๆจ๏ 1-ฉ กใชข๋ แโเฎชจ
*-------------------------------------------------------------------------
PARAMETER tmpStr
PRIVATE tmpStr

*RETURN UPPER (LEFT (tmpStr, 1)) + SUBSTR (tmpStr, 2)

RETURN RUpper (LEFT (tmpStr, 1)) + SUBSTR (tmpStr, 2)

*=========================================================================
FUNCTION RUpper		&& ฅเฅขฎค แโเฎชจ ข ขฅเๅญจฉ เฅฃจแโเ
*-------------------------------------------------------------------------
PARAMETER tmpStr
PRIVATE tmpStr, tmpLower, tmpUpper

tmpLower = "abcdefghijklmnopqrstuvwxyz กขฃคฅฆงจฉชซฌญฎฏเแโใไๅๆ็่้๊๋์ํ๎๏"
tmpUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ€…‘’“”•–—"

RETURN CHRTRAN (tmpStr, tmpLower, tmpUpper)

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฏเฎขฅเชจ ญ ซจ็จ๏ ไ ฉซ .                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE File_O
PARAMETERS f_nam

PRIVATE arr_w
RETURN adir(arr_w, f_nam) > 0

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฎฏเฅคฅซฅญจ๏ เ งฌฅเ  ไ ฉซ .                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE File_Size
PARAMETERS f_nam

PRIVATE arr_w
IF ADIR(arr_w, f_nam) = 0
  RETURN 0
ELSE
  RETURN arr_w(1,2)
ENDIF

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ    เฎฃเ ฌฌ  ฎฏเฅคฅซฅญจ๏ แขฎกฎคญฎฃฎ จ ฎก้ฅฃฎ ฏเฎแโเ ญแโข  ญ  คจแชฅ.   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Disk_Space
PARAMETERS drv, free_space, full_space
PRIVATE dir_sav, n_p, free_w
n_p = PARAMETERS()

dir_sav = SET("DEFAULT")+CURDIR()
SET DEFAULT TO (drv)

free_w = DISKSPACE()

IF n_p = 1
  SET DEFAULT TO (dir_sav)
  RETURN free_w
ENDIF

free_space = free_w

IF n_p = 3
  full_space = VAL(SYS(2020))
ENDIF

SET DEFAULT TO (dir_sav)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ฎชฎญ คซ๏ BROWSE.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE D_Wins
PARAMETERS ln, wd, m_tit, l_tit, l_foot
PRIVATE ln1, l, c, l_tit_w, l_foot_w

l_tit_w  = IIF(TYPE("l_tit")  = "N", l_tit,  0)
l_foot_w = IIF(TYPE("l_foot") = "N", l_foot, 0)
l_foot_w = IIF(l_foot_w = 0, 0, l_foot_w+2)

ln1 = ln+3+l_tit_w+l_foot_w
ln1 = MIN(ln1, WROWS("")-3)

l = CEILING((WROWS("")-ln1)/2)
c = FLOOR((WCOLS("")-wd-4)/2)

*
*    ‘ฎงค ฅฌ จ ง ฏฎซญ๏ฅฌ ฎชญฎ - ฏฎคซฎฆชใ
*
DEFINE WINDOW (win_name+"_M") FROM l, c TO l+ln1-1, c+wd+3 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (win_name+"_M")
@ 0, 0 SAY PADC(m_tit,WCOLS()) COLOR SCHEME 5

@ l_tit_w+1, 1 TO WROWS()-l_foot_w-1, WCOLS()-2
@ WROWS()-l_foot_w-1, 2 FILL TO WROWS()-l_foot_w-1, WCOLS()-2 COLOR (SCHEME(13,6))
@ l_tit_w+1, WCOLS()-2 FILL TO WROWS()-l_foot_w-1, WCOLS()-2 COLOR (SCHEME(13,6))

IF l_foot_w # 0
  @ WROWS()-l_foot_w, 1 TO WROWS()-1, WCOLS()-2
  @ WROWS()-1, 2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))
  @ WROWS()-l_foot_w, WCOLS()-2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))
ENDIF

*
*  ‘ฎงค ฅฌ ขญฅ่ญฅฅ จ ขญใโเฅญญฅฅ ฎชญ  คซ๏ BROWSE.
*
l   = l+2+l_tit_w
c   = c+2
ln1 = ln1-3-l_foot_w-l_tit_w

DEFINE WINDOW (win_name+"_E") FROM l, c TO l+ln1-1, c+wd-1 NONE COLOR SCHEME 13
DEFINE WINDOW (win_name+"_I") FROM l-3, c-2 TO l+ln1, c+wd+20   ;
                           NONE COLOR SCHEME 13
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ญ ขจฃ ๆจฎญญฎฉ แเฅค๋ คซ๏ BROWSE.           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prp_Nav_1

ACTIVATE SCREEN
IF EMPTY(menu_name)
  @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ELSE
  @ 0, 0 SAY PADL("ฅญ๎ - F10 ",WCOLS()) COLOR SCHEME 13
  SHOW MENU (menu_name)
  ON KEY LABEL F10 DO To_Menu
ENDIF

ON KEY LABEL LeftMouse DO Mouse
ACTIVATE WINDOW (win_name+"_E")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎๆฅคใเ   ชโจขจง ๆจจ  แจญๅเฎญญฎฃฎ ฌฅญ๎.               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE To_Menu

IF EMPTY(menu_name)
  RETURN
ENDIF

PUSH KEY CLEAR
ACTIVATE MENU (menu_name)
DEACTIVATE MENU (menu_name)
POP KEY

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎๆฅคใเ  - ฎกเ กฎโ็จช ฌ๋่ชจ.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Mouse
PRIVATE ml, mc, s

PUSH KEY CLEAR

IF INLIST(stat_type, 2, 3)   && BROWSE
  *
  *     ฎคญฎฉ จง แโเฎช Browse
  *
  ml = MROW(win_name+"_E")
  mc = MCOL(win_name+"_E")
  IF ml >= 0 .AND. mc >= 0
    IF stat_type = 3         && ซ๏ ฌฅญ๎ ฏเฎขฅเ๏ฅฌ ญ  คขฎฉญฎฅ ญ ฆ โจฅ
      s = SECONDS()
      IF s - last_mouse < _DBLCLICK
        KEYBOARD CHR(13)
      ELSE
        last_mouse = s
      ENDIF
    ENDIF
    POP KEY
    RETURN
  ENDIF

  ml = MROW("")
  mc = MCOL("")
  IF BETWEEN(mc, WLCOL(win_name+"_E"), WLCOL(win_name+"_E")+WCOLS(win_name+"_E")-1)
    IF ml = WLROW(win_name+"_E")-1
      = INKEY("M")
      KEYBOARD "{PgUp}"
      POP KEY
      RETURN
    ENDIF
    IF ml = WLROW(win_name+"_E")+WROWS(win_name+"_E")
      = INKEY("M")
      KEYBOARD "{PgDn}"
      POP KEY
      RETURN
    ENDIF
  ENDIF
  IF ml = 0 .AND. .NOT. EMPTY(menu_name)
    DO To_Menu
  ELSE
    = INKEY("M")
  ENDIF
  POP KEY
  RETURN
ENDIF
IF stat_type = 1       && ซ ญช
  IF MROW() < 0 .OR. MCOL() < 0
    = INKEY("M")
  ENDIF
ENDIF
POP KEY
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎๆฅคใเ  ฏฅเฅชซ๎็ฅญจ๏ เฅฆจฌ .                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sw_Mode
PARAMETERS mode

what_do = mode
KEYBOARD CHR(23)
IF .NOT. EMPTY(menu_name)
  DEACTIVATE MENU (menu_name)
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ฎชญ  คซ๏ กซ ญช .                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE D_Win_N
PARAMETERS ln, wd, m_tit
PRIVATE l, c

l = CEILING((WROWS("")-ln)/2)
c = FLOOR((WCOLS("")-wd)/2)

*
*    ‘ฎงค ฅฌ จ ง ฏฎซญ๏ฅฌ ฎชญฎ
*
DEFINE WINDOW (win_name) FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW (win_name)
@ 0, 0 SAY PADC(m_tit,WCOLS()) COLOR SCHEME 5

@ 1, 1 TO WROWS()-1, WCOLS()-2
@ WROWS()-1, 2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))
@ 1, WCOLS()-2 FILL TO WROWS()-1, WCOLS()-2 COLOR (SCHEME(13,6))

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎๆฅคใเ  แฎงค ญจ๏ ญ ขจฃ ๆจฎญญฎฉ แเฅค๋ คซ๏ กซ ญช .           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prp_Nav_2

ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
ON KEY LABEL LeftMouse DO Mouse

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎๆฅคใเ  ฏเฎเจแฎขชจ เ ฌชจ แ ง ฃฎซฎขชฎฌ.                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Draw_Box
PARAMETERS l1, c1, l2, c2, tit

@ l1, c1        TO l2, c2
@ l2, c1+1 FILL TO l2, c2 COLOR (SCHEME(13,6))
@ l1, c2   FILL TO l2, c2 COLOR (SCHEME(13,6))

IF TYPE("tit") = "C"
  @ l1, c1+2 SAY tit
ENDIF

RETURN
*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Tst_Date      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      เฎขฅเช  ชฎเเฅชโญฎแโจ ค โ๋.                       บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 01/11/1999 ออผ
PROCEDURE Tst_Date
PRIVATE mss

DO CASE
CASE EMPTY(SYS_DAT)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ‘ฅฃฎคญ๏ "+DTOC(DATE())+". โฎ ฏเ ขจซ์ญฎ? [   ] [ ฅโ ]"
  mss(3) = ""
  RETURN Out_Mess(7,"mss") = 1
CASE BETWEEN(DATE()-SYS_DAT, 0, 40)
  RETURN .T.
OTHERWISE
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"ฎแซฅคญจฉ เ ง ๋ เ กฎโ ซจ "+DTOC(SYS_DAT)+",   แฅฃฎคญ๏ "+DTOC(DATE())+"."
  mss(3) = CHR(0)+"  โฎ ฏเ ขจซ์ญฎ? [   ] [ ฅโ ]"
  mss(4) = ""
  RETURN Out_Mess(7,"mss") = 1
ENDCASE

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ข๋ค ็จ แฎฎก้ฅญจ๏.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Out_Mess
PARAMETERS c_sch, ind
*
* c_sch - ญฎฌฅเ ๆขฅโฎขฎฉ แๅฅฌ๋
* ind   - จญคฅชแ แฎฎก้ฅญจ๏
*
PRIVATE ln, l, w_sav, c_sav, alt, st_w, i1, i2, ind_w, j1, j2, i, s_d
IF TYPE(ind+"(1)") # "C"   && ญฅ ฌ แแจข
  RETURN
ENDIF
*IF c_sch = 7
*  ! spp €…
*ENDIF
s_d = SET("DEVICE") = "PRINTER"
SET DEVICE TO SCREEN
c_sav = SET("CURSOR") = "ON"
SET CURSOR OFF
ln = ALEN(&ind)            && ็จแซฎ แโเฎช ข แฎฎก้ฅญจจ
DIMENSION ind_w(ln)

*
*                ฎจแช  ซ์โฅเญ โจข
*
PRIVATE alt_n, alt_lst, max_w, c0
alt_n = 0
max_w = 0
FOR l = 1 TO ln
  st_w = &ind(l)
  max_w = MAX(max_w, LEN(st_w))
  ind_w(l) = st_w
ENDFOR
max_w = MAX(max_w+12, 34)
max_w = MIN(max_w+12, WCOLS(""))
FOR l = 1 TO ln   && ฏเฎแฌ โเจข ฅฌ ฏฎแซฅคฎข โฅซ์ญฎ แโฎชจ แฎฎก้ฅญจ๏ จ...
  st_w = ind_w(l)
  max_w = MAX(max_w, LEN(st_w))
  IF LEFT(st_w,1) = CHR(0)
    st_w = PADC(SUBSTR(st_w,2),max_w-2)
  ENDIF
  ind_w(l) = ""   && ฏฅเฅฏจแ๋ข ฅฌ จๅ ข เ กฎ็จฉ ฌ แแจข จ ฏเจ ํโฎฌ:
  DO WHILE .NOT. EMPTY(st_w)
    i1 = AT("[",st_w)   && จ้ฅฌ ญ ็ ซฎ ฎ็ฅเฅคญฎฉ  ซ์โฅเญ โจข๋
    IF i1 # 0           && ฅแซจ ญ ่ซจ โฎ...
      ind_w(l) = ind_w(l)+LEFT(st_w,i1-1)
      st_w = SUBSTR(st_w,i1+1)
      i2 = AT("]",st_w)   && จ้ฅฌ ชฎญฅๆ ํโฎฉ  ซ์โฅเญ โจข๋
      IF i2 # 0
        alt_n = alt_n+1
        DIMENSION alt_lst(alt_n,3)
        alt_lst(alt_n,1) = l
        alt_lst(alt_n,2) = LEN(ind_w(l))+1
        ind_w(l) = ind_w(l)+LEFT(st_w,i2-1)
        alt_lst(alt_n,3) = LEN(ind_w(l))
        st_w = SUBSTR(st_w,i2+1)
      ELSE
        ind_w(l) = ind_w(l)+st_w
        st_w = ""
      ENDIF
    ELSE
      ind_w(l) = ind_w(l)+st_w
      st_w = ""
    ENDIF
  ENDDO
ENDFOR
alt = alt_n > 1
*
*   ฅซ ฅฌ ฎชญฎ
*
w_sav = WOUTPUT()
ACTIVATE SCREEN
l = CEILING((WROWS()-ln-2)/2)
c0 = FLOOR((WCOLS()-max_w)/2)
DEFINE WINDOW w_mess FROM l, c0 TO l+ln+1, c0+max_w-1 COLOR SCHEME c_sch ;
                                SHADOW NONE
ACTIVATE WINDOW w_mess
@ 0,0 TO WROWS()-1, WCOLS()-1 DOUBLE
l = FLOOR((WCOLS()-IIF(alt,32,23))/2)
@ WROWS()-1, l SAY IIF(alt, " ๋กฅเจโฅ ฎโขฅโ จ ญ ฆฌจโฅ Enter ", ;
                            "  ฆฌจโฅ ซ๎กใ๎ ชซ ขจ่ใ ")
FOR l = 1 TO ln
  @ l, 1 SAY ind_w(l)
ENDFOR
i1 = 0
IF alt              && ฅแซจ แฎฎก้ฅญจฅ แ  ซ์โฅเญ โจข ฌจ...
  i1 = 1
  st_w = SCHEME(c_sch,2)
  DO WHILE .T.
    i1 = IIF(i1<1, alt_n, i1)
    i1 = IIF(i1>alt_n, 1, i1)
    @ alt_lst(i1,1),alt_lst(i1,2) FILL TO alt_lst(i1,1),alt_lst(i1,3) ;
          COLOR (st_w)
    i2 = INKEY(0,"M")
    IF i2 = 19 .OR. i2 = 4 .OR. i2 = 151
       @ alt_lst(i1,1),alt_lst(i1,2) FILL TO alt_lst(i1,1),alt_lst(i1,3) ;
            COLOR SCHEME c_sch
    ENDIF
    IF i2 = 28
      IF TYPE("hlp_idx") # "C"
        LOOP
      ENDIF
*      DO Hlp_Mess WITH hlp_idx
    ENDIF
    IF i2 = 19     && แโเฅซช  ขซฅขฎ
      i1 = i1-1
    ENDIF
    IF i2 = 4      && แโเฅซช  ขฏเ ขฎ
      i1 = i1+1
    ENDIF
    IF i2 = 13     && Enter
      EXIT
    ENDIF
    IF i2 = 151    && ฌ๋่ช 
      j1 = MROW()
      j2 = MCOL()
      FOR i = 1 TO alt_n
        IF j1 = alt_lst(i,1) .AND. ;
           BETWEEN(j2,alt_lst(i,2),alt_lst(i,3))
          i1 = i
          KEYBOARD CHR(13)
          EXIT
        ENDIF
      ENDFOR
    ENDIF
  ENDDO
ELSE
  i1 = 0
  = INKEY(0,"M")
ENDIF
RELEASE WINDOW w_mess
IF .NOT. EMPTY(w_sav)
  ACTIVATE WINDOW (w_sav)
ENDIF
IF c_sav
  SET CURSOR ON
ENDIF
IF s_d
  SET DEVICE TO PRINTER
ENDIF
RETURN i1

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  Err_Start     งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                    ็ ซ์ญ๋ฉ ฎกเ กฎโ็จช ฎ่จกฎช                          ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 04.04.2008 ฤฤู
PROCEDURE Err_Start
PARAMETERS eNum, eMss, ePrg, eLine

CLEAR
? "่จกช  ü "+STR(eNum)+"  ฏเฎฃเ ฌฌ  "+ePrg+"  ‘โเฎช  "+STR(eLine)
? eMss
? "ซ๏ ข๋ๅฎค  จง ฏเฎฃเ ฌฌ๋ ญ กฅเจโฅ QUIT จ ญ ฆฌจโฅ Enter"
SUSPEND
QUIT
