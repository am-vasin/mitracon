*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� CheckNum     맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                "꽖芯も�瑜" ��Д�� ㄵ�ⅵ�音 ㄾゃД�獸�.                 �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 06.03.2002 컴�
PROCEDURE CheckNum
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

*
*   뇿��キ畑� ㎛좂��⑨Ж ��誓Д��瑜 貰飡�輾⑨...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

PRIVATE s_sav
s_sav = SELECT()
*------------------------------------------------------------------------
*    뫌ㄵ逝졻�レ췅� �졹筍 �昔｀젹щ:
*

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�            꺇�   9999                �
*�                                      �
*퀛誓ㄿ黍汀�� .... ....:....!....:. (F1)�
*�                                      �
*�      旼 뮜� ㄾゃД��� 컴컴컴커       �
*�      �                       �       �
*�      �  ( ) 뤲ⓨ�ㄽ硫 �西��  �       �
*�      �  ( ) 맆悚�ㄽ硫 �西��  �       �
*�      �  ( ) 댂ㄵめ. �쳽收┬  �       �
*�      �  ( ) �/� (說∥�)      �       �
*�      �  ( ) �/� (USD)        �       �
*�      �  ( ) 몼�胥젺�         �       �
*�      �  ( ) 뿥�              �       �
*�      읕컴컴컴컴컴컴컴컴컴컴컴�       �
*�                                      �
*�    < OK Ctrl-W > < 롡첓쭬筍碎 Esc >  �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸

PRIVATE ex, year_d, f_c000, f_n000, f_c_old, pay_t, w1, pay_doc
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyz젩∀ㄵ坐㎤øガ���褻栒鴨鉛僥幽日全"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ�걗깂낊냷닀뒎뙇럮릲뮄뵓뼏삕슋쐻옝"
ex = 1
year_d = STR(year(DATE()),4)
f_c000 = "   "
f_n000 = SPACE(16)
f_c_old = f_c000
w1 = 1
pay_t = " "
pay_doc = "INC"
*------------------------------------------------------------------------
PUSH KEY CLEAR       && 뜝 ㏇篆Ł ��쬊惜硫 笹晨젵!
DO Prp_Nav_2
DO D_Win_N WITH 20, 41, ""
DO Sun_Bord WITH  7,  8, 16, 32, " 뮜� ㄾゃД��� "
*------------------------------------------------------------------------
*      궋�� ��ゥ� ∥젺첓
*
@  3,  14 SAY "꺇�   " GET year_d PICTURE "9999" VALID S_Year(year_d)
DO Get_Firm WITH 5, 2, .T.
@  9, 10 GET w1 PICTURE "@*VR �/O ;�/O ;댂�.�쳽�. ;�/� (說�) ;�/� (USD) ;뿥� ;�/�"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? 롡첓쭬筍碎 Esc "

READ CYCLE VALID Tst_F()

IF ex = 1

  DO CASE
    CASE w1 = 1
      pay_t = " "
      pay_doc = "INC"
    CASE w1 = 2
      pay_t = " "
      pay_doc = "EXP"
    CASE w1 = 3
      pay_t = "3"
    CASE w1 = 4
      pay_t = "1"
    CASE w1 = 5
      pay_t = "4"
    CASE w1 = 6
      pay_t = "�"
    CASE w1 = 7
      pay_t = "�"
  ENDCASE
  
  DO PREP_001 WITH pay_t, pay_doc
  DO DEF_NUM

ENDIF

POP KEY
RELEASE WINDOW (win_name)

SELECT (s_sav)
RETURN

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Tst_F        맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                   뒶薛ⅹ狩�飡� 쭬��キ��⑨ ∥젺첓.                      �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 11.03.2002 컴�
PROCEDURE Tst_F
PRIVATE s, mss
IF READKEY() % 256 = 12
  ex = 2
ENDIF
IF ex # 1
  RETURN .T.
ENDIF
IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"귣 �� 丞젳젷� �誓ㄿ黍汀��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
RETURN .T.

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� S_Year       맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                          뤲�´夕� ⇔�쩆 ．쩆.                          �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 11.03.2002 컴�
PROCEDURE S_Year
PARAMETERS t_dat

PRIVATE mss, d_001
d_001 = YEAR(DATE())

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

SELECT 0
USE (base_path+"PARMS") ALIAS P_2307 AGAIN
s_dat = YEAR(DAT_START)
USE

IF VAL(t_dat) < s_dat
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"닾ⅴ� ㄾゃД�瞬 獸レぎ � "+STR(s_dat,4)+" ．쩆!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF VAL(t_dat) > d_001
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"뫁．ㄽ� ι� 獸レぎ "+STR(d_001,4)+" ．�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� DEF_NUM      맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                  뫍ⓤ�� ��Д昔� ㄵ�ⅵ�音 ㄾゃД�獸�.                   �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 06.03.2002 컴�
PROCEDURE DEF_NUM
PRIVATE r_n, d_n, rec_n, cnt_r
cnt_r = 0

SELECT NUM_001
GO TOP
r_n = 1
SCAN
  d_n = RIGHT(DOC_N,9)
  IF r_n # ABS(VAL(d_n))
    REPLACE BAD WITH .T.
    cnt_r = cnt_r + 1
    IF .NOT.BOF()
      rec_n = RECNO()
      SKIP -1
      REPLACE BAD WITH .T.
      GO rec_n
    ENDIF
  ENDIF
  r_n = ABS(VAL(d_n)) + 1
ENDSCAN

IF cnt_r = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"뜢� �� �ㄽ�． ㄵ芯も��． ㄾゃД���!"
  mss(3) = ""
  DO Wt_Mess 
  DO Out_Mess WITH 5, "mss"
  USE
  RETURN
ELSE
  DO VIEW_DEF
ENDIF

SELECT NUM_001
USE

RETURN

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� VIEW_DEF     맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�              뤲�細�循 召ⓤ첓 ��Д昔� ㄵ�ⅵ�音 ㄾゃД�獸�.              �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 06.03.2002 컴�
PROCEDURE VIEW_DEF
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
*   롡む猶젰� �젵ル �.�., 信�젺젪エ쥯�� 聲筌� � �. �.
*

SELECT 0
DO Use_Dummy

*
*   렞誓ㄵワ�� 졹Þ魚����� Д��
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View_D OF (menu_name) PROMPT "꽡ゃД��" KEY Ctrl-A, "Enter"
ON SELECTION PAD View_D OF (menu_name) DO Sw_Mode WITH "Prnt"

DEFINE PAD Filter_D OF (menu_name) PROMPT "꽖芯も�瑜 ��Д��/귗�" KEY Ctrl-A, "F1"
ON SELECTION PAD Filter_D OF (menu_name) DO Sw_Mode WITH "Fltr"

DEFINE PAD Exit OF (menu_name) PROMPT "귣若�" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

SELECT NUM_001
SET ORDER TO TAG DOC_N
*
*   맆淞β �젳Д昔� �き�
*
PRIVATE ln, wd, tit_doc, tit_firm, flt
flt = .F.
ln = WROWS("")-11   && 뒶エ曄飡¡ ˘ㄸщ� 飡昔� BROWSE
wd = 68 
tit_doc = "뤲ⓨ�ㄽ硫 �西��"
tit_firm = f_n000
DO CASE
  CASE pay_t = " "
    IF pay_doc = "INP"
      tit_doc = "뤲ⓨ�ㄽ硫 �西��"
    ENDIF
    IF pay_doc = "EXP"
      tit_doc = "맆悚�ㄽ硫 �西��"
    ENDIF
  CASE pay_t = "3"
    tit_doc = "댂ㄵめ. �쳽收┬"
  CASE pay_t = "1"
    tit_doc = "겈㎛젷①�瑜 �쳽收┬ (說∥�)"
  CASE pay_t = "4"
    tit_doc = "겈㎛젷①�瑜 �쳽收┬ (USD)"
  CASE pay_t = "�"
    tit_doc = "몼�胥젺�"
  CASE pay_t = "�"
    tit_doc = "뿥�"
ENDCASE    
    
DO D_Wins WITH ln, wd, "뤲ⅳ�黍汀��:  "+tit_firm, 0, 0
@ 1, 3 SAY "뮜� ㄾゃД���:  "+tit_doc
@ 2, 2 SAY "컴N ㄾ�-�졘컴컴훲졻졘컴컴컴컴컴훹エ��遂컴컴컴컴컴컴컴컴컴휃僧쵟"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 뤲�細�循 召ⓤ첓

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1 DO Sw_Mode WITH "Fltr"

*    BROWSE - Д��
*
*                  (LEFT(DOC_N,1)+ALLTRIM(RIGHT(DOC_N,9))):H="":10
    BROWSE FIELDS s0001=IIF(BAD,"�"," "):H="":1,;  
                  s0002=(LEFT(DOC_N,1)+ALLTRIM(RIGHT(DOC_N,9))):H="":10,  ;
                  DOC_D:H="":10,  ;
                  ACC_N:H="":30,  ;
                  SUM_T:H="":P=IIF(pay_t="3".OR.pay_t="4","@Z 999999999.999","@Z 9999999999.99"),;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Prnt")
    ENDIF

  CASE what_do = "Prnt"    && 룯�졻� ㄾゃД���

    PRIVATE d_num
    d_num = DOC_N
    DO P_money WITH f_c000, pay_t, d_num, year_d
    what_do = "List"

  CASE what_do = "Fltr"    && 뵪レ循 : ㄵ芯も�瑜 ��Д��/㏇�

    IF .NOT. flt
      SET FILTER TO BAD
      flt = .T.
    ELSE
      SET FILTER TO
      flt = .F.
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� PREP_001     맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                       뫌ℓ젺�� №�Д���． �젵쳽.                       �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 06.03.2002 컴�
PROCEDURE PREP_001
PARAMETERS t_001, d_001
PRIVATE key_s

DO Wt_Mess WITH "룼ㄳ�獸˚� 召ⓤ첓..."

SELECT 0
DELETE FILE (tmpo_path+"NUM_001.DBF")
DELETE FILE (tmpo_path+"NUM_001.CDX")
CREATE DBF (tmpo_path+"NUM_001.DBF") ;
      (DOC_N C(10),  ;
       DOC_D D,      ;  
       DOC_T C( 1),  ; 
       ACC_C N( 7),  ;
       ACC_N C(50),  ;
       SUM_T N(15,3),;
       BAD   L       )
INDEX ON DOC_N TAG DOC_N

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2306 AGAIN

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS MON_2306 AGAIN
SET RELATION TO CUS_CODE INTO ACC_2306

key_s = f_c000+t_001+year_d

SELECT MON_2306
IF d_001 = "INC"
  SET FILTER TO .NOT."-" $ DOC_NUM
  SCAN FOR FIRM+DOC_TYPE+LEFT(DTOS(DOC_DATE),4)+DOC_NUM = key_s
    SELECT NUM_001
    APPEND BLANK
    REPLACE DOC_N WITH MON_2306.DOC_NUM,  ;
            DOC_D WITH MON_2306.DOC_DATE, ;  
            ACC_C WITH MON_2306.CUS_CODE, ;
            ACC_N WITH ACC_2306.CUS_NAME, ;
            SUM_T WITH MON_2306.MONEY_R+MON_2306.N_OF_SALE
    SELECT MON_2306
  ENDSCAN
ENDIF

IF d_001 = "EXP"
  SET FILTER TO "-" $ DOC_NUM
  SCAN FOR FIRM+DOC_TYPE+LEFT(DTOS(DOC_DATE),4)+DOC_NUM = key_s
    SELECT NUM_001
    APPEND BLANK
    REPLACE DOC_N WITH MON_2306.DOC_NUM,  ;
            DOC_D WITH MON_2306.DOC_DATE, ;  
            ACC_C WITH MON_2306.CUS_CODE, ;
            ACC_N WITH ACC_2306.CUS_NAME, ;
            SUM_T WITH MON_2306.MONEY_R+MON_2306.N_OF_SALE
    SELECT MON_2306
  ENDSCAN
ENDIF

IF d_001 = " "
  SCAN FOR FIRM+DOC_TYPE+LEFT(DTOS(DOC_DATE),4)+DOC_NUM = key_s
    SELECT NUM_001
    APPEND BLANK
    REPLACE DOC_N WITH MON_2306.DOC_NUM,  ;
            DOC_D WITH MON_2306.DOC_DATE, ;  
            ACC_C WITH MON_2306.CUS_CODE, ;
            ACC_N WITH ACC_2306.CUS_NAME, ;
            SUM_T WITH IIF(t_001="3".OR.t_001="4",MON_2306.MONEY_D,;
                           MON_2306.MONEY_R+MON_2306.N_OF_SALE)
    SELECT MON_2306
  ENDSCAN
ENDIF

SELECT MON_2306
USE

SELECT ACC_2306
USE

DO Wt_Mess

RETURN