*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� H_Price      맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                 뤲�｀젹쵟 ⇔�쩆 "ㄾ昔．˘㎛�" ㄵ�젷ⅸ                  �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 19.11.1999 컴�
PROCEDURE H_Price  
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

DEFINE PAD Corr OF (menu_name) PROMPT "뒶薛ⅹ殊昔쥯筍" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr" 

DEFINE PAD Add  OF (menu_name) PROMPT "꽡줎˘筍" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add  OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del  OF (menu_name) PROMPT "뱾젷ⓥ�" KEY Ctrl-A, "Del"
ON SELECTION PAD Del  OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "귣若�" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
DELETE FILE (tmpo_path+"H_PRICE.DBF")
DELETE FILE (tmpo_path+"H_PRICE.CDX")
CREATE DBF (tmpo_path+"H_PRICE.DBF") ;
     ( CHAR_C     C( 1),   ;
       NAME_C     C(24),   ;
       USD0       C( 8),   ;
       USD1       C( 8))
INDEX ON USD1 TAG USD1       
SELECT 0
IF FILE_O(base_path+"O_Bound.DBF")
  USE (base_path+"O_Bound") ORDER TAG H_BOUND 
ELSE
  CREATE DBF (base_path+"O_Bound");
             (CHAR_C  C(  1), ;
              NAME_C  C( 24), ;
              H_BOUND N(8,2)) 
  INDEX ON H_BOUND TAG H_BOUND
  USE (base_path+"O_Bound") ORDER TAG H_BOUND 
  APPEND BLANK
  REPLACE NAME_C  WITH "띲 �曄�� ㄾ昔（�!", ;                
          H_BOUND WITH 99999.99
ENDIF          
  
prv = ""
SCAN
  SELECT H_Price 
  APPEND BLANK
  REPLACE CHAR_C  WITH O_Bound.CHAR_C,;
          NAME_C  WITH O_Bound.NAME_C,;
          USD0    WITH prv,;
          USD1    WITH IIF(O_Bound.H_BOUND = 99999.99,"� �泣�",STR(O_Bound.H_BOUND,8,2))
  prv = STR(O_Bound.H_BOUND+0.01,8,2)       
  SELECT O_Bound
ENDSCAN         
SELECT H_Price
*
*   맆淞β �젳Д昔� �き�
*
*   꺺젮졿⑨ "ㄾ昔．˘㎛�" ㄵ�젷ⅸ ㄻ� 쭬琠��
* �뫤뜝㎖젺�Δ컴컴컴컴컴컴컴컴컴컴컴롡컴컴꽡컴커
* 쿦 XXXXXXXXXXXXXXXXXXXXXXXX     0.00 99999.99�
* 쿦 XXXXXXXXXXXXXXXXXXXXXXXX 99999.99 99999.99�
* 쿦 XXXXXXXXXXXXXXXXXXXXXXXX 99999.99 99999.99�
* 쿦 XXXXXXXXXXXXXXXXXXXXXXXX 99999.99 99999.99�
* 쿦 XXXXXXXXXXXXXXXXXXXXXXXX 99999.99 � 귣蜈  �
* 읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴켸
PRIVATE ln, wd
ln = WROWS("")-7   && 뒶エ曄飡¡ ˘ㄸщ� 飡昔� BROWSE
wd = 44

DO D_Wins WITH ln, wd, '꺺젮졿⑨ "ㄾ昔．˘㎛�" ㄵ�젷ⅸ ㄻ� 쭬琠��', 0, 0
what_do = "List"
@ 1,2 SAY "뫅Б훾젳쥯��Δ컴컴컴컴컴컴컴컴롡컴컴컴훲�컴"  
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 뤲�細�循 召ⓤ첓

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins DO Sw_Mode WITH "Add"
    ON KEY LABEL Del DO Sw_Mode WITH "Del"
    
*
*    BROWSE - Д��
*    
 BROWSE FIELDS CHAR_C:H="":4,  ;
    		   NAME_C:H="":21,  ;
    		   USD0:H="",    ;
    		   USD1:H="",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

   CASE what_do = "Corr"    && 뒶薛ⅹ殊昔쥯筍

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Price
    ENDIF
    what_do = "List"

  CASE what_do = "Add"       && 꽡줎˘筍

    DO Price WITH .T.
    what_do = "List"

  CASE what_do = "Del"       && 뱾젷ⓥ�
    IF .NOT. BOF() .AND. .NOT. EOF()
      IF USD1 = "� �泣�  "
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"앪� 쭬�ⓤ� 蝨젷汀� ��レ㏆!!!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
      ELSE
        DELETE 
      ENDIF
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

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+" 뫌魚젺ⓥ� �㎚����⑨? [ 꽑 ] [ 뜢� ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 1
  DO END_H
ENDIF  
SELECT O_BOUND    
USE  
SELECT H_PRICE
USE
DELETE FILE (tmpo_path+"H_PRICE.DBF")
DELETE FILE (tmpo_path+"H_PRICE.CDX")
SELECT (s_sav)

    
*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� END_H        맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                            뤲�璵ㅳ�� �音�쩆                            �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 23.11.1999 컴�
PROCEDURE END_H

SELECT O_BOUND
SET ORDER TO
RECALL ALL
GO TOP
SELECT H_PRICE
SET ORDER TO
SCAN
  SELECT O_BOUND
  IF EOF()   
    APPEND BLANK 
  ENDIF
  IF H_PRICE.USD1 = "� �泣�  "
    prv = 99999.99
  ELSE
    prv = VAL(ALLTRIM(H_PRICE.USD1))
  ENDIF  
  REPLACE  CHAR_C  WITH H_PRICE.CHAR_C, ;
           NAME_C  WITH H_PRICE.NAME_C, ;
           H_BOUND WITH prv
  SKIP
  SELECT H_PRICE            
ENDSCAN         
SELECT O_BOUND
DELETE REST
RETURN

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ��  Price                                                     �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�            뒶薛ⅹ與�/ㄾ줎˙���� 召ⓤ첓 ｀젮졿Ŀ "ㄾ昔．˘㎛�"          �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 17.11.1999 컴�
PROCEDURE Price  
PARAMETERS new
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

*
*   뇿��キ畑� ㎛좂��⑨Ж ��誓Д��瑜 貰飡�輾⑨...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    뫌ㄵ逝졻�レ췅� �졹筍 �昔｀젹щ:
*

PRIVATE ex, c_char, c_name, c_bound, r_sav, v_sav, is_last
ex = 1
IF new
  c_char  = SPACE(1)
  c_name  = SPACE(24)
  c_bound = 0
  is_last = .F.  
ELSE
  c_char  = CHAR_C
  c_name  = NAME_C
  c_bound = IIF(USD1="� �泣�  ",99999.99,VAL(ALLTRIM(USD1)))
  is_last = USD1 = "� �泣�  "
    
ENDIF

PUSH KEY CLEAR       && 뜝 ㏇篆Ł ��쬊惜硫 笹晨젵!
DO Prp_Nav_2
DO D_Win_N WITH 8, 52, "뇿��キⓥ� ∥젺�!"
* 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
* � 뫅Б�� X  뜝㎖젺�� XXXXXXXXXXXXXXXXXXXXXXXX   �
* �                                               �
* �            눑좂���� (USD) 99999.99            �
* �                                               �
* �      < OK Ctrl-W > < 롡첓쭬筍碎 Esc >         �
* 읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�

*      궋�� ��ゥ� ∥젺첓
*

@ 2,5 SAY "뫅Б��" GET c_char   
@ 2,15 SAY "뜝㎖젺�� " GET c_name 
@ 4,16 SAY "눑좂���� (USD)" GET c_bound PICTURE "99999.99" WHEN .NOT. is_last
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? 롡첓쭬筍碎 Esc "
READ CYCLE VALID Test_Blank()

IF ex = 1
  *
  * 롡�젩졻猶젰� ∥젺�
  * 
  IF new 
    APPEND BLANK 
  ENDIF
  REPLACE CHAR_C WITH c_char,;
          NAME_C WITH c_name,;
          USD1   WITH IIF(c_bound = 99999.99,"� �泣�", STR(c_bound,8,2))
  r_sav = RECNO()             
  v_sav = ""
  SCAN
    REPLACE USD0 WITH v_sav
    v_sav = VAL(ALLTRIM(USD1))+0.01
    v_sav = STR(v_sav,8,2)
  ENDSCAN
  GO r_sav  
ENDIF

POP KEY
RELEASE WINDOW (win_name)
RETURN

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Test_Blank   맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                       뤲�´夕� �音�쩆 �� ∥젺첓                        �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 19.11.1999 컴�
PROCEDURE Test_Blank
PRIVATE r_sav, s_sav

IF ex = 1
  s_sav = IIF(new,0,RECNO())
  IF c_bound => 99999.99 .AND. .NOT. is_last
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"뜢� �젶�� 璵��!!!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF  
  SELECT H_PRICE
  r_sav = RECNO()
  SCAN FOR RECNO() # s_sav
    IF c_char == H_PRICE.CHAR_C 
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"뮔ぎ� 歲Б�� 拾� α筍! 귣‥黍收 첓ぎ�-�Æ蝨� ㅰ膝��..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      IF BETWEEN(r_sav,1,RECCOUNT()) 
        GO r_sav
      ENDIF    
      RETURN .F.
    ENDIF
    IF c_name == H_PRICE.NAME_C 
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"뮔ぎ� 췅㎖젺�� 拾� α筍! 귣‥黍收 첓ぎ�-�Æ蝨� ㅰ膝��..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss" 
      IF BETWEEN(r_sav,1,RECCOUNT()) 
        GO r_sav
      ENDIF    
      RETURN .F.
    ENDIF
    IF STR(c_bound,8,2) = H_PRICE.USD1
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"뮔ぎ� ㎛좂���� 拾� α筍!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss" 
      IF BETWEEN(r_sav,1,RECCOUNT()) 
        GO r_sav
      ENDIF    
      RETURN .F.
    ENDIF
  ENDSCAN
  IF BETWEEN(r_sav,1,RECCOUNT()) 
    GO r_sav
  ENDIF    
ENDIF
RETURN .T.
