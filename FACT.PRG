*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Fact         맆㏅젩�洵Ø 깲エ췅 꽟ⓥ黍��ぎ                 �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                 뤲�｀젹쵟 �昔細�循� "ㄵ芯も�音" 淞β��-�젶栒�          �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 10.01.2000 컴�
PROCEDURE Fact

PRIVATE dat_1, dat_2, d_min, d_max

SELECT 0
USE (base_path+"PARMS")
STORE DAT_START TO dat_1, d_min
STORE DATE() TO dat_2, d_max
USE 

IF G_Period(dat_1,dat_2)
  DO List_Bill WITH dat_1, dat_2
ENDIF

RETURN

*�袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴�
*�                                                                        �
*�                        뫌ℓ젺�� №�Д���． �젵쳽                       �
*�                                                                        �
*훤袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴�06.01.2000 暠
PROCEDURE List_Bill
PARAMETERS d_1, d_2
  
SELECT 0
DELETE FILE (tmpo_path+"BILL_FAC.DBF")
DELETE FILE (tmpo_path+"BILL_FAC.CDX")
CREATE DBF (tmpo_path+"BILL_FAC.DBF") ;
      (FIRM  C( 3), ;
       DOC_T C( 1), ; 
       DOC_N C(10), ;
       DOC_D D( 8), ;  
       BAD   L      )
INDEX ON FIRM+DOC_T+LEFT(DTOS(DOC_D),4)+DOC_N TAG DOC_N_0 FOR BAD ADDITIVE
INDEX ON FIRM+DOC_T+LEFT(DTOS(DOC_D),4)+DOC_N TAG DOC_N_1 ADDITIVE

DO Use_Dummy

SELECT 0
USE (base_path+"BILL_F.DBF") ORDER TAG HRON ALIAS H_A000

IF .NOT. SEEK(DTOS(d_1))
  IF BETWEEN(RECNO(0),1,RECCOUNT())
    GO RECNO(0)
  ENDIF  
ENDIF
SCAN REST WHILE DOC_DATE <= d_2
  SELECT BILL_FAC 
  APPEND BLANK
  REPLACE FIRM  WITH H_A000.FIRM,     ;
          DOC_T WITH H_A000.DOC_TYPE, ; 
          DOC_N WITH H_A000.DOC_NUM,  ;
          DOC_D WITH H_A000.DOC_DATE

  SELECT H_A000
ENDSCAN  

SELECT H_A000
USE

PRIVATE n_doc, f_doc, d_doc, c_doc, k_doc, sw
SELECT BILL_FAC 
INDEX ON FIRM+DOC_T+LEFT(DTOS(DOC_D),4)+DOC_N TAG DOC_N

sw = .F.

GO TOP

n_doc = VAL(ALLTRIM(RIGHT(DOC_N,9)))-1
f_doc = FIRM
d_doc = LEFT(DTOS(DOC_D),4)
c_doc = LEFT(DOC_N,1)
k_doc = 0
SCAN 
  sw = .F.
  IF FIRM+LEFT(DTOS(DOC_D),4)+DOC_N # f_doc+d_doc+c_doc+STR(n_doc+1,9) 
    IF FIRM # f_doc
      n_doc = VAL(ALLTRIM(RIGHT(DOC_N,9)))
      f_doc = FIRM
      d_doc = LEFT(DTOS(DOC_D),4)
      c_doc = LEFT(DOC_N,1)
      LOOP
    ENDIF  
    IF LEFT(DTOS(DOC_D),4) # d_doc
      IF DOC_N = c_doc+"        1"
        n_doc = VAL(ALLTRIM(RIGHT(DOC_N,9)))
        f_doc = FIRM
        d_doc = LEFT(DTOS(DOC_D),4)
        c_doc = LEFT(DOC_N,1)
        LOOP
      ENDIF              
    ENDIF
    n_doc = VAL(ALLTRIM(RIGHT(DOC_N,9)))
    f_doc = FIRM
    d_doc = LEFT(DTOS(DOC_D),4)
    c_doc = LEFT(DOC_N,1)
    sw = .T.
  ELSE
    c_doc = LEFT(DOC_N,1)
    n_doc = n_doc + 1    
  ENDIF         
 
  IF sw 
    REPLACE BAD WITH .T.
    k_doc = k_doc + 1
  ENDIF       
          
  n_doc = VAL(ALLTRIM(RIGHT(BILL_FAC.DOC_N,9))) 
  f_doc = BILL_FAC.FIRM
  d_doc = LEFT(DTOS(BILL_FAC.DOC_D),4)
  SELECT BILL_FAC
  
ENDSCAN  

IF k_doc = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"뜢� �� �ㄽ�． ㄵ芯も��． ㄾゃД���!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"

ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"귗ⅲ� ㄵ芯も�音 ㄾゃД�獸�: "+STR(k_doc)
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  DO Def_Doc
ENDIF

SELECT BILL_FAC
USE
DELETE FILE (tmpo_path+"BILL_FAC.DBF")
DELETE FILE (tmpo_path+"BILL_FAC.CDX")

RETURN

*�袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴�
*�                                                                        �
*�                        꽖芯も�瑜 淞β�-�젶栒贍                         �
*�                                                                        �
*훤袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴 10.01.2000 暠
PROCEDURE Def_Doc

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
stat_type  = 3
what_do    = ""
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()


SELECT BILL_FAC

*
*   맆淞β �젳Д昔� �き�
*
*   뫍ⓤ�� 淞β��-�젶栒� � "����젪Œ彛臾�" ��Д�젹�
* �붲�컴묊β-�젶栒�졘컴꽑�졘커
* 쿦XX   XXXXXXXXXX  XXXXXXXX�
* 쿦XX   XXXXXXXXXX  XXXXXXXX�
* 쿦XX   XXXXXXXXXX  XXXXXXXX�
* 읕컴컴컴컴컴컴컴컴컴컴컴컴켸
*
*   렞誓ㄵワ�� 졹Þ魚����� Д��
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Switch OF (menu_name) PROMPT "꽖芯も�瑜/귗�" KEY Ctrl-A, "Tab"
ON SELECTION PAD Switch OF (menu_name) Do Sw_Mode WITH "Switch"

DEFINE PAD Exit OF (menu_name) PROMPT "귣若�" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

PRIVATE ln, wd
ln = WROWS("")-7   && 뒶エ曄飡¡ ˘ㄸщ� 飡昔� BROWSE
wd = 29

DO D_Wins WITH ln, wd, "뫍ⓤ��", 0, 0
@ 1,2 SAY "휇席컴묊β-�젶栒�졘컴훲졻졘"
what_do = "List"

SET ORDER TO DOC_N_0
GO TOP
DO WHILE .T.
  DO CASE
  CASE what_do = "List"
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab Do Sw_Mode WITH "Switch"

*
*    BROWSE - Д��
*    
 BROWSE FIELDS FIRM:H="":3,   ;
               DOC_N:H="":12, ;
               DOC_D:H="":10,  ;
               S000=IIF(BAD,"�"," "):H="":1,;  
               DUMMY.F:H="" FREEZE DUMMY.F ;
               NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
               WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Prnt")
    ENDIF
  CASE what_do = "Switch"
    IF TAG() = "DOC_N_0"
      SET ORDER TO DOC_N_1
    ELSE
      SET ORDER TO DOC_N_0
    ENDIF
    what_do = "List"
  OTHERWISE 
    EXIT
  ENDCASE
ENDDO    
CLEAR WINDOWS

RETURN
