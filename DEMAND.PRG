*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ ��｀硅 甌 甎牀甌�.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Demand

PRIVATE what_do
*
*  �. �. - �き�━キ�襯 ����:
PRIVATE sto_code, prefix, name, date, qty, price, cli_code,  ;
        client, function, amodel, who, where, fc
PRIVATE i001, i002, sav_val

HIDE MENU MAIN_M
DO Use_Dummy

SELECT 0

*USE DEMAND ORDER TAG WHERE

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
SELECT 0
USE (base_path+"DEMAND") ORDER TAG WHO
SET RELATION TO CLI_CODE INTO ACCOUNT
SCATTER MEMVAR BLANK
GO BOTTOM

*
*    PAD - �キ� �ム〓�� 窶�↓�
*

DEFINE MENU Demand IN SCREEN COLOR SCHEME 4

DEFINE PAD Edit_P OF Demand PROMPT "�イ��皋牀��碎"  KEY Ctrl-A, "Enter"
ON SELECTION PAD Edit_P OF Demand DO Ch_Mode WITH "Demand", "edit"

DEFINE PAD Add_P OF Demand PROMPT "����※碎"  KEY Ctrl-A, "Ins"
ON SELECTION PAD Add_P  OF Demand DO Ch_Mode WITH "Demand", "add"

DEFINE PAD Del_P OF Demand PROMPT "�����碎"  KEY Ctrl-A, "Del"
ON SELECTION PAD Del_P  OF Demand DO Ch_Mode WITH "Demand", "del"

DEFINE PAD Exit_P OF Demand PROMPT "�諷��"  KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit_P OF Demand DO Ch_Mode WITH "Demand", "exit"

DO Entry_P WITH "", "Demand"

what_do = "list"
DO Prp_Wins
DO WHILE .T.
  DO CASE
  CASE what_do = "list"
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("�キ� F10 ", WCOLS()) COLOR SCHEME 13
    SHOW MENU DEMAND
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Ch_Mode WITH "Demand", "add"
    ON KEY LABEL Del   DO Ch_Mode WITH "Demand", "del"
    ON KEY LABEL F10   ACTIVATE MENU Demand
    ON KEY LABEL LeftMouse Do M_Pr_�01
    ACTIVATE WINDOW Dem_E
    BROWSE FIELDS f000 = IIF(STO_CODE # 0, " � ", "   "):H="",   ;
                  DATE:H="",   ;
                  PREFIX:H="", ;
                  Brw_Name=Nm_Part(NAME, 35):35:H="",  ;
                  PRODUCER:H="", ;
                  QTY:H="",   ;
                  DUMMY.F:H=""   ;
           FREEZE DUMMY.F NOEDIT NOMENU NORGRID NOAPPEND NOCLEAR ;
           KEY user  ;
           WHEN Sh_Foot() ;
           WINDOW Dem_I IN WINDOW Dem_E
    ON KEY LABEL Enter
    ON KEY LABEL Ins
    ON KEY LABEL Del
    ON KEY LABEL LeftMouse
    ON KEY LABEL F10
    IF what_do = "list"
      what_do = IIF(READKEY()%256 = 14, "edit", "exit")
    ENDIF
  CASE what_do = "add"
    DO C_Line WITH "A"
    what_do = "list"
  CASE what_do = "del"
    DELETE
    SKIP
    what_do = "list"
  CASE what_do = "edit"
    DO C_Line
    what_do = "list"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW Dem_M
RELEASE WINDOW Dem_E
RELEASE WINDOW Dem_I
CLOSE DATABASES

DO Exit_P WITH "", "Demand"

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ ��ぃ�皰→� ����.                                �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prp_Wins
PARAMETERS md
PRIVATE ln, tit1, tit2, tit3
DIMENSION tit2(1), tit3(2)
*
*   �甅�↓�� �ムョキ� ���ォ か� BROWSE
*
tit2(1) = "����"+IIF(SET("CENTURY") = "OFF","� ���� 陳", "陳� ���� 陳")+ ;
          PADC("��━キ����┘陳陳陳陳",    ;
                 FSIZE("PREFIX")+FSIZE("PRODUCER")+35+2,"�")+"�"+  ;
          "���-〓"

tit3(1) = ""
tit3(2) = ""
ln = WROWS("")-6

tit1 = "��牀�"

DO Def_Win WITH "Dem_M", "Dem_E", "Dem_I", ln, ;
                         LEN(tit2(1)), tit1, tit2, tit3

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�        蹍������ - �÷�｀砒┴ �謌�� か� 甎�瓷� ��┘�皰�.              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE M_Pr_�01
PRIVATE mc, mr

*
*  �瓱�縲����� �キ�
*
IF MROW("") = 0
  IF .NOT. MENU() == "DEMAND"
    ACTIVATE MENU Demand
  ENDIF
  RETURN
ENDIF

*
*  ｪ�� BROWSE
*
mc = MCOL("Dem_E")
mr = MROW("Dem_E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  �甅�↓�� ����
*
mc = MCOL("Dem_M")
mr = MROW("Dem_M")
IF mc >= 0 .AND. mr >= 0
  IF mr = 1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WROWS("Cus_M")-1
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ - ぎ��←キ��/��玻オ罔� 痰牀��.                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE C_Line
PARAMETER add_p

PRIVATE ex, add_w, tit, tmpCode, tmpProd
ex = 1
add_w = TYPE("add_p") # "L"

*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*楽� ��Ж罔� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳笥� ��爼� 陳陳笥���-〓審
*崖 旭旭 旭旭碓旭葦旭旭碓旭葦旭旭碓旭葦旭旭� 崖  旭旭碓旭  崖旭旭碓崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳拈陳陳陳陳陳陳拈陳陳陳抒
*�       �祗��. ��Л�腑�┘  旭旭碓旭葦旭旭碓旭葦旭旭碓旭葦旭        �
*�                                                                  �
*�            ��ぅ�� ��������  旭旭碓旭葦旭旭碓旭葦旭�              �
*�                                                                  �
*�                < OK Ctrl-W > < �皖���碎瘴 Esc >                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
tit = IIF(add_w, "����←キ┘", "��玻オ罔�")
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
DO D_W WITH "Demand", 11, 70, tit, 1
@ 2,  2 TO 4, 45
@ 2,  4 SAY " ��Ж罔� "
@ 2, 46 TO 4, 59
@ 2, 48 SAY " ��爼� "
@ 2, 60 TO 4, 67
@ 2, 61 SAY "���-〓"
@ 5,  9 SAY "�祗��. ��Л�腑�┘"
@ 7, 14 SAY "��ぅ�� ��������"
IF add_w
  SCATTER MEMVAR BLANK
  m.qty   = 1
  m.who   = user
  m.date  = DATE()
  m.where = arm
  m.producer = "        "
  m.pro_code = 1
ELSE
  SCATTER MEMVAR
ENDIF
tmpCode = m.pro_code
tmpProd = m.producer
@ 3,  4 GET m.prefix WHEN m.sto_code = 0
@ 3,  9 GET m.name   SIZE 1, 35 WHEN m.sto_code = 0
@ 3, 49 GET tmpProd WHEN m.sto_code = 0 VALID Develop(tmpCode)
@ 3, 61 GET m.qty PICTURE "@Z 999999"
@ 5, 28 GET m.function
@ 7, 31 GET m.amodel
@ 9, 18 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID Sav_Bas()

RELEASE WINDOW Demand
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ �諷��� ├ ゛����.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sav_Bas
PRIVATE mss
IF ex = 2
  RETURN .T.
ENDIF
IF m.sto_code = 0 .AND. EMPTY(m.name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��Ж罔� � �竅硅� ��Б��┘�? ��� �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF add_w
  APPEND BLANK
ENDIF
m.pro_code = tmpCode
m.producer = tmpProd
GATHER MEMVAR
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                               Footer                                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sh_Foot

PRIVATE tmpLn

m.tmpLn = WCOLS("DEM_M")-9

ACTIVATE WINDOW DEM_M SAME
IF DEMAND.CLI_CODE # 0
  @ WROWS()-1, 1 SAY "��┘�� "+PADR(ACCOUNT.CUS_NAME, m.tmpLn)
ELSE
  @ WROWS()-1, 1 SAY "��┘�� "+PADR(DEMAND.CLIENT, m.tmpLn)
ENDIF

ACTIVATE WINDOW DEM_E SAME

RETURN .T.
