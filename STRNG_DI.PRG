*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�            蹍������ �痰���→� 瓷┐�� か� "艫Θ�" 甎�瓷��.            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Strng_Di

PRIVATE cl_w, i001, i002, cl_w1, mss, what_do, ln, wd, tt, ft
PRIVATE p_cli, p_arm

IF acc_level > 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� ��� �モ ����罔� �� 竄▲Δキ┘ 罐�."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

cl_w = SCHEME(13)
i001 = AT(",", cl_w, 6)+1
i002 = AT(",", cl_w, 7)-i001
cl_w1 = STUFF(cl_w, i001, i002, SCHEME(14,1))
cl_w  = STUFF(cl_w, i001, i002, SCHEME(14,2))

HIDE MENU Main_M

DEFINE MENU L_Sale IN SCREEN COLOR SCHEME 4
DEFINE PAD LS_List OF L_Sale PROMPT "���甌�" KEY Ctrl-A, "Enter"
DEFINE PAD LS_Info OF L_Sale PROMPT "��筮爼�罔�" KEY Ctrl-A, "F9"
DEFINE PAD LS_Let  OF L_Sale PROMPT "�癶ムえ碎" KEY Ctrl-A, "Tab"
DEFINE PAD LS_Exit OF L_Sale PROMPT "�諷��" KEY Ctrl-A, "Esc"

ON SELECTION PAD LS_List OF L_Sale Do Ch_Mode WITH "L_Sale","list"
ON SELECTION PAD LS_Info OF L_Sale Do Ch_Mode WITH "L_Sale","info"
ON SELECTION PAD LS_Let  OF L_Sale Do Ch_Mode WITH "L_Sale","let"
ON SELECTION PAD LS_Exit OF L_Sale Do Ch_Mode WITH "L_Sale","exit"

DO Use_Dummy

DO Use_Link

SELECT 0
USE ACCOUNT ORDER TAG CUS_CODE

SELECT 0
USE ARM_LST ORDER TAG ARM_C

SELECT 0
USE LIST_TIT
SET RELATION TO LST_ARM INTO ARM_LST, CLI_CODE INTO ACCOUNT

DO Prp_Link
SELECT LINK
SET RELATION TO LNK INTO LIST_TIT

what_do = "main"
DIMENSION tt(1), ft(5)
wd = FSIZE("LST_NUM","LIST_TIT")+FSIZE("LST_NAME","LIST_TIT")+25
*     FSIZE("PRESENT","LIST_TIT")+FSIZE("R_TOTAL","LIST_TIT")+5
tt(1) = "陳"+ ;
        PADC("陳 NN ",FSIZE("LST_NUM","LIST_TIT"),"�")+"�"+ ;
        PADC(" ��� 甎�瓷� 陳陳陳陳陳",FSIZE("LST_NAME","LIST_TIT"),"�")+"�"+ ;
        PADC("� �祠�� ",10,"�")+"�"+ ;
        PADC("� ����牀� ",10,"�")

ft(1) = ""
ft(2) = ""
ft(3) = PADR("���祚�皀��",LEN(tt(1)))
ft(4) = PADR("��｀腑� �メ皰",LEN(tt(1)))
ft(5) = ""

ln = WROWS("")-11

DO Def_Win WITH "WLS_M", "WLS_E", "WLS_I", ln, wd, ;
       "���瓷� か� 竄▲爨ぅ���", tt, ft
@ WROW()-4, 1 TO WROW()-1, WCOL()-2
p_cli = 13
p_arm = 16
SELECT LINK
DO WHILE RECCOUNT("LINK") # 0
  DO CASE
  CASE what_do = "main"
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("�キ� F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU L_Sale
    ACTIVATE WINDOW "WLS_E"
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "L_Sale", "WLS_M", "WLS_E"
    ON KEY LABEL F10   DO Menu_Ch
    ON KEY LABEL Tab   DO Ch_Mode WITH "L_Sale","let"
    ON KEY LABEL Enter DO Ch_Mode WITH "L_Sale","list"
    ON KEY LABEL F9    DO Ch_Mode WITH "L_Sale","info"
    BROW FIELDS f00 = IIF(EMPTY(LIST_TIT.WHO_LET), " ", "�"):H="", ;
                LIST_TIT.LST_NUM:H="",  ;
                LIST_TIT.LST_NAME:H="", ;
                LIST_TIT.R_TOTAL:H="":P="@Z 9999999.99",  ;
                LIST_TIT.PRESENT:H="":P="@Z 9999999.99",  ;
                DUMMY.F:H=""            ;
         FREEZE DUMMY.F ;
         WHEN Show_I()  ;
         NOMENU NORGRID NOAPPEND NOCLEAR WINDOW WLS_I IN WINDOW WLS_E
    ON KEY LABEL LeftMouse
    ON KEY LABEL Tab
    ON KEY LABEL Enter
    ON KEY LABEL F9
    IF what_do = "main"
      what_do = "exit"
    ENDIF
  CASE what_do = "let"
    DO Let_Sale
    what_do = "main"
  CASE what_do = "info"
    DO Dis_Info
    what_do = "main"
  CASE what_do = "list"
    DO Show_Lst
    what_do = "main"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW WLS_M
RELEASE WINDOW WLS_E
RELEASE WINDOW WLS_I
RELEASE MENU L_Sale
CLOSE DATABASES
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   蹍������ √��� � ����瘡.                           �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Show_I
PRIVATE w_n, w_a

ACTIVATE WINDOW WLS_M BOTTOM

IF FOUND("ACCOUNT")
  w_n = ACCOUNT.CUS_NAME
ELSE
  w_n = PADR("���覃", FSIZE("CUS_NAME","ACCOUNT"))
ENDIF

IF LIST_TIT.LST_ARM = "***"
  w_a = PADR("｡薑� 甎�甌�", FSIZE("ARM_N","ARM_LST"))
ELSE
  w_a = ARM_LST.ARM_N
ENDIF
@ WROWS()-3, p_cli SAY w_n COLOR (SCHEME(14,2))
@ WROWS()-2, p_arm SAY w_a COLOR (SCHEME(14,2))

ACTIVATE WINDOW (ALIAS())

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ ��ぃ�皰→� 甎�瓷��.                            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prp_Link

PRIVATE mss

SELECT LINK
ZAP
SELECT LIST_TIT
SCAN
  IF EMPTY(IS_DIS) .AND. EMPTY(IS_SPEC) .AND. EMPTY(PRESENT)
    LOOP
  ENDIF
  IF .NOT. EMPTY(SAL_NUM)   && .OR. .NOT. EMPTY(WHO_LET)
    LOOP
  ENDIF
  SELECT LINK
  APPEND BLANK
  REPLACE LNK WITH RECNO("LIST_TIT")
  SELECT LIST_TIT
ENDSCAN

IF RECCOUNT("LINK") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ 甎�瓷��, 矗ァ竡薑� 竄▲爨ぅ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

RETURN


*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ ��皋※��罔� �キ�.                            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Menu_Ch

PUSH KEY CLEAR
ACTIVATE MENU L_Sale
POP KEY

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ �襤��� ��筮爼�罔� �� 痺����.                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Dis_Info

PRIVATE sel, k_w, s11, s12, s21, s22, ln, wd, l, c

PUSH KEY CLEAR
ACTIVATE SCREEN
@ 0,0 SAY SPACE(WCOLS()) COLOR SCHEME 13
sel = SELECT()
SELECT 0
USE LIST_DET ORDER TAG L_NAME
k_w = STR(LIST_TIT.LST_NUM,6)
STORE 0 TO s11, s12, s21, s22
SEEK k_w
SCAN REST WHILE LIST_NUM = k_w
  IF .NOT. EMPTY(IS_SPEC)
    s11 = s11+QNT*DEF_PRICE
    S12 = s12+QNT*SAL_PRICE
  ELSE
    s21 = s21+QNT*DEF_PRICE
    S22 = s22+QNT*SAL_PRICE
  ENDIF
ENDSCAN
USE
SELECT (sel)

ln = 11
wd = 47
DO D_W WITH "W_Info", ln, wd, "�祠�� �� ��Ж罔鍖", 0

@ 1, 1 SAY "敖陳陳陳陳陳陳陳堕陳陳陳陳陳陳堕陳陳陳陳陳陳�"
@ 2, 1 SAY "�               �� ���. 罐��� �� 罐��� �牀�.�"
@ 3, 1 SAY "団陳陳陳陳陳陳陳田陳陳陳陳陳陳田陳陳陳陳陳陳�"
@ 4, 1 SAY "� 甌 甎ユ罐���� �             �             �"
@ 5, 1 SAY "団陳陳陳陳陳陳陳田陳陳陳陳陳陳田陳陳陳陳陳陳�"
@ 6, 1 SAY "� �痰��讚覓     �             �             �"
@ 7, 1 SAY "団陳陳陳陳陳陳陳田陳陳陳陳陳陳田陳陳陳陳陳陳�"
@ 8, 1 SAY "� �皰��         �             �             �"
@ 9, 1 SAY "青陳陳陳陳陳陳陳祖陳陳陳陳陳陳祖陳陳陳陳陳陳�"
@10, 0 SAY PADC("��Μ�皀 �遏竡 ���※蓊", WCOLS())

@ 4,19 SAY TRANSFORM(s11, "@Z 9999999.99")
@ 4,33 SAY TRANSFORM(s12, "@Z 9999999.99")

@ 6,19 SAY TRANSFORM(s21, "@Z 9999999.99")
@ 6,33 SAY TRANSFORM(s22, "@Z 9999999.99")

@ 8,19 SAY TRANSFORM(s11+s21, "@Z 9999999.99")
@ 8,33 SAY TRANSFORM(s12+s22, "@Z 9999999.99")

ln = SET("CURSOR") = "ON"
SET CURSOR OFF
= INKEY(0,"M")
IF ln
  SET CURSOR ON
ENDIF

RELEASE WINDOW W_Info

POP KEY
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�        蹍������ - �÷�｀砒┴ �謌�� か� 甎�瓷��.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE M_Proc_1
PARAMETER menu_nm, m_win, e_win
PRIVATE mc, mr

*
*  �瓱�縲����� �キ�
*
IF MENU() == UPPER(menu_nm)
  RETURN
ENDIF

IF MROW("") = 0
  IF .NOT. MENU() == UPPER(menu_nm)
    ACTIVATE MENU (menu_nm)
  ENDIF
  RETURN
ENDIF

*
*  ｪ�� BROWSE
*
mc = MCOL(e_win)
mr = MROW(e_win)
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  �甅�↓�� ����
*
mc = MCOL(m_win)
mr = MROW(m_win)
IF mc >= 0 .AND. mr >= 0
  IF mr = 1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WROWS(m_win)-1
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ �牀甃�矗� 甎�瓷�.                            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Show_Lst
PRIVATE ln, wd, tt, ft, tm, sel, k_w

PUSH KEY CLEAR
ACTIVATE SCREEN
@ 0,0 SAY SPACE(WCOLS()) COLOR SCHEME 13
sel = SELECT()
SELECT 0
USE STOCK ORDER TAG CODE
SELECT 0
USE LIST_DET ORDER TAG L_NAME
SET RELATION TO CODE INTO STOCK
k_w = STR(LIST_TIT.LST_NUM,6)

DIMENSION tt(1), ft(3)
wd = FSIZE("PREFIX","LIST_DET")+25+FSIZE("QNT","LIST_DET")+34
tt(1) = PADR("陳陳��━キ����┘",FSIZE("PREFIX","LIST_DET")+  ;
             26,"�")+"�"+ ;
        PADC("���-〓",FSIZE("QNT","LIST_DET"),"�")+"�"+ ;
        PADC("���.罐��",10,"�")+"�"+ ;
        PADC("�キ� �牀�.",10,"�")+"���.���ユ."

ft(1) = ""
ft(2) = ""
ft(3) = "Esc - �諷��"
tm = "N "+ALLTRIM(k_w)+" "+ALLTRIM(LIST_TIT.LST_NAME)
ln = WROWS("")-11

DO Def_Win WITH "LS_M", "LS_E", "LS_I", ln, wd, ;
       tm, tt, ft
ACTIVATE WINDOW "LS_E"
ON KEY LABEL LeftMouse DO M_Proc_1 WITH "", "LS_M", "LS_E"
BROW FIELDS f0000 = IIF(FOUND("STOCK"), STOCK.PREFIX, LIST_DET.PREFIX):H="",  ;
            f0001 = IIF(FOUND("STOCK"), ;
              Nm_Part(STOCK.NAME), ;
              Nm_Part(LIST_DET.NAME)):25:H="",  ;
            LIST_DET.QNT:H="", ;
            LIST_DET.DEF_PRICE:10:H="", ;
            LIST_DET.SAL_PRICE:10:H="", ;
            f0002 = IIF(LIST_DET.SAL_PRICE#LIST_DET.DEF_PRICE .AND. EMPTY(IS_SPEC), " � ", "   "):H="", ;
            f0003 = IIF(LIST_DET.SAL_PRICE#LIST_DET.DEF_PRICE .AND. .NOT. EMPTY(IS_SPEC), "  �  ", "     "):H="", ;
            DUMMY.F:H=""            ;
         FREEZE DUMMY.F ;
         WHEN Draw_Foot()    ;
         KEY k_w        ;
         NOMENU NORGRID NOAPPEND WINDOW LS_I IN WINDOW LS_E
RELEASE WINDOW LS_M
RELEASE WINDOW LS_E
RELEASE WINDOW LS_I
USE
SELECT STOCK
USE
SELECT (sel)
POP KEY

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW LS_M SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-2, 2 SAY IIF(FOUND("STOCK"), STOCK.PREFIX, LIST_DET.PREFIX) COLOR SCHEME 1
@ ROW(), COL()+1 SAY IIF(FOUND("STOCK"), STOCK.NAME,LIST_DET.NAME) COLOR SCHEME 1
@ ROW(), COL()+1 SAY IIF(FOUND("STOCK"), STOCK.PRODUCER,LIST_DET.PRODUCER) COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
