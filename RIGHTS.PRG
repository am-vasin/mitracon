*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�     蹍������ 筮爼�牀����� 皋�� ���讌���皀��.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Rights
PRIVATE wd, ln

HIDE MENU Main_M
PUSH KEY CLEAR

DEFINE MENU Rights IN SCREEN COLOR SCHEME 4
DEFINE PAD Corr OF Rights PROMPT "��玻オ罔�"  KEY Tab, "Enter"
DEFINE PAD Add  OF Rights PROMPT "����←キ┘" KEY Ins, "Ins"
DEFINE PAD Exit OF Rights PROMPT "�諷��"      KEY W, "Esc"
ON SELECTION PAD Corr OF Rights DO Modi_Type
ON SELECTION PAD Add  OF Rights DO Add_Type
ON SELECTION PAD Exit OF Rights DO Exit

DO Use_Dummy

SELECT 0
USE S_RIGHTS
IF RECCOUNT() = 0
  APPEND BLANK
  REPLACE RIGHT WITH RECNO()
ENDIF

wd = FSIZE("NAME")
ln = WROWS("")-8

@ 0, 0 SAY PADL("�キ� F10 ",WCOLS("")) COLOR SCHEME 13
SHOW MENU Rights

DO Def_Win WITH "W_Main", "W_Ext", "W_Int", ln, wd, "�┓� ぎ痰祚�"

ON KEY LABEL F10 DO To_Menu
ON KEY LABEL LeftMouse DO M_Proc
ON KEY LABEL Enter DO Modi_Type
ON KEY LABEL Ins DO Add_Type

IF RECCOUNT() # 0

  ACTIVATE WINDOW W_Ext
*  BROWSE FIELDS NAME, DUMMY.F FREEZE DUMMY.F NOMENU WINDOW W_Int IN WINDOW W_Ext
  BROWSE FIELDS NAME NOMENU NORGRID WINDOW W_Int IN WINDOW W_Ext

ENDIF

CLOSE DATABASES
CLEAR WINDOWS
POP KEY
DEACTIVATE MENU Rights
RELEASE MENU Rights

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ ��玻オ罔� 皋�� ぎ痰祚�.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Modi_Type
PRIVATE l0, ln0, c0, wd0, mss, i
PRIVATE m_file, a_level

IF .NOT. Get_Lvl()
  RETURN
ENDIF

m_file = base_path+"menu_lst"
a_level = RECNO()

DIMENSION mss(6)
mss(1) = '釪�硅 �キ�, �皙ョキ�襯 瓱�〓��� "�" �イ�痰祚�� か� ������'
mss(2) = "��皀��爬� ���讌���皀�ォ."
mss(3) = "�ムガラ�閨� �� �祗���� �キ�  �爬 ����薑 痰爛���,  ���※茱�"
mss(4) = "Enter �� ��Ε皀  竅����←│�碎/甅━�碎 ���モ��,  筮爼�珮�,"
mss(5) = "皀� ���覓, 甎�甌� ぎ痰祚�諷 粃��罔�."
mss(6) = "��� ��▲琥キ�� ��｀硅 ��Μ�皀 ���※蓊 Esc."

PUSH KEY CLEAR
DEFINE WINDOW Hide_Win FROM 0,0 TO 0,WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW Hide_Win
@ 0, 0 CLEAR TO 0, WCOLS()-1 COLOR SCHEME 13

ln0 = WROWS("")-5
wd0 = WCOLS("")-6
l0  = CEILING((WROWS("")-ln0)/2)
c0  = FLOOR((WCOLS("")-wd0)/2)
DEFINE WINDOW Modi_Type FROM l0, c0 TO l0+ln0-1, c0+wd0-1 ;
              SHADOW NONE COLOR SCHEME 13
ACTIVATE WINDOW Modi_Type
l0 = PADC('�┓ ぎ痰祚� "'+ALLTRIM(NAME)+'"',WCOLS())
@ 0,0 SAY l0 COLOR SCHEME 5
@ 2,0 TO WROWS()-1, WCOLS()-1
l0 = 1+ROUND( 2*(WROWS()-4-ALEN(mss))/3,0 )
c0 = FLOOR((WCOLS()-LEN(mss(1)))/2)
FOR i = 1 TO ALEN(mss)
  @ l0+i, c0 SAY mss(i)
ENDFOR
*****************************************************************************

PRIVATE sch_w
PRIVATE pad_sav, bar_, popup_n
pad_sav  = ""
bar_     = 0
popup_n  = ""
sch_w = 4

*
*   �皖琺��ガ ���� �������� �キ� � �瑙腑� �祗�硅 甌���甅� ����罔鍖
*
SELECT 0

USE (m_file) ORDER TAG LEVELS
SET FILTER TO .NOT. H_FOR_ALL

*
*  ��爼�珮ガ �キ�...
*
DEFINE MENU Menu_T BAR AT LINE 1 IN WINDOW MODI_TYPE ;
       MARK "�" COLOR SCHEME (sch_w)
PRIVATE pl1, bl2, bl3, mn2, mn3
SCAN
  DO CASE

  CASE EMPTY(LEVEL2)                 && 釪�� �ム〓�� 窶�↓�
    pl1 = "PAD2_"+ALLTRIM(STR(LEVEL1))
    DEFINE PAD (pl1) OF Menu_T PROMPT ALLTRIM(P_PROMPT)+" "

    IF EMPTY(COMM_STR)                   && ��ゥ� �メ皰 ��がキ�
      mn2 = "POP_"+ALLTRIM(STR(LEVEL1))
      ON PAD (pl1) OF Menu_T ACTIVATE POPUP (mn2)
      DEFINE POPUP (mn2) MARGIN MARK "�" RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl2 = 1
    ELSE                                && �痰���※碎/甅閧� ���モ��
      SET MARK OF PAD (pl1) OF Menu_T TO SUBSTR(ACC_LABELS,a_level,1) # " " 
      ON SELECTION PAD (pl1) OF Menu_T DO Mark_P
    ENDIF

  CASE EMPTY(LEVEL3)                 && 釪�� ≒�牀�� 窶�↓�
    DEFINE BAR bl2 OF (mn2) PROMPT ALLTRIM(P_PROMPT)+" "

    IF EMPTY(COMM_STR)                   && ��ゥ� �メ皰 ��がキ�
      mn3 = "POP_"+ALLTRIM(STR(LEVEL1))+"_"+ALLTRIM(STR(LEVEL2))
      ON BAR bl2 OF (mn2) ACTIVATE POPUP (mn3)
      DEFINE POPUP (mn3) MARGIN MARK "�" RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl3 = 1
    ELSE                                && �痰���※碎/甅閧� ���モ��
      SET MARK OF BAR bl2 OF (mn2) TO SUBSTR(ACC_LABELS,a_level,1) # " "
      ON SELECTION BAR bl2 OF (mn2) DO Mark_B
    ENDIF
    bl2 = bl2+1

  OTHERWISE    && 釪�� 矗モ譽�� 窶�↓� � �┴���� ��がキ�!
    DEFINE BAR bl3 OF (mn3) PROMPT ALLTRIM(P_PROMPT)+" "
    SET MARK OF BAR bl3 OF (mn3) TO SUBSTR(ACC_LABELS,a_level,1) # " "
    ON SELECTION BAR bl3 OF (mn3) DO Mark_B
    bl3 = bl3+1
  ENDCASE

ENDSCAN

*
*   ��皀���皋↓�� 竅����→� ���モ�� � �キ�
*
PRIVATE repl
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  ��縲��━ ├�キキ��? [ �� ] [ �モ ] [ �ム�ガ瘴 ]"
mss(3) = ""

DO WHILE .T.
  SET ESCAPE OFF
  DO WHILE .T.
    IF EMPTY(bar_) .AND. EMPTY(popup_n) .AND. EMPTY(pad_sav)
      ACTIVATE MENU Menu_T
    ELSE
      IF EMPTY(bar_)
        ACTIVATE MENU Menu_T PAD (pad_sav)
      ELSE
        ACTIVATE POPUP (popup_n) BAR bar_
      ENDIF
    ENDIF
    IF MOD(READKEY(),256) = 12
      EXIT
    ENDIF
    pad_sav = PAD()
  ENDDO
  repl = Out_Mess(7,"mss")
  IF repl # 3
    EXIT
  ENDIF
ENDDO

RELEASE WINDOW Modi_Type
RELEASE WINDOW Hide_Win
*
*  �痰���→� ���モ�� � ��Д ����諷 �� ���モ��� �キ�: �ム�覃 轤��
*                ���モ�� か� 皀爼��讚諷 �祗�皰�
*
IF repl = 1
  SCAN
    DO CASE

    CASE EMPTY(LEVEL2)                 && 釪�� �ム〓�� 窶�↓�
      pl1 = "PAD2_"+ALLTRIM(STR(LEVEL1))
      DEFINE PAD (pl1) OF Menu_T PROMPT ALLTRIM(P_PROMPT)+" "

      IF EMPTY(COMM_STR)                   && ��ゥ� �メ皰 ��がキ�
        REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1," ")
        mn2 = "POP_"+ALLTRIM(STR(LEVEL1))
        bl2 = 1
      ELSE                                && �痰���※碎/甅閧� ���モ��
        bar_ = IIF(MRKPAD("Menu_T",pl1), "*", " ")
        REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
      ENDIF

    CASE EMPTY(LEVEL3)                 && 釪�� ≒�牀�� 窶�↓�
      IF EMPTY(COMM_STR)                   && ��ゥ� �メ皰 ��がキ�
        REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1," ")
        mn3 = "POP_"+ALLTRIM(STR(LEVEL1))+"_"+ALLTRIM(STR(LEVEL2))
        bl3 = 1
      ELSE                                && �痰���※碎/甅閧� ���モ��
        bar_ = IIF(MRKBAR(mn2,bl2), "*", " ")
        REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
      ENDIF
      bl2 = bl2+1

    OTHERWISE    && 釪�� 矗モ譽�� 窶�↓� � �┴���� ��がキ�!
      bar_ = IIF(MRKBAR(mn3,bl3), "*", " ")
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
      bl3 = bl3+1
    ENDCASE

  ENDSCAN

*
*  �痰���→� ���モ�� � ��Д ����諷 �� ���モ��� �キ�: ≒�牀� 轤��
*                ���モ�� か� �モム���讚諷 �祗�皰�
*

  SET FILTER TO IIF(a_level=0, .T., EMPTY(SUBSTR(ACC_LABELS,a_level,1)))  ;
                .AND. .NOT. H_FOR_ALL

  SET ORDER TO TAG LEVELS DESCENDING
  PRIVATE lv1, lv2, lv3, ok
  STORE 0 TO lv1, lv2, lv3

  SCAN

    ok = .F.
    IF EMPTY(COMM_STR)        && �� 皀爼, ��ぎ ��М÷�碎瘴
      IF EMPTY(LEVEL2)       && �ム�覃 窶�▲��
        IF .NOT. EMPTY(lv2) .AND. EMPTY(lv3)
          IF LEVEL1 = lv1
            ok = .T.
          ENDIF
        ENDIF
      ELSE                   && ≒�牀� 窶�▲��
        IF .NOT. EMPTY(lv3)
          IF LEVEL1 = lv1 .AND. LEVEL2 = lv2
            ok = .T.
          ENDIF
        ENDIF
      ENDIF
      IF .NOT. ok
        REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,"*")
        LOOP
      ENDIF
    ENDIF
    lv1 = LEVEL1
    lv2 = LEVEL2
    lv3 = LEVEL3
  ENDSCAN
ENDIF

USE

SELECT S_RIGHTS

*****************************************************************************

POP KEY
*DEACTIVATE MENU Rights
*SHOW MENU Rights
ACTIVATE WINDOW S_RIGHTS

RETURN

*****************************************************************************
PROCEDURE Mark_P

SET MARK OF PAD (PAD()) OF Menu_T TO .NOT. MRKPAD("Menu_T",PAD())
*popup_n = ""
popup_n = POPUP()
pad_sav = ALLTRIM(PROMPT())
bar_ = BAR()

*****************************************************************************
PROCEDURE Mark_B

SET MARK OF BAR BAR() OF (POPUP()) TO .NOT.MRKBAR(POPUP(),BAR())
popup_n = POPUP()
*pad_sav = ""
pad_sav = ALLTRIM(PROMPT())
bar_ = BAR()


*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ �諷��� ├ 粃��罔�.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Exit
CLOSE DATABASES
CLEAR WINDOWS
DEACTIVATE MENU Rights
RELEASE MENU Rights
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   蹍������ �÷�｀皖� �謌��.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE M_Proc

IF MROW("") = 0
  ACTIVATE MENU Rights
ELSE
  IF MROW("W_Ext") >= 0 .AND. MCOL("W_Ext") >= 0
    KEYBOARD CHR(13)
  ENDIF
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�            蹍������ ��皋※��罔� �キ� �� F10.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE To_Menu

ON KEY LABEL F10
ON KEY LABEL LeftMouse
ON KEY LABEL Enter
ON KEY LABEL Ins
ACTIVATE MENU Rights
ON KEY LABEL F10 DO To_Menu
ON KEY LABEL LeftMouse DO M_Proc
ON KEY LABEL Enter DO Modi_Type
ON KEY LABEL Ins DO Add_Type

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ ぎ��←キ�� 皋�� ぎ痰祚�.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Add_Type
PRIVATE mss
DIMENSION mss(3)

HIDE MENU Rights

mss(1) = ""
mss(2) = CHR(0)+" �� 磚ムキ�, 艪� ラ� �え� 皋� ぎ痰祚� �ギ≦�え�? [ �� ] [ �モ ]"
mss(3) = ""

IF Out_Mess(7,"mss") = 1
  APPEND BLANK
  REPLACE RIGHT WITH RECNO()
ENDIF

DEACTIVATE MENU Rights
SHOW MENU Rights
ACTIVATE WINDOW S_RIGHTS

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�               蹍������ □��� 窶�↓� ぎ痰祚�.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Get_Lvl
PRIVATE ex, wk_lvl, modi_m, ln, wd

PUSH KEY CLEAR
DEFINE WINDOW Hide_Win FROM 0,0 TO 0,WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW Hide_Win
@ 0, 0 CLEAR TO 0, WCOLS()-1 COLOR SCHEME 13

ln = 8
wd = 62

DO D_W WITH "Rght_Lv", ln, wd, "�������皀 ��゛�聶", 1
ex     = 1
wk_lvl = LEFT(R_FLAGS,1)
modi_m = .T.
wk_lvl = VAL(wk_lvl)
ON READERROR
@ 3, 4 SAY "�牀▲�� ����罔� (0-3)" GET wk_lvl PICTURE "9" VALID BETWEEN(wk_lvl,0,3)  ;
                                       ERROR "�� �祀� ぎ 矗ュ!"
@ 3,30 GET modi_m PICTURE "@*C ��玻オ罔� 甎�瓷� �祗�皰�"
@ 5,15 GET ex     PICTURE "@*HT \ OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE
ON READERROR =1
IF READKEY()%256 = 12 .AND. READKEY(1) = 1
  ex = 2
ENDIF
IF ex = 1
  wk_lvl = STR(wk_lvl,1)+SUBSTR(R_FLAGS,2)
  REPLACE R_FLAGS WITH wk_lvl
ENDIF

RELEASE WINDOW Hide_Win, Rght_Lv
POP KEY

RETURN ex = 1 .AND. modi_m
