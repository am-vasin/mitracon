*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�     �ணࠬ�� �ନ஢���� ⨯� ���짮��⥫�.                         �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Rights
PRIVATE wd, ln

HIDE MENU Main_M
PUSH KEY CLEAR

DEFINE MENU Rights IN SCREEN COLOR SCHEME 4
DEFINE PAD Corr OF Rights PROMPT "���४��"  KEY Tab, "Enter"
DEFINE PAD Add  OF Rights PROMPT "����������" KEY Ins, "Ins"
DEFINE PAD Exit OF Rights PROMPT "��室"      KEY W, "Esc"
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

@ 0, 0 SAY PADL("���� F10 ",WCOLS("")) COLOR SCHEME 13
SHOW MENU Rights

DO Def_Win WITH "W_Main", "W_Ext", "W_Int", ln, wd, "���� ����㯠"

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

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�              �ணࠬ�� ���४樨 ⨯� ����㯠.                        �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Modi_Type
PRIVATE l0, ln0, c0, wd0, mss, i
PRIVATE m_file, a_level

IF .NOT. Get_Lvl()
  RETURN
ENDIF

m_file = base_path+"menu_lst"
a_level = RECNO()

DIMENSION mss(6)
mss(1) = '�㭪�� ����, �⬥祭�� ᨬ����� "�" ������㯭� ��� ������'
mss(2) = "��⥣�ਨ ���짮��⥫��."
mss(3) = "��६����� �� �㭪⠬ ����  �� ����� ��५��,  �����襩"
mss(4) = "Enter �� �����  ��⠭��������/᭨���� ������,  �ନ���,"
mss(5) = "⥬ ᠬ�, ᯨ᮪ ����㯭�� �㭪権."
mss(6) = "��� �����襭�� ࠡ��� ������ ������� Esc."

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
l0 = PADC('��� ����㯠 "'+ALLTRIM(NAME)+'"',WCOLS())
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
*   ���뢠�� 䠩� ���ᠭ�� ���� � ���祬 �㭪�� ᮣ��᭮ ᠭ���
*
SELECT 0

USE (m_file) ORDER TAG LEVELS
SET FILTER TO .NOT. H_FOR_ALL

*
*  ��ନ�㥬 ����...
*
DEFINE MENU Menu_T BAR AT LINE 1 IN WINDOW MODI_TYPE ;
       MARK "�" COLOR SCHEME (sch_w)
PRIVATE pl1, bl2, bl3, mn2, mn3
SCAN
  DO CASE

  CASE EMPTY(LEVEL2)                 && �㭪� ��ࢮ�� �஢��
    pl1 = "PAD2_"+ALLTRIM(STR(LEVEL1))
    DEFINE PAD (pl1) OF Menu_T PROMPT ALLTRIM(P_PROMPT)+" "

    IF EMPTY(COMM_STR)                   && ����� ���� �������
      mn2 = "POP_"+ALLTRIM(STR(LEVEL1))
      ON PAD (pl1) OF Menu_T ACTIVATE POPUP (mn2)
      DEFINE POPUP (mn2) MARGIN MARK "�" RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl2 = 1
    ELSE                                && ��⠭�����/���� ������
      SET MARK OF PAD (pl1) OF Menu_T TO SUBSTR(ACC_LABELS,a_level,1) # " " 
      ON SELECTION PAD (pl1) OF Menu_T DO Mark_P
    ENDIF

  CASE EMPTY(LEVEL3)                 && �㭪� ��ண� �஢��
    DEFINE BAR bl2 OF (mn2) PROMPT ALLTRIM(P_PROMPT)+" "

    IF EMPTY(COMM_STR)                   && ����� ���� �������
      mn3 = "POP_"+ALLTRIM(STR(LEVEL1))+"_"+ALLTRIM(STR(LEVEL2))
      ON BAR bl2 OF (mn2) ACTIVATE POPUP (mn3)
      DEFINE POPUP (mn3) MARGIN MARK "�" RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl3 = 1
    ELSE                                && ��⠭�����/���� ������
      SET MARK OF BAR bl2 OF (mn2) TO SUBSTR(ACC_LABELS,a_level,1) # " "
      ON SELECTION BAR bl2 OF (mn2) DO Mark_B
    ENDIF
    bl2 = bl2+1

  OTHERWISE    && �㭪� ���쥣� �஢�� � ������� �������!
    DEFINE BAR bl3 OF (mn3) PROMPT ALLTRIM(P_PROMPT)+" "
    SET MARK OF BAR bl3 OF (mn3) TO SUBSTR(ACC_LABELS,a_level,1) # " "
    ON SELECTION BAR bl3 OF (mn3) DO Mark_B
    bl3 = bl3+1
  ENDCASE

ENDSCAN

*
*   ���ࠪ⨢��� ��⠭���� ����⮪ � ����
*
PRIVATE repl
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  ���࠭�� ���������? [ �� ] [ ��� ] [ ��୥��� ]"
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
*  ��⠭���� ����⮪ � ���� ������ �� ����⪠� ����: ���� �⠯
*                ����⪨ ��� �ଠ���� �㭪⮢
*
IF repl = 1
  SCAN
    DO CASE

    CASE EMPTY(LEVEL2)                 && �㭪� ��ࢮ�� �஢��
      pl1 = "PAD2_"+ALLTRIM(STR(LEVEL1))
      DEFINE PAD (pl1) OF Menu_T PROMPT ALLTRIM(P_PROMPT)+" "

      IF EMPTY(COMM_STR)                   && ����� ���� �������
        REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1," ")
        mn2 = "POP_"+ALLTRIM(STR(LEVEL1))
        bl2 = 1
      ELSE                                && ��⠭�����/���� ������
        bar_ = IIF(MRKPAD("Menu_T",pl1), "*", " ")
        REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
      ENDIF

    CASE EMPTY(LEVEL3)                 && �㭪� ��ண� �஢��
      IF EMPTY(COMM_STR)                   && ����� ���� �������
        REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1," ")
        mn3 = "POP_"+ALLTRIM(STR(LEVEL1))+"_"+ALLTRIM(STR(LEVEL2))
        bl3 = 1
      ELSE                                && ��⠭�����/���� ������
        bar_ = IIF(MRKBAR(mn2,bl2), "*", " ")
        REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
      ENDIF
      bl2 = bl2+1

    OTHERWISE    && �㭪� ���쥣� �஢�� � ������� �������!
      bar_ = IIF(MRKBAR(mn3,bl3), "*", " ")
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
      bl3 = bl3+1
    ENDCASE

  ENDSCAN

*
*  ��⠭���� ����⮪ � ���� ������ �� ����⪠� ����: ��ன �⠯
*                ����⪨ ��� ���ଠ���� �㭪⮢
*

  SET FILTER TO IIF(a_level=0, .T., EMPTY(SUBSTR(ACC_LABELS,a_level,1)))  ;
                .AND. .NOT. H_FOR_ALL

  SET ORDER TO TAG LEVELS DESCENDING
  PRIVATE lv1, lv2, lv3, ok
  STORE 0 TO lv1, lv2, lv3

  SCAN

    ok = .F.
    IF EMPTY(COMM_STR)        && �� ��, ���� ࠧ�������
      IF EMPTY(LEVEL2)       && ���� �஢���
        IF .NOT. EMPTY(lv2) .AND. EMPTY(lv3)
          IF LEVEL1 = lv1
            ok = .T.
          ENDIF
        ENDIF
      ELSE                   && ��ன �஢���
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


*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                    �ணࠬ�� ��室� �� �㭪樨.                       �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Exit
CLOSE DATABASES
CLEAR WINDOWS
DEACTIVATE MENU Rights
RELEASE MENU Rights
RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                   �ணࠬ�� ��ࠡ�⪨ ��誨.                          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE M_Proc

IF MROW("") = 0
  ACTIVATE MENU Rights
ELSE
  IF MROW("W_Ext") >= 0 .AND. MCOL("W_Ext") >= 0
    KEYBOARD CHR(13)
  ENDIF
ENDIF

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�            �ணࠬ�� ��⨢���樨 ���� �� F10.                         �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
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

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�              �ணࠬ�� ���������� ⨯� ����㯠.                       �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Add_Type
PRIVATE mss
DIMENSION mss(3)

HIDE MENU Rights

mss(1) = ""
mss(2) = CHR(0)+" �� 㢥७�, �� �� ���� ⨯ ����㯠 ����室��? [ �� ] [ ��� ]"
mss(3) = ""

IF Out_Mess(7,"mss") = 1
  APPEND BLANK
  REPLACE RIGHT WITH RECNO()
ENDIF

DEACTIVATE MENU Rights
SHOW MENU Rights
ACTIVATE WINDOW S_RIGHTS

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�               �ணࠬ�� ����� �஢�� ����㯠.                         �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Get_Lvl
PRIVATE ex, wk_lvl, modi_m, ln, wd

PUSH KEY CLEAR
DEFINE WINDOW Hide_Win FROM 0,0 TO 0,WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW Hide_Win
@ 0, 0 CLEAR TO 0, WCOLS()-1 COLOR SCHEME 13

ln = 8
wd = 62

DO D_W WITH "Rght_Lv", ln, wd, "�������� ⠡����", 1
ex     = 1
wk_lvl = LEFT(R_FLAGS,1)
modi_m = .T.
wk_lvl = VAL(wk_lvl)
ON READERROR
@ 3, 4 SAY "�஢��� ᠭ�権 (0-3)" GET wk_lvl PICTURE "9" VALID BETWEEN(wk_lvl,0,3)  ;
                                       ERROR "�� ��� �� ���!"
@ 3,30 GET modi_m PICTURE "@*C ���४�� ᯨ᪠ �㭪⮢"
@ 5,15 GET ex     PICTURE "@*HT \ OK Ctrl-W ;\? �⪠������ Esc "
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