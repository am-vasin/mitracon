*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ ��｀硅 甌 甎�瓷���.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE M_Func
PRIVATE mrkl, fltl, what_do

DIMENSION fltl(1,1)
mrkl = ""

*
*    PAD - �キ� �ム〓�� 窶�↓�
*
DEFINE MENU List_M IN SCREEN COLOR SCHEME 4
DEFINE PAD Titl OF List_M PROMPT "������〓�"
DEFINE PAD Work OF List_M PROMPT "��｀��"
DEFINE PAD Docs OF List_M PROMPT "���祠キ硅"
DEFINE PAD Opt  OF List_M PROMPT "��痰牀���"
DEFINE PAD Exit OF List_M PROMPT "�諷��" KEY Ctrl-W, "Esc"

ON PAD Titl OF List_M ACTIVATE POPUP Titl
ON PAD Work OF List_M ACTIVATE POPUP Work
ON PAD Docs OF List_M ACTIVATE POPUP Docs
ON PAD Opt  OF List_M ACTIVATE POPUP Opt
ON SELECTION PAD Exit OF List_M Do Ex_Proc WITH "List_M"

*
*    POPUP - �キ� ≒�牀�� 窶�↓�
*
DEFINE POPUP Titl MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Titl PROMPT "��玻オ皋牀��碎 " KEY Ctrl-A, "Tab"
DEFINE BAR  2 OF Titl PROMPT "����※碎 " KEY Ctrl-B, "Ins"
DEFINE BAR  3 OF Titl PROMPT "�����碎 " KEY Ctrl-C, "Del"
DEFINE BAR  4 OF Titl PROMPT "� �♂竡 �゛�痰� "
DEFINE BAR  5 OF Titl PROMPT "�� �♂ォ �゛�痰� "

DEFINE POPUP Work MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Work PROMPT "��玻オ罔� " KEY Ctrl-D, "Enter"
DEFINE BAR  2 OF Work PROMPT "蹍����" KEY Ctrl-E, "Tab"
DEFINE BAR  3 OF Work PROMPT "���祠キ硅 "
DEFINE BAR  4 OF Work PROMPT "��А┘�┘ "
DEFINE BAR  5 OF Work PROMPT "���鎰┘ "
DEFINE BAR  6 OF Work PROMPT "��┐�� "
DEFINE BAR  7 OF Work PROMPT "���モ�� " KEY Ctrl-F, "F3"
DEFINE BAR  8 OF Work PROMPT "�癶ムえ碎 "
DEFINE BAR  9 OF Work PROMPT "｡��←キ┘ "

ON SELECTION BAR 7 OF Work DO Mrk_Lst

DEFINE POPUP Docs MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Docs PROMPT "���甌� " 
DEFINE BAR  2 OF Docs PROMPT "�腑� "
DEFINE BAR  3 OF Docs PROMPT "�����き�� "
DEFINE BAR  4 OF Docs PROMPT "踸絎き覃 �爐ム "
DEFINE BAR  5 OF Docs PROMPT "��筮爼�罔�... "

ON BAR 5 OF Docs ACTIVATE POPUP Info

DEFINE POPUP Info RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Info PROMPT "�ク�Ж�     " 
DEFINE BAR  2 OF Info PROMPT "�腑�        "


DEFINE POPUP Opt  MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR  1 OF Opt PROMPT "�┼赳� " 
DEFINE BAR  2 OF Opt PROMPT "�ウ━ "

ON SELECTION BAR 1 OF Opt DO Ch_FltL

HIDE MENU Main_M
*PUSH KEY CLEAR
*@ 0, 0 SAY PADL("�キ� F10 ",WCOLS("")) COLOR SCHEME 13
*SHOW MENU List_M
*ON KEY LABEL F10 ACTIVATE MENU List_M
DO Entry_P WITH "", "List_M"

USE LIST_TIT ORDER TAG LST_ARM
SELECT 0

USE ACCOUNT ORDER TAG CUS_CODE

DO Use_Dummy

SELECT LIST_TIT
SET RELATION TO CLI_CODE INTO ACCOUNT
GO BOTTOM

DO Prp_Fltl
DO Prep_Lst

what_do = "list"
DO WHILE .T.
  DO CASE
  CASE what_do = "list"
    ON KEY LABEL F3 DO Mrk_lst
    ON KEY LABEL LeftMouse DO Mouse_1
    
    ACTIVATE WINDOW Lst_E
    BROWSE FIELDS f001 = IIF(STR(LST_NUM,6)+"!" $ mrkl, "�"," "):H="", ;
                  LST_NUM:H="", CR_DAT:H="", LST_NAME:H="", DUMMY.F:H="" ;
                  FREEZE DUMMY.F KEY arm  ;
                  FOR Sh_Flags(2)   ;
                  WHEN Sh_Flags(1)  ;
                  NOMENU NOAPPEND NODELETE NOMODIFY SAVE ;
                  WINDOW Lst_I IN WINDOW Lst_E 

    ON KEY LABEL F3
    ON KEY LABEL LeftMouse
    IF what_do = "list"
      what_do = "exit"
    ENDIF
  CASE what_do = "filtr"
    DO Set_FltL
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW FltL
RELEASE WINDOW Lst_M
RELEASE WINDOW Lst_E
RELEASE WINDOW Lst_I
*DEACTIVATE MENU List_M
*RELEASE MENU List_M EXTENDED

CLOSE DATABASES
*POP KEY
DO Exit_P WITH "", "List_M"
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ ���モ�� 甎�瓷�.                            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Mrk_Lst
PRIVATE s, i
s = STR(LST_NUM,6)+"!"
i = AT(s,mrkl)
IF i = 0
  mrkl = mrkl+s
ELSE
  mrkl = STUFF(mrkl,i,LEN(s),"")
ENDIF
DEACTIVATE MENU List_M

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�      蹍������ �襪�腮 �����モ牀� � �牀▲爲� 竅��※� 筥�赳��.          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sh_Flags
PARAMETER mode
PRIVATE c_yes, c_no, nm, i, w_sav, cw, f_tmp, mode_w

IF TYPE("mode") = "N"
  mode_w = mode
ELSE
  mode_w = 2
ENDIF

i = ALEN(fltl,1)
DIMENSION f_tmp(i)
c_no  = SCHEME(1 ,2)
c_yes = SCHEME(14,2)

f_tmp( 1) = IS_DUE # " "     && ����腮� ぎ���
f_tmp( 2) = IS_DIS # " "     && ����腮� 瓷┐��
f_tmp( 3) = .NOT. EMPTY(BILL_NUM)     && ����腮� 瘍モ�
f_tmp( 4) = IS_DEF # " "     && ����腮� ぅ筥罔��
f_tmp( 5) = IS_SPEC # " "    && ����腮� 甎ユ罐�
f_tmp( 6) = .NOT. EMPTY(SAL_NUM)      && ����腮� �����き��
f_tmp( 7) = ORD_NUM # 0      && �牀��
f_tmp( 8) = IS_DIS = " " .AND. IS_SPEC = " " .OR. WHO_LET # 0
                              && �癶ムΔキ�
*
*   ��А���ガ瘴 � ��┘�皰�
*
f_tmp(13) = .T.              && �爛�キ�覃 ��┘��
f_tmp( 9) = .F.              && ｡諛�覃 ぅ��Ж�
f_tmp(10) = .F.              && ��ぅ�瓱珮ガ覃 ぅ��Ж�
f_tmp(11) = .F.              && ����牀�
nm = SPACE(FSIZE("CUS_NAME","ACCOUNT"))
IF .NOT. EMPTY(CLI_CODE)
  IF FOUND("ACCOUNT")
    f_tmp(13) = .F.           && �爛�キ�覃 ��┘��
    DO CASE
    CASE ACCOUNT.ACC_TYPE = 1
      f_tmp( 9) = .T.           && ｡諛�覃 ぅ��Ж�
    CASE ACCOUNT.ACC_TYPE = 2
      f_tmp(10) = .T.           && ��ぅ�瓱珮ガ覃 ぅ��Ж�
    CASE ACCOUNT.ACC_TYPE = 3
      f_tmp(11) = .T.           && ����牀�
    ENDCASE
    nm = ACCOUNT.CUS_NAME
  ENDIF
ENDIF

f_tmp(12) = .NOT. f_tmp(13)

IF mode_w = 1
*  w_sav = WOUTPUT()
  ACTIVATE WINDOW FltL
  @ WROWS()-2, 7 SAY nm COLOR (c_yes)
  nm = ALEN(f_tmp)
  FOR i = 1 TO nm
    cw = IIF(f_tmp(i), c_yes, c_no)
*    @ fltl(i,3), fltl(i,4) SAY fltl(i,2) COLOR (cw)
    @ fltl(i,3), fltl(i,4) FILL TO fltl(i,3), fltl(i,4)+LEN(fltl(i,2)) COLOR (cw)
  ENDFOR

*  ACTIVATE WINDOW (w_sav)
  ACTIVATE WINDOW LIST_TIT

  RETURN .T.

ELSE
  nm = ALEN(f_tmp)
  FOR i = 1 TO nm
    IF fltl(i,1) .AND. f_tmp(i)
      RETURN .T.
    ENDIF
  ENDFOR
  RETURN .F.
ENDIF

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ 竅����→� 筥�赳��.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Set_FltL
PRIVATE rc_sav, w_sav, i, n

*PUSH KEY CLEAR
w_sav  = WOUTPUT()
n      = ALEN(fltl,1)
DEFINE MENU Set_FltL IN SCREEN COLOR SCHEME 4
DEFINE PAD Exit OF Set_FltL PROMPT "�諷��" KEY Ctrl-W, "Esc"
ON SELECTION PAD Exit OF Set_FltL Do Ex_Proc WITH "Set_Fltl"
*ACTIVATE SCREEN
*@ 0, 0 SAY PADL("�キ� F10 ",WCOLS("")) COLOR SCHEME 13
*SHOW MENU Set_FltL

DO Entry_P WITH "List_M", "Set_FltL"

ACTIVATE WINDOW FltL

ON KEY LABEL Esc KEYBOARD CHR(23)
ON KEY LABEL LeftMouse DO Mouse_2

FOR i = 1 TO n
  @ fltl(i,3), fltl(i,4)-4 GET fltl(i,1) FUNCTION "*C"
ENDFOR
READ CYCLE

@ 1, 0 FILL TO WROWS()-1, WCOLS()-1 COLOR SCHEME 13
IF .NOT. Sh_Flags(2)
  GO TOP
ENDIF
IF EMPTY(w_sav)
  ACTIVATE SCREEN
ELSE
  ACTIVATE WINDOW (w_sav)
ENDIF
what_do = "list"
*POP KEY
*RELEASE MENU Set_FltL EXTENDED
*SHOW MENU List_M
DO Exit_P WITH "List_M", "Set_FltL"

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�            蹍������ �ムオ�鈑キ�� �� 竅����→� 筥�赳��.               �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Ch_FltL
what_do = "filtr"
KEYBOARD CHR(27)
DEACTIVATE MENU List_M
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ - �÷�｀砒┴ か� �諷��� ├ 爛Θ��.              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Ex_Proc
PARAMETERS m_name

KEYBOARD CHR(27)
DEACTIVATE MENU (m_name)

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�        蹍������ - �÷�｀砒┴ �謌�� か� 爛Θ�� �ムョ�� 甎�瓷��.       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Mouse_1
PRIVATE mc, mr

IF MROW("") = 0
  PUSH KEY CLEAR
  ACTIVATE MENU List_M
*  DEACTIVATE MENU List_M
  POP KEY
  RETURN
ENDIF

IF MROW("Lst_E") >= 0 .AND. MCOL("Lst_E") >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")
IF MCOL("Lst_M") >= 0 .AND. MROW("Lst_M") >= 0
  IF MROW("Lst_M") = 1
    KEYBOARD "{PgUp}"
  ENDIF    
  IF MROW("Lst_M") = WROWS("Lst_M")-1
    KEYBOARD "{PgDn}"
  ENDIF    
  RETURN
ENDIF

IF MROW("FltL") >= 0 .AND. MCOL("FltL") >= 0
  KEYBOARD CHR(27)
  what_do = "filtr"
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�      蹍������ - �÷�｀砒┴ �謌�� か� 爛Θ�� 竅����→� 筥�赳��.       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Mouse_2
PRIVATE mc, mr

IF MROW("") = 0
  PUSH KEY CLEAR
  ACTIVATE MENU Set_FltL
*  DEACTIVATE MENU List_M
  POP KEY
  RETURN
ENDIF

IF MROW("FltL") >= 0 .AND. MCOL("FltL") >= 0
  RETURN
ENDIF

= INKEY("M")
IF MROW("Lst_E") >= 0 .AND. MCOL("Lst_E") >= 0
  what_do = "list"
  KEYBOARD CHR(27)
  RETURN
ENDIF

RETURN

