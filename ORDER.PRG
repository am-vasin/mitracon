*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Order        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�           ��ନ஢���� ������ �� ���⠢�� �� �᭮����� �����           �
*�                                                                        �
*����������������������������������������������������������� 05.10.2005 �ͼ
PROCEDURE Order
PRIVATE tmpName1, tmpName2

m.tmpName1 = SYS(3)
m.tmpName2 = SYS(3)
DO WHILE m.tmpName2 = m.tmpName1
  m.tmpName2 = SYS(3)
ENDDO

DO Wt_Mess WITH "�����⮢�� ������"
DO Prp_Ord		&& ��ନ஢���� ᯨ᪠ ������
DO Wt_Mess

IF RECCOUNT("TMP_LIST1") = 0
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"���� ��祣� �� �������..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  DO Ord_List
ENDIF

USE IN TMP_LIST1
DELETE FILE (m.tmpo_path+m.tmpName1+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName1+".cdx")
USE IN TMP_TIT1
DELETE FILE (m.tmpo_path+m.tmpName2+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName2+".cdx")

RETURN


*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prp_Ord      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   �����⮢�� �६������ 䠩�� ������                   �
*�                                                                        �
*����������������������������������������������������������� 05.10.2005 ���
PROCEDURE Prp_Ord

PRIVATE s_sav, tmpOrd, tmpKey, ordCount, tmpNote, tmpNew, tmpSup

m.s_sav = SELECT()

SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName2) ;
	(	TMP_ORD		N( 5), ;
		ORD_NUM		C(10), ;
		ORD_TYPE	C( 1), ;
		MANAGER		N( 5), ;
		FIRM		C( 3), ;
		SUP_CODE	N( 8), ;
		NOTE		C(32)  )
USE (m.tmpo_path+m.tmpName2) EXCLUSIVE ALIAS TMP_TIT1

SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName1) ;
	(	ORD_NEW		N( 5), ;
	    CODE		N( 7), ;
	    PREFIX		C( 4), ;
	    NAME		C(35), ;
	    PRODUCER	C( 8), ;
		SEGMENT		C( 6), ;
		TMP_ORD		N( 5), ;
		QNT			N(10), ;
		LIST_NUM	C( 6), ;
		ORD_TYPE	C( 1), ;
		MANAGER		N( 5)  )
USE (m.tmpo_path+m.tmpName1) EXCLUSIVE ALIAS TMP_LIST1
SELECT 0
*USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS PRS_5A05
*SELECT 0
USE (m.path_comm+"SUBJECT") SHARED AGAIN ORDER TAG STO_CODE ALIAS SJ_5A05
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS STO_5A05
SELECT 0
USE (m.base_path+"LINK_ORD") SHARED AGAIN ORDER TAG LIST_CODE ALIAS LO_5A05
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS LT_5A05
SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ORDER TAG DUE_CODE ALIAS LD_5A05
SET RELATION TO LIST_NUM+STR(CODE) INTO LO_5A05, ;
                CODE INTO SJ_5A05, ;
                CODE INTO STO_5A05

SCAN  && FOR .NOT. FOUND("LO_5A05")
  IF .NOT. SEEK(VAL(LIST_NUM), "LT_5A05")
    LOOP
  ENDIF
  IF .NOT. EMPTY(LT_5A05.SAL_NUM)
    LOOP
  ENDIF
  m.tmpOrd = LD_5A05.QNT_DUE
  IF FOUND("LO_5A05")
    SELECT LO_5A05
    SCAN REST WHILE CODE = LD_5A05.CODE .AND. LIST_NUM = LD_5A05.LIST_NUM
      m.tmpOrd = m.tmpOrd-LO_5A05.QNT
    ENDSCAN
    IF m.tmpOrd <= 0
      SELECT LD_5A05
      LOOP
    ENDIF
  ENDIF
  IF EMPTY(LD_5A05.NEW_ORD)
    m.tmpNew = Ord_Type(LD_5A05.CODE, LD_5A05.ORD_TYPE = "R")
  ELSE
    m.tmpNew = VAL(ALLTRIM(LD_5A05.NEW_ORD))
  ENDIF
  SELECT TMP_LIST1
  APPEND BLANK
  REPLACE CODE		WITH LD_5A05.CODE,     ;
          PREFIX	WITH STO_5A05.PREFIX,  ;
          NAME		WITH STO_5A05.NAME,    ;
          PRODUCER	WITH STO_5A05.PRODUCER,;
          SEGMENT	WITH SJ_5A05.SEGMENT,  ;
          QNT		WITH m.tmpOrd,         ;
          LIST_NUM	WITH LD_5A05.LIST_NUM, ;
          MANAGER	WITH LT_5A05.WHO_CR,   ;
          ORD_NEW	WITH m.tmpNew,		   ;
          ORD_TYPE	WITH IIF(LD_5A05.ORD_TYPE = "R", "R", "B")
  SELECT LD_5A05
ENDSCAN
DO Ord_Type
SELECT TMP_LIST1
INDEX ON ORD_TYPE+STR(ORD_NEW) TAG ORD
GO TOP
m.tmpKey = "***********"
m.ordCount = 0

USE m.base_path+"ORD_TYPE" SHARED AGAIN ORDER TAG CODE ALIAS OO_5A05 IN 0
SCAN
  IF m.tmpKey # ORD_TYPE+STR(ORD_NEW)
    m.tmpKey =  ORD_TYPE+STR(ORD_NEW)
    m.ordCount = m.ordCount+1
    m.tmpSup = 10		&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    IF .NOT. EMPTY(ORD_NEW)
      = SEEK(ORD_NEW, "OO_5A05")
      IF OO_5A05.SUP_CODE # 0
        m.tmpSup = OO_5A05.SUP_CODE
      ENDIF
      m.tmpNote = ALLTRIM(OO_5A05.ORD_NOTE)
    ELSE
      m.tmpNote = IIF(ORD_TYPE="R", "����.", "����.")
    ENDIF
    SELECT TMP_TIT1
    APPEND BLANK
    REPLACE TMP_ORD  WITH m.ordCount,        ;
    		SUP_CODE WITH m.tmpSup,          ;
    		FIRM     WITH m.def_firm,        ;
            ORD_TYPE WITH LEFT(m.tmpKey, 1), ;
            MANAGER  WITH TMP_LIST1.MANAGER, ;
            NOTE     WITH m.tmpNote
    SELECT TMP_LIST1
  ENDIF
  REPLACE TMP_ORD WITH m.ordCount
ENDSCAN
DELETE TAG ORD
INDEX ON STR(TMP_ORD)+NAME+PREFIX+PRODUCER TAG ORD

USE IN LD_5A05
USE IN OO_5A05
USE IN LT_5A05
USE IN LO_5A05
USE IN SJ_5A05
USE IN STO_5A05
*USE IN PRS_5A05

SELECT (m.s_sav)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Ord_list     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    ��ᬮ�� � �ନ஢���� �������                     �
*�                                                                        �
*����������������������������������������������������������� 05.10.2005 ���
PROCEDURE Ord_list


*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

m.s_sav = SELECT()

SELECT 0
USE (m.base_path+"FIRMS") SHARED AGAIN ALIAS FRM_5A14 ORDER TAG FIRM_CODE
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS SUP_5A14 ORDER TAG CUS_CODE
SELECT 0
DO Use_Dummy
SELECT TMP_TIT1
SET RELATION TO SUP_CODE INTO SUP_5A14, FIRM INTO FRM_5A14
GO TOP

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Temp OF (menu_name) PROMPT "����⮢�� ������"
ON PAD Temp OF (menu_name) ACTIVATE POPUP Temp

  DEFINE POPUP Temp MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Temp PROMPT "��ᬮ��" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 1 OF Temp DO Sw_Mode WITH "View"

  DEFINE BAR  2 OF Temp PROMPT "�।���⨥" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Temp DO Sw_Mode WITH "Firm"

  DEFINE BAR  3 OF Temp PROMPT "���⠢騪" KEY Ctrl-A, "F3"
  ON SELECTION BAR 3 OF Temp DO Sw_Mode WITH "Sup"

DEFINE PAD Order OF (menu_name) PROMPT "�����"
ON PAD Order OF (menu_name) ACTIVATE POPUP Order

  DEFINE POPUP Order MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Order PROMPT "�������" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Order DO Sw_Mode WITH "Create"

  DEFINE BAR  2 OF Order PROMPT "������" KEY Ctrl-A, "Ctrl-P"
  ON SELECTION BAR 2 OF Order DO Sw_Mode WITH "Print"

  DEFINE BAR  3 OF Order PROMPT "��।���" KEY Ctrl-A, "Home"
  ON SELECTION BAR 3 OF Order DO Sw_Mode WITH "Send"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, tmpCode, mss, rc_sav, tmpNm
m.ln = MIN(WROWS("")-11, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
m.wd = 43

DO D_Wins WITH m.ln, m.wd, "��ନ஢���� ��� �� ���⠢��", 0, 2

@ 1, 2 SAY ;
"��ć���� �ďਬ�砭��"
*9999999999 ....:....!....:....!....:....!..

m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter  KEYBOARD CHR(23)
    ON KEY LABEL F2     DO Sw_Mode WITH "Firm"
    ON KEY LABEL F3     DO Sw_Mode WITH "Sup"
    ON KEY LABEL Tab    DO Sw_Mode WITH "Create"
    ON KEY LABEL Ctrl+P DO Sw_Mode WITH "Print"
    ON KEY LABEL Home   DO Sw_Mode WITH "Send"

*
*    BROWSE - ����
*
    BROWSE FIELDS ORD_NUM:H="", ;
                  NOTE:H="",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN D_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "View")
    ENDIF

  CASE m.what_do = "View"    && ��ᬮ�� ����⮢��

    DO V_Ord WITH TMP_ORD
    m.what_do = "List"

  CASE m.what_do = "Firm"    && ����� �।�����

    IF .NOT. EMPTY(ORD_NUM)
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"����� 㦥 ᮧ���, �������� �।���⨥ �����..."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      m.tmpCode = FIRM
      DO Ch_Firm WITH m.tmpCode
      REPLACE FIRM WITH m.tmpCode
    ENDIF

    m.what_do = "List"

  CASE m.what_do = "Sup"     && ����� ���⠢騪�

    IF .NOT. EMPTY(ORD_NUM)
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"����� 㦥 ᮧ���, �������� ���⠢騪� �����..."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      m.tmpCode = SUP_CODE
      DO Customer WITH m.tmpCode
      REPLACE SUP_CODE WITH m.tmpCode
    ENDIF

    m.what_do = "List"

  CASE m.what_do = "Create"  && �������� ���

*    m.rc_sav = RECNO()
*    SCAN
    IF .NOT. EMPTY(ORD_NUM)
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"����� 㦥 ᮧ���..."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DO Mk_Ord
    ENDIF
*    ENDSCAN
*    GO m.rc_sav
    m.what_do = "List"

  CASE m.what_do = "Print"   && ����� ���

    IF EMPTY(ORD_NUM)
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"����� �� �� ᮧ���..."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DO P_Ord WITH FIRM, ORD_NUM, DATE()
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Send"    && ��।�� ���

    IF EMPTY(ORD_NUM)
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"����� �� �� ᮧ���..."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      m.tmpPth = Arm_P("ORDER.PATH")
      m.tmpPth = Int_Dir(m.tmpPth, "�㤠 �������� १����")
      IF .NOT. EMPTY(m.tmpPth)
        IF EMPTY(m.tmpPth)
          m.tmpPth = LEFT(m.tmpo_path, 3)
        ENDIF
*        m.rc_sav = RECNO()
*        SCAN
          m.tmpNm = "real"
          DO CASE
          CASE "����." $ NOTE
            m.tmpNm = "buy"
          CASE "�ந��." $ NOTE
            m.tmpNm = "prod"
          ENDCASE
          DO s_Ord1 WITH FIRM, ORD_NUM, DATE(), m.tmpNm, m.tmpPth
*        ENDSCAN
        DO Arm_P WITH "ORDER.PATH", m.tmpPth
*        GO m.rc_sav
      ENDIF
    ENDIF
    m.what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
USE IN SUP_5A14
USE IN FRM_5A14

SELECT (m.s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Draw_Foot    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE D_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
@ WROWS()-3, 2 SAY PADR(FRM_5A14.L_NAME, 43)
@ WROWS()-2, 2 SAY PADR(SUP_5A14.CUS_NAME, 43)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_Ord        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            ��ᬮ�� ������                             �
*�                                                                        �
*����������������������������������������������������������� 05.10.2005 ���
PROCEDURE V_Ord
PARAMETERS prmOrd

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

PRIVATE tmpName
m.s_sav = SELECT()

SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS CCC_5B09 ORDER TAG CUS_CODE
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS TTT_5B09 ORDER TAG LST_NUM
SET RELATION TO CLI_CODE INTO CCC_5B09
SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ALIAS PPP_5B09 ORDER TAG CODE
SELECT 0
DO Use_Dummy
SELECT TMP_LIST1
SET RELATION TO VAL(LIST_NUM) INTO TTT_5B09, MANAGER INTO PPP_5B09
GO TOP

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

*DEFINE PAD View OF (menu_name) PROMPT "�� �������" KEY Ctrl-A, "Enter"
*ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, tmpName
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = 60

DO D_Wins WITH m.ln, m.wd, "����� "+ALLTRIM(TMP_TIT1.NOTE), 0, 2
@ 1, 7 SAY "������������"
@ 1, 55 SAY "���-��"
@ WROWS()-4, 2 SAY "���᮪Č�������"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter  KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE KEY STR(m.prmOrd)    ;
           FIELDS PREFIX:H="",  ;
                  NAME:H="",    ;
                  PRODUCER:H="",;
                  QNT:H="":P="9999999999",;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           WHEN D_Foot0()  ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
*      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "View")
      m.what_do = "Exit"
    ENDIF

  CASE what_do = "View"    && View
    m.tmpName = SYS(3)
    DO Prp_Man WITH CODE, TMP_TIT1.FIRM, TMP_TIT1.ORD_NUM
    DO V_Man WITH CODE
    DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
    what_do = "List"
  
  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
USE IN TTT_5B09
USE IN PPP_5B09
USE IN CCC_5B09
SELECT (m.s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Draw_Foot    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE D_Foot0

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
@ WROWS()-3, 2 SAY LIST_NUM COLOR SCHEME 1
@ WROWS()-3, COL()+1 SAY PADR(  ;
                         ALLTRIM(PPP_5B09.FAMILY)+" "+ ;
                         LEFT(PPP_5B09.NAME, 1)+" "+   ;
                         LEFT(PPP_5B09.S_NAME, 1), 43) ;
                         COLOR SCHEME 1
@ WROWS()-2, 2 SAY CCC_5B09.CUS_NAME COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Mk_Ord       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            �������� ������.                            �
*�                                                                        �
*����������������������������������������������������������� 06.10.2005 ���
PROCEDURE Mk_Ord

PRIVATE tmpName, s_sav
m.s_sav = SELECT()
m.tmpNum = Doc_Num("ORDER", TMP_TIT1.FIRM, m.arm, DATE())
m.tmpNum = STR(m.tmpNum, 10)
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS STO_5A06
SELECT 0
USE (m.base_path+"ORDER") SHARED AGAIN ALIAS OD_5A06
m.tmpName = SYS(3)
COPY STRUCTURE TO (m.tmpo_path+m.tmpName)
SELECT 0
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_5A06
INDEX ON CODE TAG CODE
SELECT 0
USE (m.base_path+"ORD_TIT") SHARED AGAIN ALIAS OT_5A06
APPEND BLANK
REPLACE ORD_NUM		WITH m.tmpNum,      ;
        ORD_DATE	WITH DATE(),        ;
        FIRM		WITH TMP_TIT1.FIRM, ;
        CUS_CODE	WITH m.sup_own,     ;
        SUP_CODE	WITH TMP_TIT1.SUP_CODE, ;
        NOTE		WITH TMP_TIT1.NOTE, ;
        WHO_ORD		WITH m.user,        ;
        WHERE_ORD	WITH m.arm
REPLACE TMP_TIT1.ORD_NUM WITH m.tmpNum
USE (m.base_path+"LINK_ORD") SHARED AGAIN ALIAS LO_5A06
SELECT TMP_LIST1
SET RELATION TO CODE INTO STO_5A06
SEEK STR(TMP_TIT1.TMP_ORD)
SCAN REST WHILE TMP_TIT1.TMP_ORD = TMP_ORD
  SELECT LO_5A06
  APPEND BLANK
  REPLACE CODE		WITH TMP_LIST1.CODE,     ;
          LIST_NUM	WITH TMP_LIST1.LIST_NUM, ;
          ORD_FIRM	WITH TMP_TIT1.FIRM,      ;
          ORD_NUM	WITH m.tmpNum,           ;
          ORD_YEAR	WITH STR(YEAR(DATE()),4),;
          MANAGER	WITH TMP_LIST1.MANAGER,  ;
          QNT		WITH TMP_LIST1.QNT
  SELECT TMP_5A06
  IF .NOT. SEEK(TMP_LIST1.CODE)
    APPEND BLANK
    REPLACE FIRM		WITH TMP_TIT1.FIRM,		;
            ORD_NUM		WITH m.tmpNum,			;
            ORD_DATE	WITH DATE(),			;
            CODE		WITH TMP_LIST1.CODE,	;
            PREFIX		WITH STO_5A06.PREFIX,	;
            NAME		WITH STO_5A06.NAME,		;
            PRODUCER	WITH STO_5A06.PRODUCER
  ENDIF
  REPLACE QNT WITH QNT+TMP_LIST1.QNT
  SELECT TMP_LIST1
ENDSCAN
USE IN TMP_5A06
SELECT OD_5A06
APPEND FROM (m.tmpo_path+m.tmpName+".dbf")
USE IN OD_5A06
USE IN STO_5A06
USE IN LO_5A06

DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")

SELECT (m.s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_Man        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�            ���᮪ �������஢, ��������� ������                       �
*�                                                                        �
*����������������������������������������������������������� 08.11.2005 ���
PROCEDURE V_Man
PARAMETERS prmCode

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

PRIVATE tmpName
m.s_sav = SELECT()

SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS STO_5B08
SEEK m.prmCode
m.tmpName = ALLTRIM(PREFIX)+ALLTRIM(NAME)+" "+IIF(EMPTY(PRODUCER), "", "/"+ALLTRIM(PRODUCER)+"/")
USE
DO Use_Dummy
SELECT TMP_MAN
GO TOP

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = 50

DO D_Wins WITH m.ln, m.wd, m.tmpName, 0, 1
@ 1, 2 SAY ;
"��������������������������������đ��᮪��Ċ��-��"
*....:....!....:....!....:....!.. ....:. ....:....!
@ WROWS()-3, 2 SAY "���㯠⥫�"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter  KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS MANAGER:H="",  ;
                  LIST_NUM:H="",    ;
                  QNT:H="":P="9999999999",;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN D_Foot1()  ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = "Exit"
    ENDIF

*  CASE what_do = "View"    && View
*
*    DO V_Man WITH CODE, TMP_TIT1.FIRM, TMP_TIT1.ORD_NUM
*    what_do = "List"
  
  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
USE IN TMP_MAN
SELECT (m.s_sav)

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prp_Man      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�            �����⮢�� ᯨ᪠ �������஢, ��������� ������            �
*�                                                                        �
*����������������������������������������������������������� 08.11.2005 ���
PROCEDURE Prp_Man
PARAMETERS tmpCode, tmpFirm, tmpDoc

PRIVATE s_sav, tmpQnt

m.s_sav = SELECT()
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
  ( MANAGER  C(32), ;
    LIST_NUM C( 6), ;
    CUSTOMER C(50), ;
    QNT      N(10)  )
USE (m.tmpo_path+m.tmpName) ALIAS TMP_MAN
SELECT 0
USE (m.base_path+"LINK_ORD") SHARED AGAIN ORDER TAG LIST_CODE ALIAS LNK_5B08
SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS PRS_5B08
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS CUS_5B08
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS TIT_5B08
SET RELATION TO WHO_CR INTO PRS_5B08, CLI_CODE INTO CUS_5B08
SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ORDER TAG DUE_CODE ALIAS LST_5B08
SET RELATION TO VAL(LIST_NUM) INTO TIT_5B08

SEEK m.tmpCode
SCAN REST WHILE m.tmpCode = CODE
  m.tmpQnt = 0
  IF EMPTY(m.tmpDoc)
    m.tmpQnt = QNT_DUE
    SELECT LNK_5B08
    SEEK LST_5B08.LIST_NUM+STR(LST_5B08.CODE)
    SCAN REST WHILE CODE = LST_5B08.CODE .AND. LIST_NUM = LST_5B08.LIST_NUM
      m.tmpQnt = m.tmpQnt-QNT
    ENDSCAN
  ELSE
    SELECT LNK_5B08
    SEEK LST_5B08.LIST_NUM+STR(LST_5B08.CODE)
    SCAN REST WHILE CODE = LST_5B08.CODE .AND. LIST_NUM = LST_5B08.LIST_NUM
      IF ORD_FIRM = m.tmpFirm .AND. ORD_NUM = m.tmpDoc .AND. ORD_YEAR = LEFT(DTOS(DATE()), 4)
        m.tmpQnt = m.tmpQnt+QNT
      ENDIF
    ENDSCAN      
  ENDIF
  IF m.tmpQnt # 0
    SELECT TMP_MAN
    APPEND BLANK
    REPLACE LIST_NUM WITH LST_5B08.LIST_NUM, ;
            QNT      WITH m.tmpQnt,          ;
            CUSTOMER WITH CUS_5B08.CUS_NAME, ;
            MANAGER  WITH ALLTRIM(PRS_5B08.FAMILY)+" "+LEFT(PRS_5B08.NAME, 1)+" "+LEFT(PRS_5B08.S_NAME, 1)
  ENDIF
  SELECT LST_5B08
ENDSCAN

USE IN LST_5B08
USE IN TIT_5B08
USE IN PRS_5B08
USE IN LNK_5B08
USE IN CUS_5B08

SELECT (m.s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Draw_Foot    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE D_Foot1

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
@ WROWS()-2, 2 SAY CUSTOMER

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.