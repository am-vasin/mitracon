*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Cmp_Prc      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                  �ࠢ����� 業 ᯨ᪠ � 業��� ���.                  �
*�                                                                        �
*����������������������������������������������������������� 02.05.2006 �ͼ
PROCEDURE Cmp_Prc
PARAMETERS l_num, mark_only

*
* l_num     - �����ᯨ᪠ (C);
* mark_only - ⮫쪮 �� �⬥祭�� ������
*

PRIVATE s_sav, tmpName, mss, tmpNds, tmpUSD

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS LT_6502
IF .NOT. SEEK(VAL(m.l_num))
  USE
  SELECT (m.s_sav)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"���᪠ � ����஬ "+ALLTRIM(m.l_mum)+" �� �������."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (m.s_sav)
  RETURN .F.
ENDIF
m.tmpUSD = IS_USD

IF EMPTY(BILL_NUM)
  USE
  SELECT (m.s_sav)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"� ᯨ�� "+ALLTRIM(m.l_mum)+" ��� ���."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (m.s_sav)
  RETURN .F.
ENDIF

SELECT 0
USE (m.base_path+"BILLS") SHARED AGAIN ORDER TAG DOC_NUM ALIAS BL_6502
IF .NOT. SEEK(LT_6502.BILL_FIRM+LT_6502.BILL_YEAR+LT_6502.BILL_NUM)
  USE
  USE IN LT_6502
  SELECT (m.s_sav)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"� ᯨ᪠ ��� ���."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (m.s_sav)
  RETURN .F.
ENDIF
m.tmpNDS = (100+BL_6502.NDS_)/100

IF (BL_6502.VALUTA = "$") # LT_6502.IS_USD
  USE
  USE IN LT_6502
  SELECT (m.s_sav)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"� ᯨ᪠ � � ��� ࠧ��� �����."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (m.s_sav)
  RETURN .F.
ENDIF

m.tmpName = SYS(3)
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	CODE		N( 7),  ;
		PREFIX		C( 4),  ;
		NAME		C(35),  ;
		PRODUCER	C( 8),  ;
		QNT_LIST	N(10),  ;
		QNT_BILL	N(10),  ;
		L_PRICE		N(12,2),;
		B_PRICE		N(12,2),;
		BILL_RC		N(10),  ;
		LIST_RC		N(10),  ;
		IS_DIF		L		)
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS CMP_PRC
INDEX ON IIF(CODE = 0, PREFIX+" "+NAME+" "+PRODUCER, STR(CODE, 39)) TAG POS_ID
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS ST_6502
SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ORDER TAG LIST ALIAS LD_6502
SET RELATION TO CODE INTO ST_6502
SEEK m.l_num
SCAN REST WHILE m.l_num = LIST_NUM
  IF m.mark_only .AND. EMPTY(MARK_TMP)
    LOOP
  ENDIF
  SELECT CMP_PRC
  APPEND BLANK
  REPLACE	CODE		WITH LD_6502.CODE,    ;
			QNT_LIST	WITH LD_6502.QNT,     ;
			LIST_RC		WITH RECNO("LD_6502"),;
			IS_DIF		WITH .T.

  IF EMPTY(LD_6502.CODE)
    REPLACE	PREFIX		WITH LD_6502.PREFIX, ;
			NAME		WITH LD_6502.NAME,   ;
			PRODUCER	WITH LD_6502.PRODUCER
  ELSE
    REPLACE	PREFIX		WITH ST_6502.PREFIX, ;
			NAME		WITH ST_6502.NAME,   ;
			PRODUCER	WITH ST_6502.PRODUCER
  ENDIF
  IF LT_6502.IS_USD
	REPLACE	L_PRICE	WITH ROUND(LD_6502.USD_PRICE, 2)
  ELSE
    REPLACE	L_PRICE	WITH ROUND(LD_6502.SAL_PRICE/(100+BL_6502.NDS_)*100, 2)
  ENDIF

  SELECT LD_6502
ENDSCAN
SELECT CMP_PRC
IF RECCOUNT() = 0
  IF m.mark_only
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"�� �� �⬥⨫� ������� ����樨."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"���᮪ ����..."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
  m.tmpName = DBF()
  USE
  USE IN BL_6502
  USE IN ST_6502
  USE IN LT_6502
  USE IN LD_6502
  DELETE FILE (m.tmpName)
  DELETE FILE (LEFT(m.tmpName, LEN(m.tmpName)-3)+"cdx")
  SELECT (m.s_sav)
  RETURN .F.
ENDIF
INDEX ON IIF(CODE = 0, PREFIX+" "+NAME+" "+PRODUCER, STR(CODE, 39)) TAG POS_ID

SELECT 0
USE (m.base_path+"BILLS_D") SHARED AGAIN ORDER TAG DOC_NUM ALIAS BD_6502
= SEEK(LT_6502.BILL_FIRM+LT_6502.BILL_YEAR+LT_6502.BILL_NUM)
SCAN REST WHILE DOC_NUM  = BL_6502.DOC_NUM  .AND. ;
                DOC_DATE = BL_6502.DOC_DATE .AND. ;
                FIRM     = BL_6502.FIRM

  IF EMPTY(CODE)
    IF .NOT. SEEK(NAME, "CMP_PRC")
      LOOP
    ENDIF
  ELSE
    IF .NOT. SEEK(STR(CODE, 39), "CMP_PRC")
      LOOP
    ENDIF
  ENDIF

  SELECT CMP_PRC
  REPLACE	QNT_BILL	WITH BD_6502.QNT,     ;
			B_PRICE		WITH BD_6502.PRICE_0, ;
			BILL_RC		WITH RECNO("BD_6502")
  REPLACE IS_DIF WITH B_PRICE # L_PRICE
  SELECT BD_6502
ENDSCAN

SELECT CMP_PRC
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
INDEX ON NAME+PREFIX+PRODUCER TAG DIFFS FOR IS_DIF
GO TOP
USE IN BD_6502
USE IN BL_6502
USE IN ST_6502
USE IN LT_6502
USE IN LD_6502
m.tmpName = DBF()
IF EOF("CMP_PRC") .OR. BOF("CMP_PRC")
  USE
  DELETE FILE (m.tmpName)
  DELETE FILE (LEFT(m.tmpName, LEN(m.tmpName)-3)+"cdx")
  SELECT (m.s_sav)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"�� 業� ᮢ������."
  mss[3] = ""
  DO Out_Mess WITH 5, "mss"
  SELECT (m.s_sav)
  RETURN .T.
ENDIF

DO View_Diff
USE
DELETE FILE (m.tmpName)
DELETE FILE (LEFT(m.tmpName, LEN(m.tmpName)-3)+"cdx")
SELECT (m.s_sav)

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� View_Diff    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          ��ᬮ�� १����                           �
*�                                                                        �
*����������������������������������������������������������� 02.05.2006 ���
PROCEDURE View_Diff

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
PRIVATE r_sav

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = "W_Diff"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT CMP_PRC

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Filter OF (menu_name) PROMPT "������" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Restore OF (menu_name) PROMPT "����⠭�����" KEY Ctrl-A, "Tab"
ON SELECTION PAD Restore OF (menu_name) DO Sw_Mode WITH "Restore"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = 71

DO D_Wins WITH m.ln, m.wd, "���宦����� ����� 業��� ᯨ᪠ � ���", 0, 1
@ 1, 2 SAY ;
"����č����������������������������������Ė��� �� ᯨ����Ė��� �� ����"
*���� ������������������������� �������� 999999999.99999 999999999.99999
@ WROWS()-3, 8 SAY "������ ��������"
m.what_do = "List"
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F6  DO Sw_Mode WITH "Filter"
    ON KEY LABEL Tab DO Sw_Mode WITH "Restore"

*
*    BROWSE - ����
*
    BROWSE FIELDS PREFIX:H="",     ;
                  Brw_Name=Nm_Part(NAME):25:H="",;
                  PRODUCER:H="",   ;
                  P_L = ROUND(L_PRICE*m.tmpNds, 5):P="@Z 999999999.99999", ;
                  P_B = ROUND(B_PRICE*m.tmpNds, 5):P="@Z 999999999.99999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Filter"    && ������

    IF UPPER(TAG()) = "NAME"
      SET ORDER TO TAG DIFFS
    ELSE
      SET ORDER TO TAG NAME
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Restore"    && ����⠭�����

    SET ORDER TO TAG DIFFS
    USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS LIST_6502 IN 0
    SCAN
      SELECT LIST_6502
      GO CMP_PRC.LIST_RC
      IF m.tmpUSD
        REPLACE USD_PRICE WITH CMP_PRC.B_PRICE
        REPLACE IS_SPEC WITH IIF(ROUND(USD_DEF, 2) = USD_PRICE, " ", "*")
        IF EMPTY(IS_SPEC)
          REPLACE USD_PRICE WITH USD_DEF
        ENDIF
      ELSE
        REPLACE SAL_PRICE WITH ROUND(CMP_PRC.B_PRICE*m.tmpNDS, 5)
        REPLACE IS_SPEC WITH IIF(DEF_PRICE = SAL_PRICE, " ", "*")
      ENDIF
      SELECT CMP_PRC
    ENDSCAN
    USE IN LIST_6502
    EXIT

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
SELECT (m.s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Draw_Foot    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
@ WROWS()-2, 2 SAY PREFIX COLOR SCHEME 1
@ ROW(), COL()+1 SAY NAME COLOR SCHEME 1
@ ROW(), COL()+1 SAY PRODUCER COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
