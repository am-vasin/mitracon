*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Prp_Ord      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                            ��錢�� �� 瓷���                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 11.11.2005 様�
PROCEDURE Prp_Ord
PARAMETERS prmList
*
* prmList - ���ム 甎�瓷� (C)
*

PRIVATE s_sav, mss, pOnly

pOnly = .F.
s_sav = SELECT()
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS LT_5B11
SEEK VAL(m.prmList)
IF .NOT. EMPTY(LT_5B11.SAL_NUM)
*  DIMENSION mss[3]
*  mss[1] = ""
*  mss[2] = CHR(0)+"���甌� 礒� �牀���! ����� ��Ε� °碎 ��錢��..."
*  mss[3] = ""
*  DO Out_Mess WITH 7, "mss"
*  USE
*  SELECT (m.s_sav)
*  RETURN
  pOnly = .T.
ENDIF

DO V_Ord

USE IN LT_5B11
SELECT (m.s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� V_Ord        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                  蹍甃�矗 � �÷�｀皖� 甎�瓷� ��錢��.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.11.2005 陳�
PROCEDURE V_Ord

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
m.s_sav = SELECT()
PRIVATE ord_hold

SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS CUS_5B11
SEEK LT_5B11.CLI_CODE
SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS SM_5B11
SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS MN_5B11
SELECT 0
USE (m.base_path+"ORD_T") SHARED AGAIN ORDER TAG DOC_NUM ALIAS ORD_5B11
m.ord_hold = Scan_Ord(m.prmList)
SET RELATION TO WHO INTO MN_5B11, STO_MARK INTO SM_5B11
SELECT 0
DO Use_Dummy
SELECT ORD_5B11

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View OF (menu_name) PROMPT "蹍甃." KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

IF .NOT. pOnly
  DEFINE PAD Status OF (menu_name) PROMPT "���矣�" KEY Ctrl-A, "Tab"
  ON SELECTION PAD Status OF (menu_name) DO Sw_Mode WITH "Status"

  DEFINE PAD Mark OF (menu_name) PROMPT "�癶." KEY Ctrl-A, "Home"
  ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

  DEFINE PAD Create OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Ins"
  ON SELECTION PAD Create OF (menu_name) DO Sw_Mode WITH "Create"
ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = 71

DO D_Wins WITH m.ln, m.wd, "���甌� � "+ALLTRIM(m.prmList)+": ��錢�� �� 瓷���" , 1, 0
@ 1, 2 SAY "��┘�� "
@ 1, COL() SAY CUS_5B11.CUS_NAME COLOR SCHEME 1
USE IN CUS_5B11
@ 2, 2 SAY ;
"陳���祠陳陳����陳陳��Г���陳陳陳陳陳陳陳��÷���陳陳陳陳陳陳陳���矣�"
*999 999 ��.��.���� ....:....!....:....! ....:....!....:....! �� 甌；��碎
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+P DO Sw_Mode WITH "Print"
    IF .NOT. pOnly
      ON KEY LABEL Tab    DO Sw_Mode WITH "Status"
      ON KEY LABEL Home   DO Sw_Mode WITH "Mark"
      ON KEY LABEL Ins    DO Sw_Mode WITH "Create"
    ENDIF

*
*    BROWSE - �キ�
*
    BROWSE KEY m.prmList REST ;
           FIELDS ORD_NUM:3:H="",     ;
                  CHECK_S:H="",       ;
                  ORD_DATE:H="",      ;
                  P001 = IIF(EMPTY(WHO), "", MN_5B11.FAMILY):20:H="", ;
                  P002 = IIF(EMPTY(STO_MARK), "", SM_5B11.FAMILY):20:H="", ;
                  S001 = IIF(EMPTY(REQ_DATE), "�� 甌；��碎", ""):11:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "View")
    ENDIF

  CASE m.what_do = "View"    && 蹍甃�矗 ��錢��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO V_Ord1 WITH m.prmList, ORD_NUM
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Create"  && ��Г��┘ ��錢��

    DO Md_Ord
    m.what_do = "List"

  CASE m.what_do = "Print"  && �ョ�碎 ��錢��

    IF .NOT. BOF() .AND. .NOT. EOF()
      IF EMPTY(REQ_DATE)
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"��� 甄イ礇� �爛Δ� ├�キ�碎 痰�矣� ��錢��..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
      ELSE
        IF new_ord
          DO Prn_OrdN WITH VAL(LIST_NUM), ORD_NUM
        ELSE
          DO Prn_Ord WITH VAL(LIST_NUM), ORD_NUM
        ENDIF
      ENDIF
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Status"  && ���矣� ��錢��

    IF .NOT. BOF() .AND. .NOT. EOF()
      IF .NOT. EMPTY(REQ_DATE)
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+'�ムア�� ��錢�� � 痰�矣� "�� 甌；��碎" ���爛薀�...'
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
      ELSE
        IF EMPTY(STO_MARK)
          DO Clear_H WITH LIST_NUM
        ELSE
          DIMENSION mss[3]
          mss[1] = ""
          mss[2] = CHR(0)+'��錢�� 礒� 竄▲爨ぅ��...'
          mss[3] = ""
          DO Out_Mess WITH 7, "mss"
        ENDIF
      ENDIF
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Mark"  && �癶ムΔキ┘ ��錢��

    IF .NOT. BOF() .AND. .NOT. EOF()
      IF .NOT. EMPTY(WHO)
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"��錢�� �� 矗ァ礇� 竄▲爨ぅ���..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
      ELSE
        REPLACE WHO WITH m.user
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
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
USE IN SM_5B11
USE IN MN_5B11
USE IN ORD_5B11
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Md_Ord       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            ��Г��┘ ��錢��                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.11.2005 陳�
PROCEDURE Md_Ord

PRIVATE s_sav, tmpName, ordLen, isDef, delta, mss, tmpCnt, tmpZone, tmpDoc
PRIVATE tmpNote, tmpPrint, tmpStop, ordList, tmpNeg, tmpPos

m.tmpNote  = SPACE(40)
m.tmpPrint = .T.
m.tmpStop  = .F.
m.ordLen = 0
m.s_sav = SELECT()
SELECT 0
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	ZONE	C( 2), ;
		ADDRESS C( 8), ;
		TMP_ORD	N( 3), ;
		LINK	N(10), ;
		CODE	N( 7), ;
		QNT		N(10)  )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_5B12
SELECT 0
USE (m.base_path+"ORD_D") SHARED AGAIN ALIAS OD_5B12
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS ST_5B12 ORDER TAG CODE
SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS L_D_5B12 ORDER TAG LIST
SET RELATION TO CODE INTO ST_5B12

m.isDef = .F.
SEEK m.prmList
SCAN REST WHILE m.prmList = LIST_NUM FOR QNT # QNT_REQ
  IF CODE = 0 .AND. QNT # 0
    m.isDef = .T.
    LOOP
  ENDIF
  m.delta = QNT-QNT_REQ
  IF m.delta > 0 .AND. m.delta > ST_5B12.QNT
    m.delta = ST_5B12.QNT
    m.isDef = .T.
    IF m.delta = 0
      LOOP
    ENDIF
  ENDIF
  m.tmpCode = WhatZone(ST_5B12.ADDRESS)
  SELECT TMP_5B12
  APPEND BLANK
  REPLACE ZONE		WITH m.tmpCode,         ;
		  ADDRESS	WITH ST_5B12.ADDRESS,   ;
		  LINK		WITH RECNO("L_D_5B12"), ;
		  CODE		WITH L_D_5B12.CODE,     ;
		  QNT		WITH m.delta
  UNLOCK
  SELECT L_D_5B12
ENDSCAN

SET RELATION TO
SET ORDER TO

IF USED("ZONES")
  USE IN ZONES
ENDIF

IF RECCOUNT("TMP_5B12") = 0
  IF m.isDef
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"� 甎�瓷� メ碎 ぅ筥罔�. ��錢�� �� 痲�爼�牀����."
    mss[3] = ""
  ELSE
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"�瓮 礒� ��������. ��錢�� �� 痲�爼�牀����."
    mss[3] = ""
  ENDIF
  DO Out_Mess WITH 7, "mss"
  USE IN TMP_5B12
  USE IN L_D_5B12
  USE IN OD_5B12
  USE IN ST_5B12
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
  SELECT (m.s_sav)
  RETURN
ENDIF

IF m.isDef
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"� 甎�瓷� メ碎 ぅ筥罔�. �� ≡� ����腑痰〓 ������ � ��錢��."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
ENDIF


IF OrdMode()
  SELECT TMP_5B12
  INDEX ON ADDRESS TAG ADDRESS
  SET RELATION TO CODE INTO ST_5B12, LINK INTO L_D_5B12
  GO TOP
  m.tmpZone = ""
  m.tmpDoc  = 0
  m.tmpCnt  = 0
  STORE 0 TO m.tmpNeg, m.tmpPos

  SCAN
*    IF ST_5B12.QNT <= 0
*      LOOP
*    ENDIF
    IF m.tmpCnt > m.ordLen .AND. m.ordLen > 0 .OR. .NOT. m.tmpZone == ZONE
      m.tmpZone = ZONE
      IF m.tmpNeg+m.tmpPos # 0
        REPLACE ORD_5B11.SIZE_NEG WITH m.tmpNeg, ;
                ORD_5B11.SIZE_POS WITH m.tmpPos
      ENDIF
      m.tmpDoc = m.tmpDoc+1
      DIMENSION ordList[m.tmpDoc]
      SELECT ORD_5B11
      APPEND BLANK
      REPLACE LT_5B11.ORD_NUM WITH LT_5B11.ORD_NUM+1
      ordList[m.tmpDoc] = LT_5B11.ORD_NUM
      REPLACE LIST_NUM	WITH L_D_5B12.LIST_NUM, ;
              LIST_NAME	WITH LT_5B11.LST_NAME,  ;
              ORD_NUM	WITH LT_5B11.ORD_NUM,   ;
              ORD_DATE	WITH DATE(),            ;
              ORD_TIME	WITH TIME(),            ;
              ZONE		WITH TMP_5B12.ZONE,     ;
              WHO		WITH m.user,            ;
              WHERE		WITH m.arm,             ;
              NOTE		WITH m.tmpNote
      m.tmpCnt = 0
      STORE 0 TO m.tmpNeg, m.tmpPos
    ENDIF
    m.tmpCnt = m.tmpCnt+1
    SELECT OD_5B12
    APPEND BLANK
    REPLACE LIST_NUM WITH ORD_5B11.LIST_NUM, ;
            ORD_NUM  WITH ORD_5B11.ORD_NUM,  ;
            CODE     WITH TMP_5B12.CODE,     ;
            QNT      WITH MIN(TMP_5B12.QNT, ST_5B12.QNT)
    IF QNT < 0
      m.tmpNeg = m.tmpNeg+1
    ELSE
      m.tmpPos = m.tmpPos+1
    ENDIF
    REPLACE ST_5B12.QNT      WITH ST_5B12.QNT-QNT, ;
            ST_5B12.QNT_SAVE WITH ST_5B12.QNT_SAVE+QNT
    REPLACE L_D_5B12.QNT_REQ WITH L_D_5B12.QNT_REQ+QNT
    SELECT TMP_5B12
  ENDSCAN
  IF m.tmpNeg+m.tmpPos # 0
    SELECT ORD_5B11
    REPLACE ORD_5B11.SIZE_NEG WITH m.tmpNeg, ;
            ORD_5B11.SIZE_POS WITH m.tmpPos
  ENDIF
  IF .NOT. m.tmpStop
    DO Clear_H WITH STR(LT_5B11.LST_NUM, 6)
  ENDIF

  IF m.tmpPrint .AND. m.tmpDoc # 0
    FOR m.tmpCnt = 1 TO m.tmpDoc
      IF new_ord
        DO Prn_OrdN WITH LT_5B11.LST_NUM, ordList[m.tmpCnt]
      ELSE
        DO Prn_Ord WITH LT_5B11.LST_NUM, ordList[m.tmpCnt]
      ENDIF
    ENDFOR
  ENDIF

ENDIF

USE IN TMP_5B12
USE IN L_D_5B12
USE IN ST_5B12
USE IN OD_5B12
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
SELECT (m.s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� OrdMode      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �����モ琺 ��錢��                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.11.2005 陳�
PROCEDURE OrdMode
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�     [ ] �� 甌；��碎  [ ] �ョ�碎 ��錢��     �
*� 敖 踸�ョ��┘ 陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �....:....!....:....!....:....!....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�      < OK Ctrl-W > < �皖���碎瘴 Esc >      �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 48, "��Г��┘ ��錢��"
DO Sun_Bord WITH  3,  3,  5, 44, " 踸�ョ��┘ "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2,  7 GET m.tmpStop PICTURE "@*C �� 甌；��碎" VALID ExitExe() WHEN m.ord_hold
@ 2, 24 GET m.tmpPrint WHEN .NOT. m.tmpStop PICTURE "@*C �ョ�碎 ��錢��"
@ 4,  4 GET m.tmpNote
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ExitExe      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                 蹍������ �諷��� ├ ���� "�� 甌；��碎"                 �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.11.2005 陳�
PROCEDURE ExitExe

IF m.tmpStop
  m.tmpPrint = .F.
  SHOW GET m.tmpPrint
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Scan_Ord     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�          �瘁�イ����┘ ��錢�� �� �爛がモ ���モ�� "�� 甌；��碎"          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 17.12.2005 陳�
PROCEDURE Scan_Ord
PARAMETERS l_num	&& ���ム 甎�瓷� (C)

PRIVATE s_sav, no, nc

STORE 0 TO m.no, m.nc

SEEK m.l_num
SCAN REST WHILE LIST_NUM = m.l_num
  IF .NOT. EMPTY(REQ_DATE)
    m.no = m.no+1	&& ���-〓 ��錢��, ��皰琺� ��ぎ 甌；��碎
  ENDIF
  IF .NOT. EMPTY(STO_MARK)
    m.nc = m.nc+1	&& ���-〓 竄▲爨ぅ��諷 ��錢��
  ENDIF
ENDSCAN
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS LT_5C17 ORDER TAG LST_NUM
SEEK VAL(m.l_num)
SELECT (m.s_sav)
SEEK m.l_num
IF m.no = 0 .AND. m.nc = 0	&& �� �き� ��錢�� �� 甌÷���, 
                            &&    �� �き� �� ��ぎ 甌；��碎
  m.no = 0
  SCAN REST WHILE LIST_NUM = m.l_num	&& �� ≡鍼┤ ����爿覃 �腮��ガ ���ム� 痺��. ��錢��
    IF .NOT. EMPTY(CHECK_S)
      REPLACE CHECK_S WITH ""
    ENDIF
    m.no = MAX(m.no, ORD_NUM)
  ENDSCAN
  IF FOUND("LT_5C17")
    IF LT_5C17.ORD_NUM # m.no .OR. LT_5C17.ORD_COUNT # 0
      REPLACE LT_5C17.ORD_NUM   WITH m.no, ;
              LT_5C17.ORD_COUNT WITH 0
    ENDIF
  ENDIF
  USE IN LT_5C17
  GO TOP
  RETURN .T.
ENDIF

*
*   ��錢�� �礒�� 甌；��碎!
m.no = 0
m.nc = 0
SCAN REST WHILE LIST_NUM = m.l_num	&& �� ≡鍼┤ ����爿覃 �腮��ガ ���ム� 痺��. ��錢��
  IF EMPTY(REQ_DATE)
    REPLACE REQ_DATE WITH DATE()
  ENDIF
  IF EMPTY(STO_MARK)
    m.nc = m.nc+1		&& ��錢��, 矗ァ竡薑� ��ぁ�爲�
  ENDIF
  m.no = MAX(m.no, ORD_NUM)
ENDSCAN
IF FOUND("LT_5C17")
  IF LT_5C17.ORD_NUM # m.no .OR. LT_5C17.ORD_COUNT # m.nc
    REPLACE LT_5C17.ORD_NUM   WITH m.no, ;
            LT_5C17.ORD_COUNT WITH m.nc
  ENDIF
ENDIF
USE IN LT_5C17
GO TOP
RETURN .F.
