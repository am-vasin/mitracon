*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Cus_Bank     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         ���甌� ������ ��┘���                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 22.10.2006 様�
PROCEDURE Cus_Bank
PARAMETERS prmCode, prmFlags, prmStart
*
*  prmCode  - ��� ��┘���
*  prmFlags - 筰�Κ� (痰牀�� - �竅皰/�ク竅皰): 1-�襦��, 2- ��玻オ罔�
*  prmStart - �����讚�� Л�腑�┘ �����
*

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
m.menu_name  = "C_BNK"
m.last_mouse = 0
m.win_name   = "C_BNK"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
PRIVATE tmpName, tmpFlags, retVal
m.retVal = -1
m.s_sav = SELECT()
m.tmpFlags = IIF(TYPE("m.prmFlags")="C", PADR(m.prmFlags, 2), "  ")

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS ACC_61022
SEEK m.prmCode
SELECT 0
USE (m.base_path+"CUS_BIL") SHARED AGAIN ORDER TAG CUS_CODE ALIAS BIL_61022
SELECT 0
USE (m.base_path+"CUS_BANK") SHARED AGAIN ORDER TAG BANK_CODE ALIAS BANK_61022
m.tmpName = SYS(3)
SEEK STR(m.prmCode)
COPY REST WHILE CUS_CODE=m.prmCode TO (m.tmpo_path+m.tmpName) WITH CDX FOR .NOT. EMPTY(BANK_CODE)
SELECT 0
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_61022 ORDER TAG BANK_CODE
IF SEEK(m.prmCode, "BIL_61022")
  APPEND BLANK
  REPLACE	CUS_CODE	WITH BIL_61022.CUS_CODE,	;
  			BANK		WITH BIL_61022.BANK,		;
  			BIK			WITH BIL_61022.BIK,			;
  			CITY		WITH BIL_61022.CITY,		;
  			CORR_BILL	WITH BIL_61022.CORR_BILL,	;
  			ACC_BILL	WITH BIL_61022.ACC_BILL
ENDIF
IF TYPE("m.prmStart") = "N"
  LOCATE FOR BANK_CODE = m.prmStart
  IF .NOT. FOUND()
    GO TOP
  ENDIF
ELSE
  GO TOP
ENDIF

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF .NOT. EMPTY(SUBSTR(m.tmpFlags, 1, 1))
  DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"
ENDIF

IF .NOT. EMPTY(SUBSTR(m.tmpFlags, 2, 1))
  DEFINE PAD Modi OF (menu_name) PROMPT "��玻オ罔�" KEY Ctrl-A, "Ctrl-Enter"
  ON SELECTION PAD Modi OF (menu_name) DO Sw_Mode WITH "Modi"
  DEFINE PAD Add OF (menu_name) PROMPT "����←キ┘" KEY Ctrl-A, "Ins"
  ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"
ENDIF

IF .NOT. EMPTY(SUBSTR(m.tmpFlags, 1, 1))
  DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ELSE
  DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ENDIF
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd

* ��┘�� ....:....!....:....!....:....!....:....!....:....!
*���Б��┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳���牀つ陳陳陳陳陳�
*�....:....!....:....!....:....!....:....! ....:....!....:.�
*                 ��� ....:....!....:....!
*    �/� ....:....!....:....!   �/� ....:....!....:....!

m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("BANK")+FSIZE("CITY")+1

DO D_Wins WITH m.ln, m.wd, "���甌� ������ ��┘���", 1, 2
@ 1, 2 SAY "��┘��"
@ 1, 9 SAY ACC_61022.CUS_NAME COLOR SCHEME 1
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    IF .NOT. EMPTY(SUBSTR(m.tmpFlags, 1, 1))
      ON KEY LABEL Enter KEYBOARD CHR(23)
    ENDIF
    IF .NOT. EMPTY(SUBSTR(m.tmpFlags, 2, 1))
      ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "Modi"
      ON KEY LABEL Ins        DO Sw_Mode WITH "Add"
    ENDIF

*
*    BROWSE - �キ�
*
    BROWSE FIELDS BANK:H="",  ;
                  CITY:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && �襦��

    IF EMPTY(SUBSTR(m.tmpFlags, 1, 1)) .OR. BOF() .OR. EOF()
      m.what_do = "List"
    ELSE
      m.retVal = BANK_CODE
      EXIT
    ENDIF

  CASE m.what_do = "Modi"    && ��玻オ罔� ����瓱
    IF .NOT. BOF() .AND. .NOT. EOF()
      DO ModiBank WITH BANK_CODE
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Add"    && ����←キ┘ ����瓱

    DO ModiBank
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
USE IN ACC_61022
USE IN BIL_61022
USE IN BANK_61022
USE IN TMP_61022
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
SELECT (m.s_sav)

RETURN m.retVal

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
*                 ��� ....:....!....:....!
*    �/� ....:....!....:....!   �/� ....:....!....:....!

@ WROWS()-3, 18 SAY "��� "+BIK
@ WROWS()-2,  5 SAY "�/� "+CORR_BILL
@ WROWS()-2, 32 SAY "�/� "+ACC_BILL

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ModiBank     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ��玻オ罔�/ぎ��←キ┘ �����                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.10.2006 陳�
PROCEDURE ModiBank
PARAMETERS prmCode

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� ��┘�� ....:....!....:....!....:....!....:....!....:....! �
*楽����陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳���牀つ陳陳陳陳陳審
*崖....:....!....:....!....:....!....:....! ....:....!....:.崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�                 ��� ....:....!....:....!                  �
*�                                                           �
*�    �/� ....:....!....:....!   �/� ....:....!....:....!    �
*�                                                           �
*�             < OK Ctrl-W > < �皖���碎瘴 Esc >              �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, new
m.ex = 1
m.new = TYPE("m.prmCode") # "N"
PRIVATE s_sav, tmpBank, tmpCity, tmpBIK, tmpCS, tmpRS

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 63, "���� ��┘���"
DO Sun_Bord WITH  3,  2,  5, 60, ""
@ 2, 3 SAY "��┘�� "
@ 2, COL() SAY ACC_61022.CUS_NAME COLOR SCHEME 1
@ 3,  3 SAY "����"
@ 3, 44 SAY "��牀�"

*------------------------------------------------------------------------
*    �����讚�� ������キ┘ ���ォ ゛����
*
IF m.new
  m.tmpBank = SPACE(FSIZE("BANK", "BANK_61022"))
  m.tmpCity = SPACE(FSIZE("CITY", "BANK_61022"))
  m.tmpBIK  = SPACE(FSIZE("BIK",  "BANK_61022"))
  m.tmpCS   = SPACE(FSIZE("CORR_BILL", "BANK_61022"))
  m.tmpRS   = SPACE(FSIZE("ACC_BILL", "BANK_61022"))
ELSE
  m.tmpBank = TMP_61022.BANK
  m.tmpCity = TMP_61022.CITY
  m.tmpBIK  = TMP_61022.BIK
  m.tmpCS   = TMP_61022.CORR_BILL
  m.tmpRS   = TMP_61022.ACC_BILL
ENDIF

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  4,  3 GET m.tmpBank
@  4, 44 GET m.tmpCity
@  6, 19 SAY "���" GET m.tmpBIK
@  8,  6 SAY "�/�" GET m.tmpCS
@  8, 33 SAY "�/�" GET m.tmpRS

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID TstBank()

IF m.ex = 1
  PRIVATE tmpCode
  *
  * �矗���硅��ガ ゛���
  IF m.new
    SELECT TMP_61022
    GO BOTTOM
    IF BOF() .OR. EOF()
      m.tmpCode = 0
    ELSE
      m.tmpCode = BANK_CODE+1
    ENDIF
    APPEND BLANK
    REPLACE	CUS_CODE	WITH ACC_61022.CUS_CODE, ;
    		BANK_CODE	WITH m.tmpCode
  ENDIF
  REPLACE	BANK		WITH m.tmpBank,	;
  			BIK			WITH m.tmpBIK,	;
  			CITY		WITH m.tmpCity,	;
  			CORR_BILL	WITH m.tmpCS,	;
  			ACC_BILL	WITH m.tmpRS
  *
  *  ���с Ε �矗���硅��ガ ├�キキ�� � ��Д ����諷
  *
  IF BANK_CODE = 0
    SELECT BIL_61022
    IF .NOT. SEEK(TMP_61022.CUS_CODE)
      APPEND BLANK
      REPLACE	CUS_CODE	WITH ACC_61022.CUS_CODE,	;
      			CUS_NAME	WITH ACC_61022.CUS_NAME,	;
      			DATE_CORR	WITH DATE(),				;
      			WHO_CORR	WITH m.user,				;
      			WHERE_CORR	WITH m.arm
    ENDIF
    IF	BANK # m.tmpBank	.OR.	;
  		BIK # m.tmpBIK		.OR.	;
  		CITY # m.tmpCity	.OR.	;
  		CORR_BILL # m.tmpCS	.OR.	;
  		ACC_BILL # m.tmpRS
  	  REPLACE	BANK		WITH m.tmpBank,	;
  				BIK			WITH m.tmpBIK,	;
  				CITY		WITH m.tmpCity,	;
  				CORR_BILL	WITH m.tmpCS,	;
  				ACC_BILL	WITH m.tmpRS,	;
      			DATE_CORR	WITH DATE(),	;
      			WHO_CORR	WITH m.user,	;
      			WHERE_CORR	WITH m.arm
    ENDIF
  ELSE
    SELECT BANK_61022
    IF .NOT. SEEK(STR(TMP_61022.CUS_CODE)+STR(TMP_61022.BANK_CODE))
      APPEND BLANK
      REPLACE	CUS_CODE	WITH TMP_61022.CUS_CODE, ;
      			BANK_CODE	WITH TMP_61022.BANK_CODE
    ENDIF
    IF	BANK # m.tmpBank	.OR.	;
  		BIK  # m.tmpBIK		.OR.	;
  		CITY # m.tmpCity	.OR.	;
  		CORR_BILL # m.tmpCS	.OR.	;
  		ACC_BILL # m.tmpRS
  	  REPLACE	BANK		WITH m.tmpBank,	;
  				BIK			WITH m.tmpBIK,	;
  				CITY		WITH m.tmpCity,	;
  				CORR_BILL	WITH m.tmpCS,	;
  				ACC_BILL	WITH m.tmpRS
    ENDIF
  ENDIF
  SELECT TMP_61022
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TstBank      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             �諷�� � ゛����                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.10.2006 陳�
PROCEDURE TstBank

PRIVATE mss

IF m.ex # 1
  RETURN .T.
ENDIF

IF EMPTY(m.tmpBank)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��Б��┘ ����� �� ��Ε� °碎 �竍硅�..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
