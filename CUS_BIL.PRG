*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Cus_Bil      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     �竇���皀珀�┘ �矗─竄� ��┘���                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 21.10.2006 様�
PROCEDURE Cus_Bil
PARAMETERS prmCode, prmName, stopCorr
*
* prmCode  - ��� ��┘���
* prmName  - ━� ��┘��� (�瓷�鈑�皀�讚� か� 甌↑メ皋��痰�)
* stopCorr - ���爛� ��玻オ罔� (.T.)
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
PRIVATE tmpName, tmpINN, tmpAddr, tmpPhone, tmpKPP, tmpOKONX, tmpOKPO, tmpNote
STORE "" TO m.tmpName, m.tmpINN, m.tmpAddr, m.tmpPhone, ;
            m.tmpKPP, m.tmpOKONX, m.tmpOKPO, m.tmpNote
PRIVATE nmCorr, dtCorr
STORE "" TO m.dtCorr
STORE "" TO m.nmCorr
PRIVATE bankNm, bankCity, bankBIK, corrBl, accBl, bankSw
STORE "" TO m.bankNm, m.bankCity, m.bankBIK, m.corrBl, m.accBl
m.bankSw = .F.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*���┘�� ....:....!....:....!....:....!....:....!....:....! ��� ....:....!..�
*�                                                                          �
*�    �むメ ....:....!....:....!....:....!....:....!....:....!....:....!    �
*�                                                                          �
*�  �カ. ....:....!....:. ��� ....:....!....:. ����� ....:. ���� ....:...   �
*�    敖[ ] ����陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�     �
*�    �       ��Б��┘ ....:....!....:....!....:....!....:....!       �     �
*�    �                                                               �     �
*�    �       ��牀� ....:....!....:.  ��� ....:....!....:....!        �     �
*�    �                                                               �     �
*�    ���玻. 瘍モ ....:....!....:....! ��瘍. 瘍モ ....:....!....:....!�     �
*�    青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�     �
*楽�踸�ョ��┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖....:....!....:....!....:....!....:....!....:....!....:....!....:....!..崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�      ��玻オ罔� ....:....!....:....!....:....!....:....! 99.99.9999       �
*�                    < OK Ctrl-W > < �皖���碎瘴 Esc >                      �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ex, s_sav, tmpNew
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 20, 78, "�竇���皀珀�┘ �矗─竄� ��┘���"
DO Sun_Bord WITH  7,  6, 13, 70, ""
DO Sun_Bord WITH 14,  2, 16, 75, "踸�ョ��┘"

*------------------------------------------------------------------------
*      �����讚�� ������キ┘ ���ォ ゛����
*
m.s_sav = SELECT()
SELECT 0
USE m.base_path+"ACCOUNT" SHARED AGAIN ORDER TAG CUS_CODE ALIAS CUS_61021
SEEK m.prmCode
SELECT 0
USE m.base_path+"CUS_BIL" SHARED AGAIN ORDER TAG CUS_CODE ALIAS BIL_61021
m.tmpNew = .NOT. SEEK(m.prmCode)
SELECT 0
USE m.base_path+"PERSONS" SHARED AGAIN ORDER TAG CODE ALIAS PRS_61021
DO Fil_Bl0
DO Fil_Bank

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  2,  2 SAY "��┘��" GET m.tmpName
@  2, 60 SAY "���" GET m.tmpINN
@  4,  6 SAY "�むメ" GET m.tmpAddr SIZE 1, 60
@  6,  4 SAY "�カ." GET m.tmpPhone
@  6, 26 SAY "���" GET m.tmpKPP
@  6, 47 SAY "�����" GET m.tmpOKONX
@  6, 60 SAY "����" GET m.tmpOKPO
IF m.tmpNew
  @  7, 8 SAY "����"
ELSE
  @  7, 8 GET m.bankSw PICTURE "@*C ����" VALID Bank_Lst()
ENDIF

@  8, 14 SAY "��Б��┘" GET m.bankNm
@ 10, 14 SAY "��牀�" GET m.bankCity
@ 10, 38 SAY "���" GET m.bankBIK
@ 12,  7 SAY "��玻. 瘍モ" GET m.corrBl
@ 12, 39 SAY "��瘍. 瘍モ" GET m.accBl

@ 15,  3 GET m.tmpNote

@ 17,  8 SAY "��玻オ罔�" GET m.nmCorr WHEN .F.
@ 17, 59 GET m.dtCorr WHEN .F.

IF m.stopCorr
  CLEAR GETS
  @  7, 8 GET m.bankSw PICTURE "@*C ����" VALID Bank_Lst()
  @ WROWS()-2, FLOOR(WCOLS()/2-6) GET m.ex PICTURE "@*HT \? �諷�� Esc "
ELSE
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
ENDIF
READ CYCLE

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  SELECT BIL_61021
  IF m.tmpNew
    APPEND BLANK
    REPLACE CUS_CODE WITH m.prmCode
  ENDIF
  IF m.tmpName  # BIL_61021.CUS_NAME .OR. ;
     m.tmpINN   # BIL_61021.INN .OR. ;
     m.tmpAddr  # BIL_61021.ADDRESS .OR. ;
     m.tmpPhone # BIL_61021.PHONE .OR. ;
     m.tmpKPP   # BIL_61021.KPP .OR. ;
     m.tmpOKONX # BIL_61021.OKOHX .OR. ;
     m.tmpOKPO  # BIL_61021.OKPO .OR. ;
     m.tmpNote  # BIL_61021.NOTE .OR. ;
     m.bankNm   # BIL_61021.BANK .OR. ;
     m.bankCity # BIL_61021.CITY .OR. ;
     m.bankBIK  # BIL_61021.BIK .OR. ;
     m.corrBl   # BIL_61021.CORR_BILL .OR. ;
     m.accBl    # BIL_61021.ACC_BILL
    REPLACE	BIL_61021.CUS_NAME	WITH m.tmpName,	;
    		BIL_61021.INN		WITH m.tmpINN,	;
			BIL_61021.ADDRESS	WITH m.tmpAddr,	;
			BIL_61021.PHONE		WITH m.tmpPhone,;
			BIL_61021.KPP		WITH m.tmpKPP,	;
			BIL_61021.OKOHX		WITH m.tmpOKONX,;
			BIL_61021.OKPO		WITH m.tmpOKPO,	;
			BIL_61021.NOTE		WITH m.tmpNote,	;
			BIL_61021.BANK		WITH m.bankNm,	;
			BIL_61021.CITY		WITH m.bankCity,;
			BIL_61021.BIK		WITH m.bankBIK,	;
			BIL_61021.CORR_BILL	WITH m.corrBl,	;
			BIL_61021.ACC_BILL	WITH m.accBl,	;
			BIL_61021.DATE_CORR	WITH DATE(),	;
			BIL_61021.WHO_CORR	WITH m.user,	;
			BIL_61021.WHERE_CORR WITH m.arm
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
USE IN CUS_61021
USE IN BIL_61021
USE IN PRS_61021
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fil_Bl0      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�            ������キ┘ ���ォ ＜紕��皀珀��� �矗─竄�� ��┘���            �
*�                       (．� �����≡��� �矗─竄��)                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.10.2006 陳�
PROCEDURE Fil_Bl0


IF m.tmpNew
  IF .NOT. EMPTY(m.prmName)
    m.tmpName = PADR(m.prmName, FSIZE("CUS_NAME", "BIL_61021"))
  ELSE
    m.tmpName = ALLTRIM(CUS_61021.CUS_NAME)
    m.tmpName = PADR(m.tmpName, FSIZE("CUS_NAME", "BIL_61021"))
  ENDIF
  m.tmpINN   = SPACE(FSIZE("INN", "BIL_61021"))
  m.tmpAddr  = SPACE(FSIZE("ADDRESS", "BIL_61021"))
  m.tmpPhone = SPACE(FSIZE("PHONE", "BIL_61021"))
  m.tmpKPP   = SPACE(FSIZE("KPP", "BIL_61021"))
  m.tmpOKONX = SPACE(FSIZE("OKOHX", "BIL_61021"))
  m.tmpOKPO  = SPACE(FSIZE("OKPO", "BIL_61021"))
  m.tmpNote  = SPACE(FSIZE("NOTE", "BIL_61021"))
ELSE
  IF .NOT. EMPTY(BIL_61021.CUS_NAME)
    m.tmpName = BIL_61021.CUS_NAME
  ELSE
    m.tmpName = CUS_61021.CUS_NAME
    m.tmpName = PADR(m.tmpName, FSIZE("CUS_NAME", "BIL_61021"))
  ENDIF
  m.tmpINN   = BIL_61021.INN
  m.tmpAddr  = BIL_61021.ADDRESS
  m.tmpPhone = BIL_61021.PHONE
  m.tmpKPP   = BIL_61021.KPP
  m.tmpOKONX = BIL_61021.OKOHX
  m.tmpOKPO  = BIL_61021.OKPO
  m.tmpNote  = BIL_61021.NOTE
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fil_Bank     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�           �����讚�� ������キ┘ �����≡��� �矗─竄�� ��┘���            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.10.2006 陳�
PROCEDURE Fil_Bank

m.tmpNew = .NOT. SEEK(m.prmCode, "BIL_61021")

IF m.tmpNew
  m.bankNm   = SPACE(FSIZE("BANK", "BIL_61021"))
  m.bankCity = SPACE(FSIZE("CITY", "BIL_61021"))
  m.bankBIK  = SPACE(FSIZE("BIK", "BIL_61021"))
  m.corrBl   = SPACE(FSIZE("CORR_BILL", "BIL_61021"))
  m.accBl    = SPACE(FSIZE("ACC_BILL", "BIL_61021"))
  STORE SPACE(10) TO m.dtCorr
  STORE SPACE(40) TO m.nmCorr
ELSE
  m.bankNm   = BIL_61021.BANK
  m.bankCity = BIL_61021.CITY
  m.bankBIK  = BIL_61021.BIK
  m.corrBl   = BIL_61021.CORR_BILL
  m.accBl    = BIL_61021.ACC_BILL
  m.dtCorr = DTOC(BIL_61021.DATE_CORR)
  = SEEK(BIL_61021.WHO_CORR, "PRS_61021")
  m.nmCorr = ALLTRIM(PRS_61021.FAMILY)+" "+LEFT(PRS_61021.NAME, 1)+" "+LEFT(PRS_61021.S_NAME, 1)
  m.nmCorr = PADR(m.nmCorr, 40)
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Bank_Lst     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    ��玻オ罔� 甎�瓷� ������ ��┘���                     �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.10.2006 陳�
PROCEDURE Bank_Lst
PRIVATE mss

m.bankSw = .F.
SHOW GET m.bankSw
IF WEXIST("H_WIND")
  HIDE WINDOW H_WIND
ENDIF
DO Cus_Bank WITH CUS_61021.CUS_CODE, " *"
IF WEXIST("H_WIND")
  SHOW WINDOW H_WIND SAME
ENDIF
RETURN .T.
