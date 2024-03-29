*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� S_Mail       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 ��爼�牀���┘ ���牀�� �� �皴��→� ��艪�                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 05.02.2006 様�
PROCEDURE S_Mail
PARAMETER s_name, c_code, txt_n, dbf_n
*
* s_name - 皀�� 甌�♂キ��;
* c_code - ��� ��┘���;
* txt_n  - ━� 皀�痰�〓�� ����� か� ←�Ε���;
* dbf_n  - ━� DBF ����� か� ←�Ε���.
*

PRIVATE tmpAddr, tmpName, tmpText, tmpSw, tmpSub, tmpNot, sv_eml
PRIVATE tmpSwTxt, tmpSwDBF, tmpID, tmpFile, i, sourceEml, mss, sw_addr

m.sv_eml  = .F.
m.tmpAddr = ""
USE (m.base_path+"PERSONS") SHARED AGAIN ALIAS PS_6205 ORDER TAG CODE IN 0
IF FSIZE("EML", "PS_6205") # 0
  = SEEK(m.user, "PS_6205")
  m.tmpAddr = ALLTRIM(PS_6205.EML)
ELSE
  m.tmpAddr = PADR(PersParm("MAILADDR"), 60)
ENDIF
USE IN PS_6205

m.tmpName = PADR(PersParm("MAILNAME"), 60)
m.tmpNot  = .NOT. EMPTY(PersParm("NOTIFICAT"))
m.tmpText = PersParm("MAILTEXT")
m.tmpSw   = VAL(PersParm("MAILMODE"))
m.tmpSw   = MAX(m.tmpSw, 1)
m.tmpSub  = PADR(s_name, 60)
m.sw_addr = .NOT. EMPTY(PersParm("MAILTOSELF"))

PRIVATE to_eml, to_nam, s_sav
m.to_eml = ""
m.sourceEml = ""
m.to_nam = ""
m.tmpSwTxt = .T.
m.tmpSwDBF = .NOT. EMPTY(dbf_n)

s_sav = SELECT()
SELECT 0
IF .NOT. EMPTY(m.c_code)
  USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS CS_6205 ORDER TAG CUS_CODE
  IF SEEK(m.c_code)
    m.to_nam = ALLTRIM(CUS_NAME)
  ENDIF
  USE (m.base_path+"CLI_INFO") SHARED AGAIN ALIAS CS_6205 ORDER TAG CUS_CODE
  IF SEEK(m.c_code)
    m.to_eml = ALLTRIM(EML)
    m.sourceEml = m.to_eml
  ENDIF
  USE
ENDIF

m.to_eml = PADR(m.to_eml, 60)
m.to_nam = PADR(m.to_nam, 50)

IF m.tmpSw # 2 .OR. EMPTY(m.to_eml)	.OR. m.tmpSwDBF		&& ��痰�→� ≡ィ�� ┼� �モ �むメ�
  IF .NOT. Interact()
    SELECT (m.s_sav)
    RETURN
  ENDIF
ENDIF

USE (m.send_log+"MAILPRM") SHARED AGAIN
REPLACE MAILCOUNT WITH MAILCOUNT+1
m.tmpID = MAILCOUNT
USE
m.tmpID = RIGHT(STR(m.tmpID*69489693, 20), 8)
m.tmpID = PADL(ALLTRIM(m.tmpID), 8, "0")

IF .NOT. Is_Dir(m.send_log+"MAIL")		&& �モ え爛�皰爬� か� ←�Ε�┤ - 甌Г�え�!
  m.tmpFile = SET("DEFAULT")+CURDIR()
  SET DEFAULT TO (m.send_log)
  ! MD "MAIL"
  SET DEFAULT TO (m.tmpFile)
ENDIF

IF m.tmpSwTxt	&& ����る��ガ TXT ����
  IF File_O(m.txt_n)
    COPY FILE (m.txt_n) TO (m.send_log+"MAIL\"+m.tmpID+".txt")
  ENDIF
ENDIF

IF m.tmpSwDBF	&& ����る��ガ DBF ����
  m.i = RAT(".", m.dbf_n)
  IF m.i = 0
    m.tmpFile = ALLTRIM(m.dbf_n)
  ELSE
    m.tmpFile = ALLTRIM(LEFT(m.dbf_n, m.i-1))
  ENDIF

  IF File_O(m.tmpFile+".dbf")
    COPY FILE (m.tmpFile+".dbf") TO (m.send_log+"MAIL\"+m.tmpID+".dbf")
  ENDIF

  IF File_O(m.tmpFile+".fpt")
    COPY FILE (m.tmpFile+".fpt") TO (m.send_log+"MAIL\"+m.tmpID+".fpt")
  ENDIF

  IF File_O(m.tmpFile+".cdx")
    COPY FILE (m.tmpFile+".cdx") TO (m.send_log+"MAIL\"+m.tmpID+".cdx")
  ENDIF

ENDIF

USE (m.send_log+"MAILSEND") SHARED AGAIN
APPEND BLANK
REPLACE	LETTERID	WITH m.tmpID,	;
		SUBJECT		WITH m.tmpSub,	;
		NOTIFY		WITH m.tmpNot,	;
		TO_SELF		WITH m.sw_addr,	;
		LETT_TEXT	WITH m.tmpText,	;
		CUS_NAME	WITH m.to_nam,	;
		CUS_MAIL	WITH m.to_eml,	;
		AUTHOR_NAM	WITH m.tmpName,	;
		AUTHOR_EML	WITH m.tmpAddr,	;
		DATE_CR		WITH DATE(),	;
		TIME_CR		WITH TIME(),	;
		WHO_CR		WITH m.user

REPLACE STATUS WITH "*"
USE (m.base_path+"SERVERS") SHARED AGAIN ORDER TAG SERV_ACT DESCENDING
IF .NOT. SEEK(PADR("E-MAIL", FSIZE("SERV_TYPE")))
  DIMENSION mss[4]
  mss[1] = ""
  mss[2] = CHR(0)+"��������! ��茱 ��瘡�� �� ＜ぅ� �皴��←キ�,"
  mss[3] = CHR(0)+"���� �� ＜ぅ� ���竕キ 瓮燿ム 蹕オ矗����� ��艪�"
  mss[4] = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  IF START_DATE # DATE()
    DIMENSION mss[4]
    mss[1] = ""
    mss[2] = CHR(0)+"��������! ��絎Ε 艪� 瓮燿ム 蹕オ矗����� ��艪� �� ���竏キ."
    mss[3] = CHR(0)+"�皰膈�皀, 艪� � �━, ┃�腑 ��茱 ��瘡�� �� ＜ぅ� ����←キ�."
    mss[4] = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
ENDIF
USE

SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Interact     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                  ��皀���皋↓�� ������キ┘ �����モ牀�                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.02.2006 陳�
PROCEDURE Interact

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*楽� ���竍�皀�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖      ��� ....:....!....:....!....:....!....:....!....:....!       崖
*崖                                                                   崖
*崖E-mail ....:....!....:....!....:....!....:....!....:....!....:....!崖
*垣陳陳陳陳陳陳陳陳陳�[ ] ��縲���碎 E-mail ��┘���陳陳陳陳陳陳陳陳陳陳抒
*�                     [ ] �▲ぎ��キ┘ � �牀艪キ┬                     �
*��ガ� ....:....!....:....!....:....!....:....!....:....!....:. <�襦��>�
*楽� �オ痰 ��瘡�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖                                                                   崖
*崖                                                                   崖
*崖                                                                   崖
*崖                                                                   崖
*崖                                                                   崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�                    ���Ε���: [ ] �オ痰   [ ] DBF                    �
*�                    < OK Ctrl-W > < �皖���碎瘴 Esc >                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, tmpNm, tmpCh
m.ex = 1
m.tmpCh = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH IIF(m.tmpSwDBF, 19, 18), 73, "�皴��→� ��瘡�� �� 蹕オ矗����� ��艪�"
DO Sun_Bord WITH  2,  2,  6, 70, " ���竍�皀�� "
DO Sun_Bord WITH  9,  2, 15, 70, " �オ痰 ��瘡�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  9 SAY "���" GET m.to_nam
@  5,  3 SAY "E-mail" GET m.to_eml VALID S_Flag()
@  6, 22 GET m.sv_eml PICTURE "@*C ��縲���碎 E-mail ��┘���" ;
               WHEN .NOT. EMPTY(c_code) .AND. .NOT. m.sourceEml == ALLTRIM(m.to_eml)
@  7, 23 GET m.tmpNot PICTURE "@*C �▲ぎ��キ┘ � �牀艪キ┬"
@  8,  2 SAY "�ガ�" GET m.tmpSub PICTURE "@S56"
@  8, 64 GET m.tmpCh PICTURE "@*N �襦��" VALID LettList()
@ 10,  3 EDIT m.tmpText SIZE 5, 67 COLOR (SCHEME(1, 1))
IF m.tmpSwDBF
  @ WROWS()-3, 22 SAY "���Ε���:" GET m.tmpSwTxt PICTURE "@*C �オ痰"
  @ WROWS()-3, 44 GET m.tmpSwDBF PICTURE "@*C DBF"
ENDIF
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Eml()

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���: 甌縲��錺� �むメ
  IF m.sv_eml .AND. .NOT. EMPTY(m.c_code) .AND. .NOT. EMPTY(m.to_eml)
    USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS CS_6205 ORDER TAG CUS_CODE
    IF SEEK(m.c_code)
      m.tmpNm = ALLTRIM(CUS_NAME)
    ENDIF
    USE (m.base_path+"CLI_INFO") SHARED AGAIN ALIAS CS_6205 ORDER TAG CUS_CODE
    IF .NOT. SEEK(m.c_code)
      APPEND BLANK
      REPLACE CUS_CODE WITH m.c_code, ;
              NAME     WITH m.tmpNm
    ENDIF
    REPLACE EML WITH m.to_eml
    USE
  ENDIF

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Eml      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍▲爲� ��玻オ皚�痰� ゛����                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.02.2006 陳�
PROCEDURE Tst_Eml

PRIVATE mss

IF m.ex # 1
  RETURN .T.
ENDIF

IF EMPTY(m.to_eml)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��艪��覃 �むメ ぎ�Ε� °碎 礫����..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� S_Flag       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ���Ξ� 甌縲��キ�� �むメ�                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 06.02.2006 陳�
PROCEDURE S_Flag

m.sv_eml = .NOT. EMPTY(c_code) .AND. .NOT. m.sourceEml == ALLTRIM(m.to_eml)
SHOW GET m.sv_eml 

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� LettList     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         �襦�� ����皰→� ��瘡��                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10.04.2006 陳�
PROCEDURE LettList

PRIVATE tSub, tText

m.tSub  = ""
m.tText = ""
DO Ch_Lett WITH m.tSub, m.tText

IF .NOT. EMPTY(m.tSub)
  m.tmpSub = m.tSub
  SHOW GET m.tmpSub
ENDIF

IF .NOT. EMPTY(m.tText)
  m.tmpText = m.tText
  SHOW GET m.tmpText
ENDIF

RETURN .T.
