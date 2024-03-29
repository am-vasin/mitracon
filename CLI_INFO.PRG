*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�            蹍������ 甌Г����/��玻オ罔� ┃筮爼�罔� � ��┘�皀.         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Cli_Info
PARAMETERS ext_menu, let_corr, parmCode

PRIVATE is_new, ln, wd, sel_sav, mss, ex, crsr, manager_n, k_sw, k_code, k_name
PRIVATE tmpName, tmpCode

sel_sav = SELECT()
manager_n = ""
IF EMPTY(parmCode)
  tmpCode = ACCOUNT.CUS_CODE
ELSE
  tmpCode = parmCode
ENDIF
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_1507 AGAIN
IF .NOT. SEEK(tmpCode)
  USE
  SELECT (sel_sav)
  RETURN
ENDIF
tmpName = CUS_NAME
IF .NOT. EMPTY(CUS_1507.WHO_NEW)
  SELECT 0
  USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PRS_0C29
  SEEK CUS_1507.WHO_NEW
  manager_n = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
  USE
ENDIF
manager_n = PADR(manager_n, 28)
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    DEACTIVATE MENU (ext_menu)
  ENDIF
ENDIF

DEFINE WINDOW H_Wind FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW H_Wind
SELECT 0
USE (base_path+"CLI_KIND") ORDER TAG CODE ALIAS KIND_TMP AGAIN
SELECT 0
USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE ALIAS INFO_TMP AGAIN
k_sw   = .F.
k_code = SPACE(FSIZE("KIND_CODE", "KIND_TMP"))
k_name = SPACE(FSIZE("KIND_NAME", "KIND_TMP"))
is_new = .NOT. SEEK(tmpCode)
IF .NOT. is_new .AND. .NOT. EMPTY(INFO_TMP.KIND_CODE)
  IF SEEK(INFO_TMP.KIND_CODE, "KIND_TMP")
    k_code = KIND_TMP.KIND_CODE
    k_name = KIND_TMP.KIND_NAME
  ENDIF
ENDIF

IF is_new .AND. .NOT. let_corr
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 轤��� ���祚�皀�� �モ ����諷..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RELEASE WINDOW H_Wind
  USE
  SELECT KIND_TMP
  USE
  SELECT CUS_1507
  USE
  SELECT (sel_sav)
  RETURN
ENDIF

PUSH KEY CLEAR
IF is_new
  SCATTER MEMVAR BLANK
ELSE
  SCATTER MEMVAR
ENDIF
ln = IIF(let_corr, 24, 23)
wd = 78

DO D_W WITH "Cli_Info", ln, wd, "���祚�皀��: "+ALLTRIM(tmpName), 0
@ 0, 0 SAY ""
*       ���祚�皀��: < ��� ��┘��� >
? "敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�"
? "�    敖陳� �矗��� 陳陳陳陳陳 �ィ┏� 陳陳陳陳陳陳陳陳陳 ��牀� 陳陳陳陳朕     �"
? "�    �  ....:....1....:.  ....:....1....:....1....  ....:....1....:.  �     �"
? "�    青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰     �"
? "楽陳 �むメ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審"
? "崖....:.  ....:....1....:....1....:....1....:....1....:....1....:....1.... 崖"
? "崖              �カヤ�� ....:....1....       ���� ....:....1               崖"
? "崖               ��. ��艪� ....:....1....:....1....:....1..                崖"
? "垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�"
? "� �.�.�. ....:....1....:....1....:....1..   ���Ν�痰� ....:....1....:....1  �"
? "楽�[ ] �筌�� ぅ閧. 陳陳笥陳 ��ユ���├�罔� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�"
? "崖                     崖[ ] �磴┏ & �祀赳━イ��  [ ] �イ. �｀珮ぎ���┘    崖"
? "崖                     崖[ ] �┐ギ & �謔. 皀紲��  [ ] �≒�-皀絖┴�         崖"
? "崖                     崖[ ] �爛め癶� 瓣錻�       [ ] �カオ���祗┴�罔�     崖"
? "崖                     崖[ ] �К. 皀絖┴�         [ ] ｡�珮�.��え�▲�.� TV 崖"
? "崖                     崖[ ] �痰�膈┴� �������    [ ] ��※���.�｀珮ぎ���┘ 崖"
? "崖                     崖[ ] ����跪皀爿�� 皀絖┴�     ....:....1....:.     崖"
? "崖                     崖[ ] �縲���襯 瓱痰ガ�                              崖"
? "垣陳陳陳陳陳陳陳陳陳陳潰青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�"
? "�踸�ョ��┘ ....:....1....:....1....:....1....:....1....:....1....:....1....�"
? "�                   �キイΕ� ....:....!....:....!....:...                   �"
IF let_corr
  ? "�                     < OK Ctrl-W > < �皖���碎瘴 Esc >                      �"
  ? "青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�"
ELSE
  ? "青陳陳陳陳陳陳陳陳陳陳陳陳 ��Μ�皀 �遏竡 ���※蓊 陳陳陳陳陳陳陳陳陳陳陳陳陳潰"
ENDIF

ex = 1

@  3,  8 GET m.country
@  3, 26 GET m.region
@  3, 52 GET m.city

@  6,  2 GET m.index
@  6, 10 GET m.address PICTURE "@S64"
@  7, 24 GET m.phone
@  7, 50 GET m.fax
@  8, 27 GET m.eml

@ 10,  9 GET m.contact
@ 10, 54 GET m.job

*!*@ 11, 5  GET k_sw PICTURE "@*C �筌�� ぅ閧. " VALID Ch_Kind()
*!*@ 14, 7  GET k_name WHEN .F.

@ 11, 3  SAY " �筌�� ぅ閧. "
@ 12, 3 GET m.production PICTURE "@*C 蹍├〓め癶�"
@ 13, 3 GET m.work       PICTURE "@*C ��о�｀皖�"
@ 14, 3 GET m.sale       PICTURE "@*C 蹍����"
@ 15, 3 GET m.repair     PICTURE "@*C �ガ���"
@ 16, 7 GET m.f_other

@ 12, 25 GET m.audio      PICTURE "@*C �磴┏ & �祀赳━イ��"
@ 13, 25 GET m.video      PICTURE "@*C �┐ギ & �謔���� 皀�."
@ 14, 25 GET m.connect    PICTURE "@*C �爛め癶� 瓣錻�"
@ 15, 25 GET m.meastech   PICTURE "@*C �К. 皀絖┴�"
@ 16, 25 GET m.power      PICTURE "@*C �痰�膈┴� �������"
@ 17, 25 GET m.computer   PICTURE "@*C ����跪皀爿�� 皀絖┴�"
@ 18, 25 GET m.quardsys   PICTURE "@*C �縲���襯 瓱痰ガ�"

@ 12, 50 GET m.medequip   PICTURE "@*C �イ. �｀珮ぎ���┘"
@ 13, 50 GET m.auto       PICTURE "@*C �≒�-皀絖┴�"
@ 14, 50 GET m.telecom    PICTURE "@*C �カオ���祗┴�罔�"
@ 15, 50 GET m.radio      PICTURE "@*C ｡�珮�.��え�▲�.� TV"
@ 16, 50 GET m.navig      PICTURE "@*C ��※���.�｀珮ぎ���┘"
@ 17, 54 GET m.p_other

@ 20, 12 GET m.note
@ 21, 20 SAY "�キイΕ� " GET manager_n WHEN .F.
IF let_corr
  crsr = SET("CURSOR") = "OFF"
  SET CURSOR ON
  @ 22, 22 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
  READ CYCLE VALID Tst_Blank()
  IF ex = 1
    m.who = user
    m.where = arm
    m.cus_code = tmpCode
    m.name     = tmpName
    IF is_new
      APPEND BLANK
    ENDIF
    m.kind_code = k_code
    GATHER MEMVAR
  ENDIF
  IF crsr
    SET CURSOR OFF
  ENDIF
ELSE
  = INKEY(0,"HM")
  READ WHEN .F.
ENDIF

USE
SELECT KIND_TMP
USE
SELECT CUS_1507
USE
RELEASE WINDOW H_Wind
RELEASE WINDOW Cli_Info
SELECT (sel_sav)
POP KEY

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Kind      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �襦�� 皋�� ��┘���.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.03.2001 陳�
PROCEDURE Ch_Kind

IF .NOT. k_sw
  RETURN .T.
ENDIF

k_sw = .F.
SHOW GET k_sw

DO Cli_Kind WITH k_code, k_name

SHOW GET k_name

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                                                        �
*�                           �諷�� ├ ゛����.                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.03.2003 陳�
PROCEDURE Tst_Blank
PRIVATE mss

IF EMPTY(m.country) .AND. let_corr .AND. ex = 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��Б��┘ 痰���� ぎ�Ν� °碎 ������キ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
