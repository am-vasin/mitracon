*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Dev_Refr     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �▲爲� 甎�瓷� ÷キぎ�                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 09.12.2005 様�
PROCEDURE Dev_Refr
PARAMETERS prmCus, prmFile

*
* prmCus	- ��� ��痰�∫┴�,
* prmFile	- ��Б��┘ ����� ��痰�∫┴�.
*

* �矗礫矣�� ����� ��痰�∫┴�
*	CODE - ��� ÷キ�� (�ギ￥��皀�讚�)
*   NAME - ��Б��┘ ÷キ�� (┼� B_NAME)
*

PRIVATE s_sav, keyCus, tmpCode, tmpName

m.keyCus = STR(prmCus)
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"TEACH_U") SHARED AGAIN ORDER TAG CODE ALIAS TC_5C09
SELECT 0
USE (m.prmFile) SHARED AGAIN ALIAS SRC_5C09
SCAN
  DO CASE
  CASE TYPE("CODE") = "C"
    m.tmpCode = CODE
  CASE TYPE("CODE") = "N"
    m.tmpCode = STR(CODE, 15)
  CASE TYPE("NAME") = "C"
    m.tmpCode = LEFT(NAME, 8)
  OTHERWISE
    m.tmpCode = LEFT(B_NAME, 8)
  ENDCASE

  IF TYPE("NAME") = "C"
    m.tmpName = LEFT(NAME, 8)
  ELSE
    m.tmpName = LEFT(B_NAME, 8)
  ENDIF
    
  IF SEEK("D"+m.keyCus+m.tmpCode, "TC_5C09")
    LOOP
  ENDIF
  IF .NOT. Connect(m.tmpCode, m.tmpName)
    USE
    USE IN TC_5C09
    SELECT (m.s_sav)
    RETURN .F.
  ENDIF
ENDSCAN

USE
USE IN TC_5C09
SELECT (m.s_sav)
RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Connect      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            踸�錻�� ���瘁�                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.12.2005 陳�
PROCEDURE Connect
PARAMETERS pCode, pName

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                                     �
*� �爛�� ��痰�∫┴� ....:... �爬�錻�碎 � ....:... (F1) �
*�                                                     �
*�           < OK Ctrl-W > < �皖���碎瘴 Esc >          �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, tmpCd, tmpNm, tmpRet, s_sav
m.ex = 1
m.tmpSw  = .F.
m.tmpRet = .F.
m.tmpCd = 0
m.tmpNm  = SPACE(8)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 57, "���Θ皀 甌�癶モ痰※�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  3 SAY "�爛�� ��痰�∫┴�" GET m.pName WHEN .F.
@ 3, 29 SAY "�爬�錻�碎 �" GET m.tmpNm VALID T_Dev()
@ 3, 50 SAY "(F1)"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

ON KEY LABEL F1 DO Tmp_Ch
READ CYCLE
ON KEY LABEL F1

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  IF .NOT. EMPTY(m.tmpCd)
    m.s_sav = SELECT()
    SELECT TC_5C09
    APPEND BLANK
    REPLACE TEACH_TYPE	WITH "D",      ;
            CUS_CODE	WITH m.prmCus, ;
            CODE_CUS	WITH m.pCode,  ;
            NAME_CUS	WITH m.pName,  ;
            CODE_OWN	WITH STR(m.tmpCd,12)
    SELECT (m.s_sav)
    m.tmpRet = .T.
  ENDIF
  
ENDIF
*--------------------------------------------------------------------------
	
POP KEY
RELEASE WINDOW (win_name)
RETURN m.tmpRet

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� T_Dev        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            蹍▲爲� ÷キ��                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.12.2005 陳�
PROCEDURE T_Dev

PRIVATE s_sav

IF EMPTY(m.tmpNm)
  m.tmpCd = 0
  RETURN .T.
ENDIF

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"DEVELOP") SHARED AGAIN ORDER TAG B_NAME ALIAS BRN_5C09
IF SEEK(ALLTRIM(m.tmpNm))
  m.tmpNm = LEFT(B_NAME, 8)
  m.tmpCd = CODE
ELSE
  m.tmpNm = SPACE(8)
  m.tmpCd = 0
ENDIF
SHOW GET m.tmpNm

USE
SELECT (m.s_sav)

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tmp_Ch        ��о�｀砒┴ ��むォ ��瓱�                     �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             �襦�� ÷キ��                               �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.12.2005 陳�
PROCEDURE Tmp_Ch

ON KEY LABEL F1

DO Ch_Dev WITH m.tmpCd, m.tmpNm
SHOW GET m.tmpNm

ON KEY LABEL F1 DO Tmp_Ch

RETURN
