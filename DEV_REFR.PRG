*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Dev_Refr     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                         ���ઠ ᯨ᪠ �७���                          �
*�                                                                        �
*����������������������������������������������������������� 09.12.2005 �ͼ
PROCEDURE Dev_Refr
PARAMETERS prmCus, prmFile

*
* prmCus	- ��� ���⠢騪�,
* prmFile	- �������� 䠩�� ���⠢騪�.
*

* ������� 䠩�� ���⠢騪�
*	CODE - ��� �७�� (����易⥫쭮)
*   NAME - �������� �७�� (��� B_NAME)
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Connect      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            �ਢ離� �����                             �
*�                                                                        �
*����������������������������������������������������������� 09.12.2005 ���
PROCEDURE Connect
PARAMETERS pCode, pName

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

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*�����������������������������������������������������Ŀ
*�                                                     �
*� �७� ���⠢騪� ....:... �ਢ易�� � ....:... (F1) �
*�                                                     �
*�           < OK Ctrl-W > < �⪠������ Esc >          �
*�������������������������������������������������������

PRIVATE ex, tmpCd, tmpNm, tmpRet, s_sav
m.ex = 1
m.tmpSw  = .F.
m.tmpRet = .F.
m.tmpCd = 0
m.tmpNm  = SPACE(8)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 57, "������ ᮮ⢥��⢨�"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3,  3 SAY "�७� ���⠢騪�" GET m.pName WHEN .F.
@ 3, 29 SAY "�ਢ易�� �" GET m.tmpNm VALID T_Dev()
@ 3, 50 SAY "(F1)"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

ON KEY LABEL F1 DO Tmp_Ch
READ CYCLE
ON KEY LABEL F1

IF m.ex = 1
  *
  * ��ࠡ��뢠�� �����
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� T_Dev        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            �஢�ઠ �७��                             �
*�                                                                        �
*����������������������������������������������������������� 09.12.2005 ���
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tmp_Ch        ���ࠡ��稪 ���३ ��ᨭ                     �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                             �롮� �७��                               �
*�                                                                        �
*����������������������������������������������������������� 09.12.2005 ���
PROCEDURE Tmp_Ch

ON KEY LABEL F1

DO Ch_Dev WITH m.tmpCd, m.tmpNm
SHOW GET m.tmpNm

ON KEY LABEL F1 DO Tmp_Ch

RETURN