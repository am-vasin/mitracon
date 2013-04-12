*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Cls_Refr     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                         ���ઠ ᯨ᪠ ����ᮢ                          �
*�                                                                        �
*����������������������������������������������������������� 09.12.2005 �ͼ
PROCEDURE Cls_Refr
PARAMETERS prmCus, prmFile, nameMode

*
* prmCus	- ��� ���⠢騪�,
* prmFile	- �������� 䠩�� ���⠢騪�,
* nameMode  - 0 - ��⠢�塞 ᢮� ����� (�� 㬮�砭��),
*             1 - ��⠭�������� ����� ���⠢騪�,
*             2 - ��直� ࠧ ��訢���.
*

* ������� 䠩�� ���⠢騪�
*	CODE - ��� �����
*   NAME - �������� �����
*

PRIVATE s_sav, keyCus, tmpCode, tmpMode, mss, tmpName, ret_v

m.tmpName = SYS(3)

IF TYPE("m.nameMode") = "N"
  m.tmpMode = IIF(BETWEEN(m.nameMode, 0, 2), m.nameMode, 0)
ELSE
  m.tmpMode = 0
ENDIF
m.keyCus = STR(prmCus)
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE ALIAS CL_5C09
SELECT 0
USE (m.base_path+"TEACH_U") SHARED AGAIN ORDER TAG CODE ALIAS TC_5C09
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
  ( NAME C(FSIZE("NAME_CUS", "TC_5C09")) )
USE (m.tmpo_path+m.tmpName) ALIAS TMP_LIST
SELECT 0
USE (m.prmFile) SHARED AGAIN ALIAS SRC_5C09
SCAN
  m.tmpCode = IIF(TYPE("CODE")="C", CODE, STR(CODE, 15))
  IF SEEK("C"+m.keyCus+m.tmpCode, "TC_5C09")
    SELECT TC_5C09
    REPLACE NAME_CUS WITH ALLTRIM(SRC_5C09.NAME)
    IF m.tmpMode = 0
      SELECT SRC_5C09
      LOOP
    ENDIF
    = SEEK(TC_5C09.CODE_OWN, "CL_5C09")
    IF .NOT. ALLTRIM(TC_5C09.NAME_CUS) == ALLTRIM(CL_5C09.NAME)
      SELECT CL_5C09
      IF m.tmpMode = 1
        REPLACE NAME WITH TC_5C09.NAME_CUS
      ELSE
        DIMENSION mss[5]
        mss[1] = ""
        mss[2] = CHR(0)+"����� ���⠢騪�: "+ALLTRIM(TC_5C09.NAME_CUS)
        mss[3] = CHR(0)+"��� ��������: "+ALLTRIM(CL_5C09.NAME)
        mss[4] = CHR(0)+"  ��२�������� �����? [ �� ] [ ��� ]"
        mss[5] = ""
        IF Out_Mess(5, "mss") = 1
          REPLACE NAME WITH TC_5C09.NAME_CUS
        ENDIF
      ENDIF
    ENDIF
    SELECT SRC_5C09
    LOOP
  ENDIF
  IF .NOT. Connect(m.tmpCode, NAME)
    SELECT TMP_LIST
    APPEND BLANK
    REPLACE NAME WITH ALLTRIM(SRC_5C09.NAME)
    SELECT SRC_5C09
  ENDIF
ENDSCAN

USE
USE IN TC_5C09
USE IN CL_5C09
m.ret_v = RECCOUNT("TMP_LIST") = 0
IF .NOT. m.ret_v
  DO View_Tmp
ENDIF
USE IN TMP_LIST
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
SELECT (m.s_sav)
RETURN m.ret_v

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

*��������������������������������������������������������������������Ŀ
*� �� ����� ���⠢騪� ��������������������������������������������Ŀ �
*� �....:....!....:....!....:....!....:....!....:....!....:....!....� �
*� ������������������������������������������������������������������ �
*�[ ] ....:....!....:....!....:....!....:....!....:....!....:....!....�
*�                    < OK Ctrl-W > < �⪠������ Esc >                �
*����������������������������������������������������������������������

PRIVATE ex, tmpCls, tmpNm, tmpSw, tmpRet, s_sav
m.ex = 1
m.tmpSw  = .F.
m.tmpRet = .F.
m.tmpCls = "            "
m.tmpNm  = SPACE(64)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 72, "������ ᮮ⢥��⢨�"
DO Sun_Bord WITH  2,  3,  4, 68, " ����� ���⠢騪� "
@ 3,  4 SAY m.pName
@ WROWS()-1, 2 SAY PADC(" Ins - ���४�� ᯨ᪠ ����ᮢ ", WCOLS()-4, "�")

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 5,  2 GET m.tmpSw PICTURE "@*C " VALID C_Cls()
@ 5,  6 GET m.tmpNm WHEN .F.

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

ON KEY LABEL Ins DO Classes
READ CYCLE
ON KEY

IF m.ex = 1
  *
  * ��ࠡ��뢠�� �����
  IF .NOT. EMPTY(m.tmpCls)
    m.s_sav = SELECT()
    SELECT TC_5C09
    APPEND BLANK
    REPLACE TEACH_TYPE	WITH "C",      ;
            CUS_CODE	WITH m.prmCus, ;
            CODE_CUS	WITH m.pCode,  ;
            CODE_OWN	WITH m.tmpCls, ;
            NAME_CUS	WITH m.pName
    SELECT (m.s_sav)
    m.tmpRet = .T.
  ENDIF
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.tmpRet

*������������������������������������������������������������������������Ŀ
*�   ��楤�� C_Cls        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                              �롮� �����                              �
*�                                                                        �
*����������������������������������������������������������� 09.12.2005 ���
PROCEDURE C_Cls

DO Ch_Class WITH m.tmpCls, m.tmpNm
m.tmpSw = .F.
SHOW GET m.tmpSw
SHOW GET m.tmpNm

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� View_Tmp     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ��ᬮ�� �६������ 䠩��                        �
*�                                                                        �
*����������������������������������������������������������� 14.01.2006 ���
PROCEDURE View_Tmp

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
DO Use_Dummy
SELECT TMP_LIST
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
m.ln = MIN(RECCOUNT(), WROWS("")-11)   && ������⢮ ������� ��ப BROWSE
m.wd = FSIZE("NAME")

DO D_Wins WITH m.ln, m.wd, "�� �� ��।����� ᫥���騥 ������ ���⠢騪�", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

*  CASE m.what_do = "Choose"    && ��ᬮ�� ᯨ᪠

*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    m.what_do = "List"

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