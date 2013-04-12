*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� OptKoeff     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�            ��⠭���� �����樥�⮢ ��� ���� ��⮢�� 業             �
*�                                                                        �
*����������������������������������������������������������� 08.05.2006 �ͼ
PROCEDURE OptKoeff

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
m.win_name   = "kff"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
m.s_sav = SELECT()
PRIVATE tmpName, maxBound
m.maxBound = 9999999999.9999
m.tmpName = SYS(3)

SELECT 0
USE (m.base_path+"OPTKOEFF") SHARED AGAIN ALIAS KFF_6508 ORDER TAG PRC_BOUND
COPY TO (m.tmpo_path+m.tmpName) WITH CDX
USE (m.tmpo_path+m.tmpName) SHARED AGAIN ALIAS KFF_6508 ORDER TAG PRC_BOUND

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "���४�஢���" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "�������" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = 24

DO D_Wins WITH m.ln, m.wd, "�����. ��� ���. ���. 業", 0, 0
@ 1, 2 SAY ;
"��Ė��� �� ($)��Ċ����."
*9999999999.9999 999.9999
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"

*
*    BROWSE - ����
*
    BROWSE FIELDS PPP=PRC_B(PRC_BOUND):15:H="", ;
                  PRC_K:H="":P="999.9999" ;
                  FREEZE PRC_K ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE m.what_do = "Modify"    && ���४��

    DO ModiLine
    m.what_do = "List"

  CASE m.what_do = "Add"       && ����������

    DO ModiLine WITH .T.
    m.what_do = "List"

  CASE m.what_do = "Del"       && ��������

    IF PRC_BOUND = m.maxBound
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"��� ��ப� ����� 㤠����"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DELETE
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
PRIVATE tmpArr
SELECT 0
SET DELETED OFF
USE (m.base_path+"OPTKOEFF") SHARED AGAIN ALIAS RS_6508
SELECT KFF_6508
SCAN FOR .NOT. DELETED()
  SCATTER TO tmpArr
  SELECT RS_6508
  IF EOF()
    APPEND BLANK
  ELSE
    RECALL
  ENDIF
  GATHER FROM tmpArr
  SKIP
  SELECT KFF_6508
ENDSCAN
USE
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
SELECT RS_6508
IF .NOT. EOF()
  DELETE REST
ENDIF
SET DELETED ON
USE
SELECT (m.s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prc_B        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                               ���� ��...                               �
*�                                                                        �
*����������������������������������������������������������� 08.05.2006 ���
PROCEDURE Prc_B
PARAMETERS prmPrc 


IF m.prmPrc = m.maxBound
  RETURN "               "
ENDIF

RETURN TRANSFORM(m.prmPrc, "9999999999.9999")

*������������������������������������������������������������������������Ŀ
*�   ��楤�� ModiLine     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ���४��/���������� ��ப�                       �
*�                                                                        �
*����������������������������������������������������������� 08.05.2006 ���
PROCEDURE ModiLine
PARAMETER addMode

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

*�������������������������������������Ŀ
*�                                     �
*�   �࠭�� 業 ($) 9999999999.9999   �
*�                                     �
*�        �����樥�� 999.9999         �
*�                                     �
*�  < OK Ctrl-W > < �⪠������ Esc >   �
*���������������������������������������

PRIVATE ex, tmpB, tmpK, letMax
m.ex = 1
IF m.addMode
  STORE 0 TO m.tmpB, m.tmpK
ELSE
  m.tmpB = PRC_BOUND
  m.tmpK = PRC_K
ENDIF
m.letMax = PRC_BOUND=m.maxBound
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 9, 41, "�����樥�� ��� 業���� ��㯯�"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@  3,  5 SAY "�࠭�� 業 ($) "
IF m.tmpB < m.maxBound
  @  3, COL() GET m.tmpB PICTURE "@Z 9999999999.9999"
ENDIF
@  5, 10 SAY "�����樥��" GET m.tmpK PICTURE "@Z 999.9999"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE VALID TstBound()

IF m.ex = 1
  *
  * ��ࠡ��뢠�� �����
  IF m.addMode
    APPEND BLANK
  ENDIF
  REPLACE PRC_BOUND WITH m.tmpB, PRC_K WITH m.tmpK
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� TstBound     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      �஢�ઠ ���४⭮�� ������                      �
*�                                                                        �
*����������������������������������������������������������� 08.05.2006 ���
PROCEDURE TstBound

PRIVATE mss

IF m.ex # 1
  RETURN .T.
ENDIF

IF m.tmpB <= 0 .OR. m.tmpB > 9999999999 .AND. .NOT. m.letMax
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"������� �࠭�� ������ ���� ����� ��� � ����� 9999999999"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF m.tmpK < 0
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"�����樥�� ������ ���� ����� ��� ࠢ�� ���."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.