*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Ch_O_T       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                           �롮� ⨯� ������                            �
*�                                                                        �
*����������������������������������������������������������� 16.11.2007 �ͼ
PROCEDURE Ch_O_T
PARAMETERS srcType

PRIVATE tmpType, s_sav, tmpName, rc

m.tmpType = PADL(ALLTRIM(m.srcType), 10)
m.s_sav = SELECT()
SELECT 0
USE m.base_path+"ORD_TYPE" SHARED AGAIN ALIAS TMP_OT
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(CODE C(10), NAME C(FSIZE("ORD_NAME", "TMP_OT")))
USE (m.tmpo_path+m.tmpName) ALIAS TMP_LIST
m.rc = 1
APPEND BLANK
REPLACE NAME WITH "��⮬���᪨� �롮� ⨯� ������"
APPEND BLANK
REPLACE CODE WITH "         0", ;
        NAME WITH "��� ������ �� 㬮�砭��"
IF m.tmpType = CODE
  m.rc = RECNO()
ENDIF
SELECT TMP_OT
SCAN
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE CODE WITH STR(TMP_OT.CODE), ;
          NAME WITH TMP_OT.ORD_NAME
  IF m.tmpType = CODE
    m.rc = RECNO()
  ENDIF
  SELECT TMP_OT
ENDSCAN

USE

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
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
SELECT 0
DO Use_Dummy
SELECT 0
SELECT TMP_LIST
GO m.rc

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = FSIZE("NAME")

DO D_Wins WITH m.ln, m.wd, "������ ⨯ ������", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

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

  CASE m.what_do = "Choose"    && ��ᬮ�� ᯨ᪠

    m.tmpType = CODE
    EXIT
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
m.tmpName = DBF()
USE
DELETE FILE (m.tmpName)
SELECT (m.s_sav)

RETURN m.tmpType