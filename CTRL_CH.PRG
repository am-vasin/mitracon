********************************************************************************
*    ���� Ctrl_Ch        ���ࠡ��稪 ���३ ��ᨭ                05.02.2012    *
********************************************************************************
*                                                                              *
*                         �롮� ��࠭� �� �ࠢ�筨��                          *
*                                                                              *
********************************************************************************
PROCEDURE Ctrl_Ch
PARAMETERS s_code
*
* s_code - ��� ��� ��砫쭮�� ���祭��
*


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
PRIVATE retVal, s_shab, tmpCode, r_sav, mss

m.retVal = ""
m.s_shab = ""
m.tmpCode = 0

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
SELECT 0
USE (base_path+"OKSM") SHARED AGAIN ORDER TAG B_NAME ALIAS CTRL_CH

IF TYPE("m.s_code") = "C"
  m.tmpCode = VAL(ALLTRIM(m.s_code))
ENDIF
IF TYPE("m.s_code") = "N"
  m.tmpCode = m.s_code
ENDIF
IF m.tmpCode # 0
  m.tmpCode = ALLTRIM(STR(m.tmpCode))
  m.tmpCode = PADL(m.tmpCode, 3, "0")
  SET ORDER TO TAG CODE
  IF .NOT. SEEK(m.tmpCode)
    SET ORDER TO TAG B_NAME
    GO TOP
  ELSE
    SET ORDER TO TAG B_NAME
  ENDIF
ENDIF

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�롮�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "����"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "...�� ����" KEY Ctrl-A, "F3"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "Seek_C"
  DEFINE BAR  2 OF Seek PROMPT "...�� ���⥪���" KEY Ctrl-A, "F7"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "Seek_T"
  DEFINE BAR  3 OF Seek PROMPT "...�த�������" KEY Ctrl-A, "Shift-F7"
  ON SELECTION BAR 3 OF Seek DO Sw_Mode WITH "Seek_A"

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = FSIZE("CODE")+FSIZE("B_NAME")+1

DO D_Wins WITH m.ln, m.wd, "�����䨪��� ��࠭", 0, 0

@ 1, 2 SAY "���"
@ 1, 6 SAY "��������"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter    KEYBOARD CHR(23)
    ON KEY LABEL F3       DO Sw_Mode WITH "Seek_C"
    ON KEY LABEL F7       DO Sw_Mode WITH "Seek_T"
    ON KEY LABEL Shift-F7 DO Sw_Mode WITH "Seek_A"

*
*    BROWSE - ����
*
    BROWSE FIELDS CODE:H="",  B_NAME:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F       ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && �롮�

    IF .NOT. EOF() .AND. .NOT. BOF()
      m.retVal = CODE+B_NAME
    ENDIF
    EXIT

  CASE m.what_do = "Seek_C"    && ���� �� ����

    m.tmpCode = VAL(BankName("��� ��࠭�", 4))
    IF m.tmpCode # 0
      m.r_sav = RECNO()
      m.tmpCode = PADL(ALLTRIM(STR(m.tmpCode)), 3, "0")
      SET ORDER TO CODE
      IF .NOT. SEEK(m.tmpCode)
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"��� �� ������..."
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
      ENDIF
      SET ORDER TO TAG B_NAME
    ENDIF
    
    m.what_do = "List"

  CASE m.what_do = "Seek_T"    && ���� �� ���⥪���

    m.s_shab = BankName("���⥪��� ����", FSIZE("B_NAME"))
    IF .NOT. EMPTY(m.s_shab)
      m.s_shab = SYS(15, m.lwr, m.s_shab)
      m.r_sav = RECNO()
      LOCATE FOR m.s_shab $ SYS(15, m.lwr, B_NAME)
      IF .NOT. FOUND()
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"��祣� �� �������..."
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
      ENDIF
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Seek_A"    && ����� ���᪠

    IF .NOT. EMPTY(m.s_shab)
      m.r_sav = RECNO()
      CONTINUE
      IF .NOT. FOUND()
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"��祣� �� �������..."
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
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
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
USE
SELECT (m.s_sav)

RETURN m.retVal
