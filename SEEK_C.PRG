*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Seek_C       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                         ������ ��࠭� �� ����                          �
*�                                                                        �
*����������������������������������������������������������� 27.12.2012 �ͼ
PROCEDURE Seek_C
PARAMETERS prmName 

PRIVATE tmpName, tmpShab, fltExpr, rSav, mss

m.tmpShab = ""
m.fltExpr = ""
m.tmpName = SYS(15, m.lwr, ALLTRIM(m.prmName))
DIMENSION mss[3]
mss[1] = ""
mss[2] = CHR(0)+"H�� �� ������ ���室�饣� ��������"
mss[3] = ""

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
SELECT 0
USE (m.base_path+"OKSM") SHARED AGAIN ORDER TAG B_NAME ALIAS CH_OKSM
SET FILTER TO m.fltExpr $ SYS(15, m.lwr, B_NAME) .OR. EMPTY(m.fltExpr)

PRIVATE i, rc, retV
m.rc = RECNO()
FOR m.i = 1 TO LEN(m.tmpName)
  IF .NOT. SEEK(LEFT(m.tmpName, m.i))
    EXIT
  ENDIF
  m.rc = RECNO()
ENDFOR
GO m.rc
m.retV = ""

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Filter OF (menu_name) PROMPT "������" KEY Ctrl-A, "Tab"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = FSIZE("B_NAME")+FSIZE("CODE")+1

DO D_Wins WITH m.ln, m.wd, ALLTRIM(m.prmName), 1, 0
@ 1, 2 SAY "������"
@ 2, 2 SAY "���đ�࠭�"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Filter"

*
*    BROWSE - ����
*
    BROWSE FIELDS CODE:H="",     ;
                  B_NAME:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && ��ᬮ�� ᯨ᪠

    IF .NOT. EOF() .AND. .NOT. BOF()
      m.retV = CODE
    ENDIF
    EXIT

  CASE m.what_do = "Filter"    && ��⠭���� 䨫���

    m.tmpName = F_Shab(m.tmpShab)

    IF TYPE("m.tmpName") = "C"
      m.fltExpr = SYS(15, m.lwr, m.tmpName)
      m.rSav = RECNO()
      IF .NOT. m.fltExpr $ SYS(15, m.lwr, B_NAME) .AND. .NOT. EMPTY(m.fltExpr)
        GO TOP
        IF EOF()
          DO Out_Mess WITH 7, "mss"
          m.tmpName = m.tmpShab
          m.fltExpr = SYS(15, m.lwr, m.tmpShab)
          GO m.rSav
        ENDIF
      ENDIF
      m.tmpShab = m.tmpName
      ACTIVATE WINDOW (win_name+"_M") SAME
      @ 1, 9 SAY PADR(m.tmpShab, 40)
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

RETURN m.retV

*������������������������������������������������������������������������Ŀ
*�   ��楤�� F_Shab       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                        ���� 蠡���� ��� 䨫���                        �
*�                                                                        �
*����������������������������������������������������������� 28.12.2012 ���
PROCEDURE F_Shab
PARAMETERS sValue

PRIVATE rValue

m.rValue = .F.
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

*������������������������������������������Ŀ
*� ....:....!....:....!....:....!....:....! �
*�                                          �
*�     < OK Ctrl-W > < �⪠������ Esc >     �
*��������������������������������������������

PRIVATE ex, tmpSab
m.ex = 1
m.tmpShab = ""
IF TYPE("m.sValue") = "C"
  m.tmpShab = ALLTRIM(m.sValue)
ENDIF
m.tmpShab = PADR(m.tmpShab, 40)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 6, 46, "������ ��� 䨫���"

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 2, 3 GET m.tmpShab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF m.ex = 1
  *
  * ��ࠡ��뢠�� �����
  m.rValue = ALLTRIM(m.tmpShab)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.rValue