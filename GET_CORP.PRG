*������������������������������������������������������������������������ͻ
*� ���� Get_Corp     ���ࠡ��稪 ���३ ��ᨭ           04.04.2004        �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                             �롮� �����.                             �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Get_Corp
PARAMETERS prmCode, prmName

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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
USE (path_comm+"CORPUS") ORDER TAG CODE ALIAS TMP_4404 AGAIN
IF prmCode > 0 .AND. SEEK(prmCode)
  SET ORDER TO TAG NAME
ELSE
  SET ORDER TO TAG NAME
  GO TOP
ENDIF

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "����"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "�� ���⥪���" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "Seek_1"
  
  DEFINE BAR  2 OF Seek PROMPT "�����" KEY Ctrl-A, "Ctrl-F7"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "Again"
  
DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, tmpShab, r_sav, mss
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "�����", 1, 0
@ 1, 2 SAY "���� "
@ 1, 8 SAY "Tab" COLOR (SCHEME(14,2))
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter   KEYBOARD CHR(23)
    ON KEY LABEL Tab     DO Locator WITH 1, 8, 10
    ON KEY LABEL F7      DO Sw_Mode WITH "Seek_1"
    ON KEY LABEL Ctrl+F7 DO Sw_Mode WITH "Again"
*
*    BROWSE - ����
*
    BROWSE FIELDS NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      IF READKEY() % 256 = 12
        what_do = "Exit"
      ENDIF
      IF READKEY() % 256 = 14
        what_do = "Choose"
      ENDIF
    ENDIF

  CASE what_do = "Seek_1"    && ���⥪��� ����

    m.tmpShab = Get_Shab()
    IF .NOT. EMPTY(m.tmpShab)
      m.r_sav = RECNO()
      LOCATE FOR m.tmpShab $ NAME
      IF .NOT. FOUND()
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"��祣� �� �������..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Again"    && ����� ���᪠

    
    IF .NOT. EMPTY(m.tmpShab)
      m.r_sav = RECNO()
      IF .NOT. EOF()
        SKIP
      ENDIF
      LOCATE REST FOR m.tmpShab $ NAME
      IF .NOT. FOUND()
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"��祣� �� �������..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(m.r_sav, 1, RECCOUNT())
          GO m.r_sav
        ENDIF
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Choose"    && �롮�

    prmCode = CODE
    IF TYPE("prmName") = "C"
      prmName = NAME
    ENDIF
    EXIT

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
SELECT TMP_4404
USE

SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Locator      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ���� �� ��砫�� �㪢��                        �
*�                                                                        �
*����������������������������������������������������������� 04.05.2001 ���
PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t, v_sav, r_sav, c_sav

is_ins = INSMODE()   && ����� ��⠢��/������

*
*   "*" - ����, ᮮ⢥�����騥 ����� ᨬ�����
*
chr_type = SPACE(31)+     REPLICATE("*",126-31 )+ ;
           SPACE(127-126)+REPLICATE("*",175-127)+ ;
           SPACE(223-175)+REPLICATE("*",241-223)+ ;
           SPACE(255-223)
*
*   "I" - ����, ᮮ⢥�����騥 ᨬ����� ����७��� ������樨
*
chr_type = STUFF(chr_type,   1, 1, "I")  &&  Home
chr_type = STUFF(chr_type,   4, 1, "I")  &&  ��५�� ��ࠢ�
chr_type = STUFF(chr_type,   6, 1, "I")  &&  End
chr_type = STUFF(chr_type,   7, 1, "I")  &&  Del
chr_type = STUFF(chr_type,  13, 1, "I")  &&  Enter
chr_type = STUFF(chr_type,  19, 1, "I")  &&  ��५�� �����
chr_type = STUFF(chr_type,  22, 1, "I")  &&  Ins
chr_type = STUFF(chr_type, 127, 1, "I")  &&  BackSpace

*
*   "E" - ����, ᮮ⢥�����騥 ᨬ����� ���譥� ������樨 ( �� ���� )
*

chr_type = STUFF(chr_type,  3, 1, "E")  &&  PgDn
chr_type = STUFF(chr_type,  5, 1, "E")  &&  ��५�� �����
chr_type = STUFF(chr_type, 18, 1, "E")  &&  PgUp
chr_type = STUFF(chr_type, 24, 1, "E")  &&  ��५�� ����

prf_flt = ""    && ��䨪�
c_flt   = 0     && ������ ������ �����

DO WHILE .T.
  ACTIVATE WINDOW (win_name+"_M") SAME
  @ l001, c001 SAY PADR(prf_flt, ln, "�")
  @ l001, c001+c_flt SAY ""
  v_sav   = prf_flt
  c_sav   = c_flt

  c_c = INKEY(0)                && ��� ᨬ����
  IF c_c >= 0
    c = CHR(c_c)                && ������
    c_t = SYS(15, chr_type, c)  && ��� ᨬ����
  ENDIF
  IF INLIST(c_c, 27, 9)
    EXIT
  ENDIF
  DO CASE
  CASE c_c < 0                  && �㭪樮���쭠� ������
  CASE c_t = "*"                && ����� ᨬ���
    prf_flt = LEFT(prf_flt, c_flt)+c+  ;
              SUBSTR(prf_flt, c_flt+IIF(is_ins,1,2))
    prf_flt = LEFT(prf_flt, ln)
    c_flt = c_flt+1
    c_flt = MIN(c_flt, ln)
  CASE c_t = "E"                && �������� �� ����
  CASE c_t = "I"                && ����७��� ��������
    DO CASE
    CASE c_c = 7                && Del
      prf_flt = LEFT(prf_flt, c_flt)+SUBSTR(prf_flt, c_flt+2)
    CASE c_c = 127              && BackSpace
      prf_flt = LEFT(prf_flt, c_flt-1)+SUBSTR(prf_flt, c_flt+1)
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 19               && ��५�� �����
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 4                && ��५�� ��ࠢ�
      c_flt = MIN(c_flt+1,LEN(prf_flt))
    CASE c_c = 22               && Ins
      is_ins = .NOT. is_ins
      = INSMODE(is_ins)
    CASE c_c = 13
      EXIT
    ENDCASE
  OTHERWISE
    LOOP
  ENDCASE
  IF .NOT. v_sav == prf_flt
    r_sav   = RECNO()
    IF .NOT. SEEK(prf_flt)
      prf_flt = v_sav
       c_flt   = c_sav
       IF BETWEEN(r_sav, 1, RECCOUNT())
          GO r_sav
       ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) SAME REFRESH
  ENDIF

ENDDO

@ 1, 8 SAY PADR("Tab", ln) COLOR (SCHEME(14,2))

RETURN