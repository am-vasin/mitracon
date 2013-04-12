*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Corpuses     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                       ���४�� ᯨ᪠ ����ᮢ                        �
*�                                                                        �
*����������������������������������������������������������� 17.12.2005 �ͼ
PROCEDURE Corpuses

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
USE (m.path_comm+"CORPUS") SHARED AGAIN ALIAS COR_5C17 ORDER TAG NAME

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "���४�஢���" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, pic, new_rec
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = FSIZE("NAME")+FSIZE("WEIGHT")+1
m.pic = REPLICATE("9", FSIZE("WEIGHT")-4)+".999"

DO D_Wins WITH m.ln, m.wd, "���� ����ᮢ", 1, 0
@ 1, 2 SAY "���� Tab"
@ 2, 2 SAY "��������"
@ 2, WCOLS()-5 SAY "���"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Tab   DO Locator WITH 1, 2, 16

*
*    BROWSE - ����
*
    BROWSE FIELDS NAME:H="", ;
                  WEIGHT:H="":P=M.pic, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE m.what_do = "Modify"    && ���४��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Corpus WITH CODE
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Add"       && ����������

    m.new_rec = 0
    DO Corpus WITH 0
    IF BETWEEN(m.new_rec, 1, RECCOUNT())
      GO m.new_rec
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
USE IN COR_5C17
SELECT (m.s_sav)

RETURN


*������������������������������������������������������������������������Ŀ
*�   ��楤�� Locator      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ��ᨬ����� ����                           �
*�                                                                        �
*����������������������������������������������������������� 17.12.2005 ���
PROCEDURE Locator

PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t
PRIVATE prf_sav, c_sav, r_sav

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
prf_sav = prf_flt
c_sav   = c_flt

ACTIVATE WINDOW (m.win_name+"_M") SAME
DO WHILE .T.
  IF .NOT. prf_sav == prf_flt
    r_sav = RECNO()
    IF .NOT. SEEK(prf_flt)
      prf_flt = prf_sav
      c_flt   = c_sav
      IF BETWEEN(r_sav, 1, RECCOUNT())
        GO r_sav
      ENDIF
    ELSE
      prf_sav = prf_flt
      c_sav   = c_flt
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
  ENDIF
  c_sav   = c_flt
  @ l001, c001 SAY PADR(prf_flt, ln, "�")
  @ l001, c001+c_flt SAY ""

  c_c = INKEY(0)                && ��� ᨬ����
  IF c_c >= 0
    c = CHR(c_c)                && ������
    c_t = SYS(15, chr_type, c)  && ��� ᨬ����
  ENDIF
  DO CASE
  CASE c_c < 0                  && �㭪樮���쭠� ������
  CASE c_t = "*"                && ����� ᨬ���
    prf_flt = LEFT(prf_flt, c_flt)+c+  ;
              SUBSTR(prf_flt, c_flt+IIF(is_ins,1,2))
    prf_flt = LEFT(prf_flt, ln)
    c_flt = c_flt+1
    c_flt = MIN(c_flt, ln)
    LOOP
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
    EXIT
  ENDCASE
ENDDO

@ l001, c001 SAY PADR("���� Tab", ln)

ACTIVATE WINDOW (ALIAS())

RETURN
