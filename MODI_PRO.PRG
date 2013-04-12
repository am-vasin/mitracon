*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Modi_Pro     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                        ���४�� ᯨ᪠ �७���                        �
*�                                                                        �
*����������������������������������������������������������� 24.12.2005 �ͼ
PROCEDURE Modi_Pro

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
USE (m.base_path+"DEVELOP") SHARED AGAIN ALIAS DV_5C24 ORDER TAG B_NAME


*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "����." KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Delete OF (menu_name) PROMPT "�������" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Seek OF (menu_name) PROMPT "����. ����" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, seekTxt, seekMode, r_sav, mss, new_rec
m.seekTxt  = ""
m.seekMode = 1
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = 69

DO D_Wins WITH m.ln, m.wd, "����-�ந�����⥫�", 1, 1
@ 2, 2 SAY "����.���ď����� ��������"
@ 1, 2 SAY "����"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 8 SAY PADR("Enter", 8) COLOR (SCHEME(14,2))
    DO Prp_Nav_1

    ON KEY LABEL Tab   DO Sw_Mode WITH "Modify"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Delete"
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL Enter DO Locator WITH 1, 8, 8

*
*    BROWSE - ����
*
    BROWSE FIELDS F000 = LEFT(B_NAME, 8):H="", ;
                  L_NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Modify"    && ���४��

    IF .NOT. BOF() .AND. .NOT. EOF() .AND. CODE # 1
      DO Prod1 WITH CODE
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Add"       && ����������

    m.new_rec = 0
    DO Prod1
    IF BETWEEN(m.new_rec, 1, RECCOUNT())
      GO m.new_rec
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Delete"    && ��������

    IF .NOT. BOF() .AND. .NOT. EOF() .AND. CODE # 1
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"����⢨⥫쭮 㤠�塞? [ �� ] [ ��� ]"
      mss[3] = ""
      IF Out_Mess(5, "mss") = 1
        REPLACE DATE_OFF WITH DATE(), ;
                WHO_DEL  WITH m.user
      ENDIF
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Seek"      && ���⥪��� ����

    IF Get_Shab()
      IF .NOT. EMPTY(m.seekTxt)
        m.r_sav = RECNO()
        IF m.seekMode = 1
          GO TOP
        ELSE
          IF .NOT. EOF()
            SKIP
          ENDIF
        ENDIF
        LOCATE REST FOR m.seekTxt $ B_NAME .OR. m.seekTxt $ L_NAME
        IF .NOT. FOUND()
          DIMENSION mss[3]
          mss[1] = ""
          mss[2] = CHR(0)+""
          mss[3] = ""
          DO Out_Mess WITH 7, "mas"
          IF BETWEEN(m.r_sav, 1, RECCOUNT())
            GO m.r_sav
          ENDIF
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

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Draw_Foot    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
GO RECNO()		&& ���� �������� ���祭�� ���� PERIOD (��������!)
@ WROWS()-2, 3     SAY "�ப ���⠢�� "
@ WROWS()-2, COL() SAY PERIOD COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Locator      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ��ᨬ����� ����                           �
*�                                                                        �
*����������������������������������������������������������� 24.12.2005 ���
PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t
PRIVATE rc_near, flt_sav, p_sav

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
flt_sav = prf_flt
c_flt   = 0     && ������ ������ �����

DO WHILE .T.
  IF .NOT. flt_sav == prf_flt
    rc_near = RECNO()
    IF .NOT. SEEK(prf_flt)
      prf_flt = flt_sav
      c_flt   = p_sav
      IF BETWEEN(rc_near, 1, RECCOUNT())
        GO rc_near
      ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH
  ENDIF
  @ l001, c001 SAY PADR(prf_flt, ln, "�")
  @ l001, c001+c_flt SAY ""
  flt_sav = prf_flt
  p_sav   = c_flt
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

ACTIVATE WINDOW (win_name+"_M") SAME
@ 1, 8 SAY PADR("Enter", 8) COLOR (SCHEME(14,2))
ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Get_Shab     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                        ���� 蠡���� ��� ���᪠                         �
*�                                                                        �
*����������������������������������������������������������� 24.12.2005 ���
PROCEDURE Get_Shab
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

*������������������������������������Ŀ
*� �� ������ ��� ���᪠ �����������Ŀ �
*� �                                � �
*� ���������������������������������� �
*�( ) �᪠�� ᭠砫�  ( ) �᪠�� ������
*�  < OK Ctrl-W > < �⪠������ Esc >  �
*��������������������������������������

PRIVATE ex, ret, tmpTxt, tmpMode
m.ex = 1
m.ret = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 40, "���⥪��� ����"
DO Sun_Bord WITH  2,  3,  4, 36, " ������ ��� ���᪠ "

*------------------------------------------------------------------------
*      ���� ����� ������
*
m.tmpTxt  = m.seekTxt
m.tmpMode = m.seekMode
@ 3, 4 EDIT m.tmpTxt SIZE 1, 32 COLOR (","+SCHEME(1,1))
@ 5, 2 GET  m.tmpMode PICTURE "@*RH �᪠�� ᭠砫�; �᪠�� �����"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF m.ex = 1
  *
  * ��ࠡ��뢠�� �����
  m.seekTxt  = m.tmpTxt
  m.seekMode = m.tmpMode
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN m.ex = 1