*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Dlv_Sto      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                         ����������� ���⠢��.                         �
*�                                                                        �
*����������������������������������������������������������� 29.03.2000 �ͼ
PROCEDURE Dlv_Sto

PRIVATE chr_shab, int_1, int_2, tmpValue
DIMENSION int_1(2), int_2(2)   && ���ᠭ�� �㭪⮢ ���� ��� �ணࠬ�� ���᪠
int_1(1) = 27      && Esc
int_2(1) = "{Esc}"
int_1(2) = -6      && F7
int_2(2) = "{F7}"
chr_shab = ' !"'+    ;
           "#$%&'()*+,-./0123456789:;<=>?@"+  ;
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"+         ;
           "[\]^_`"+                             ;
           "abcdefghijklmnopqrstuvwxyz"+         ;
           "{|}~"+                               ;
           "��������������������������������"+   ;
           "��������������������������������"

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
USE (base_path+"DLV_STO") ORDER TAG CODE ALIAS DSTO_0329 AGAIN

SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME ALIAS STO_0329 AGAIN
SET RELATION TO CODE INTO DSTO_0329

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "��⠢���/�᪫����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "���⥪��� ����" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Stock OF (menu_name) PROMPT "���쪮 ���⠢��" KEY Ctrl-A, "F2"
ON SELECTION PAD Stock OF (menu_name) DO Sw_Mode WITH "Stock"

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE tmpName, seek_shab

seek_shab = SPACE(25)
PRIVATE ln, wd
ln = WROWS("")-8   && ������⢮ ������� ��ப BROWSE
wd = 51
*�����������������������������������������Ŀ
*�.... ....:....!....:....!....: ....:... V�
*�������������������������������������������

DO D_Wins WITH ln, wd, "����������� ���⠢��", 1, 0
@ 1, 3 SAY "����:"
@ 1, 10 SAY PADR("Tab",WCOLS()-10) COLOR (SCHEME(14,2))

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Seek_
    ON KEY LABEL F7  DO Sw_Mode WITH "Seek"
    ON KEY LABEL F2  DO Sw_Mode WITH "Stock"

*
*    BROWSE - ����
*
    BROWSE FIELDS PREFIX:4:H="",   ;
                  NAME:35:H="",    ;
                  PRODUCER:8:H="", ;
                  MARK = IIF(FOUND("DSTO_0329"), "�", " "):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �롮�

    IF .NOT. FOUND("DSTO_0329")
      SELECT DSTO_0329
      IF .NOT. SEEK(0)
        APPEND BLANK
      ENDIF
      REPLACE CODE WITH STO_0329.CODE, ;
              NAME WITH ALLTRIM(STO_0329.PREFIX)+ALLTRIM(STO_0329.NAME)
      SELECT STO_0329
      GO RECNO()
    ELSE
      REPLACE DSTO_0329.CODE WITH 0
    ENDIF
        
    what_do = "List"

  CASE what_do = "Seek"      && ���⥪��� ����

    tmpValue = Seek_C()
    IF tmpValue > 0
      GO tmpValue
    ENDIF
    what_do = "List"

  CASE what_do = "Stock"      && ���쪮 ���⠢��

    DO Sto_Dlv
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_E1")
RELEASE WINDOW (win_name+"_I1")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT DSTO_0329
USE
SELECT STO_0329
USE
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                 �ணࠬ�� ���᪠ �� ��砫�� �㪢��.                  �
*�                                                                        �
*����������������������������������������������������� 06.05.97 22:05:53 ͼ
PROCEDURE Seek_

PRIVATE s, prf, ln, bg, rc_s, i

ln = 29
ACTIVATE WINDOW (win_name+"_M") SAME
prf = ""
@ 1, 3 SAY "����:"
@ 1, 10 SAY PADR("Tab",ln) COLOR (SCHEME(14,2))
bg = REPLICATE("�", ln)
DO WHILE .T.
  @ 1, 10 SAY bg
  @ 1, 10 SAY prf
  s = INKEY(0)
  i = ASCAN(int_1, s)
  IF i # 0
    KEYBOARD int_2(i)
    EXIT
  ENDIF
  IF s < 0     && �㭪樮���쭠� ������
    IF s = -9  && F10 - ����᪠�� ����
      KEYBOARD "{F10}"
      EXIT
    ENDIF
    LOOP
  ENDIF
  s = CHR(s)
  IF s $ chr_shab .AND. LEN(prf) < ln   && ������
    rc_s = RECNO()
    IF SEEK(prf+s)
      prf = prf+s
    ELSE
      IF rc_s < RECCOUNT() .AND. rc_s > 0
        GO rc_s
      ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH
    LOOP
  ENDIF
  IF s = CHR(13)       && Enter
    KEYBOARD CHR(23)
    EXIT
  ENDIF
  IF s = CHR(3)        && PgDn
    KEYBOARD "{PgDn}"
    EXIT
  ENDIF
  IF s = CHR(18)       && PgUp
    KEYBOARD "{PgUp}"
    EXIT
  ENDIF
  IF s = CHR(5)        && UpArrow
    KEYBOARD "{UpArrow}"
    EXIT
  ENDIF
  IF s = CHR(24)       && DnArrow
    KEYBOARD "{DnArrow}"
    EXIT
  ENDIF
  IF s = CHR(127)      && BackSpace
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      SEEK prf
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
    LOOP
  ENDIF
ENDDO

@ 1, 10 SAY PADR("Tab",ln) COLOR (SCHEME(14,2))

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_C       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ���⥪��� ����.                           �
*�                                                                        �
*����������������������������������������������������������� 26.02.2000 ���
PROCEDURE Seek_C

IF .NOT. Get_Shab()
  RETURN ""
ENDIF
IF EMPTY(seek_shab)
  RETURN ""
ENDIF

PRIVATE s_sav, tmpShab
tmpShab = ALLTRIM(seek_shab)
s_sav = SELECT()
SELECT 0
DO Use_Link
ZAP
SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME ALIAS ST_0329 AGAIN
DO Wt_Mess WITH "����..."
SCAN
  IF tmpShab $ ALLTRIM(PREFIX)+ALLTRIM(NAME)
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("ST_0329")
    SELECT ST_0329
  ENDIF
ENDSCAN
DO Wt_Mess

tmpShab = Ch_P()

SELECT ST_0329
USE
SELECT (s_sav)

RETURN tmpShab

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Get_Shab     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ������ ��� ���᪠.                           �
*�                                                                        �
*����������������������������������������������������������� 26.02.2000 ���
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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*���������������������������������Ŀ
*�  �� ������ ��� ���᪠ ������Ŀ  �
*�  � ....:....!....:....!....: �  �
*�  �����������������������������  �
*�< OK Ctrl-W > < �⪠������ Esc > �
*�����������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 37, "���⥪��� ����"
DO Sun_Bord WITH  2,  4,  4, 32, " ������ ��� ���᪠ "

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 6 GET seek_shab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Choose_P     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�               �롮� ����樨 �� �⮡࠭��� �� ���⥪���.                �
*�                                                                        �
*����������������������������������������������������������� 26.02.2000 ���
PROCEDURE Ch_P

SET ORDER TO
SELECT LINK
IF RECCOUNT() = 0
  PRIVATE mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��祣� �� �������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN ""
ENDIF

SET RELATION TO LNK INTO ST_0329
GO TOP

PRIVATE retVal
retVal = ""
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
ln = WROWS("")-10   && ������⢮ ������� ��ப BROWSE
wd = 49

DO D_Wins WITH ln, wd, "����樨 ������������", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS ST_0329.PREFIX:H="", ;
                  ST_0329.NAME:H="", ;
                  ST_0329.PRODUCER:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �롮�

    retVal = LINK.LNK
    EXIT

  OTHERWISE

    retVal = 0
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

RETURN retVal