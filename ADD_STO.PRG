*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Add_Sto      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                  ���� � �������⥫쭮� �����������                   �
*�                                                                        �
*����������������������������������������������������������� 18.03.2008 �ͼ
PROCEDURE Add_Sto
PARAMETER tmpList, rc_no

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
USE (m.base_path+"LIST_DET") SHARED AGAIN ORDER TAG L_NAME ALIAS ADD_STO_L
SELECT 0
USE (m.base_path+"ADD_CLS") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (m.base_path+"ADD_STO") SHARED AGAIN ORDER TAG NAME
SET RELATION TO SOURCE+CLASS INTO ADD_CLS
IF .NOT. EMPTY(m.rc_no)
  GO m.rc_no
ENDIF

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF .NOT. EMPTY(m.tmpList)
  DEFINE PAD Choose OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"
ENDIF

DEFINE PAD Seek OF (menu_name) PROMPT "����.���� F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD ASeek OF (menu_name) PROMPT "���� �� ���. Ctrl-F7"
ON SELECTION PAD ASeek OF (menu_name) DO Sw_Mode WITH "ASeek"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


DIMENSION interr1(1), interr2(1)
interr1(1) = -6
interr2(1) = "{F7}"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, fnc
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = 75

DO D_Wins WITH m.ln, m.wd, "�������⥫쭠� �����������", 1, 6
ACTIVATE WINDOW (win_name+"_M")
@ 1, 3 SAY "����"
@ 1, 9 SAY PADR("Tab",25) COLOR (SCHEME(14,2))

m.fnk = ""
@ WROWS()-6, 2 EDIT m.fnk SIZE 5, 75 NOMODIFY
@ 2,  5 SAY "������������"
@ 2, 52 SAY "�����"
@ 2, 69 SAY "�������"

@ WROWS()-8, 2 SAY "�㭪�. �����祭��"
SCROLL WROWS()-8, 1, WROWS()-7, WCOLS()-2, -1
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Seek_P
    ON KEY LABEL F7  DO Sw_Mode WITH "Seek"
    ON KEY LABEL Ctrl+F7  DO Sw_Mode WITH "ASeek"

*
*    BROWSE - ����
*
    BROWSE FIELDS PREFIX:H="",		;
                  NAME:H="",		;
                  PRODUCER:H="",	;
                  CORPUS:H="",		;
                  SEGMENT:H="",		;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && ��ᬮ�� ᯨ᪠

    IF .NOT. EMPTY(m.tmpList)
      IF Tst_Dubl()
        DO Add_Sto1
      ENDIF
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Seek"      && ����

    CLEAR GETS
    DO Seek_C
    m.what_do = "List"

  CASE m.what_do = "ASeek"      && ���� �� ��⨪��

    CLEAR GETS
    DO Seek_A
    m.what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR READ
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
USE IN ADD_STO
USE IN ADD_STO_L
USE IN ADD_CLS

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
CLEAR GETS
m.fnk = FUNCTION
@ WROWS()-8, 2 SAY "�����" GET ADD_CLS.NAME
@ WROWS()-6, 2 EDIT m.fnk SIZE 5, 75 NOMODIFY
@ WROWS()-1, WCOLS()-23 SAY "��⨪� "
@ WROWS()-1, COL() SAY Articul(CODE) COLOR (SCHEME(1, 1))
CLEAR GETS

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                      ���� �� ��砫�� ᨬ�����.                      �
*�                                                                        �
*����������������������������������������������������� 10.06.97 10:43:18 ͼ
PROCEDURE Seek_P
PRIVATE prf, s, ln, rc_sav, i

ACTIVATE WINDOW (win_name+"_M") SAME

prf = ""
ln = 35
DO WHILE .T.
  ACTIVATE WINDOW (win_name+"_M") SAME
  @ 1, 9 SAY REPLICATE("�",35) COLOR SCHEME 1
  @ 1, 9 SAY prf COLOR SCHEME 1
  s = INKEY(0)
  *
  *   ��᫥�㥬 �� �।��� ������ ��ࠡ��稪�
  *
  i = ASCAN(interr1, s)
  IF i # 0
    KEYBOARD interr2(i)
    EXIT
  ENDIF
  IF s < 0       && �㭪樮���쭠� ������
    IF s = -9
      KEYBOARD "{F10}"
      EXIT
    ENDIF
    LOOP
  ENDIF
  IF BETWEEN(s,  32,126) .OR.   ;
     BETWEEN(s, 128,175) .OR.   ;
     BETWEEN(s, 224,241)     && ������
    IF LEN(prf) >= 35        && ��! ����� ���㤠!
      LOOP
    ENDIF
    rc_sav = RECNO()
    IF SEEK(UPPER(prf+CHR(s)))
      prf = prf+CHR(s)
      SHOW WINDOW (ALIAS()) REFRESH
    ELSE
      ?? CHR(7)
      GO rc_sav
    ENDIF
  ENDIF
  IF s = 127       && Back Space
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      = SEEK(UPPER(prf))
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
  ENDIF
  IF s = 13        && Enter
    EXIT
  ENDIF
  IF s = 24        && Down Arrow
    KEYBOARD "{DnArrow}"
    EXIT
  ENDIF
  IF s = 5         && Up Arrow
    KEYBOARD "{UpArrow}"
    EXIT
  ENDIF
  IF s = 3         && Page Down
    KEYBOARD "{PgDn}"
    EXIT
  ENDIF
  IF s = 18        && Page Up
    KEYBOARD "{PgUp}"
    EXIT
  ENDIF
  IF s = 27        && Escape
    KEYBOARD "{Esc}"
    EXIT
  ENDIF
ENDDO

@ 1, 9 SAY PADR("Tab",35) COLOR (SCHEME(14,2))
  
ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_C       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ���⥪��� ����                            �
*�                                                                        �
*����������������������������������������������������������� 18.03.2008 ���
PROCEDURE Seek_C

PRIVATE tmpShab, tmpFile, s_sav, r_sav

m.tmpSahb = Get_Shab()
IF EMPTY(m.tmpSahb)
  RETURN 
ENDIF

m.s_sav = SELECT()
SELECT ADD_STO
m.r_sav = RECNO()
m.tmpFile = SYS(3)
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpFile) ;
	(	LINK		N(10),	;
		PREFIX		C(FSIZE("PREFIX", "ADD_STO")),	;
		NAME		C(FSIZE("NAME", "ADD_STO")),	;
		PRODUCER	C(FSIZE("PRODUCER", "ADD_STO"))	)
USE (m.tmpo_path+m.tmpFile) EXCLUSIVE ALIAS ADD_SEEK
m.tmpSahb = UPPER(m.tmpSahb)
SELECT ADD_STO
DO Wt_Mess WITH "����..."
SCAN
  IF m.tmpSahb $ UPPER(PREFIX+NAME+PRODUCER)
    SELECT ADD_SEEK
    APPEND BLANK
    REPLACE LINK		WITH RECNO("ADD_STO"),	;
            PREFIX		WITH ADD_STO.PREFIX,	;
            NAME		WITH ADD_STO.NAME,		;
            PRODUCER	WITH ADD_STO.PRODUCER
    SELECT ADD_STO
  ENDIF
ENDSCAN
DO Wt_Mess
IF BETWEEN(m.r_sav, 1, RECCOUNT())
  GO m.r_sav
ENDIF

SELECT ADD_SEEK
m.tmpFile = DBF()
USE

DO Ch_Found WITH m.tmpFile

SELECT (m.s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Ch_Found     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          �롮� �� ����������                           �
*�                                                                        �
*����������������������������������������������������������� 18.03.2008 ���
PROCEDURE Ch_Found
PARAMETERS tmpName

PRIVATE mss

SELECT 0
USE (m.tmpName) EXCLUSIVE ALIAS ADD_FOUND
IF RECCOUNT() = 0
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��祣� �� �������..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (m.tmpName)
  RETURN
ENDIF

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

DO Use_Dummy
SELECT ADD_FOUND

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
m.wd = 49

DO D_Wins WITH m.ln, m.wd, "������� ���᪠", 0, 0
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
    BROWSE FIELDS PREFIX:H="",	;
                  NAME:H="",	;
                  PRODUCER:H="",;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && ��ᬮ�� ᯨ᪠

    GO ADD_FOUND.LINK IN ADD_STO
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
USE
DELETE FILE (m.tmpName)

SELECT (m.s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_Dubl     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                � ����� ⠪�� ������ � ������ 㦥 ����?                �
*�                                                                        �
*����������������������������������������������������������� 23.03.2008 ���
PROCEDURE Tst_Dubl

PRIVATE tmpName, mss

m.tmpName = PADR(ALLTRIM(ADD_STO.NAME),     FSIZE("NAME",   "ADD_STO_L"))+ ;
            PADL(ALLTRIM(ADD_STO.PREFIX),   FSIZE("PREFIX", "ADD_STO_L"))+ ;
            PADR(ALLTRIM(ADD_STO.PRODUCER), FSIZE("PRODUCER",   "ADD_STO_L"))
IF SEEK(m.tmpList+m.tmpName, "ADD_STO_L")
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"�� ������ 㦥 ���� � ������"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Add_Sto1     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ���������� ����樨 � �����                       �
*�                                                                        �
*����������������������������������������������������������� 23.03.2008 ���
PROCEDURE Add_Sto1
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
PRIVATE s_sav

PRIVATE tPrf, tNam, tProd, tQnt

m.tPrf  = PADL(ALLTRIM(ADD_STO.PREFIX),   FSIZE("PREFIX", "ADD_STO_L"))
m.tNam  = PADR(ALLTRIM(ADD_STO.NAME),     FSIZE("NAME",   "ADD_STO_L"))
m.tProd = PADR(ALLTRIM(ADD_STO.PRODUCER), FSIZE("PRODUCER",   "ADD_STO_L"))
m.tQnt  = 0

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

*�������������������������������������������������������������Ŀ
*� �� ������ ��������������������������������������Ŀڊ��-��� �
*� �.... ....:....!....:....!....:....!....: ....:...��123456� �
*� ����������������������������������������������������������� �
*�               < OK Ctrl-W > < �⪠������ Esc >              �
*���������������������������������������������������������������

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 65, "���������� ����樨 � �����"
DO Sun_Bord WITH  2,  3,  4, 53, " ������ "
DO Sun_Bord WITH  2,  54,  4, 61, ""
@ 2, 55 SAY "���-��"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3,  4 SAY m.tPrf COLOR SCHEME 1
@ 3,  9 SAY m.tNam COLOR SCHEME 1
@ 3, 45 SAY m.tProd COLOR SCHEME 1
@ 3, 55 GET m.tQnt PICTURE "@Z 999999"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF m.ex = 1 .AND. m.tQnt > 0
  *
  * ��ࠡ��뢠�� �����
  m.s_sav = SELECT()
  SELECT ADD_STO_L
  APPEND BLANK
  REPLACE LIST_NUM	WITH m.tmpList,	;
          PREFIX	WITH m.tPrf, 	;
          NAME		WITH m.tNam, 	;
          PRODUCER	WITH m.tProd, 	;
          QNT		WITH m.tQnt
  
  SELECT (m.s_sav)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_A       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ���� �� ��⨪��                            �
*�                                                                        �
*����������������������������������������������������������� 24.03.2008 ���
PROCEDURE Seek_A

PRIVATE tmpShab, seekSh, i, c

m.tmpSahb = Get_ShA()

m.seekSh = ""
IF LEN(m.tmpSahb) > 0
  FOR m.i = 1 TO LEN(m.tmpSahb)
    m.c = SUBSTR(m.tmpSahb, m.i, 1)
    IF m.c $ "0123456789"
      m.seekSh = m.seekSh+m.c
    ENDIF
  ENDFOR
ENDIF

m.seekSh = VAL(m.seekSh)

IF EMPTY(m.seekSh)
  RETURN 
ENDIF

USE (m.base_path+"ADD_STO") SHARED AGAIN ALIAS SEEK_TMP ORDER TAG CODE IN 0
IF SEEK(STR(m.seekSh), "SEEK_TMP")
  GO RECNO("SEEK_TMP") IN ADD_STO
ELSE
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��� ⠪��� ��⨪㫠!"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

USE IN SEEK_TMP

RETURN