*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Classes      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                       ���४�� ᯨ᪠ ����ᮢ.                        �
*�                                                                        �
*����������������������������������������������������������� 03.09.1999 �ͼ
PROCEDURE Classes

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
PRIVATE i, flt_code, tmpName
s_sav = SELECT()
flt_code = ""

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE DESCENDING
SELECT 0
CREATE DBF (tmpo_path+"TMP____1") ;
   ( CODE      C(12),  ;
     NAME      C(FSIZE("NAME", "CLASSES")), ;
     GOOD_NAME C(FSIZE("GOOD_NAME", "CLASSES")), ;
     DEL  L )
INDEX ON SYS(15, lwr, NAME) TAG NAME
FOR i = 2 TO 6
  COPY STRUCTURE TO (tmpo_path+"TMP____"+STR(i,1)) WITH CDX
ENDFOR
FOR i = 2 TO 6
  SELECT 0
  USE (tmpo_path+"TMP____"+STR(i,1)) ORDER TAG NAME EXCLUSIVE
ENDFOR
DO Prp_Level

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "���४�஢���" KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

*DEFINE PAD Add OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Ins"
*ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

*DEFINE PAD Del OF (menu_name) PROMPT "�������" KEY Ctrl-A, "Del"
*ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-15   && ������⢮ ������� ��ப BROWSE
wd = 66

DO D_Wins WITH ln, wd, "������", 0, 6
@ WROWS()-8, 3 SAY " �஢�� "
@ WROWS()-7, WCOLS()-3 SAY ""
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    IF LEN(flt_code) < 10
      DEFINE PAD Choose BEFORE Modify OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
      ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"
    ENDIF
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Modify"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"

*
*    BROWSE - ����
*
    BROWSE FIELDS NAME:H="", ;
           F000 = IIF(DEL, "X", " "):H="":1,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF
    IF LEN(flt_code) < 10
      RELEASE PAD Choose OF (menu_name)
    ENDIF

  CASE what_do = "Choose"    && �롮�

    DO Dn_Level
    what_do = "List"

  CASE what_do = "Add"       && ��������

    DO Add_Class
    what_do = "List"

  CASE what_do = "Del"       && �������

    DO Del_Class
    what_do = "List"

  CASE what_do = "Modify"    && ���४�� �����

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Add_Class WITH .T.
    ENDIF
    what_do = "List"

  CASE what_do = "Exit"      && ������

    IF EMPTY(flt_code)
      what_do = "Escape"
    ELSE
      DO Up_Level
      what_do = "List"
    ENDIF

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
*CLOSE DATABASES
USE IN CLASSES
FOR i = 1 TO 6
  tmpName = "TMP____"+STR(i,1)
  USE IN (tmpName)
  DELETE FILE (tmpo_path+tmpName+".DBF")
  DELETE FILE (tmpo_path+tmpName+STR(i,1)+".CDX")
ENDFOR

SELECT (s_sav)

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
@ WROWS()-7+LEN(flt_code)/2, 2 SAY NAME
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prp_Level    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�             ��ନ஢���� �६������ 䠩�� ⥪�饣� �஢��.             �
*�                                                                        �
*����������������������������������������������������������� 03.09.1999 ���
PROCEDURE Prp_Level

PRIVATE a_name, ln

a_name = "TMP____"+STR(LEN(flt_code)/2+1,1)
SELECT CLASSES
SEEK flt_code
ln = LEN(flt_code)
SCAN REST WHILE LEFT(CODE, ln) = flt_code ;
     FOR EMPTY(SUBSTR(CODE, ln+3)) .AND. .NOT. EMPTY(SUBSTR(CODE, ln+1, 2))
  SELECT (a_name)
  APPEND BLANK
  REPLACE CODE      WITH CLASSES.CODE, ;
          NAME      WITH CLASSES.NAME, ;
          GOOD_NAME WITH CLASSES.GOOD_NAME, ;
          DEL  WITH .NOT. EMPTY(CLASSES.DATE_OFF)
  SELECT CLASSES
ENDSCAN

SELECT (a_name)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Add_Class    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            �������� �����.                             �
*�                                                                        �
*����������������������������������������������������������� 03.09.1999 ���
PROCEDURE Add_Class
PARAMETER modi

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

*����������������������������������������������������������������������Ŀ
*� �� �������� ����� �����������������������������������������������Ŀ �
*� � ....:....!....:....!....:....!......:....!....:....!....:....!.. � �
*� �������������������������������������������������������������������� �
*�                   < OK Ctrl-W > < �⪠������ Esc >                   �
*������������������������������������������������������������������������

*����������������������������������������������������������������������Ŀ
*� �� �������� ����� �����������������������������������������������Ŀ �
*� � ....:....!....:....!....:....!......:....!....:....!....:....!.. � �
*� �������������������������������������������������������������������� �
*�  �������� ⮢�� ....:....!....:....!....:....!....:....!....:....!  �
*�                   < OK Ctrl-W > < �⪠������ Esc >                   �
*������������������������������������������������������������������������
PRIVATE ex, nm, g_nm, g_num, cntry, l_no, s_sav, sw_gtd, sw_cnt, tmpQnt
ex = 1
IF modi
  nm = NAME
  g_nm = GOOD_NAME
ELSE
  sw_gtd = .F.
  sw_cnt = .F.
  s_sav = SELECT()
  SELECT 0
  USE (path_comm+"GTD") SHARED AGAIN ALIAS TMP_2410
  g_num = SPACE(FSIZE("GTD_NUM"))
  cntry = SPACE(FSIZE("COUNTRY"))
  l_no  = SPACE(FSIZE("GTD_LINE"))
  tmpQnt = 0
  USE
  SELECT (s_sav)
  nm = SPACE(FSIZE("NAME"))
  g_nm = SPACE(FSIZE("GOOD_NAME"))
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
*DO D_Win_N WITH IIF(modi, 7, 10), 74, "���������� �����"
DO D_Win_N WITH 8, 74, "���४�� �����"
DO Sun_Bord WITH  2,  3,  4, 70, " �������� ����� "
IF .F. &&&&&&&&&&& .NOT. modi		&&&&&&&&&&&&&&
  DO Sun_Bord WITH  5,  5,  7, 68, ""
  @ 5,  7 SAY " [ ] ����� ��� "
  @ 5, 35 SAY " [ ] ��࠭� "
  @ 5, 53 SAY "���."
  @ 5, 60 SAY "���-��."
ENDIF

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 5 GET nm
@ 5, 4 SAY "�������� ⮢��" GET g_nm
IF .F. &&&&&&&&&&&&&&&&&& .NOT. modi
  @ 5,  8 GET sw_gtd PICTURE "@*C ����� ��� " VALID G_GTD()
  @ 5, 36 GET sw_cnt PICTURE "@*C ��࠭� " VALID G_Cntry()
  @ 6,  7 GET g_num WHEN .F.
  @ 6, 35 GET cntry WHEN .F.
  @ 6, 53 GET l_no
  @ 6, 59 GET tmpQnt PICTURE "@Z 99999999"
ENDIF
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
READ CYCLE VALID Tst_Blank()
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_Blank    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ��室 �� ��� ���������� �����.                    �
*�                                                                        �
*����������������������������������������������������������� 03.09.1999 ���
PROCEDURE Tst_Blank

IF ex = 2
  RETURN .T.
ENDIF

PRIVATE a_name, last, mss, tmpDate
IF EMPTY(nm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�������� ����� �� ����� ���� �����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF
a_name = ALIAS()
IF .NOT. modi
  IF .F. &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    IF EMPTY(g_num)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"�� �� 㪠���� ���!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      RETURN 2
    ENDIF
    IF EMPTY(cntry)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"�� �� 㪠���� ��࠭�!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      RETURN 3
    ENDIF
    IF EMPTY(l_no)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"�� �� 㪠���� ��ப� ���!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      RETURN 6
    ENDIF
  ENDIF		&&&&&&&&&&&&&&&&&&&&&&&&
  last = 0
  IF SEEK(flt_code, "CLASSES")
    last = VAL(SUBSTR(CLASSES.CODE, LEN(flt_code)+1,2))
  ENDIF
  IF last >= 99
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� ���௠�� �����⨬�� ������⢮ ����ᮢ � ������ ࠧ����"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .T.
  ENDIF
  last = RIGHT(STR(100+last+1),2)
  APPEND BLANK
  REPLACE CODE WITH flt_code+last, NAME WITH nm, GOOD_NAME WITH g_nm
  SELECT CLASSES
  APPEND BLANK
  REPLACE CODE WITH flt_code+last, NAME WITH nm, GOOD_NAME WITH g_nm
  IF .F.
    SELECT 0
    USE path_comm+"GTD_TIT" SHARED AGAIN ALIAS GTD_2410 ORDER TAG GTD_NUM
    SEEK g_num
    tmpDate = GTD_DATE
    USE path_comm+"GTD_C" SHARED AGAIN ALIAS GTD_2410
    APPEND BLANK
    REPLACE GTD_NUM  WITH g_num,   ;
            GTD_DATE WITH tmpDate, ;
            COUNTRY  WITH cntry,   ;
            GTD_LINE WITH l_no,    ;
            CLASS    WITH flt_code+last,;
            QNT      WITH tmpQnt
    USE
  ENDIF
  SELECT (a_name)
ELSE
  = SEEK(CODE, "CLASSES")
  REPLACE NAME WITH nm, GOOD_NAME WITH g_nm
  REPLACE CLASSES.NAME WITH nm, CLASSES.GOOD_NAME WITH g_nm
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Dn_Level     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           �� �஢��� ����...                           �
*�                                                                        �
*����������������������������������������������������������� 03.09.1999 ���
PROCEDURE Dn_Level

IF BOF() .OR. EOF()
  RETURN
ENDIF
IF LEN(flt_code) = 10
  RETURN
ENDIF

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-7+LEN(flt_code)/2, WCOLS()-3 SAY " "
flt_code = ALLTRIM(CODE)
DO Prp_Level
@ WROWS()-7+LEN(flt_code)/2, WCOLS()-3 SAY ""

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Up_Level     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           �� �஢��� �����...                          �
*�                                                                        �
*����������������������������������������������������������� 03.09.1999 ���
PROCEDURE Up_Level
PRIVATE a_name

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-7+LEN(flt_code)/2, 2 CLEAR TO WROWS()-2, WCOLS()-3
flt_code = LEFT(flt_code, LEN(flt_code)-2)
ZAP
a_name = "TMP____"+STR(LEN(flt_code)/2+1,1)
SELECT (a_name)

@ WROWS()-7+LEN(flt_code)/2, WCOLS()-3 SAY ""

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Del_Class    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            ������� �����.                              �
*�                                                                        �
*����������������������������������������������������������� 03.09.1999 ���
PROCEDURE Del_Class

IF EOF() .OR. BOF()
  RETURN
ENDIF

PRIVATE mss, s_sav, k, a_name
s_sav = SELECT()
IF .NOT. DEL
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"  ��������! ��࠭�� ���� ����� �㤥� 㤠��� ����� � �������ᠬ�,  "
  mss(3) = CHR(0)+"  �᫨ ⠪��� ����� ����. �� �ࠢ��쭮? [ �� ] [ ��� ]"
  mss(4) = ""
  IF Out_Mess(5, "mss") = 2
    RETURN
  ENDIF
  REPLACE DEL WITH .T.
  k = ALLTRIM(CODE)
  SELECT CLASSES
  SEEK k
  SCAN REST WHILE LEFT(CODE, LEN(k)) = k
    IF EMPTY(DATE_OFF)
      REPLACE DATE_OFF WITH DATE()
    ENDIF
  ENDSCAN
  SELECT (s_sav)
ELSE
  IF LEN(flt_code) # 0
    a_name = "TMP____"+STR(LEN(flt_code)/2,1)
    SELECT (a_name)
    IF DEL
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"  ��������! ������ த�⥫�᪨� �����, ����⠭���� �०�� ���!"
      mss(3) = ""
      DO Out_Mess WITH 5, "mss"
      SELECT (s_sav)
      RETURN
    ENDIF
    SELECT (s_sav)
  ENDIF
  REPLACE DEL WITH .F.
  = SEEK(CODE, "CLASSES")
  REPLACE CLASSES.DATE_OFF WITH {}
ENDIF

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� G_GTD        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                               �롮� ���                                �
*�                                                                        �
*����������������������������������������������������������� 10.04.2002 ���
PROCEDURE G_GTD

IF .NOT. sw_gtd
  RETURN .T.
ENDIF

g_num = GTD_List(g_num)

sw_gtd = .F.
SHOW GET sw_gtd
SHOW GET g_num

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� G_Cntry      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                               �롮� ��࠭�                             �
*�                                                                        �
*����������������������������������������������������������� 10.04.2002 ���
PROCEDURE G_Cntry

IF .NOT. sw_cnt
  RETURN .T.
ENDIF

cntry = Country(cntry)

sw_cnt = .F.
SHOW GET sw_cnt
SHOW GET cntry

RETURN .T.