*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� GTD_List     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                         �롮� �� ��娢� ���.                           �
*�                                                                        �
*����������������������������������������������������������� 09/04/2002 �ͼ
PROCEDURE GTD_List
PARAMETER s_Value
PRIVATE r_value

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
r_value = ""
IF TYPE("s_value") = "C"
  IF .NOT. EMPTY(s_value)
    r_value = s_value
  ENDIF
ENDIF


s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS DET_0303 AGAIN

SELECT 0
USE (path_comm+"GTD_C") ORDER TAG GTD_NUM ALIAS CLS_0303 AGAIN

SELECT 0
USE (path_comm+"GTD_TIT") ALIAS TIT_0302 AGAIN ORDER TAG GTD_NUM
SET RELATION TO GTD_NUM INTO DET_0303
SET RELATION TO GTD_NUM INTO CLS_0303 ADDITIVE
IF .NOT. EMPTY(r_value)
  IF .NOT. SEEK(r_value)
    SET ORDER TO TAG HRON
    GO TOP
    r_value = ""
    GO TOP
  ENDIF
ELSE
  SET ORDER TO TAG HRON
  GO TOP
ENDIF
SET ORDER TO TAG HRON
IF EMPTY(r_value)
  r_value = SPACE(FSIZE("GTD_NUM"))
ENDIF

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�롮�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Modify OF (menu_name) PROMPT "����ন���"
ON PAD Modify OF (menu_name) ACTIVATE POPUP Modify

  DEFINE POPUP Modify MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Modify PROMPT "����樨 ���" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Modify DO Sw_Mode WITH "Modify"

  DEFINE BAR  2 OF Modify PROMPT "������ ����ᮢ" KEY Ctrl-A, "Shift-Tab"
  ON SELECTION BAR 2 OF Modify DO Sw_Mode WITH "Classes"

DEFINE PAD Print OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Seek OF (menu_name) PROMPT "����"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "���� ���" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "Seek"

  DEFINE BAR  2 OF Seek PROMPT "����� ���᪠" KEY Ctrl-A, "Ctrl-Enter"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "Again"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 43

*��� ��� ���� ����� ��������������������ďĊ�
*���.��.���� NNNNNNNNNNNNNNNNNNNNNNNNNN X X X�
*���������������������������������������������

DO D_Wins WITH ln, wd, "���祭� ���", 0, 0
@ 1, 2 SAY "�� ��� ���� ����� ��������������������ďĊ"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter   KEYBOARD CHR(23)
    ON KEY LABEL Tab     DO Sw_Mode WITH "Modify"
    ON KEY LABEL BackTab DO Sw_Mode WITH "Classes"
    ON KEY LABEL Ctrl+P  DO Sw_Mode WITH "Print"
    ON KEY LABEL F7      DO Sw_Mode WITH "Seek"
    ON KEY LABEL Ctrl+F7 DO Sw_Mode WITH "Again"

*
*    BROWSE - ����
*
    BROWSE FIELDS GTD_DATE:H="", GTD_NUM:H="", GTD_TYPE:H="", ;
           M001 = IIF(FOUND("DET_0303"), "�", " "):1:H= "", ;
           M002 = IIF(FOUND("CLS_0303"), "�", " "):1:H= "", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �롮� ���

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_value = GTD_NUM
    ENDIF
    EXIT

  CASE what_do = "Modify"    && ��ᬮ�� ᮤ�ন���� ���

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO View_GTD
    ENDIF
    what_do = "List"

  CASE what_do = "Classes"    && ��ᬮ�� ᮤ�ন���� ���

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO View_Cls
    ENDIF
    what_do = "List"

  CASE what_do = "Print"    && �����

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_GTD WITH GTD_NUM
    ENDIF
    what_do = "List"

  CASE what_do = "Seek"     && ����

    DO SeekGTD
    what_do = "List"

  CASE what_do = "Again"    && �����

    DO SeekGTD WITH .T.
    what_do = "List"

  CASE what_do = "Ins"       && ���������� ��������� ���

    DO Add_GTD
    what_do = "List"

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
SELECT DET_0303
USE
SELECT CLS_0303
USE
SELECT (s_sav)

RETURN r_value

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Add_GTD      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ���������� ��������� ���.                        �
*�                                                                        �
*����������������������������������������������������������� 03/02/2000 ���
PROCEDURE Add_GTD

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

*���������������������������������������Ŀ
*�  ����� ��� ....:....!....:....!....:. �
*�                                       �
*�  ��� ���  ��.��.���� (F1)            �
*�                                       �
*�  [ ] ������                           �
*�                                       �
*�   < OK Ctrl-W > < �⪠������ Esc >    �
*�����������������������������������������

PRIVATE ex, tmpNum, tmpDate, tmpFlg
tmpNum  = SPACE(26)
tmpDate = DATE()
tmpFlg  = .F.
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 10, 34, "���������� ���"

@ 2, 4 SAY "����� ���"
@ 4, 4 SAY "��� ���             (F1)"
*------------------------------------------------------------------------
*      ���� ����� ������
*
ON KEY LABEL F1 DO Set_Dt
@ 2, 14 GET tmpNum
@ 4, 14 GET tmpDate
@ 6,  4 GET tmpFlg PICTURE "@*C ������"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE VALID Tst_GTD()
ON KEY LABEL F1

IF ex = 1
  *
  * ��ࠡ��뢠�� �����
  APPEND BLANK
  REPLACE GTD_NUM  WITH tmpNum,  ;
          GTD_DATE WITH tmpDate, ;
          GTD_TYPE WITH IIF(tmpFlg, "�", " ")
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Set_Dt       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ��⠭���� ���� �� ���������.                      �
*�                                                                        �
*����������������������������������������������������������� 02.03.2000 ���
PROCEDURE Set_Dt

tmpDate = Get_Date(tmpDate)
SHOW GET tmpDate

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_GTD      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       �஢�ઠ ���४⭮�� ���.                       �
*�                                                                        �
*����������������������������������������������������������� 03/02/2000 ���
PROCEDURE Tst_GTD

PRIVATE mss, r_sav, o_sav, sw

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(tmpNum)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ��� ������ ����! �� ��� �����-�����..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

r_sav = RECNO()
o_sav = TAG()

SET ORDER TO GTD_NUM
sw =  SEEK(tmpNum)
SET ORDER TO (o_sav)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
IF sw
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� � ⠪�� ����஬ 㦥 �������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� View_GTD     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                        ��ᬮ�� ᮤ�ন���� ���.                       �
*�                                                                        �
*����������������������������������������������������������� 03/03/2000 ���
PROCEDURE View_GTD

DO Prep_Tmp
DO Modi_Tmp
SELECT GTD_TMP
USE
DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT TIT_0302

RETURN


*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Tmp     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      �����⮢�� �६������ 䠩��.                      �
*�                                                                        �
*����������������������������������������������������������� 03/03/2000 ���
PROCEDURE Prep_Tmp

DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS GTD_0303 AGAIN

COPY STRUCTURE FIELDS NAME, COUNTRY, GTD_LINE, QNT TO (tmpo_path+"GTD_TMP")
CREATE DBF (tmpo_path+"GTD_TMP") ;
  ( MARK C(1), NAME C(29), COUNTRY C(16), GTD_LINE C( 4), QNT N( 8) )

USE (tmpo_path+"GTD_TMP") EXCLUSIVE

SELECT GTD_0303
SEEK TIT_0302.GTD_NUM
SCAN REST WHILE GTD_NUM = TIT_0302.GTD_NUM
  SELECT GTD_TMP
  APPEND BLANK
  REPLACE NAME     WITH GTD_0303.NAME,    ;
          COUNTRY  WITH GTD_0303.COUNTRY, ;
          GTD_LINE WITH GTD_0303.GTD_LINE,;
          QNT      WITH GTD_0303.QNT
  SELECT GTD_0303
ENDSCAN

USE
SELECT GTD_TMP
INDEX ON NAME TAG NAME

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Modi_Tmp     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          ���४�� ����⮢��.                          �
*�                                                                        �
*����������������������������������������������������������� 03.03.2000 ���
PROCEDURE Modi_Tmp


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
SELECT GTD_TMP

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE WINDOW (win_name+"_H") FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
ACTIVATE WINDOW (win_name+"_H") NOSHOW
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
*��č����������������������������đ�࠭�����������N ����Ċ��-��Ŀ
*�M ....:....!....:....!....:.... ....:....!....:. .... ....:...�
*����������������������������������������������������������������
*��������������������������������������������������������������Ŀ
*�        ������������ ....:....!....:....!....:....            �
*�                                                              �
*� [ ] ��࠭� ....:....!....:.  ��ப� N 9999  ���-�� 99999999  �
*�                                                              �
*�              < OK Ctrl-W > < �⪠������ Esc >                �
*����������������������������������������������������������������
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 62

DO D_Wins WITH ln, wd, "��� N "+ALLTRIM(TIT_0302.GTD_NUM)+" �� "+DTOC(TIT_0302.GTD_DATE), 0, 5
@ 1, 2 SAY "�č����������������������������đ�࠭�����������N ����Ċ��-��"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1

    BROWSE FIELDS MARK:H="",  ;
                  NAME:H="",  ;
                  COUNTRY:H="", ;
                  GTD_LINE:H="",;
                  QNT:H="":P="@Z 99999999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*
*    BROWSE - ����
*
    ON KEY
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_H")

RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
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
@ WROWS()-6,  2 CLEAR TO WROWS()-2, WCOLS()-3
@ WROWS()-6, 10 SAY "������������ " + NAME
@ WROWS()-4,  7 SAY "��࠭� " + COUNTRY
@ WROWS()-4, 32 SAY "��ப� N " + GTD_LINE
@ WROWS()-4, 47 SAY "���-�� " + STR(QNT,8)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� SeekGTD      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         ���⥪��� ���� ���                          �
*�                                                                        �
*����������������������������������������������������������� 12.03.2002 ���
PROCEDURE SeekGTD
PARAMETERS repeat

PRIVATE tmpShab, o_sav, r_sav

SELECT TIT_0302
r_sav = RECNO()

IF .NOT. repeat
  tmpShab = Get_Shab()
  IF EMPTY(tmpShab)
    RETURN
  ENDIF
  tmpShab = UPPER(tmpShab)
  SELECT 0
  DELETE FILE (tmpo_path+"SeekGTD.DBF")
  CREATE DBF (tmpo_path+"SeekGTD.DBF") ( LINK N(10) )
  SELECT TIT_0302
  SCAN FOR tmpShab $ UPPER(GTD_NUM)
    SELECT SeekGTD
    APPEND BLANK
    REPLACE LINK WITH RECNO("TIT_0302")
    SELECT TIT_0302
  ENDSCAN
  SELECT SeekGTD
  USE
  SELECT TIT_0302
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

IF .NOT. File_O(tmpo_path+"SeekGTD.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��祣� �� �������..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
o_sav = ORDER()
SET ORDER TO
SELECT 0
USE (tmpo_path+"SeekGTD")
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��祣� �� �������..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT TIT_0302
  SET ORDER TO TAG (o_sav)
  RETURN  
ENDIF

SET RELATION TO LINK INTO TIT_0302

tmpShab = Ch_Link()

USE
SELECT TIT_0302
SET ORDER TO TAG (o_sav)

IF BETWEEN(tmpShab, 1, RECCOUNT())
  GO tmpShab
ELSE
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

RETURN
*������������������������������������������������������������������������Ŀ
*�   ��楤�� Ch_Link      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          �롮� �� �⮡࠭����                          �
*�                                                                        �
*����������������������������������������������������������� 12.03.2002 ���
PROCEDURE Ch_Link

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

PRIVATE tmpRet
tmpRet = 0

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
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 39

*��� ��� ���� ����� �������������������Ŀ
*���.��.���� NNNNNNNNNNNNNNNNNNNNNNNNNN X�
*�����������������������������������������

DO D_Wins WITH ln, wd, "������� ���...", 0, 0
@ 1, 2 SAY "�� ��� ���� ����� "
what_do = "List"
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS TIT_0302.GTD_DATE:H="", TIT_0302.GTD_NUM:H="", TIT_0302.GTD_TYPE:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �롮�

    tmpRet = LINK
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN tmpRet

*������������������������������������������������������������������������Ŀ
*�   ��楤�� View_Cls     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                 ��ᬮ�� ᮤ�ন���� ��� � ��� ����ᮢ.              �
*�                                                                        �
*����������������������������������������������������������� 03/03/2000 ���
PROCEDURE View_Cls

DO Prep_Cls
DO Corr_Cls
SELECT GTD_TMP
USE
DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT TIT_0302

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Cls     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                  �����⮢�� �६������ 䠩�� ����ᮢ.                  �
*�                                                                        �
*����������������������������������������������������������� 03/03/2000 ���
PROCEDURE Prep_Cls

DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT 0
USE (path_comm+"GTD_C") ORDER TAG GTD_NUM ALIAS GTD_0303 AGAIN

COPY STRUCTURE FIELDS NAME, COUNTRY, GTD_LINE, QNT TO (tmpo_path+"GTD_TMP")
CREATE DBF (tmpo_path+"GTD_TMP") ;
  ( MARK C(1), CLASS C(12), COUNTRY C(16), GTD_LINE C( 4), QNT N( 8) )

USE (tmpo_path+"GTD_TMP") EXCLUSIVE

SELECT GTD_0303
SEEK TIT_0302.GTD_NUM
SCAN REST WHILE GTD_NUM = TIT_0302.GTD_NUM
  SELECT GTD_TMP
  APPEND BLANK
  REPLACE CLASS    WITH GTD_0303.CLASS,   ;
          COUNTRY  WITH GTD_0303.COUNTRY, ;
          GTD_LINE WITH GTD_0303.GTD_LINE,;
          QNT      WITH GTD_0303.QNT
  SELECT GTD_0303
ENDSCAN

USE
SELECT GTD_TMP
INDEX ON CLASS TAG NAME
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Corr_Cls     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�              ���४�� ����⮢�� � ��� ����ᮢ.                      �
*�                                                                        �
*����������������������������������������������������������� 03.03.2000 ���
PROCEDURE Corr_Cls


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
USE (base_path+"CLASSES") SHARED AGAIN ALIAS CCC_0303 ORDER TAG CODE
DO Use_Dummy
SELECT GTD_TMP
SET RELATION TO CLASS INTO CCC_0303

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE WINDOW (win_name+"_H") FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
ACTIVATE WINDOW (win_name+"_H") NOSHOW
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
*��č����������������������������đ�࠭�����������N ����Ċ��-��Ŀ
*�M ....:....!....:....!....:.... ....:....!....:. .... ....:...�
*����������������������������������������������������������������
*��������������������������������������������������������������Ŀ
*� ....:....!....:....!....:....!....:....!....:....!....:....! �
*�                                                              �
*� [ ] ��࠭� ....:....!....:.  ��ப� N 9999  ���-�� 99999999  �
*�                                                              �
*�              < OK Ctrl-W > < �⪠������ Esc >                �
*����������������������������������������������������������������
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 62

DO D_Wins WITH ln, wd, "��� N "+ALLTRIM(TIT_0302.GTD_NUM)+" �� "+DTOC(TIT_0302.GTD_DATE), 0, 5
@ 1, 2 SAY "�Ċ����������������������������đ�࠭�����������N ����Ċ��-��"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1

    BROWSE FIELDS MARK:H="",  ;
                  CCC_0303.NAME:29:H="",  ;
                  COUNTRY:H="", ;
                  GTD_LINE:H="",;
                  QNT:H="":P="@Z 99999999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Dr_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*
*    BROWSE - ����
*
    ON KEY
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_H")

RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT CCC_0303
USE
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Dr_Foot      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE Dr_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
@ WROWS()-6,  2 CLEAR TO WROWS()-2, WCOLS()-3
@ WROWS()-6,  3 SAY LEFT(CCC_0303.NAME, 60)
@ WROWS()-4,  7 SAY "��࠭� " + COUNTRY
@ WROWS()-4, 32 SAY "��ப� N " + GTD_LINE
@ WROWS()-4, 47 SAY "���-�� " + STR(QNT,8)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.