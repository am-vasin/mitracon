*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Customer     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                             �롮� ������.                             �
*�                                                                        �
*����������������������������������������������������������� 04.05.2001 �ͼ
PROCEDURE Customer
PARAMETERS parmCode, parmName, parmFlags, A000, B000, C000
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
PRIVATE tmpFlags
IF TYPE("parmFlags") = "C"
  tmpFlags = parmFlags
ELSE
  tmpFlags = ""
ENDIF

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
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS TMP_1504 AGAIN
IF parmCode > 0 .AND. SEEK(parmCode)
  SET ORDER TO TAG CUS_NAME
ELSE
  SET ORDER TO TAG CUS_NAME
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
  
IF "I" $ UPPER(tmpFlags)

  DEFINE PAD Info OF (menu_name) PROMPT "���ଠ��"
  ON PAD Info OF (menu_name) ACTIVATE POPUP Info

    DEFINE POPUP Info MARGIN RELATIVE SHADOW COLOR SCHEME 4

    DEFINE BAR  1 OF Info PROMPT "�����" KEY Ctrl-A, "F1"
    ON SELECTION BAR 1 OF Info DO Sw_Mode WITH "History"

    DEFINE BAR  2 OF Info PROMPT "�������" KEY Ctrl-A, "F2"
    ON SELECTION BAR 2 OF Info DO Sw_Mode WITH "Deposit"

    DEFINE BAR  3 OF Info PROMPT "��થ⨭����� ���ଠ��" KEY Ctrl-A, "Shift-F1"
    ON SELECTION BAR 3 OF Info DO Sw_Mode WITH "Market"

    DEFINE BAR  4 OF Info PROMPT "��ਡ��� ��� ���-䠪����" KEY Ctrl-A, "Shift-F2"
    ON SELECTION BAR 4 OF Info DO Sw_Mode WITH "Cus_Bil"

    DEFINE BAR  5 OF Info PROMPT "����" KEY Ctrl-A, "Ctrl-F1"
    ON SELECTION BAR 5 OF Info DO Sw_Mode WITH "Dossier"

    DEFINE BAR  6 OF Info PROMPT "��ᯮ��� �����" KEY Ctrl-A, "Ctrl-F2"
    ON SELECTION BAR 6 OF Info DO Sw_Mode WITH "Pasport"

ENDIF

IF "S" $ UPPER(tmpFlags)
  DEFINE PAD Add OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Ins"
  ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"
ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("CUS_NAME")

DO D_Wins WITH ln, wd, IIF("S" $ UPPER(tmpFlags),parmName,"�������"), 1, 0
@ 1, 2 SAY "���� "
@ 1, 8 SAY "Tab" COLOR (SCHEME(14,2))
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7       DO Sw_Mode WITH "Seek_1"
    ON KEY LABEL Ctrl+F7  DO Sw_Mode WITH "Again"
    IF "I" $ UPPER(tmpFlags)
      ON KEY LABEL Shift+F1 DO Sw_Mode WITH "Market"
      ON KEY LABEL F2       DO Sw_Mode WITH "Deposit"
      ON KEY LABEL Shift+F2 DO Sw_Mode WITH "Cus_Bil"
      ON KEY LABEL Ctrl+F2  DO Sw_Mode WITH "Pasport"
      ON KEY LABEL F1       DO Sw_Mode WITH "History"
      ON KEY LABEL Ctrl+F1  DO Sw_Mode WITH "Dossier"
    ENDIF
    IF "S" $ UPPER(tmpFlags)
      ON KEY LABEL Ins DO Sw_Mode WITH "Add"
    ENDIF    
    ON KEY LABEL Tab      DO Locator WITH 1, 8, 30
*
*    BROWSE - ����
*
    BROWSE FIELDS CUS_NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
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

  CASE what_do = "Choose"    && �롮�

    parmCode = CUS_CODE
    IF TYPE("parmName") = "C"
      parmName = CUS_NAME
    ENDIF
    EXIT

  CASE what_do = "Seek_1"    && ���⥪��� ����

    IF Seek_1()
      what_do = "Again"
    ELSE
      what_do = "List"
    ENDIF

  CASE what_do = "Again"      && �����

    = Ch_Found()
    what_do = "List"

  CASE what_do = "Market"     && ��થ⨭����� ���ଠ��
   
  IF "I" $ tmpFlags
    DO Cli_Info WITH "", "I" $ tmpFlags, CUS_CODE
    what_do = "List"
  ENDIF

  CASE what_do = "History"    && �����

    DO Acc_Hist WITH CUS_CODE
    what_do = "List"

  CASE what_do = "Deposit"    && �������

    DO V_Note WITH "I" $ tmpFlags, CUS_CODE
    what_do = "List"

  CASE what_do = "Dossier"     && ����

    DO Dossier WITH CUS_CODE, IIF("I" $ tmpFlags, "C", "")
    what_do = "List"

  CASE what_do = "Cus_Bil"     && ��ਡ��� ��� �-�

    DO Cus_Bil WITH CUS_CODE, CUS_NAME, "i" $ tmpFlags
    what_do = "List"

  CASE what_do = "Pasport"     && ��ᯮ��� �����

    DO Pasport WITH CUS_CODE, .F., .F., .F., .F., .F., .F., .F., ;
                    IIF("i" $ tmpFlags, "", "C")
    what_do = "List"

  CASE what_do = "Add"         && ���� ������
  
    DO Add_Cus WITH parmName, parmCode
    SET ORDER TO TAG CUS_CODE
    IF SEEK(parmCode)
      = RECNO()
    ENDIF
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
SELECT TMP_1504
USE

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

*ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*

*ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_1       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ���⥪��� ����                            �
*�                                                                        �
*����������������������������������������������������������� 04.05.2001 ���
PROCEDURE Seek_1

PRIVATE v_shab, r_sav, s_sav

v_shab = Get_Shab()
v_shab = SYS(15, lwr, v_shab)

IF EMPTY(v_shab)
  RETURN .F.
ENDIF

r_sav = RECNO()
s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"LINK.DBF")
CREATE DBF (tmpo_path+"LINK") ( LNK N(10) )
USE (tmpo_path+"LINK")
SELECT TMP_1504
DO Wt_Mess WITH "�饬..."
SCAN FOR v_shab $ SYS(15, lwr, CUS_NAME)
  SELECT LINK
  APPEND BLANK
  REPLACE LNK WITH RECNO("TMP_1504")
  SELECT TMP_1504
ENDSCAN

SELECT LINK
USE

SELECT (s_sav)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

DO Wt_Mess

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Ch_Found     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          �롮� �� ����������.                          �
*�                                                                        �
*����������������������������������������������������������� 04.05.2001 ���
PROCEDURE Ch_Found

PRIVATE mss, s_sav, r_sav, ret_v

ret_v = .F.

IF .NOT. File_O(tmpo_path+"LINK.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��祣� �� �������..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

r_sav = RECNO()
s_sav = SELECT()

SELECT 0
USE (tmpo_path+"LINK")

IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��祣� �� �������..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN .F.
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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

SELECT 0
DO Use_Dummy

SELECT TMP_1504
SET ORDER TO

SELECT LINK
SET RELATION TO LNK INTO TMP_1504
GO TOP
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
ln = MIN(ln, RECCOUNT())
wd = FSIZE("CUS_NAME", "TMP_1504")

DO D_Wins WITH ln, wd, "������� ���⥪�⭮�� ���᪠", 0, 0
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
    BROWSE FIELDS TMP_1504.CUS_NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �롮�

    ret_v = .T.
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
SELECT LINK
SET RELATION TO
USE

SELECT TMP_1504
SET ORDER TO TAG CUS_NAME
IF .NOT. ret_v
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

SELECT (s_sav)

RETURN ret_v

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
    IF .NOT. SEEK(SYS(15, lwr, prf_flt))
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

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                        �ணࠬ�� ᮧ����� ������.                    �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Add_Cus
PARAMETER cc_name, cc_code
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
PRIVATE c_sav
*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()
c_sav = SELECT()
*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

PRIVATE ex, nm_w, sw_i, sw_a, cd, rc_sav, typ_cli, tmpINN, tmpKPP
                     &&   ���塞 � ������塞 ���� ������
ex   = 1             &&
m.tmpINN = ""
m.tmpKPP = ""
DO Get_INN WITH 0, m.tmpINN, m.tmpKPP

IF EMPTY(cc_name)
  nm_w = SPACE(50)
ELSE
  nm_w = cc_name
ENDIF  
sw_i = .F.
sw_a = .F.
SELECT 0
USE (A000) ALIAS ACC_C AGAIN
IF EMPTY(MARK1)
  typ_cli = "0"
ELSE
  typ_cli = MARK1
ENDIF
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE SHARED ALIAS ACC_2832 AGAIN
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
*
*��������������������������������������������������������Ŀ
*� �� ��� ������ �������������������������������������Ŀ �
*� � ....:....!....:....!....:....!....:....!....:....! � �
*� ������������������������������������������������������ �
*�        ��� ....:....!..    ��� ....:....!....:.        �
*�      [ ] ����窠 ������    [ ] ��ਡ��� ������      �
*�                                                        �
*�            < OK Ctrl-W > < �⪠������ Esc >            �
*�                                                        �
*����������������������������������������������������������

DO Prp_Nav_2
DO D_Win_N WITH 11, 60, "������"
DO Sun_Bord WITH 2, 3, 4, WCOLS()-4
@ 2, 5 SAY " ��� ������ "

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3,  5 GET nm_w
@ 5, 10 SAY "���" GET m.tmpINN
@ 5, 30 SAY "���" GET m.tmpKPP
@ 6,  8 GET sw_i PICTURE "@*C ����窠 ������"
@ 6, 32 GET sw_a PICTURE "@*C ��ਡ��� ������"
@ 8, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE VALID Tst_Name()

IF ex = 1
  IF sys_char = "A" .OR. sale_mode = "Mitin"   && �᭮���� ���頤��: ������塞 ������

    cd = Doc_Num("CUS_CODE  ", "", arm, DATE())
    SELECT ACC_2832
    APPEND BLANK
    REPLACE SUBSYS   WITH sys_char,;
            CUS_CODE WITH cd,      ;
            CUS_NAME WITH nm_w,    ;
            DATE_ON  WITH DATE(),  ;
            WHO      WITH user,    ;
            WHERE    WITH arm,     ;
            MARK1    WITH typ_cli
    rc_sav = RECNO()
    DO Put_INN WITH CUS_CODE, m.tmpINN, m.tmpKPP

  ELSE     && �� �᭮���� ���頤��: �ᯮ��㥬 ����⮢��

    rc_sav = RECNO()
    SET ORDER TO TAG EMP_NAME
    GO TOP
    IF EOF() .OR. BOF()
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"�� ���௠�� �� ����⮢��!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      sw_i = .F.
      sw_a = .F.
    ELSE
      REPLACE SUBSYS   WITH sys_char, ;
              CUS_NAME WITH nm_w,     ;
              DATE_ON  WITH DATE(),   ;
              WHO      WITH user,     ;
              WHERE    WITH arm,      ;
              MARK1    WITH typ_cli
      rc_sav = RECNO()
    ENDIF
    SET ORDER TO TAG CUS_NAME
    IF rc_sav > 0 .AND. rc_sav <= RECCOUNT()
      GO rc_sav
    ENDIF
  ENDIF
  cc_code = CUS_CODE
  IF sw_i
    DO Cli_Inf WITH "", CUS_CODE
  ENDIF
  IF sw_a
    DO Cus_Bilm WITH ACC_2832.CUS_CODE, ACC_2832.CUS_NAME
  ENDIF
ENDIF
*--------------------------------------------------------------------------
SELECT ACC_2832
USE
SELECT ACC_C
USE
POP KEY
RELEASE WINDOW (win_name)
SELECT(c_sav)

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                    �஢�ઠ ����� ������.                            �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Tst_Name

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���⮥ ��� ������? ��� �� ������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN Tst_INN(0, m.tmpINN)

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                  �ணࠬ�� ᮧ����� ��ਡ�⮢ ������.                �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Cus_Bilm
PARAMETERS cd, nm
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
*��������������������������������������������������������������������������Ŀ
*�      ���㯠⥫� ....:....!....:....!....:....!....:....!....:....!       �
*�                                                                          �
*�    ���� ....:....!....:....!....:....!....:....!....:....!....:....!    �
*�                                                                          �
*����. ....:....!....:. �/� ....:....!....:....! �/� N ....:....!....:....! �
*�                                                                          �
*�             � ����� ....:....!....:....!....:....!....:....!             �
*�                                                                          �
*�          ��� ....:....!....:....!        ��த ....:....!....:.          �
*�                                                                          �
*�       ��� ....:....!..   ��� �� ����� ....:   ��� �� ���� ....:...       �
*��� �ਬ�砭�� ����������������������������������������������������������Ŀ�
*��....:....!....:....!....:....!....:....!....:....!....:....!....:....!..��
*��                                                                        ��
*��                   < OK Ctrl-W > < �⪠������ Esc >                     ��
*��������������������������������������������������������������������������ٳ
*� �� ᮧ���                                 ��� ᮧ�����                 �
*� �� ���४�஢��                          ��� ���४樨                �
*����������������������������������������������������������������������������
PRIVATE f01, f02, f03, f04, f05, f06, f07, f08, f09, f10, f11, f12, ex
PRIVATE sel, us, new
PRIVATE c01, c02, c03, c04, c11, c12

sel = SELECT()
**************************
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_2833 AGAIN

c01 = ""
c02 = ""
c03 = {}
c04 = {}
c11 = " "
c12 = " "

SELECT 0
USE (B000) ALIAS BILL_C AGAIN
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2833 AGAIN
SET RELATION TO WHO_CORR INTO P_2833
SET RELATION TO WHO INTO P_2833

IF SEEK (cd)
  c01 = ACC_2833.WHO
  c02 = ACC_2833.WHO_CORR
  c03 = DATE_ON
  c04 = DATE_CORR
ENDIF
USE

SELECT P_2833
IF SEEK (c01)
  c11 = ALLTRIM(P_2833.FAMILY)+" "+LEFT(P_2833.NAME,1)+". "+ ;
              LEFT(P_2833.S_NAME,1)+"."
ENDIF
IF SEEK (c02)              
  c12 = ALLTRIM(P_2833.FAMILY)+" "+LEFT(P_2833.NAME,1)+". "+ ;
              LEFT(P_2833.S_NAME,1)+"."
ENDIF              
USE

********************************
IF USED("CUS_BIL")
  us = .T.
  SELECT CUS_BIL
ELSE
  us = .F.
  SELECT 0
  USE CUS_BIL ORDER TAG CUS_CODE
ENDIF

SELECT BILL_C
IF EMPTY(ADDRESS)
 f02 = SPACE(FSIZE("ADDRESS"))
ELSE
  f02 = ADDRESS
ENDIF

IF EMPTY(INN)
  f09 = SPACE(FSIZE("INN"))
ELSE
  f09 = INN
ENDIF
ex   = 1
IF EMPTY(nm)
  f01 = PADR(nm, FSIZE("CUS_NAME"))
ELSE
  f01 = nm  
ENDIF
f03 = SPACE(FSIZE("PHONE"))
f04 = SPACE(FSIZE("ACC_BILL"))
f05 = SPACE(FSIZE("BANK"))
f06 = SPACE(FSIZE("CORR_BILL"))
f07 = SPACE(FSIZE("BIK"))
f08 = SPACE(FSIZE("CITY"))
f10 = SPACE(FSIZE("OKOHX"))
f11 = SPACE(FSIZE("OKPO"))
f12 = SPACE(FSIZE("NOTE"))
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 22, 78, "��ਡ��� ������"

*------------------------------------------------------------------------
*      ���� ����� ������
*

@  2,  8 SAY "���㯠⥫�" GET f01
@  4,  6 SAY "����" GET f02
@  6,  2 SAY "���." GET f03
@  6, 24 SAY "�/�" GET f06
@  6, 49 SAY "�/�" GET f04
@  8, 15 SAY "� �����" GET f05
@ 10, 12 SAY "���" GET f07
@ 10, 44 SAY "��த" GET f08
@ 12,  9 SAY "���" GET f09
@ 12, 28 SAY "��� �� �����" GET f10
@ 12, 49 SAY "��� �� ����" GET f11
DO Sun_Bord WITH 13, 2, 15, 75
@ 13, 4 SAY " �ਬ�砭�� "
@ 14,  3 GET f12
DO Sun_Bord WITH 16, 2, 19, 75
@ WROWS()-5,  3 SAY "�� ᮧ���         "+c11
@ WROWS()-4,  3 SAY "�� ���४�஢��  "+c12
@ WROWS()-5, 47 SAY "��� ᮧ�����   "+DTOC(c03)
@ WROWS()-4, 47 SAY "��� ���४樨  "+DTOC(c04)
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
READ CYCLE

IF ex = 1
  APPEND BLANK
  REPLACE CUS_CODE  WITH cd,  ;
          CUS_NAME  WITH f01, ;
          ADDRESS   WITH f02, ;
          PHONE     WITH f03, ;
          ACC_BILL  WITH f04, ;
          BANK      WITH f05, ;
          CORR_BILL WITH f06, ;
          BIK       WITH f07, ;
          CITY      WITH f08, ;
          INN       WITH f09, ;
          OKOHX     WITH f10, ;
          OKPO      WITH f11, ;
          NOTE      WITH f12, ;
          DATE_CORR  WITH DATE(),  ;
          WHO_CORR   WITH user,    ;
          WHERE_CORR WITH arm
ENDIF
*------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF .NOT. us
  USE
ENDIF
IF USED("CUS_BIL")
  SELECT CUS_BIL
  APPEND FROM (B000)
  USE
ENDIF
SELECT (sel)

RETURN ex = 1
*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                    �ணࠬ�� ᮧ����� ���ଠ樨 � ������.           �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Cli_Inf
PARAMETERS ext_menu, parmCode

PRIVATE ln, wd, sel_sav, mss, ex, crsr, manager_n, k_sw, k_code, k_name
PRIVATE tmpName

sel_sav = SELECT()
manager_n = ""

IF EMPTY(parmCode)
  RETURN
  SELECT (sel_sav)
ENDIF
SELECT 0

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2901 AGAIN
IF .NOT. SEEK(parmCode)
  USE
  SELECT (sel_sav)
  RETURN
ENDIF

tmpName = CUS_NAME
IF .NOT. EMPTY(ACC_2901.WHO_NEW)
  SELECT 0
  USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PRS_2901
  SEEK ACC_2901.WHO_NEW
  manager_n = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
  USE
ENDIF
manager_n = PADR(manager_n, 28)
IF TYPE("ext_menu") = "C"
  IF .NOT. EMPTY(ext_menu)
    DEACTIVATE MENU (ext_menu)
  ENDIF
ENDIF

DEFINE WINDOW H_Wind FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW H_Wind
SELECT 0
USE (C000) ALIAS INF_C AGAIN
SELECT 0
USE (base_path+"CLI_KIND") ORDER TAG CODE ALIAS KIND_2901 AGAIN
SELECT 0
USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE ALIAS INFO_2901 AGAIN
k_sw   = .F.
k_code = SPACE(FSIZE("KIND_CODE", "KIND_2901"))
k_name = SPACE(FSIZE("KIND_NAME", "KIND_2901"))
 
PUSH KEY CLEAR
SCATTER MEMVAR BLANK

SELECT INF_C
m.address = ADDRESS
m.fax = FAX
ln = 24
wd = 78

DO D_W WITH "Cli_Info", ln, wd, "���㯠⥫�: "+ALLTRIM(tmpName), 0
@ 0, 0 SAY ""
*       ���㯠⥫�: < ��� ������ >
? "���������������������������������������������������������������������������Ŀ"
? "�    ����� ��࠭� ���������� ������ ������������������ ��த ��������Ŀ     �"
? "�    �  ....:....1....:.  ....:....1....:....1....  ....:....1....:.  �     �"
? "�    ������������������������������������������������������������������     �"
? "���� ���� ���������������������������������������������������������������Ŀ�"
? "��....:.  ....:....1....:....1....:....1....:....1....:....1....:....1.... ��"
? "��              ����䮭 ....:....1....       ���� ....:....1               ��"
? "��               ��. ���� ....:....1....:....1....:....1..                ��"
? "���������������������������������������������������������������������������ٳ"
? "� �.�.�. ....:....1....:....1....:....1..   ��������� ....:....1....:....1  �"
? "���[ ] ��� ����. ���Ŀ��� ���樠������ ��������������������������������Ŀ�"
? "��                     ��[ ] �㤨� & ���⨬����  [ ] ���. ����㤮�����    ��"
? "��                     ��[ ] ����� & ���. �娪�  [ ] ���-�孨��         ��"
? "��                     ��[ ] �।�⢠ �裡       [ ] ��������㭨����     ��"
? "��                     ��[ ] ���. �孨��         [ ] �����.ࠤ�����.� TV ��"
? "��                     ��[ ] ���筨�� ��⠭��    [ ] �������.����㤮����� ��"
? "��                     ��[ ] �������ୠ� �孨��     ....:....1....:.     ��"
? "��                     ��[ ] ��࠭�� ��⥬�                              ��"
? "���������������������������������������������������������������������������ٳ"
? "��ਬ�砭�� ....:....1....:....1....:....1....:....1....:....1....:....1....�"
? "�                   �������� ....:....!....:....!....:...                   �"
? "�                     < OK Ctrl-W > < �⪠������ Esc >                      �"
? "�����������������������������������������������������������������������������"

ex = 1

@  3,  8 GET m.country
@  3, 26 GET m.region
@  3, 52 GET m.city

@  6,  2 GET m.index
@  6, 10 GET m.address PICTURE "@S64"
@  7, 24 GET m.phone
@  7, 50 GET m.fax
@  8, 27 GET m.eml

@ 10,  9 GET m.contact
@ 10, 54 GET m.job

@ 11, 3  SAY " ��� ����. "
@ 12, 3 GET m.production PICTURE "@*C �ந�����⢮"
@ 13, 3 GET m.work       PICTURE "@*C ���ࠡ�⪠"
@ 14, 3 GET m.sale       PICTURE "@*C �த���"
@ 15, 3 GET m.repair     PICTURE "@*C ������"
@ 16, 7 GET m.f_other

@ 12, 25 GET m.audio      PICTURE "@*C �㤨� & ���⨬����"
@ 13, 25 GET m.video      PICTURE "@*C ����� & ��⮢�� ��."
@ 14, 25 GET m.connect    PICTURE "@*C �।�⢠ �裡"
@ 15, 25 GET m.meastech   PICTURE "@*C ���. �孨��"
@ 16, 25 GET m.power      PICTURE "@*C ���筨�� ��⠭��"
@ 17, 25 GET m.computer   PICTURE "@*C �������ୠ� �孨��"
@ 18, 25 GET m.quardsys   PICTURE "@*C ��࠭�� ��⥬�"

@ 12, 50 GET m.medequip   PICTURE "@*C ���. ����㤮�����"
@ 13, 50 GET m.auto       PICTURE "@*C ���-�孨��"
@ 14, 50 GET m.telecom    PICTURE "@*C ��������㭨����"
@ 15, 50 GET m.radio      PICTURE "@*C �����.ࠤ�����.� TV"
@ 16, 50 GET m.navig      PICTURE "@*C �������.����㤮�����"
@ 17, 54 GET m.p_other

@ 20, 12 GET m.note
@ 21, 20 SAY "�������� " GET manager_n WHEN .F.

crsr = SET("CURSOR") = "OFF"
SET CURSOR ON
@ 22, 23 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
READ CYCLE VALID Tst_Blank()
IF ex = 1
  m.who = user
  m.where = arm
  m.cus_code = parmCode
  m.name     = tmpName
  APPEND BLANK
  m.kind_code = k_code
  GATHER MEMVAR
  REPLACE CUS_CODE WITH m.parmCode
ENDIF
IF crsr
  SET CURSOR OFF
ENDIF

USE
SELECT KIND_2901
USE
SELECT ACC_2901
USE
SELECT INFO_2901
APPEND FROM (C000)
USE
RELEASE WINDOW H_Wind
RELEASE WINDOW Cli_Info
SELECT (sel_sav)
POP KEY

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Ch_Kind      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          �롮� ⨯� ������.                           �
*�                                                                        �
*����������������������������������������������������������� 20.03.2001 ���
PROCEDURE Ch_Kind

IF .NOT. k_sw
  RETURN .T.
ENDIF

k_sw = .F.
SHOW GET k_sw

DO Cli_Kind WITH k_code, k_name

SHOW GET k_name
RETURN .T.
*������������������������������������������������������������������������Ŀ
*�                                                                        �
*�                            ��室 �� ������.                            �
*�                                                                        �
*����������������������������������������������������������� 25.03.2003 ���
PROCEDURE Tst_Blank
PRIVATE mss

IF EMPTY(m.country) .AND. ex = 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�������� ��࠭� ������ ���� ���������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.