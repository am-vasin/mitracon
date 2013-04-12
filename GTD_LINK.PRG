*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� GTD_Link     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                   ���������� ��� ��� ᯨ᪠ ����権.                   �
*�                                                                        �
*����������������������������������������������������������� 26.02.2000 �ͼ
PROCEDURE GTD_Link
PARAMETERS view

PRIVATE s_sav, cnt_hole, cnt_cur, tmpName, p, nameWd, cnt_found, r_sav, flt_val, swFull
PRIVATE period
PRIVATE p_num, p_country, p_cntC, p_line, p_badG, p_GTDName
period = 120       &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
swFull = .F.
p_GTDName = ""
flt_val = SPACE(25)
s_sav = SELECT()

SELECT GTD_LINK
cnt_cur = 0
cnt_found = 0
cnt_hole = RECCOUNT()
DO Wt_Mess WITH "   ������ ���...   "

SCAN
  p_num     = ""
  p_country = ""
  p_line    = ""
  p_badG    = ""
  p_cntC    = ""
  DO Set_Gtd WITH GTD_LINK.CODE, GTD_LINK.PREFIX, GTD_LINK.NAME, GTD_LINK.PRODUCER, ;
             p_num, p_country, p_cntC, p_line, p_badG, p_GTDName
  IF .NOT. EMPTY(p_num)
    cnt_found = cnt_found+1
    REPLACE GTD_NAME WITH IIF(p_badG, "��. �����", p_GTDName), ;
            GTD_NUM  WITH p_num,      ;
            GTD_LINE WITH p_line,     ;
            COUNTRY  WITH p_country,  ;
            COUNTRYC WITH p_cntC,     ;
            B_GTD    WITH p_badG
  ENDIF
  SELECT GTD_LINK
  cnt_cur = cnt_cur+1
  IF cnt_cur % 10 = 0
    p = ALLTRIM(STR(cnt_cur))+"/"+ALLTRIM(STR(cnt_hole))
    @ WROWS()-1, 0 SAY PADC(p, WCOLS())
  ENDIF
ENDSCAN

DO Wt_Mess

IF view
  PRIVATE mss0
  DIMENSION mss0(3)
  mss0(1) = ""
  mss0(2) = CHR(0)+"��ࠡ�⠭� "+ALLTRIM(STR(cnt_hole))+" ����権, ������� ��� ��� "+ALLTRIM(STR(cnt_found))+"."
  mss0(3) = ""
  DO Out_Mess WITH 5, "mss0"
  DO V_GTD
ENDIF

SELECT (s_sav)

RETURN cnt_found

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_GTD        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ��ᬮ�� १���� �ਢ離� ���.                    �
*�                                                                        �
*����������������������������������������������������������� 26.02.2000 ���
PROCEDURE V_GTD

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
SELECT 0
DO Use_Dummy
SELECT 0
DO Use_Link
ZAP
SELECT GTD_LINK

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "�⬥⪨"
ON PAD Mark OF (menu_name) ACTIVATE POPUP Mark

  DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Mark PROMPT "������஢���" KEY Ctrl-A, "Ins"
  ON SELECTION BAR 1 OF Mark DO Sw_Mode WITH "MarkOne"

  DEFINE BAR  2 OF Mark PROMPT "������஢��� ��" KEY Ctrl-A, "*"
  ON SELECTION BAR 2 OF Mark DO Sw_Mode WITH "MarkAll"

  DEFINE BAR  3 OF Mark PROMPT "��⠭����� ���⮪" KEY Ctrl-A, "+"
  ON SELECTION BAR 3 OF Mark DO Sw_Mode WITH "Set"

  DEFINE BAR  4 OF Mark PROMPT "������ ���⮪" KEY Ctrl-A, "-"
  ON SELECTION BAR 4 OF Mark DO Sw_Mode WITH "Clear"

DEFINE PAD GTD OF (menu_name) PROMPT "���" KEY Ctrl-A, "Enter"
ON SELECTION PAD GTD OF (menu_name) DO Sw_Mode WITH "GTD"

DEFINE PAD Filter OF (menu_name) PROMPT "������" KEY Ctrl-A, "F7"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Copy OF (menu_name) PROMPT "����஢���"
ON PAD Copy OF (menu_name) ACTIVATE POPUP Copy

  DEFINE POPUP Copy MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Copy PROMPT "�⠫��" KEY Ctrl-A, "Ctrl-C"
  ON SELECTION BAR 1 OF Copy DO Sw_Mode WITH "Copy"

  DEFINE BAR  2 OF Copy PROMPT "��⠢���" KEY Ctrl-A, "Ctrl-V"
  ON SELECTION BAR 2 OF Copy DO Sw_Mode WITH "PasteOne"

  DEFINE BAR  3 OF Copy PROMPT "��⠢��� �� ⥪�饩 ��ப�"
  ON SELECTION BAR 3 OF Copy DO Sw_Mode WITH "PasteNext"

  DEFINE BAR  4 OF Copy PROMPT "��⠢��� � �⬥祭��"
  ON SELECTION BAR 4 OF Copy DO Sw_Mode WITH "PasteMark"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE g_sav, c_sav, cc_sav, l_sav
STORE 0 TO g_sav, c_sav, cc_sav, l_sav
PRIVATE tmpGTD, point1, point2, point3
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 51

DO D_Wins WITH ln, wd, "��� ��� ᯨ᪠ ����権", 1, 2
what_do = "List"
@ WROWS()-4, 2 SAY "�� ����� ��� �������������� ��࠭� "
@ WROWS()-2, 3 SAY "��� ���"
@ 1, 2 SAY "������:"
*���������������������������������������Ŀ
*�XXXX XXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXX�
*�����������������������������������������
*��� ����� ��� �������������� ��࠭� ���Ŀ
*�XXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXX�
*� ��� ��� XXXXXXXXXXXXXXXXXXXXXXXXXXXXX �
*�����������������������������������������

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Home   GO TOP
    ON KEY LABEL End    GO BOTTOM
    ON KEY LABEL Enter  KEYBOARD CHR(23)
    ON KEY LABEL Ins    DO Sw_Mode WITH "MarkOne"
    ON KEY LABEL "*"    DO Sw_Mode WITH "MarkAll"
    ON KEY LABEL "+"    DO Sw_Mode WITH "Set"
    ON KEY LABEL "-"    DO Sw_Mode WITH "Clear"
    ON KEY LABEL F7     DO Sw_Mode WITH "Filter"
    ON KEY LABEL Ctrl+C DO Get_Copy
    ON KEY LABEL Ctrl+V DO P_One

*
*    BROWSE - ����
*
    BROWSE FIELDS M000 = IIF(GTD_LINK.MARK, "�", " "):1:H="", ;
                  GTD_LINK.PREFIX:4:H="", ;
                  GTD_LINK.NAME:35:H="",  ;
                  GTD_LINK.PRODUCER:8:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "GTD")
    ENDIF

  CASE what_do = "MarkOne"    && ������஢���

    IF .NOT. BOF() .AND. .NOT. EOF()
      REPLACE GTD_LINK.MARK WITH .NOT. GTD_LINK.MARK
      SKIP
      IF EOF()
        GO BOTTOM
      ENDIF
    ENDIF
    what_do = "List"
      
  CASE what_do = "MarkAll" && ������஢��� ��

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_sav = RECNO()
      REPLACE ALL GTD_LINK.MARK WITH .NOT. GTD_LINK.MARK
      GO r_sav
    ENDIF
    what_do = "List"
      
  CASE what_do = "Set"     && ��⠭����� ���⮪

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_sav = RECNO()
      REPLACE REST GTD_LINK.MARK WITH .T.
      GO r_sav
    ENDIF
    what_do = "List"
      
  CASE what_do = "Clear"   && ������ ���⮪

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_sav = RECNO()
      REPLACE REST GTD_LINK.MARK WITH .F.
      GO r_sav
    ENDIF
    what_do = "List"
      
  CASE what_do = "GTD"        && ���᮪ ���

    IF .NOT. BOF() .AND. .NOT. EOF()
      tmpGTD = Get_GTD(GTD_LINK.PREFIX, GTD_LINK.NAME, GTD_LINK.PRODUCER)
      point1 = AT(CHR(13), tmpGTD)
      point2 = AT(CHR(13), tmpGTD, 2)
      point3 = AT(CHR(13), tmpGTD, 3)
      IF point1 # 0
        REPLACE GTD_LINK.GTD_NUM  WITH LEFT(tmpGTD,point1-1), ;
                GTD_LINK.COUNTRY  WITH SUBSTR(tmpGTD,point1+1, point2-point1-1), ;
                GTD_LINK.GTD_LINE WITH SUBSTR(tmpGTD,point2+1, point3-point2-1), ;
                GTD_LINK.COUNTRYC WITH SUBSTR(tmpGTD,point3+1)
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Filter"        && ������

    DO Set_Fltr
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 10 SAY flt_val
    what_do = "List"

  CASE what_do = "Copy"        && ����஢���

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Get_Copy
    ENDIF
    what_do = "List"

  CASE what_do = "PasteOne"    && ��⠢��� � ⥪���� ��ப�

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_One
    ENDIF
    what_do = "List"

  CASE what_do = "PasteNext"    && ��⠢��� �� ⥪�饩 ��ப�

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Next
    ENDIF
    what_do = "List"

  CASE what_do = "PasteMark"    && ��⠢��� � �⬥祭��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Mark
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

SELECT LINK
ZAP
SET RELATION TO
USE
SELECT GTD_LINK

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

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
@ WROWS()-3, 2 SAY GTD_LINK.GTD_NUM COLOR SCHEME 13
@ WROWS()-3, COL()+1 SAY LEFT(GTD_LINK.COUNTRY,12) COLOR SCHEME 13
@ WROWS()-2, 11 SAY GTD_LINK.GTD_NAME COLOR SCHEME 13

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Set_Fltr     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ��⠭����/���⪠ 䨫���.                       �
*�                                                                        �
*����������������������������������������������������������� 05.04.2000 ���
PROCEDURE Set_Fltr
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

*���������������������������������������������Ŀ
*�      �� ������ ���������������������Ŀ      �
*�      �   ....:....!....:....!....:   �      �
*�      ���������������������������������      �
*�              [ ] ���쪮 � ���               �
*�< OK Ctrl-W > < �⪠������ Esc > < ������ >�
*�����������������������������������������������

PRIVATE ex, tmpVal, tmpSw
ex = 1
tmpVal = flt_val
tmpSw = swFull
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 49, "������"
DO Sun_Bord WITH  2,  8,  4, 40, " ������ "

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 12 GET tmpVal
@ 5, 16 GET tmpSw PICTURE "@*C ���쪮 � ���"
IF EMPTY(flt_val)
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
ELSE
  @ WROWS()-2, 2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc ;\ ������ "
ENDIF

READ CYCLE

IF ex # 2 .AND. EMPTY(tmpVal) .AND. .NOT. tmpSw
  ex = 3
ENDIF

DO CASE
CASE ex = 1
  *
  * ��ࠡ��뢠�� �����
  tmpVal = ALLTRIM(tmpVal)
  swFull = tmpSw
  SELECT LINK
  SET RELATION TO
  ZAP
  SELECT GTD_LINK
  DO Wt_Mess WITH "����..."
  SCAN
    IF .NOT. EMPTY(tmpVal)
      IF .NOT. tmpVal $ ALLTRIM(GTD_LINK.PREFIX)+ALLTRIM(GTD_LINK.NAME)+ALLTRIM(GTD_LINK.PRODUCER)
        LOOP
      ENDIF
    ENDIF
    IF swFull
      IF EMPTY(GTD_LINK.GTD_NUM)
        LOOP
      ENDIF
    ENDIF
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("GTD_LINK")
    SELECT GTD_LINK
  ENDSCAN
  DO Wt_Mess
  IF RECCOUNT("LINK") = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��祣� �� 㤠���� ����. ��頥� 䨫���!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT GTD_LINK
    flt_val = SPACE(LEN(flt_val))
  ELSE
    SELECT LINK
    SET RELATION TO LNK INTO GTD_LINK
    flt_val = PADR(tmpVal, LEN(flt_val))
  ENDIF
CASE ex = 3
  *
  * ��ࠡ��뢠�� �����
  swFull = tmpSw
  SELECT LINK
  ZAP
  SET RELATION TO
  SELECT GTD_LINK
  flt_val = SPACE(LEN(flt_val))
  swFull = .F.
ENDCASE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Get_Copy     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                               ����஢���                               �
*�                                                                        �
*����������������������������������������������������������� 05.04.2000 ���
PROCEDURE Get_Copy

IF .NOT. BOF() .AND. .NOT. EOF()
  g_sav = GTD_LINK.GTD_NUM
  c_sav = GTD_LINK.COUNTRY
  cc_sav = GTD_LINK.COUNTRYC
  l_sav = GTD_LINK.GTD_LINE
ENDIF

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� P_One        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                 ��⠢��� �� ��ଠ�� � ⥪���� ��ப�.                  �
*�                                                                        �
*����������������������������������������������������������� 05.04.2000 ���
PROCEDURE P_One

IF .NOT. BOF() .AND. .NOT. EOF()
  IF TYPE("g_sav") = "C"
    REPLACE GTD_LINK.GTD_NUM  WITH g_sav, ;
            GTD_LINK.COUNTRY  WITH c_sav, ;
            GTD_LINK.COUNTRYC WITH cc_sav,;
            GTD_LINK.GTD_LINE WITH l_sav
    DO Draw_Foot
  ENDIF
ENDIF

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� P_Next       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�              ��⠢��� �� ��ଠ�� � ᫥���騥 ��ப�.                   �
*�                                                                        �
*����������������������������������������������������������� 05.04.2000 ���
PROCEDURE P_Next

PRIVATE r_sav

IF .NOT. BOF() .AND. .NOT. EOF()
  IF TYPE("g_sav") = "C"
    r_sav = RECNO()
    REPLACE REST GTD_LINK.GTD_NUM  WITH g_sav, ;
                 GTD_LINK.COUNTRY  WITH c_sav, ;
                 GTD_LINK.COUNTRYC WITH cc_sav,;
                 GTD_LINK.GTD_LINE WITH l_sav
    GO r_sav
  ENDIF
ENDIF

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� P_Mark       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�             ��⠢��� �� ��ଠ�� � �⬥祭�� ��ப�.                   �
*�                                                                        �
*����������������������������������������������������������� 05.04.2000 ���
PROCEDURE P_Mark

PRIVATE r_sav

IF .NOT. BOF() .AND. .NOT. EOF()
  IF TYPE("g_sav") = "C"
    r_sav = RECNO()
    REPLACE ALL FOR GTD_LINK.MARK  ;
                 GTD_LINK.GTD_NUM  WITH g_sav, ;
                 GTD_LINK.COUNTRY  WITH c_sav, ;
                 GTD_LINK.COUNTRYC WITH cc_sav,;
                 GTD_LINK.GTD_LINE WITH l_sav
    GO r_sav
  ENDIF
ENDIF

RETURN