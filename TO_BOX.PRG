*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� To_Box       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                    �����饭�� ����⮢�� �� ���ᠬ                     �
*�                                                                        �
*����������������������������������������������������������� 23.10.2001 �ͼ
PROCEDURE To_Box
PARAMETERS tmpList, tmpCus, tmpName
PRIVATE addrList, c_name, k, tmp, j, is_filter
is_filter = .F.
addrList = ""
PRIVATE mss, mss1
DIMENSION mss(3), mss1(3)
mss(1) = ""
mss(2) = CHR(0)+"���࠭��� ���������? [ �� ] [ ��� ]"
mss(3) = ""

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
USE (base_path+"ACCOUNT") AGAIN ALIAS LST_1A23 ORDER TAG CUS_CODE
SEEK tmpCus
c_name = CUS_NAME
USE (base_path+"BOXCONT") AGAIN ALIAS LST_1A23 ORDER TAG LIST_IN
SEEK STR(tmpList,6)
SCAN REST WHILE LIST_NUM = STR(tmpList,6)
  addrList = addrList+STR(BOX_NO)+"!"
ENDSCAN
SET ORDER TO FIL_BOX
SELECT 0
USE (base_path+"BOXES") AGAIN ALIAS BOX_1A23 ORDER TAG BOXADDRESS
SET RELATION TO STR(BOX_NO) INTO LST_1A23
IF LEN(addrList) > 0
  LOCATE FOR BOX_NO = VAL(ALLTRIM(LEFT(addrList,10)))
ENDIF
SET FILTER TO .NOT. FOUND("LST_1A23") .OR. LST_1A23.LIST_NUM = STR(tmpList,6)

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "��������/������" KEY Ctrl-A, "Enter"
ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Seek OF (menu_name) PROMPT "����" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Filter OF (menu_name) PROMPT "������" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 51

DO D_Wins WITH ln, wd, "���� ��� ᯨ᪠ � "+ALLTRIM(STR(tmpList)), 3, 0
DO Sun_Bord WITH 1, 1, 3, WCOLS()-2, " ������ "
@ 2, 3 SAY c_name
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F6    DO Sw_Mode WITH "Filter"
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS MARK = IIF(STR(BOX_NO)+"!" $ addrList, "�", " "):1:H="",  ;
                  BOXADDRESS:H="", ;
                  NOTE:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && ��ᬮ�� ᯨ᪠

    tmp = STR(BOX_NO)+"!"
    j = AT(tmp, addrList)
    IF j # 0
      addrList = STUFF(addrList, j, 11, "")
    ELSE
      addrList = addrList+tmp
    ENDIF
    what_do = "List"

  CASE what_do = "Seek"      && ����

    DO Seek_Line
    what_do = "List"

  CASE what_do = "Filter"    && ������

    is_filter = .NOT. is_filter
    DEFINE PAD Filter OF (menu_name) PROMPT IIF(is_filter, "�", "")+"������" KEY Ctrl-A, "F6" BEFORE Exit
    IF is_filter
      SET FILTER TO STR(BOX_NO)+"!" $ addrList
    ELSE
      SET FILTER TO .NOT. FOUND("LST_1A23") .OR. LST_1A23.LIST_NUM = STR(tmpList,6)
    ENDIF
    what_do = "List"

  OTHERWISE

    IF Out_Mess(5, "mss") = 1
      SELECT LST_1A23
      SET ORDER TO FIL_BOX
      j = 1
      DO WHILE j < LEN(addrList)
        k = SUBSTR(addrList, j, 10)
        j = j+11
        IF SEEK(k)
          IF LIST_NUM # STR(tmpList, 6)
            SELECT BOX_1A23
            SET FILTER TO
            SET ORDER TO BOX_NO
            SEEK LST_1A23.BOX_NO
            mss1(1) = ""
            mss1(2) = CHR(0)+"�祩�� � ���ᮬ "+ALLTRIM(BOXADDRESS)+" 㦥 �����!"
            mss1(3) = ""
            DO Out_Mess WITH 7, "mss1"
            SELECT LST_1A23
            SET ORDER TO TAG LIST_IN
            SELECT BOX_1A23
            SET ORDER TO TAG BOXADDRESS
            SET RELATION TO STR(BOX_NO) INTO LST_1A23
            SET FILTER TO .NOT. FOUND("LST_1A23") .OR. LST_1A23.LIST_NUM = STR(tmpList,6)
            GO TOP
            addrList = STUFF(addrList, j, 11, "")
            what_do = "List"
            EXIT
          ENDIF
        ENDIF
      ENDDO
      IF what_do # "List"
        SET ORDER TO LIST_IN
        SEEK STR(tmpList,6)
        SCAN REST WHILE LIST_NUM = STR(tmpList,6)
          IF .NOT. STR(BOX_NO)+"!" $ addrList
            REPLACE WHO_OUT  WITH user,   ;
                    DATE_OUT WITH DATE(), ;
                    TIME_OUT WITH TIME()
          ENDIF
        ENDSCAN
        SET ORDER TO FIL_BOX
        j = 1
        DO WHILE j < LEN(addrList)
          k = SUBSTR(addrList, j, 10)
          j = j+11
          IF .NOT. SEEK(k)
            k = VAL(ALLTRIM(k))
            APPEND BLANK
            REPLACE BOX_NO    WITH k,      ;
                    LIST_NUM  WITH STR(tmpList,6), ;
                    LST_NAME  WITH tmpName,;
                    CUS_CODE  WITH tmpCus, ;
                    WHO_IN    WITH user,   ;
                    DATE_IN   WITH DATE(), ;
                    TIME_IN   WITH TIME()
          ENDIF
        ENDDO
      ENDIF
    ENDIF

    IF what_do # "List"
      EXIT
    ENDIF

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT BOX_1A23
USE
SELECT LST_1A23
USE
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_Line    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                              ���� ����                              �
*�                                                                        �
*����������������������������������������������������������� 22.11.2001 ���
PROCEDURE Seek_Line
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
* ���� ����
*�����������������������������������������������Ŀ
*� ��砫�� ᨬ���� ��� ���᪠ AAAAAAAAAAAAAAAA �
*�                                               �
*�       < OK Ctrl-W > < �⪠������ Esc >        �
*�������������������������������������������������

PRIVATE ex, shab, r_sav
shab = SPACE(16)
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 6, 51, "���� ����"

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 2, 3 SAY "��砫�� ᨬ���� ��� ���᪠" GET shab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(shab)
  *
  * ��ࠡ��뢠�� �����
  shab = TRIM(shab)
  r_sav = RECNO()
  IF .NOT. SEEK(shab)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��祭� �� �������..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    IF BETWEEN(r_sav, 1, RECCOUNT())
      GO r_sav
    ENDIF
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN