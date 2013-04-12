*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Ch_Bank      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                              �롮� �����.                              �
*�                                                                        �
*����������������������������������������������������������� 14.01.1999 �ͼ
PROCEDURE Ch_Bank
PARAMETERS f_code, f_name_b, f_name_l, b_code, b_name_b, b_name_l, all_f
*
*  f_code   - ��� �।�����, �᫨ �� 㪠��� ��� ����, � ��;
*  f_name_b - ��� �।�����, ���������� �ணࠬ��� �᫨ ��� ����;
*  f_name_l - ��� �।�����, ���������� �ணࠬ��� �᫨ ��� ����;
*  b_code   - ��� �����;
*  b_name_b - ���⪮� ��� �����;
*  b_name_l - ������� ��� �����;
*  all_f    - ��� ��� �।���⨩.
*

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
PRIVATE k_w, mss
SELECT 0
DO Use_Dummy
SELECT 0
IF all_f .OR. EMPTY(f_code)
  k_w = ""
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9114 AGAIN
  SELECT 0
  USE (base_path+"BANKS") ORDER TAG ALL_NAMES ALIAS B_9114 AGAIN
  SET FILTER TO EMPTY(DATE_OFF) .AND. EMPTY(HIDE)
  SET RELATION TO FIRM_CODE INTO F_9114
  IF .NOT. EMPTY(f_code) .AND. .NOT. EMPTY(b_code)
    SET ORDER TO TAG BANK
    IF .NOT. SEEK(f_code+b_code) .OR. .NOT. EMPTY(DATE_OFF)
      SET ORDER TO TAG ALL_NAMES
      GO TOP
    ELSE
      SET ORDER TO TAG ALL_NAMES
    ENDIF
  ENDIF
ELSE
  k_w = f_code
  USE (base_path+"BANKS") ORDER TAG NAME ALIAS B_9114 AGAIN
  IF .NOT. SEEK(k_w)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� �।���� ��� �� ������ �����!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
  IF .NOT. EMPTY(b_code)
    SET ORDER TO TAG BANK
    IF .NOT. SEEK(f_code+b_code)
      SET ORDER TO TAG NAME
      SEEK k_w
    ELSE
      IF .NOT. EMPTY(DATE_OFF)
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"��� ���� 㤠���!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        USE
        SELECT (s_sav)
        RETURN
      ENDIF
      SET ORDER TO TAG NAME
    ENDIF
  ENDIF
ENDIF  

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
PRIVATE ln, wd, tit
ln = WROWS("")-IIF(EMPTY(k_w), 14, 11)   && ������⢮ ������� ��ப BROWSE
wd = 16

DO D_Wins WITH ln, wd, "���᮪ ������", 0, IIF(EMPTY(k_w), 1, 0)
IF EMPTY(k_w)
  @ WROWS()-3, 3 SAY "�।���⨥"
ENDIF
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
    BROWSE KEY k_w FIELDS B_NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �롮�

    IF EMPTY(k_w)
      f_code   = F_9114.FIRM_CODE
      f_name_b = F_9114.B_NAME
      f_name_l = F_9114.L_NAME
      b_code   = B_9114.BANK_CODE
      IF TYPE("b_name_b") = "C"
        b_name_b = B_9114.B_NAME
      ENDIF
      IF TYPE("b_name_l") = "C"
        b_name_l = B_9114.L_NAME
      ENDIF
    ELSE
      b_code   = B_9114.BANK_CODE
      IF TYPE("b_name_b") = "C"
        b_name_b = B_9114.B_NAME
      ENDIF
      IF TYPE("b_name_l") = "C"
        b_name_l = B_9114.L_NAME
      ENDIF
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
USE
IF EMPTY(k_w)
  SELECT F_9114
  USE
ENDIF
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

IF EMPTY(k_w)
  ACTIVATE WINDOW (win_name+"_M") SAME

  *
  *   ��� �����, ��㥬...
  *
  @ WROWS()-2, 2 SAY F_9114.B_NAME

  ACTIVATE WINDOW (ALIAS()) SAME
ENDIF

RETURN .T.