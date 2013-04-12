*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Arm_Info     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                      ���ଠ�� � ࠡ�祬 ����.                       �
*�                                                                        �
*����������������������������������������������������������� 01.09.1999 �ͼ
PROCEDURE Arm_Info
PARAMETERS arm_cd, hide_name

PRIVATE s_sav, tmpArm

s_sav = SELECT()

tmpArm = arm_cd

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C AGAIN ALIAS A1_9901
IF .NOT. EMPTY(tmpArm)
  IF .NOT. SEEK(tmpArm)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�����४�� ��� ࠡ�祣� ����!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
ENDIF

PRIVATE prs_c, prs_n, a_name, a_place, a_phone

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS P_9901

SELECT 0
USE (base_path+"ARM_INFO") ORDER TAG ARM_C AGAIN ALIAS A2_9901
prs_c = 0
prs_n = ""
prs_c = 0
a_place = SPACE(FSIZE("LOCATE"))
a_phone = SPACE(FSIZE("PHONE"))
a_name  = SPACE(FSIZE("ARM_N", "A1_9901"))
IF .NOT. EMPTY(tmpArm)
  a_name  = A1_9901.ARM_N
  IF SEEK(tmpArm)
    prs_c = WHO
    a_place = LOCATE
    a_phone = PHONE
    IF SEEK(prs_c, "P_9901") .AND. prs_c # 0
      prs_n = ALLTRIM(P_9901.FAMILY)+" "+LEFT(P_9901.NAME,1)+" "+LEFT(P_9901.S_NAME,1)
    ELSE
      prs_c = 0
    ENDIF
  ENDIF
ENDIF

prs_n = PADR(prs_n, 28)

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

*�����������������������������������������������������Ŀ
*� �������� ....:....!....:.  ����䮭 ....:....!....:. �
*�                                                     �
*�         ��� ��室���� ....:....!....:....!          �
*�                                                     �
*�   [ ] ���짮��⥫� ....:....!....:....!....:....!   �
*�                                                     �
*�          < OK Ctrl-W > < �⪠������ Esc >           �
*�������������������������������������������������������

PRIVATE ex, prs_sw
ex = 1
prs_sw = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 10, 57, "���ᠭ�� ࠡ�祣� ����"

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 2,  3 SAY "��������" GET a_name WHEN .NOT. hide_name
@ 2, 30 SAY "����䮭" GET a_phone
@ 4, 11 SAY "��� ��室����" GET a_place
@ 6,  5 GET prs_sw PICTURE "@*C ���짮��⥫�" VALID S_Prs()
@ 6, 22 GET prs_n WHEN .F.

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * ��ࠡ��뢠�� �����
  IF EMPTY(tmpArm)
    SELECT A1_9901
    GO BOTTOM
    IF EOF() .OR. BOF()
      tmpArm = "  1"
    ELSE
      tmpArm = STR(VAL(ARM_C)+1, 3)
    ENDIF
    APPEND BLANK
    REPLACE ARM_C WITH tmpArm, ;
            ARM_S WITH sys_char
    SELECT A2_9901
  ENDIF
  IF .NOT. SEEK(tmpArm)
    APPEND BLANK
  ENDIF
  REPLACE ARM_C  WITH tmpArm, ;
          LOCATE WITH a_place,  ;
          PHONE  WITH a_phone,  ;
          WHO    WITH prs_c
  REPLACE A1_9901.ARM_N WITH a_name
ENDIF
*--------------------------------------------------------------------------

POP KEY
USE
SELECT A1_9901
USE
SELECT P_9901
USE
SELECT (s_sav)
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� S_Prs        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           �롮� ���㤭���.                            �
*�                                                                        �
*����������������������������������������������������������� 02.09.1999 ���
PROCEDURE S_Prs

IF .NOT. prs_sw
  RETURN .T.
ENDIF

prs_sw = .F.

DO Ch_Pers WITH prs_c, prs_n

SHOW GET prs_sw
SHOW GET prs_n

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_Blank    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           �஢�ઠ ������.                             �
*�                                                                        �
*����������������������������������������������������������� 03.01.2004 ���
PROCEDURE Tst_Blank
PRIVATE mss

IF ex # 1
  RETURN .T.
ENDIF

IF EMPTY(a_name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�������� ࠡ�祣� ���� �� ����� ���� �����."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.