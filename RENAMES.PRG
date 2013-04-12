*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Renames      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                     ��ୠ� ���४樨 ������������.                     �
*�                                                                        �
*����������������������������������������������������������� 11/03/1998 �ͼ
PROCEDURE Renames
PARAMETER context

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
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P8B03 AGAIN

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS A8B03 AGAIN

SELECT 0
USE (base_path+"DEVELOP") ORDER TAG CODE ALIAS D18B03 AGAIN

SELECT 0
USE (base_path+"DEVELOP") ORDER TAG CODE ALIAS D28B03 AGAIN

SELECT 0
USE (base_path+"STO_LOG") ALIAS S8B03 AGAIN
SET RELATION TO OLD_PRO  INTO D18B03
SET RELATION TO NEW_PRO  INTO D28B03 ADDITIVE
SET RELATION TO WHO_CORR INTO P8B03  ADDITIVE
SET RELATION TO WHERE_C  INTO A8B03  ADDITIVE
GO BOTTOM

*
*   ��ࠬ���� ��� ���᪠
*
PRIVATE s_pattern, s_dir, s_all
PRIVATE choose, ret_v
ret_v = 0
IF TYPE("context") = "C"
  choose = .T.
  s_pattern = context
ELSE
  s_pattern = ""
  choose = .F.
ENDIF
*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF choose
  DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"
ENDIF

DEFINE PAD Seek_1 OF (menu_name) PROMPT "����" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek_1 OF (menu_name) DO Sw_Mode WITH "Seek_1"

DEFINE PAD Seek_2 OF (menu_name) PROMPT "�த������ ����" KEY Ctrl-A, "Ctrl-F7"
ON SELECTION PAD Seek_2 OF (menu_name) DO Sw_Mode WITH "Seek_2"

DEFINE PAD Cnt_M OF (menu_name) PROMPT "������ ��२���������" KEY Ctrl-A, "F1"
ON SELECTION PAD Cnt_M OF (menu_name) DO Sw_Mode WITH "Cnt_M"

IF choose
  DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ELSE
  DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ENDIF
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

s_dir  = 1
s_all  = 1

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, cnt_sav
ln = WROWS("")-9   && ������⢮ ������� ��ப BROWSE
snt_sav = SET("CENTURY") = "OFF"
SET CENTURY ON
wd = IIF(SET("CENTURY") = "ON", 72, 70)
*
*�Ą���������đ�஥ �����������������������č���� ���������������������Ŀ
*���.��.���� ���� ������������������������� ���� ��������������������������
*��������������������������������������������������������������������������
*�Ċ� ���४�஢������������Đ���祥 �������Ĕ�ଠ���Ĕ�ଠ���Ă६���Ŀ
*����������������������������� ���������������� �������� �������� ���������
*��������������������������������������������������������������������������

DO D_Wins WITH ln, wd, "��ୠ� ���४樨 ������������", 0, 1
@ 1, 2 SAY "Ą���������đ�஥ �����������������������č���� ������������"
@ WROWS()-3, 2 SAY "Ċ� ���४�஢������������Đ���祥 �������Ĕ�ଠ���Ĕ�ଠ���Ă६�"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    IF choose
      ON KEY LABEL Enter KEYBOARD CHR(23)
    ENDIF
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek_1"
    ON KEY LABEL Ctrl+F7 DO Sw_Mode WITH "Seek_2"
    ON KEY LABEL F1 DO Sw_Mode WITH "Cnt_M"
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM

*
*    BROWSE - ����
*
    BROWSE FIELDS DAT_CORR:P="@D":H="", ;
                  OLD_PRF:4:H="",  ;
                  OLD_NAM:25:H="", ;
                  NEW_PRF:4:H="",  ;
                  NEW_NAM:25:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      IF choose
        what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
      ELSE
        what_do = "Exit"
      ENDIF
    ENDIF

  CASE what_do = "Choose"    && �롮�

    ret_v = CODE
    EXIT

  CASE what_do = "Seek_1"    && ����

    DO Seek_1
    what_do = "List"

  CASE what_do = "Seek_2"    && �த������� ���᪠

    DO Seek_2 WITH s_pattern, s_dir, 2
    what_do = "List"

  CASE what_do = "Cnt_M"     && ����� ��२���������

    DO Cnt_Move
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
SELECT P8B03
USE
SELECT A8B03
USE
SELECT D18B03
USE
SELECT D28B03
USE
SELECT S8B03
USE
SELECT (s_sav)
IF snt_sav
  SET CENTURY OFF
ENDIF

RETURN ret_v

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
PRIVATE u_name, arm_name
u_name = ""
arm_name = A8B03.ARM_N
IF S8B03.WHERE_C = "***"
  arm_name = "���⠢騪"
ELSE
  IF WHO_CORR = 0
    u_name = "��� ���४�஢��!"
  ELSE
    u_name = ALLTRIM(P8B03.FAMILY)+" "+LEFT(P8B03.NAME,1)+" "+LEFT(P8B03.S_NAME,1)
  ENDIF
ENDIF
u_name = PADR(u_name,28)
arm_name = PADR(arm_name, FSIZE("ARM_N", "A8B03"))

@ WROWS()-2, 2 SAY u_name COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY arm_name COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY LEFT(D18B03.B_NAME,8) COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY LEFT(D28B03.B_NAME,8) COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY TIM_CORR COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_1       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ���������� ⠡���� ���᪠.                       �
*�                                                                        �
*����������������������������������������������������������� 03.11.1998 ���
PROCEDURE Seek_1

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

*�������������������������������������������������Ŀ
*� �� ������ ��� ���᪠ ������������������������Ŀ �
*� �                                             � �
*� ����������������������������������������������� �
*� �� ��ࠬ���� ���᪠ �������������������������Ŀ �
*� �  ( ) �� �ᥬ ᯨ᪥  ( ) � ⥪�饩 ����樨  � �
*� � ( ) �� ���� � ��砫� ( ) �� ��砫� � ����� � �
*� ����������������������������������������������� �
*�        < OK Ctrl-W > < �⪠������ Esc >         �
*���������������������������������������������������

PRIVATE w_pattern, w_dir, w_all
PRIVATE ex
w_pattern = s_pattern
w_dir = s_dir
w_all = s_all
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 11, 53, "���⥪��� ����"
DO Sun_Bord WITH  2,  3,  4, 49, " ������ ��� ���᪠ "
DO Sun_Bord WITH  5,  3,  8, 49, " ��ࠬ���� ���᪠ "

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 5 EDIT w_pattern SIZE 1, 43 PICTURE "@K" COLOR (SCHEME(1,1))
@ 6, 6 GET w_all PICTURE "@*RH �� �ᥬ ᯨ᪥ ;� ⥪�饩 ����樨"
@ 7, 5 GET w_dir PICTURE "@*RH �� ���� � ��砫�;�� ��砫� � �����"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1
  *
  * ��ࠡ��뢠�� �����
  s_pattern = w_pattern
  s_dir = w_dir
  s_all = w_all
  DO Seek_2 WITH s_pattern, s_dir, s_all
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)PRETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_2       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ����⢥��� ����.                            �
*�                                                                        �
*����������������������������������������������������������� 03.11.1998 ���
PROCEDURE Seek_2
PARAMETERS patt, dir, all

PRIVATE r_sav, dir_w
r_sav = RECNO()
dir_w = IIF(dir = 1, -1, 1)

DO Wt_Mess WITH "����..."
IF all = 1
  IF dir_w = 1
    GO TOP
  ELSE
    GO BOTTOM
  ENDIF
ELSE
  SKIP dir_w
ENDIF

DO WHILE .NOT. BOF() .AND. .NOT. EOF()
  IF patt $ OLD_PRF+" "+OLD_NAM .OR. ;
     patt $ NEW_PRF+" "+NEW_NAM
    EXIT
  ENDIF
  SKIP dir_w
ENDDO

DO Wt_Mess

IF EOF() .OR. BOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������ �� ������..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

RETURN
*������������������������������������������������������������������������Ŀ
*�   ��楤�� Cnt_Move     ���ࠡ��稪 ������ ����ਥ���                 �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ������⢮ ��२��������� ����権.                   �
*�                                                                        �
*����������������������������������������������������������� 19.04.2002 ���
PROCEDURE Cnt_Move
PRIVATE s_sav, d01, d02, mss, cnt_s

s_sav = SELECT()
d01 = DATE() - 30
d02 = DATE()

DO G_Period WITH d01, d02
SELECT S8B03
COUNT TO cnt_s FOR DAT_CORR >= d01 .AND. DAT_CORR <= d02

IF cnt_s = 0
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��ਮ� � "+DTOC(d01)+" �� "+DTOC(d02)+" ��२��������� ����権"
  mss(3) = CHR(0)+" �� �஢�������."
  mss(4) = ""
  DO Out_Mess WITH 5, "mss"
ELSE
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��ਮ� � "+DTOC(d01)+" �� "+DTOC(d02)+" �뫮 �஢����� "+ALLTRIM(STR(cnt_s))
  mss(3) = CHR(0)+" ��२��������� ����権."
  mss(4) = ""
  DO Out_Mess WITH 5, "mss"
ENDIF  

SELECT(s_sav)
RETURN