*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                      ��⠭���� ⨯� ������.                          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE S_C_Type
PARAMETER super
PRIVATE v, pl

PUSH KEY CLEAR
HIDE MENU Main_M

DO Use_Dummy
DO Use_Link
SELECT 0
USE ACCOUNT ORDER TAG CUS_NAME
IF super
  pl = Choose_P()
ELSE
  pl = sys_char
ENDIF
DO WHILE .T.
  SELECT ACCOUNT
  IF Custs("", .F., .F., .F., pl)
    DO Set_Type
  ELSE
    EXIT
  ENDIF
ENDDO

CLOSE DATABASES
POP KEY

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Set_Type     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                        ��⠭���� ⨯� ������.                         �
*�                                                                        �
*����������������������������������������������������������� 05.07.2000 ���
PROCEDURE Set_Type

PRIVATE t_codes, t_names, n, point, cus_tp, w_set, d_set
SELECT 0
USE (base_path+"CLI_TYPE") ORDER TAG CUS_DEP ALIAS CTP_0705
IF SEEK(STR(ACCOUNT.CUS_CODE)+STR(user_dep,3))
  cus_tp = CTP_0705.TYPE
  w_set  = WHO_SET
  d_set  = DTOC(DATE_SET)
ELSE
  cus_tp = "  0"
  w_set  = 0
  d_set  = SPACE(10)
ENDIF
SELECT 0
USE (base_path+"CUS_TYPE") ORDER TAG CODE ALIAS TMP_0705
n = 0
point = 1
SEEK STR(user_dep,3)
SCAN REST WHILE DEP = STR(user_dep,3)
  n = n+1
  DIMENSION t_codes(n), t_names(n)
  IF cus_tp = STR(CODE,3)
    point = n
  ENDIF
  t_codes(n) = STR(CODE,3)
  t_names(n) = NAME
ENDSCAN

IF w_set # 0
  USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PR_0705
  IF SEEK(w_set)
    w_set = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
  ELSE
    w_set = ""
  ENDIF
ELSE
  w_set = ""
ENDIF

USE
SELECT ACCOUNT

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
*...:....!....:....!....:....!....:....!....:....!
*��������������������������������������������������������Ŀ
*�                     ������������������������Ŀ         �
*�         ��� ������ �....:....!....:....!....�         �
*�                     ��������������������������         �
*� ��� ��⠭���� ....:....!....:....!........  ��.��.���� �
*�        < OK Ctrl-W > < �⪠������ Esc >                �
*����������������������������������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 60, "������ "+ALLTRIM(ACCOUNT.CUS_NAME)
@ 3, 11 SAY "��� ������"
@ 5,  3 SAY "��� ��⠭���� "+w_set
@ 5, 47 SAY d_set

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 2, 23 GET point PICTURE "@^" FROM t_names SIZE 2, 26
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

SELECT CTP_0705
IF ex = 1 .AND. cus_tp # t_codes(point)
  *
  * ��ࠡ��뢠�� �����
  IF .NOT. FOUND()
    APPEND BLANK
    REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE,  ;
            DEP      WITH STR(user_dep,3)
  ENDIF
  REPLACE TYPE     WITH t_codes(point), ;
          WHO_SET  WITH user,           ;
          DATE_SET WITH DATE()
ENDIF
USE
SELECT ACCOUNT
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN