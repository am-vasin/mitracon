*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      �痰���→� 皋�� ��┘���.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Type     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �痰���→� 皋�� ��┘���.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.07.2000 陳�
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
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*
*...:....!....:....!....:....!....:....!....:....!
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                     敖陳陳陳陳陳陳陳陳陳陳陳朕         �
*�         �┓ ��┘��� �....:....!....:....!....�         �
*�                     青陳陳陳陳陳陳陳陳陳陳陳潰         �
*� �┓ 竅����※� ....:....!....:....!........  ��.��.���� �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 60, "��┘�� "+ALLTRIM(ACCOUNT.CUS_NAME)
@ 3, 11 SAY "�┓ ��┘���"
@ 5,  3 SAY "�┓ 竅����※� "+w_set
@ 5, 47 SAY d_set

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 23 GET point PICTURE "@^" FROM t_names SIZE 2, 26
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

SELECT CTP_0705
IF ex = 1 .AND. cus_tp # t_codes(point)
  *
  * �矗���硅��ガ ゛���
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
