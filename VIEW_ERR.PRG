*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 蹍������ �牀甃�矗� �窶���� �荐｀�.                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE View_Err

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

PRIVATE ex, nm_w     &&
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
nm_w = PADR(base_path,40)   &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 67, "����� �窶��� 甃�矗━..."
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                                               �
*� 鉘� � ����� �窶���� ....:....1....:....2....:....3....:....4 �
*�                                                               �
*�               < OK Ctrl-W > < �皖���碎瘴 Esc >                �
*�                                                               �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�


*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  3 SAY "鉘� � ����� �窶����" GET nm_w PICTURE "@K"
@ 5, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "


READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 2 .OR. EMPTY(nm_w)
  RETURN
ENDIF
*--------------------------------------------------------------------------

nm_w = ALLTRIM(nm_w)

USE (nm_w+"USERS") ORDER TAG USER_CODE

DO Use_Dummy

SELECT 0
USE (nm_w+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (nm_w+"ERR_LOG") ORDER TAG ERR_DAT
SET RELATION TO ER_USE INTO USERS,  ;
                ER_ARM INTO ARM_LST

GO BOTT
DO Err_Brws

CLOSE DATABASES
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                                                                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Err_Brws

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※�∽┬
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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Hole_N OF (menu_name) PROMPT "������ 甌�♂キ┘" KEY Ctrl-A, "F1"
ON SELECTION PAD Hole_N OF (menu_name) Do Sw_Mode WITH "hole_n"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(WROWS("")-10, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("CENTURY")="ON", 73, 71)

DO D_Wins WITH ln, wd, "�窶��� �荐｀�", 1, 1

@ 1, 2 SAY IIF(SET("CENTURY")="ON","   ����    ","  ����   ")+  ;
           " �爛��   ���ム    ���♂キ┘                   蹍������ �矗���"
@ WROWS()-3, 3 SAY " ���. �メ皰 "
@ WROWS()-3, 4+FSIZE("ARM_N","ARM_LST") SAY " ���讌���皀�� "

what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F1 WAIT WINDOW ALLTRIM(ER_MES)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS ER_DAT:H="", ;
                  ER_TIM:H="", ;
                  ER_NUM:H="", ;
                  m00 = IIF(LEN(ALLTRIM(ER_MES)) < 31, LEFT(ER_MES,30), LEFT(ER_MES,29)+""):30,  ;
                  m01 = LEFT(ER_PRO,10):10,  ;
                  ER_LIN:H="", ;
                  DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WHEN S_Foot()     ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "edit")
    ENDIF

  CASE what_do = "hole_n"

    WAIT WINDOW ALLTRIM(ER_MES)
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE MENU (menu_name) EXTENDED
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")

RETURN

***********************************************************************
PROCEDURE S_Foot
PRIVATE u_n

IF ER_USE = 0
  u_n = PADR("���!", FSIZE("USER_NAME", "USERS"))
ELSE
  u_n = USERS.USER_NAME
ENDIF
ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-2, 3 SAY ARM_LST.ARM_N COLOR (SCHEME(1,2))
@ WROWS()-2, 4+FSIZE("ARM_N","ARM_LST") SAY u_n  COLOR (SCHEME(1,2))

ACTIVATE WINDOW (ALIAS())

RETURN .T.
