*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� V_Arms       ��о�｀砒┴ ��むォ ��瓱�           21.01.98 11:15:58 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        ��痰�鎰�� ��｀腮� �メ�.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE V_Arms

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Switch OF (menu_name) PROMPT "���讓� ��皋↓襯" KEY Ctrl-A, "F6"
ON SELECTION PAD Switch OF (menu_name) DO Sw_Mode WITH "Switch"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
DO Use_Dummy
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P0001 AGAIN
SELECT 0
USE (base_path+"ARM_INFO") ORDER TAG ARM_C
SET RELATION TO WHO INTO P0001
SELECT 0
USE (base_path+"ARM_LST")
SET FILTER TO ARM_S = sys_char
SET RELATION TO US_CODE INTO PERSONS
SET RELATION TO ARM_C INTO ARM_INFO ADDITIVE
*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, dt_l, fn_l, sw
ln = WROWS("")-14   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("ARM_N")+FSIZE("SYS_NAM")+IIF(SET("CENTURY") = "ON", 21, 19)
dt_l = IIF(SET("CENTURY") = "ON", 10, 8)
sw = .T.

DO D_Wins WITH ln, wd, "��痰�鎰┘ ��｀腮� �メ�", 0, 5
@ 1, 2 SAY   PADR("��｀腑� �メ皰", FSIZE("ARM_N"), "�")
@ 1, COL()+1 SAY PADR("��痰ガ�", FSIZE("SYS_NAM"), "�")
@ 1, COL()+1 SAY PADC("����", dt_l, "�")
@ 1, COL()+1 SAY PADC("�爛��", 8, "�")

fn_l = FSIZE("ARM_N")+FSIZE("SYS_NAM")+1
@ WROWS()-7, 2 SAY PADR("�祗�罔�", fn_l, "�")
@ WROWS()-7, COL()+1 SAY PADC("����", dt_l, "�")
@ WROWS()-7, COL()+1 SAY PADC("�爛��", 8, "�")
what_do = "List"

*!....:....!....:....!....:....!....:....!....:....
*!��甎. ....:....!....:....!  �カ. ....:....!....:.
*!        �モア�� ━� ....:....!....:....!
*!   ���讌���皀�� ....:....!....:....!....:....!

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F6 DO Sw_Mode WITH "Switch"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS ARM_N:H="", SYS_NAM:H="",     ;
                  F000 = IIF(EMPTY(SYS_DAT), SPACE(dt_l), DTOC(SYS_DAT)):H="", ;
                  SYS_TIM:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_F() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "exit", "edit")
    ENDIF

  CASE what_do = "Switch"    && ��キ� 筥�赳��

    DO Sw_List
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                          ��筮爼�罔����� ����.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 21.01.98 14:52:35 夕
PROCEDURE Show_F
PRIVATE nm

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-6, 2 CLEAR TO WROWS()-5, WCOLS()-3

IF .NOT. EMPTY(SYS_NAM)
  @ WROWS()-6, 2 SAY LEFT(FUN_NAM,fn_l)+" "+DTOC(FUN_DAT)+" "+FUN_TIM
ENDIF

IF .NOT. EMPTY(US_DAT)
  IF EMPTY(US_CODE)
    nm = "��� ��｀��モ!"
  ELSE
    IF .NOT. FOUND("PERSONS")
      nm = "�皰-皰 �エБメ皚覃!"
    ELSE
      nm = ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+"."+LEFT(PERSONS.S_NAME,1)+"."
    ENDIF
  ENDIF
  @ WROWS()-5, 2 SAY PADR(nm,fn_l)+" "+DTOC(US_DAT)+" "+US_TIM
ENDIF

*!��甎. ....:....!....:....!  �カ. ....:....!....:.
*!        �モア�� ━� ....:....!....:....!
*!   ���讌���皀�� ....:....!....:....!....:....!
@ WROWS()-4, 2 CLEAR TO WROWS()-2, WCOLS()-3

IF FOUND("ARM_INFO")
  @ WROWS()-4,  2 SAY "�ぅ "+ARM_INFO.LOCATE+"  �カ. "+ARM_INFO.PHONE
  @ WROWS()-3, 10 SAY "�モア�� ━� "+ARM_INFO.NET_NAME
  nm = ALLTRIM(P0001.FAMILY)+" "+LEFT(P0001.NAME,1)+" "+LEFT(P0001.S_NAME,1)
  @ WROWS()-2, 5 SAY "���讌���皀�� "+PADR(nm, 30)
ENDIF

ACTIVATE WINDOW (ALIAS())

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                         �ムオ�鈑キ┘ 筥�赳��.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 21.01.98 15:05:45 夕
PROCEDURE Sw_List

sw = .NOT. sw
IF sw
  DEFINE PAD Switch OF (menu_name) PROMPT "���讓� ��皋↓襯" KEY Ctrl-A, "F6"
  SET FILTER TO ARM_S = sys_char
ELSE
  DEFINE PAD Switch OF (menu_name) PROMPT "�瓮" KEY Ctrl-A, "F6"
  SET FILTER TO ARM_S = sys_char .AND. .NOT. EMPTY(SYS_NAM)
ENDIF

RETURN


