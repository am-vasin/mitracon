*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Arms_K       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�              ��瓷��お� �甅�↓�� ��瘁� �� ��｀腮� �メ���.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 03/17/1999 様�
PROCEDURE Arms_K

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
*  �����皀 �ム┏�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� �����讚�� ���� ��.��.���� ���ョ��� ���� ��.��.���� �
*�                                                    �
*�          < OK Ctrl-W > < �皖���碎瘴 Esc >          �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, dt0, dt1, s_sav, min_d
ex = 1
s_sav = SELECT()
SELECT 0
USE (base_path+"PARMS")
min_d = DAT_START
USE
SELECT (s_sav)
STORE DATE() TO dt0, dt1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 56, "�����皀 �ム┏�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2,  3 SAY "�����讚�� ����" GET dt0
@ 2, 29 SAY "���ョ��� ����" GET dt1
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_D()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  DO View_
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_D        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    蹍▲爲� ��玻オ皚�痰� ┃皀燿���.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/17/1999 陳�
PROCEDURE Tst_D
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF dt0 < min_d
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����襯 縲��閧瘴 皰�讓� � "+DTOC(min_d)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF dt0 > dt1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� ��� �����讚�� ���� ｀�跏� ���ョ���..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�               ��瘍モ � �牀甃�矗 ��瘁� �� ��｀腮� �メ���.               �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/17/1999 陳�
PROCEDURE View_

PRIVATE s_sav, n

s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"V_K_A.DBF")
DELETE FILE (tmpo_path+"V_K_A.CDX")
CREATE DBF (tmpo_path+"V_K_A.DBF") ;
  ( ARM_C C( 3),  ;
    ARM_N C(16),  ;
    SUM_R N(15,2),;
    SUM_D N(15,4)  )
INDEX ON ARM_C TAG ARM_C
SELECT 0
USE (base_path+"KASSA_M") ORDER TAG HRON ALIAS K_9317 AGAIN
IF .NOT. SEEK(DTOS(dt0))
  n = RECNO(0)
  IF BETWEEN(n, 1, RECCOUNT())
    GO n
  ENDIF
ENDIF
SCAN REST WHILE DOC_DATE <= dt1
  SELECT V_K_A
  IF .NOT. SEEK(K_9317.WHERE)
    APPEND BLANK
    REPLACE ARM_C WITH K_9317.WHERE
  ENDIF
  REPLACE SUM_R WITH SUM_R+K_9317.SUM_R ,;
          SUM_D WITH SUM_D+K_9317.SUM_D
  SELECT K_9317
ENDSCAN
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS A_9317 AGAIN
SELECT V_K_A
SET RELATION TO ARM_C INTO A_9317
REPLACE ALL ARM_N WITH A_9317.ARM_N
SELECT A_9317
USE
DO Use_Dummy
SELECT V_K_A
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�※Ε��� ぅ�ィ �� °��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE

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
  PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

  *
  *   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
  *
  stat_type  = 3
  menu_name  = PROGRAM()
  last_mouse = 0
  win_name   = PROGRAM()

  *
  *   ｯ爛ぅ�錺� �瓱�縲����� �キ�
  *
  DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

  DEFINE PAD Choose OF (menu_name) PROMPT "ｯム�罔�" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

  DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
  ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


  *
  *   ��瘍モ ��Кム�� ����
  *
  PRIVATE ln, wd
  ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
  wd = 48
  DO D_Wins WITH ln, wd, "�祠�� �� ��｀腮� �メ���", 0, 0
  @ 1, 2 SAY "���. �メ皰陳陳陳陳陳陳��磧��陳陳陳陳陳�USD"
  what_do = "List"
*
*����. �メ皰陳陳陳陳陳陳��磧��陳陳陳陳陳陳�USD陳陳�
*�....:....!....:. 999_999_999.99 999_999_999.9999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

  DO WHILE .T.

    DO CASE

    CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

      statys_type = 3
      DO Prp_Nav_1
      ON KEY LABEL Enter KEYBOARD CHR(23)

  *
  *    BROWSE - �キ�
  *
      BROWSE FIELDS ARM_N:H="", ;
                    SUM_R:H="":P="999 999 999.99", ;
                    SUM_D:H="":P="999 999 999.9999", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
      ON KEY
      IF what_do = "List"
        IF READKEY() % 256 = 12
          what_do = "Exit"
        ELSE
          what_do = "Choose"
        ENDIF
      ENDIF

    CASE what_do = "Choose"    && �襦��

      DO Prep_List WITH dt0, dt1, ARM_C
      DO Viewlist WITH SUM_R, SUM_D
      what_do = "List"

    OTHERWISE

      EXIT

    ENDCASE

  ENDDO

  RELEASE WINDOW (win_name+"_I")
  RELEASE WINDOW (win_name+"_E")
  RELEASE WINDOW (win_name+"_M")
  RELEASE MENU (menu_name) EXTENDED

ENDIF

USE
DELETE FILE (tmpo_path+"V_K_A.DBF")
DELETE FILE (tmpo_path+"V_K_A.CDX")
SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_List    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��ぃ�皰→� 甎�瓷�                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 14.02.2002 陳�
PROCEDURE Prep_List
PARAMETERS d0, d1, tmpArm

PRIVATE tmpName, s_sav, r_sav, tmpRec

s_sav = SELECT()
SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ( LINK N(10) )
USE (tmpo_path+tmpName) ALIAS TMP_LIST
SELECT VIEW
r_sav = RECNO()
IF .NOT. SEEK(DTOS(d0))
  tmpRec = RECNO(0)
  IF BETWEEN(tmpRec, 1, RECCOUNT())
    GO tmpRec
  ENDIF
ENDIF
SCAN REST WHILE DOC_DATE <= d1
  IF WHERE # tmpArm
    LOOP
  ENDIF
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE LINK WITH RECNO("VIEW")
  SELECT VIEW
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT (s_sav)

RETURN
