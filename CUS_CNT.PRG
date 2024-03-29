*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 ��め腑� �����き諷 � ��┘�����.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Cus_Cnt
PRIVATE ln, wd, dat_0, dat_1, dat_v0, dat_v1, ex, set_c

SELECT 0
USE (base_path+"PARMS.DBF")

dat_v0 = DAT_START   && ���珥���� ���� 縲��キ�� ����諷
dat_v1 = DATE()      && �オ竕�� ����
USE

IF .NOT. G_Period (dat_v0, dat_v1)
  RETURN
ENDIF
dat_0 = dat_v0
dat_1 = dat_v1

SET EXCLUSIVE ON
DELETE FILE(tmpo_path+"TMP_CUS.DBF")
DELETE FILE(tmpo_path+"TMP_CUS.CDX")

CREATE DBF (tmpo_path+"TMP_CUS")  ;
    (    S_MAN N( 3),  ;
         CNT_H N( 5),  ;
         CNT_C N( 5),  ;
         PRCNT N( 3),  ;
         NAME  C(30)   )
INDEX ON S_MAN TAG S_MAN
SET EXCLUSIVE OFF

DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS.DBF") ORDER TAG CODE

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON

IF .NOT. SEEK(DTOS(dat_0))
  ln = RECNO(0)
  IF BETWEEN(ln, 1, RECCOUNT())
    GO ln
  ELSE
    GO BOTTOM
  ENDIF
ENDIF

SCAN REST WHILE DOC_DATE <= dat_1
  SELECT TMP_CUS
  IF .NOT. SEEK(SALE_TIT.WHO)
    = SEEK(SALE_TIT.WHO,"PERSONS")
    APPEND BLANK
    REPLACE S_MAN WITH SALE_TIT.WHO, ;
            NAME  WITH ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
  ENDIF
  REPLACE CNT_H WITH CNT_H+1,  ;
          CNT_C WITH CNT_C+IIF(SALE_TIT.CUS_CODE # 0, 1, 0)
  SELECT SALE_TIT
ENDSCAN

SELECT TMP_CUS
INDEX ON NAME TAG NAME
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 礫�����覃 ���� �ム┏� �� °�� �牀���..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  REPLACE ALL PRCNT WITH ROUND(100*CNT_C/CNT_H,0)
  DO V_Res
ENDIF

CLOSE DATABASES
DELETE FILE(tmpo_path+"TMP_CUS.DBF")
DELETE FILE(tmpo_path+"TMP_CUS.CDX")

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      蹍甃�矗 爛с�赳���.                             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE V_Res

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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 46

DO D_Wins WITH ln, wd, "�ム┏� � "+TRANSFORM(dat_0, "@D")+" �� "+TRANSFORM(dat_1, "@D"), 1, 0
@ 1, 2 SAY PADR("蹍��▲�", 30)+  ;
           " �瓮�� � ��.  %"

what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:H="",     ;
                  CNT_H:H="":P="@Z 99999",  ;
                  CNT_C:H="":P="@Z 99999",  ;
                  PRCNT:H="":P="@Z 999",    ;                  
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E") COLOR SCHEME 13
    ON KEY
    what_do = "exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN
