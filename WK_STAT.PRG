*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Wk_Stat      ��о�｀砒┴ ��むォ ��瓱�           26.02.98 10:34:45 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      ���皋痰┴� �� �祗���� �キ�.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Wk_Stat
PRIVATE s, q, q_h

USE (base_path+"MENU_LST") ORDER TAG LEVELS
SELECT 0
DELETE FILE (tmpo_path+"MENU_TMP.DBF")
CREATE DBF (tmpo_path+"MENU_TMP.DBF") ;
   ( P_PROMPT C(FSIZE("P_PROMPT", "MENU_LST")+4), ;
     QNT      N(6),                           ;
     PART     N(6,4) )
SELECT 0
IF .NOT. File_O(base_path+"WORK_LOG.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �窶����."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"MENU_TMP.DBF")
  RETURN
ENDIF

DO Wt_Mess WITH "��め腮硅��ガ..."

USE (base_path+"WORK_LOG.DBF") ORDER TAG HRON

SELECT MENU_LST
SCAN
  s = ""
  IF LEVEL2 # 0
    s = s+"  "
    IF LEVEL3 # 0
      s = s+"  "
    ENDIF
  ENDIF
  s = s+ALLTRIM(P_PROMPT)
  IF EMPTY(COMM_STR)
    s = PADR(s, FSIZE("P_PROMPT","MENU_TMP")-1,"=")+""
  ENDIF
  SELECT MENU_TMP
  APPEND BLANK
  REPLACE P_PROMPT WITH s
  SELECT MENU_LST
ENDSCAN
USE
SELECT MENU_TMP
q_h = 0
SCAN FOR RIGHT(P_PROMPT,1) # ""
  SELECT WORK_LOG
  COUNT FOR ALLTRIM(MENU_TMP.P_PROMPT)+"  (�������)" $ WORK_LOG.NOTE TO q
  q_h = q_h+q
  SELECT MENU_TMP
  REPLACE QNT WITH q
ENDSCAN
IF q_h = 0
  DO Wt_Mess
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ���ぅ�� �� �き��� �祗���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  REPLACE ALL PART WITH QNT/q_h
  DO Wt_Mess
  DO View_L
ENDIF
CLOSE DATABASES
DELETE FILE (tmpo_path+"MENU_TMP.DBF")
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                          蹍甃�矗 爛с�赳���.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 26.02.98 12:17:19 夕
PROCEDURE View_L
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

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
DO Use_Dummy
SELECT MENU_TMP
*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("P_PROMPT", "MENU_TMP")+14

DO D_Wins WITH ln, wd, "��痰��� �÷�薀��� � �祗���� �キ�", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - �キ�
*
    BROWSE FIELDS P_PROMPT:H="", ;
                  QNT:H="":P="@Z 999999", ;
                  PART:H="":P="@Z 9.9999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN
