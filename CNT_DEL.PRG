*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Cnt_Del      ��о�｀砒┴ ��むォ ��瓱�           09.09.98 10:57:03 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               ��め腑� 磴��キ�諷 ����瓮� � ���キ���矣爛.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Cnt_Del

DO Prep_Lst
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ 磴��キ�諷 ����瓮�!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"CNT_DEL.DBF")
  DELETE FILE (tmpo_path+"CNT_DEL.CDX")
  RETURN
ENDIF

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

DEFINE PAD On_Off OF (menu_name) PROMPT "�����碎/��瘁����※碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD On_Off OF (menu_name) DO Sw_Mode WITH "On_Off"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

DO Use_Dummy
SELECT CNT_DEL
GO TOP

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = 10
wd = FSIZE("NAME")+FSIZE("PREFIX")+FSIZE("PRODUCER")+4

DO D_Wins WITH ln, wd, "����キ�襯 ��Ж罔�", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="",    ;
                  NAME:H="",  ;
                  PRODUCER:H="", SW:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "On_Off")
    ENDIF

  CASE what_do = "On_Off"    && ����キ┘/��瘁����←キ┘

    SELECT STOCK
    GO CNT_DEL.RC_NO
    IF EMPTY(CNT_DEL.SW)
      DELETE
      SELECT CNT_DEL
      REPLACE SW WITH "�"
    ELSE
      RECALL
      SELECT CNT_DEL
      REPLACE SW WITH " "
    ENDIF
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
*�                  ��ぃ�皰→� 甎�瓷� 磴��キ�諷 ����瓮�.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 09.09.98 10:58:12 夕
PROCEDURE Prep_Lst
PRIVATE del_sw, cnt
del_sw = SET("DELETED") = "ON"

SET DELETED OFF

DELETE FILE (tmpo_path+"CNT_DEL.DBF")
DELETE FILE (tmpo_path+"CNT_DEL.CDX")
CREATE DBF (tmpo_path+"CNT_DEL") ;
  ( RC_NO    N(10), ;
    CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(78), ;
    PRODUCER C( 8), ;
    SW       C( 1)  )
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE (base_path+"STOCK")
DO Wt_Mess WITH "蹍甃�矗 ���キ���矣琺..."

cnt = 0
SCAN
  cnt = cnt+1
  @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)),WCOLS())
  IF .NOT. DELETED()
    LOOP
  ENDIF
  SELECT CNT_DEL
  APPEND BLANK
  REPLACE RC_NO    WITH RECNO("STOCK"), ;
          CODE     WITH STOCK.CODE,     ;
          PREFIX   WITH STOCK.PREFIX,   ;
          NAME     WITH STOCK.NAME,     ;
          PRODUCER WITH STOCK.PRODUCER, ;
          SW       WITH "�"
  SELECT STOCK
ENDSCAN

DO Wt_Mess

SELECT CNT_DEL
IF del_sw
  SET DELETED ON
ENDIF

RETURN
