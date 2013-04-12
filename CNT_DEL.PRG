*������������������������������������������������������������������������ͻ
*� ���� Cnt_Del      ���ࠡ��稪 ���३ ��ᨭ           09.09.98 10:57:03 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�               ������ 㤠������ ����ᥩ � �����������.                �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Cnt_Del

DO Prep_Lst
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� 㤠������ ����ᥩ!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"CNT_DEL.DBF")
  DELETE FILE (tmpo_path+"CNT_DEL.CDX")
  RETURN
ENDIF

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD On_Off OF (menu_name) PROMPT "�������/����⠭�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD On_Off OF (menu_name) DO Sw_Mode WITH "On_Off"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

DO Use_Dummy
SELECT CNT_DEL
GO TOP

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = 10
wd = FSIZE("NAME")+FSIZE("PREFIX")+FSIZE("PRODUCER")+4

DO D_Wins WITH ln, wd, "�������� ����樨", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
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

  CASE what_do = "On_Off"    && ��������/����⠭�������

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


*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                  �����⮢�� ᯨ᪠ 㤠������ ����ᥩ.                  �
*�                                                                        �
*����������������������������������������������������� 09.09.98 10:58:12 ͼ
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
DO Wt_Mess WITH "��ᬮ�� ������������..."

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