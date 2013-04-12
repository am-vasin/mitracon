*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Refusal      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                           ��ࠢ�� �� ����                            �
*�                                                                        �
*����������������������������������������������������������� 07/25/2001 �ͼ
PROCEDURE Refusal

PRIVATE dt_s, dt_e, tmpName

dt_e = DATE()
dt_s = GOMONTH(dt_e, -1)

IF .NOT. G_Period(dt_s, dt_e)
  RETURN
ENDIF
tmpName = SYS(3)

CREATE DBF (tmpo_path+tmpName) ;
  ( PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    DATE     D,     ;
    QNT      N( 6), ;
    PRICE    N(8,2),;
    PRICE_D  N(10,4) )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_REF

SELECT 0
USE base_path+"REFUSAL" ORDER TAG NAME_DATE

SCAN FOR BETWEEN(DATE, dt_s, dt_e)
  SELECT TMP_REF
  APPEND BLANK
  REPLACE PREFIX   WITH REFUSAL.PREFIX,  ;
          NAME     WITH REFUSAL.NAME,    ;
          PRODUCER WITH REFUSAL.PRODUCER,;
          DATE     WITH REFUSAL.DATE,    ;
          QNT      WITH REFUSAL.QTY,     ;
          PRICE    WITH REFUSAL.PRICE,   ;
          PRICE_D  WITH REFUSAL.PRICE_D
  SELECT REFUSAL
ENDSCAN
USE
SELECT TMP_REF
INDEX ON NAME+PREFIX+PRODUCER+DTOS(DATE) TAG TMP
GO TOP


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

SELECT 0
DO Use_Dummy
SELECT TMP_REF

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 77

DO D_Wins WITH ln, wd, "�⪠�� �� 業� � "+DTOC(dt_s)+" �� "+DTOC(dt_e), 0, 1
@ 1, 2 SAY ;
"����ď�����������������������������������Ą������Ė���(�)���Ė���($)Ċ��-��"
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
    BROWSE FIELDS PREFIX:H="",  ;
                  Brw_Name=Nm_Part(NAME):25:H="", ;
                  PRODUCER:H="",;
                  DATE:H="",    ;
                  PRICE:H="",   ;
                  PRICE_D:H="", ;
                  QNT:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"
*    IF what_do = "List"
*      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
*    ENDIF

*  CASE what_do = "Choose"    && ��ᬮ�� ᯨ᪠
*
*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    what_do = "List"
*
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
CLOSE DATABASES
DELETE FILE (tmpo_path+tmpName+".dbf")
DELETE FILE (tmpo_path+tmpName+".cdx")
RETURN

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
@ WROWS()-2, 3 SAY PREFIX COLOR SCHEME 1
@ ROW(), COL()+1 SAY NAME COLOR SCHEME 1
@ ROW(), COL()+1 SAY PRODUCER COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.