*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� New_Name     ��о�｀砒┴ ��むォ ��瓱�           20.07.97 14:49:25 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               蹍甃�矗 �窶���� �ムエ�キ����┤ � 筥�����.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE New_Name
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

IF .NOT. File_O(log_path+"NEW_NAME.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�窶��� �ムエ�キ����┤ �竅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
USE (log_path+"NEW_NAME")
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�窶��� �ムエ�キ����┤ �竅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF
GO BOTTOM
DO Use_Dummy

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

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷 �� ���皀�痰�" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD S_Seek OF (menu_name) PROMPT "蹍�. ���瓷�" KEY Ctrl-A, "F8"
ON SELECTION PAD S_Seek OF (menu_name) DO Sw_Mode WITH "S_Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

SELECT NEW_NAME

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, prf_w
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("CENTURY") = "ON", 72, 70)

DO D_Wins WITH ln, wd, "�ムョキ� ├�キキ�諷 ��爲�牀〓�", 0, 0
what_do = "List"
prf_w = ""

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek"
    ON KEY LABEL F8 DO Sw_Mode WITH "S_Seek"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS DIS_DAT:H="",     ;
                  OLD = PADR(ALLTRIM(OLD_PRF)+" "+ ;
                             ALLTRIM(OLD_NAME)+" "+ ;
                             ALLTRIM(OLD_PROD),30), ;
                  NEW = PADR(ALLTRIM(NEW_PRF)+" "+ ;
                             ALLTRIM(NEW_NAME)+" "+ ;
                             ALLTRIM(NEW_PROD),30), ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Seek"    && ���皀�痰�覃 ���瓷

    DO Seek_C
    what_do = "List"

  CASE what_do = "S_Seek"  && 蹍ぎ�Ε�┘  ���瓷�

    DO S_Seek
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
*�                     蹍������ ���皀�痰���� ���瓷�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 20.07.97 21:14:05 夕
PROCEDURE Seek_C
PRIVATE rc_n, mss

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

*        ��゛�� か� ���瓷�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                 �
*�    ....:....!....:....!....:    �
*�                                 �
*�< OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex = 1
prf_w = PADR(prf_w,25)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 37, "��゛�� か� ���瓷�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  6 GET prf_w
@ 5,  2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

prf_w = ALLTRIM(prf_w)
IF ex = 1 .AND. .NOT. EMPTY(prf_w)
  rc_n = RECNO()
  LOCATE FOR prf_w $ OLD_PRF .OR. ;
             prf_w $ OLD_NAME .OR. ;
             prf_w $ OLD_PROD .OR. ;
             prf_w $ NEW_PRF .OR. ;
             prf_w $ NEW_NAME .OR. ;
             prf_w $ NEW_PROD
  IF .NOT. FOUND()
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��腑��� �� ���ぅ��..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    GO rc_n
  ENDIF
ENDIF
*--------------------------------------------------------------------------

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�               蹍������ �牀ぎ�Ε��� ���皀�痰���� ���瓷�.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 20.07.97 21:29:34 夕
PROCEDURE S_Seek
PRIVATE rc_n, mss
IF EMPTY(prf_w)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� ������ ��゛�� か� ���瓷�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
rc_n = RECNO()
CONTINUE
IF .NOT. FOUND()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���跏� ��腑��� �� ���ぅ��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  GO rc_n
ENDIF

RETURN
