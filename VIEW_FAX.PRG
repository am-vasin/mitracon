*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� View_Fax     ��о�｀砒┴ ��むォ ��瓱�           30.09.98 16:00:53 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                            蹍甃�矗 ���甌�.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE View_Fax

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

DEFINE PAD View OF (menu_name) PROMPT "蹍甃�矗" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Delete OF (menu_name) PROMPT "����キ┘"
ON PAD Delete OF (menu_name) ACTIVATE POPUP Delete

  DEFINE POPUP Delete MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Delete PROMPT "�オ竕ォ 痰牀��" KEY Ctrl-A, "Del"
  ON SELECTION BAR 1 OF Delete DO Sw_Mode WITH "Del_1"

  DEFINE BAR  2 OF Delete PROMPT "�瓮� �皴��←キ�諷" KEY Ctrl-A, "F8"
  ON SELECTION BAR 2 OF Delete DO Sw_Mode WITH "Del_All"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

DO Use_Dummy
SELECT 0
USE (send_log+"SENDMESS")
SET FILTER TO WHERE = arm .AND. FAX # " "
GO TOP

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS()-12    && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("CENTURY") = "ON", 71, 67)

DO D_Wins WITH ln, wd, "�皴��→� ���甌�", 0, 2
@ WROWS()-3, 3 SAY "�ガ�"
@ WROWS()-2, 3 SAY "����"
IF SET("CENTURY") = "ON"
  @ 1, 2 SAY ;
  "���ム陳�����陳陳陳���玻メ���ぅ�眥陳陳陳陳陳陳�皴��←キ陳�ェ祀赳��"
ELSE
  @ 1, 2 SAY ;
  "���ム陳�����陳陳���玻メ���ぅ�眥陳陳陳陳陳陳�皴��←.��ェ祀赳��"
ENDIF
what_do = "List"
GO BOTT

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del   DO Sw_Mode WITH "Del_1"
    ON KEY LABEL F8    DO Sw_Mode WITH "Del_All"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS CODE:H="",    ;
                  DATE_ON:H="", ;
                  TO:H="",      ;
                  SENDFAX:H="", ;
                  F000 = Res(NOTE):H="":15, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Footer()  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "View")
    ENDIF

  CASE what_do = "View"    && 蹍甃�矗 ぎ�祠キ��

    DELETE FILE (tmpo_path+"VIEW_FAX.TXT")
    IF .NOT. File_O(ALLTRIM(NAME))
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"���� ��甄��┘ �� ���━-皰 �爬腮��� 竄��腑��..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DO Copy_W_D WITH NAME, tmpo_path+"VIEW_FAX.TXT"
      DO View_Txt WITH tmpo_path+"VIEW_FAX.TXT", ALLTRIM(SUBJECT), 66, 0, "1"
      DELETE FILE (tmpo_path+"VIEW_FAX.TXT")
    ENDIF
    what_do = "List"

  CASE what_do = "Del_1"   && ����キ┘ 痰牀��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DELETE FILE (ALLTRIM(NAME))
      DELETE
    ENDIF
    what_do = "List"

  CASE what_do = "Del_All"    && ����キ┘ �皴��←キ�諷 ���甌�

    SCAN FOR .NOT. EMPTY(SENDFAX)
      DELETE
      DELETE FILE (ALLTRIM(NAME))
      DELETE
    ENDSCAN
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
*�                   踳�÷�М���┘ 痰牀�� 爛с�赳���.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 30.09.98 16:41:05 夕
PROCEDURE Res
PARAMETER s
PRIVATE i

i = AT(";", s)
IF i = 0
  i = 16
ENDIF

RETURN LEFT(s,i-1)

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                                Footer!                                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 30.09.98 16:45:32 夕
PROCEDURE Footer

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-3, 8 SAY LEFT(SUBJECT,WCOLS()-10)
@ WROWS()-2, 8 SAY PHONE

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
