*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� MM_Group     ��о�｀砒┴ ��むォ ��瓱�           07.09.98 12:27:42 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       �珮��� ��瘁��諷 ��ム�罔�.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE MM_Group

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
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

*
*   ���甌� ��ム�罔�
USE (base_path+"MM_TYPES") ORDER TAG NAME
PRIVATE oper_n, oper_c, oper_cnt
oper_cnt = 0
SCAN
  oper_cnt = oper_cnt+1
  DIMENSION oper_n(oper_cnt), oper_c(oper_cnt)
  oper_n(oper_cnt) = " "+NAME
  oper_c(oper_cnt) = CODE
ENDSCAN
USE
IF oper_cnt = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� ��爛ぅ�┼� �� �き�� ��瘁�〓� ��ム�罔�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
DO Use_Dummy
SELECT 0
USE (base_path+"MM_GROUP")

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "��玻オ皋牀��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Delete OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = FLOOR(WROWS("")*0.7)   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "�珮��� ��ム�罔�", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL dEL   DO Sw_Mode WITH "Delete"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && ��玻オ罔�

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Corr_Gr
    ENDIF
    what_do = "List"

  CASE what_do = "Add"       && ����←キ┘

    DO Corr_Gr WITH .T.
    what_do = "List"

  CASE what_do = "Delete"   && ����キ┘

    IF .NOT. BOF() .AND. .NOT. EOF()
      DELETE
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
*�                 ��玻オ罔�, ぎ��△キ┘ �珮��� ��ム�罔�.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.09.98 12:52:54 夕
PROCEDURE Corr_Gr
PARAMETER add

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�  敖 ��Б��┘ �珮��� 陳陳陳陳�  �
*�  � ....:....!....:....!.... �  �
*�  青陳陳陳陳陳陳陳陳陳陳陳陳潰  �
*�      [ ] ���甌� ��ム�罔�       �
*�                                �
*�< OK Ctrl-W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, nm_w, o_list, s_list
ex   = 1
o_list = .F.
IF add
  nm_w = SPACE(24)
  s_list = ""
ELSE
  nm_w = NAME
  s_list = OPER_LST
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 36, IIF(add,"����←キ┘ ", "��玻オ罔� ")+"�珮���"
DO Sun_Bord WITH 2, 4, 4, 31, " ��Б��┘ �珮��� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3, 6 GET nm_w
@ 5, 8 GET o_list PICTURE "@*C ���甌� ��ム�罔�" VALID C_List()
@ 7, 2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  IF add
    APPEND BLANK
  ENDIF
  REPLACE NAME     WITH nm_w, ;
          OPER_LST WITH s_list
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                       ��玻オ罔� 甎�瓷� ��ム�罔�.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.09.98 14:46:07 夕
PROCEDURE C_List
PRIVATE i
IF .NOT. o_list
  RETURN .T.
ENDIF

FOR i = 1 TO oper_cnt
  IF STR(oper_c(i),3)+"!" $ s_list
    oper_n(i) = "�"+SUBSTR(oper_n(i),2)
  ELSE
    oper_n(i) = " "+SUBSTR(oper_n(i),2)
  ENDIF
ENDFOR
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

*  ｯム�罔�
*敖陳陳陳陳陳陳陳陳�
*�                 �
*�                 �
*�                 �
*�                 �
*�                 �
*�                 �
*�                 �
*青陳陳陳陳陳陳陳陳�
* Enter - �皙モ�碎
*   Esc - �諷��

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 21, "ｯム�罔�"
@ 1, 1 CLEAR TO WROWS()-1, WCOLS()-1
@ WROWS()-2, 0 SAY PADC("Enter - �皙モ�碎", WCOLS())
@ WROWS()-1, 0 SAY PADC("Esc - �諷��", WCOLS())

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
i = 1
DO WHILE .T.
  i = MAX(i,1)
  i = MIN(i, oper_cnt)
  @ 1, 1 MENU oper_n, oper_cnt, 7
  READ MENU TO i
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF LEFT(oper_n(i), 1) = " "
    oper_n(i) = "�"+SUBSTR(oper_n(i),2)
  ELSE
    oper_n(i) = " "+SUBSTR(oper_n(i),2)
  ENDIF
ENDDO
s_list = ""
FOR i = 1 TO oper_cnt
  IF EMPTY(LEFT(oper_n(i),1))
    LOOP
  ENDIF
  s_list = s_list+STR(oper_c(i),3)+"!"
ENDFOR
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
o_list = .F.
SHOW GET o_list
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            蹍▲爲� ゛����.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.09.98 16:32:40 夕
PROCEDURE Tst_Blank

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� �� ��Ε� °碎 �竅硅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
