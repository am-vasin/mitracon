*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� N_List       ��о�｀砒┴ ��むォ ��瓱�           11.09.98 13:20:10 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ��玻オ罔� ��甎������ 爛����キ皰�.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE N_List

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

DEFINE PAD Add OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Modify OF (menu_name) PROMPT "��玻オ皋牀��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
DO Use_Dummy
SELECT 0
USE (base_path+"NIGHT")

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-10   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("NAME")+2

DO D_Wins WITH ln, wd, "���甌� 爛����キ皰�", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:H="", F000 = IIF(STOP, "X", " "):H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && ��玻オ罔�

    IF .NOT. EOF() .AND. .NOT. BOF()
      DO Corr0
    ENDIF
    what_do = "List"

  CASE what_do = "Add"       && ����←キ┘

    DO Corr0 WITH .T.
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
*�                      ��玻オ罔� �祗��� 爛����キ��.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 11.09.98 13:33:02 夕
PROCEDURE Corr0
PARAMETER new

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��Б��┘ �祗��� 陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� 敖 �┓ ��甎������ 陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �    ( ) ��甄� �メ閹�    ( ) ��� �イカ�    � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�   [ ] ��玻オ罔� ��甎������  [ ] �����牀→�   �
*� 敖 �����き�� 痰牀�� 陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � ....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�       < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, p_name, p_type, p_corr, p_stop, p_comm, p_list
ex = 1
IF new
  p_name = SPACE(40)
  p_type = 1
  p_corr = .F.
  p_stop = .F.
  p_comm = SPACE(40)
  p_list = SPACE(31)
ELSE
  p_name = NAME
  p_type = TYPE_LIST
  p_corr = .F.
  p_stop = STOP
  p_comm = COMM_STR
  p_list = LOG_LIST
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 14, 50, "釪�� 爛����キ��"

DO Sun_Bord WITH 2, 3,  4, 46, " ��Б��┘ �祗��� "
DO Sun_Bord WITH 5, 3,  7, 46, " �┓ ��甎������ "
DO Sun_Bord WITH 9, 3, 11, 46, " �����き�� 痰牀�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  5 GET p_name
@  6,  8 GET p_type PICTURE "@*RH ��甄� �メ閹�   ;��� �イカ�"
@  8,  5 GET p_corr PICTURE "@*C ��玻オ罔� ��甎������" VALID Corr_L()
@  8, 31 GET p_stop PICTURE "@*C �����牀→�"
@ 10,  5 GET p_comm PICTURE "@S40"
@ 12,  9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  IF new
    APPEND BLANK
  ENDIF
  REPLACE NAME      WITH p_name, ;
          TYPE_LIST WITH p_type, ;
          STOP      WITH p_stop, ;
          COMM_STR  WITH p_comm, ;
          LOG_LIST  WITH p_list
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                ��玻オ罔� ��甎������ �� 腮甄�� �メ閹�.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 11.09.98 13:33:02 夕
PROCEDURE Corr1

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   [ ]  1     [ ] 11     [ ] 21   �
*�   [ ]  2     [ ] 12     [ ] 22   �
*�   [ ]  3     [ ] 13     [ ] 23   �
*�   [ ]  4     [ ] 14     [ ] 24   �
*�   [ ]  5     [ ] 15     [ ] 25   �
*�   [ ]  6     [ ] 16     [ ] 26   �
*�   [ ]  7     [ ] 17     [ ] 27   �
*�   [ ]  8     [ ] 18     [ ] 28   �
*�   [ ]  9     [ ] 19     [ ] 29   �
*�   [ ] 10     [ ] 20     [ ] 30   �
*�                         [ ] 31   �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, wk_list
ex = 1
DIMENSION wk_list(31)
FOR i = 1 TO 31
  wk_list(i) = .NOT. EMPTY(SUBSTR(p_list, i, 1))
ENDFOR

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 16, 38, "��甎����┘ �� 腮甄�� �メ閹�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
FOR i = 1 TO 31
  DO CASE
  CASE i < 11
    @ 1+i, 5 GET wk_list(i) PICTURE "@*C "+STR(i,2)
  CASE i < 21
    @ i-9, 16 GET wk_list(i) PICTURE "@*C "+STR(i,2)
  OTHERWISE
    @ i-19, 27 GET wk_list(i) PICTURE "@*C "+STR(i,2)
  ENDCASE
ENDFOR
@ 14, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

p_list = ""
IF ex = 1
  FOR i = 1 TO 31
    p_list = p_list+IIF(wk_list(i), "X", " ")
  ENDFOR
ENDIF
p_list = PADR(p_list,31)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                ��玻オ罔� ��甎������ �� き鍖 �イカ�.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 11.09.98 13:33:02 夕
PROCEDURE Corr2

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�          [ ] ���イカ讚┴         �
*�          [ ] �皰爿┴ 2           �
*�          [ ] �爛�� 3             �
*�          [ ] �モ▲爍 4           �
*�          [ ] �閧���� 5           �
*�          [ ] �磧｀�� 6           �
*�          [ ] ��瓷爛瓮�譽         �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, wk_list
ex = 1
DIMENSION wk_list(7)
wk_list(1) = SUBSTR(p_list,2,1) # " "
wk_list(2) = SUBSTR(p_list,3,1) # " "
wk_list(3) = SUBSTR(p_list,4,1) # " "
wk_list(4) = SUBSTR(p_list,5,1) # " "
wk_list(5) = SUBSTR(p_list,6,1) # " "
wk_list(6) = SUBSTR(p_list,7,1) # " "
wk_list(7) = SUBSTR(p_list,1,1) # " "

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 38, "��甎����┘ �� き鍖 �イカ�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2, 12 GET wk_list(1) PICTURE "@*C ���イカ讚┴"
@ 3, 12 GET wk_list(2) PICTURE "@*C �皰爿┴"
@ 4, 12 GET wk_list(3) PICTURE "@*C �爛��"
@ 5, 12 GET wk_list(4) PICTURE "@*C �モ▲爍"
@ 6, 12 GET wk_list(5) PICTURE "@*C �閧����"
@ 7, 12 GET wk_list(6) PICTURE "@*C �磧｀��"
@ 8, 12 GET wk_list(7) PICTURE "@*C ��瓷爛瓮�譽"
@ 10, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

p_list = ""
IF ex = 1
  p_list = p_list+IIF(wk_list(7), "X", " ")
  p_list = p_list+IIF(wk_list(1), "X", " ")
  p_list = p_list+IIF(wk_list(2), "X", " ")
  p_list = p_list+IIF(wk_list(3), "X", " ")
  p_list = p_list+IIF(wk_list(4), "X", " ")
  p_list = p_list+IIF(wk_list(5), "X", " ")
  p_list = p_list+IIF(wk_list(6), "X", " ")
ENDIF
p_list = PADR(p_list,31)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                         ��玻オ罔� ��甎������.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 11.09.98 16:05:27 夕
PROCEDURE Corr_L

IF .NOT. p_corr
  RETURN .T.
ENDIF

IF p_type = 1
  DO Corr1
ELSE
  DO Corr2
ENDIF

p_corr = .F.
SHOW GET p_corr

RETURN .T.
