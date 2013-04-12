*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Ch_OperG     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�             �롮� �ଠ � ������� ���ᮢ�� ����樨.                 �
*�                                                                        �
*����������������������������������������������������������� 10/23/1998 �ͼ
PROCEDURE Ch_OperG
PARAMETERS g_code, g_name

PRIVATE w_code, w_name
PRIVATE fl_level, s_sav, what_do, titles, w
fl_level = 0
DIMENSION titles(1)   && ��� ��� ���ᨢ - �� �� ����, ��� �⥪ ����������
titles = "�᭮��� ࠧ����"

s_sav = SELECT()
DO Use_Dummy

SELECT 0
USE (base_path+"OPERSTRU") ORDER TAG PREV_LINE ALIAS V8A27 AGAIN && ����⢥��� ���४��

SELECT 0
USE (base_path+"OPERSTRU") ORDER TAG CODE ALIAS C8A27 AGAIN   && ���筨� �����

what_do = "Level"
IF SEEK(g_code)               && ��ந� �⥪ ���ࠧ�����
  w = RECNO()
  GO w IN V8A27
  g_name = C8A27.NAME
  fl_level = C8A27.PREV_LINE
  PRIVATE tmp_arr, ok, nxt_w
  DIMENSION tmp_arr(1)
  nxt_w = PREV_LINE
  ok = nxt_w # 0
  DO WHILE nxt_w # 0
    IF .NOT. SEEK(nxt_w, "C8A27")
      ok = .F.
      EXIT
    ENDIF
    DIMENSION tmp_arr(ALEN(tmp_arr)+1)
    tmp_arr(ALEN(tmp_arr)) = ALLTRIM(C8A27.NAME)
    nxt_w = C8A27.PREV_LINE
  ENDDO
  IF ok
    DIMENSION titles(ALEN(tmp_arr))
    FOR ok = 2 TO ALEN(tmp_arr)
      titles(ok) = tmp_arr(ALEN(tmp_arr)-ok+2)
    ENDFOR
  ENDIF
ENDIF
SELECT V8A27
DO WHILE .T.
  DO CASE
  CASE what_do = "Level"
    what_do = Level()
  CASE what_do = "Term"
    g_code = V8A27.CODE
    g_name = V8A27.NAME
    EXIT
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

SELECT V8A27
USE
SELECT C8A27
USE
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Level        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�               �롮� �� ᯨ᪠ ࠧ����� ⥪�饣� �஢��.                �
*�                                                                        �
*����������������������������������������������������������� 10/22/1998 ���
PROCEDURE Level

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
PRIVATE ret_v
*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Next OF (menu_name) PROMPT "����. �஢���" KEY Ctrl-A, "Enter"
ON SELECTION PAD Next OF (menu_name) DO Sw_Mode WITH "Next"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, mss, rc_sav, w
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("NAME")+2

DO D_Wins WITH ln, wd, titles(ALEN(titles)), 0, 0
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
    BROWSE KEY fl_level FIELDS NAME:H="",  ;
           F000 = IIF(IS_TERM, "�"," "):H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Next")
    ENDIF

  CASE what_do = "Next"    && �� ᫥���騩 �஢���

    IF .NOT. BOF() .AND. .NOT. EOF()
      fl_level = CODE
      DIMENSION titles(ALEN(titles)+1)
      titles(ALEN(titles)) = ALLTRIM(NAME)
      ret_v =IIF(IS_TERM, "Term", "Level")
      EXIT
    ELSE
      what_do = "List"
    ENDIF

  OTHERWISE

    IF fl_level = 0
      ret_v = "Exit"
    ELSE
      = SEEK(fl_level,"C8A27")
      GO RECNO("C8A27") IN V8A27
      fl_level = C8A27.PREV_LINE
      DIMENSION titles(ALEN(titles)-1)
      ret_v = "Level"
    ENDIF
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

RETURN ret_v