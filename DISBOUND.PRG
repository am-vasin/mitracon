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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*
*����������������������������������Ŀ
*�                                  �
*�    [ ] ��⠭����� �࠭��� 99%    �
*�                                  �
*� < OK Ctrl-W > < �⪠������ Esc > �
*������������������������������������

PRIVATE ex, disbound, yes
IF File_O(base_path+"DISBOUND.MEM")
  RESTORE FROM (base_path+"DISBOUND.MEM") ADDITIVE
  yes = .T.
ELSE
  disbound = 0
  yes = .F.
ENDIF
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, '"������" ᪨���'

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 32 SAY "%"
@ 3, 6 GET yes PICTURE "@*C ��⠭����� �࠭���" VALID C_Val()
@ 3, 29 GET disbound WHEN yes PICTURE "@Z 99"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1
  *
  * ��ࠡ��뢠�� �����
  IF yes
    SAVE ALL LIKE disbound TO (base_path+"DISBOUND.MEM")
  ELSE
    DELETE FILE (base_path+"DISBOUND.MEM")
 ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*******************************************************************
PROCEDURE C_Val
IF .NOT. yes
  disbound = 0
  SHOW GET disbound
ENDIF

RETURN .T.
