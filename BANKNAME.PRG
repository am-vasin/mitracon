********************************************************************************
*    ���� BankName       ���ࠡ��稪 ���३ ��ᨭ                14.01.2012    *
********************************************************************************
*                                                                              *
*                             ���� �������� �����                              *
*                                                                              *
********************************************************************************
PROCEDURE BankName
PARAMETERS prmTit, prmLen, prmStart
*
* prmTit   - ��������� ���
* prmLen   - ����� ��������
* prmStart - ��砫쭮� ���祭��
*
*
*  ��६���� ���ﭨ� ��� ������樨
*
PRIVATE tmpTit, tmpLen, tmpWid, tmpShab

m.tmpTit  = IIF(TYPE("m.prmTit") = "C", m.prmTit, "������ 蠡��� ��� ���᪠")
m.tmpLen  = IIF(TYPE("m.prmLen") = "N", m.prmLen, 20)
m.tmpWid  = MIN(m.tmpLen, WCOLS("")-8)
m.tmpShab = IIF(TYPE("m.prmStart") = "C", m.prmStart, "")

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*+-------------------------------------------------+
*!                                                 !
*!        < OK Ctrl-W > < �⪠������ Esc >         !
*+-------------------------------------------------+

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 5, MAX(m.tmpWid+6, 38), m.tmpTit

*------------------------------------------------------------------------
*      ���� ����� ������
*


@ WROWS()-3, FLOOR(WCOLS()/2-m.tmpWid/2) EDIT m.tmpShab SIZE 1, m.tmpWid, m.tmpLen COLOR (SCHEME(1,1))
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF m.ex # 1
  *
  * ��ࠡ��뢠�� �����
  m.tmpShab = ""
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.tmpShab

