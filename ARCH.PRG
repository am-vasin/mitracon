*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                  �ணࠬ�� ��娢�஢���� ��४�ਨ.                  �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Arch
PARAMETER pth, arch_n, ttl

*   pth    - ��娢��㥬�� ��४���;
*   arch_n - ��� ᮧ��������� 䠩�� ��娢�;
*   ttl    - ������� �� ���窥
*
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

PRIVATE ex, max_sz   &&
                     &&   ���塞 � ������塞 ���� ������
ex     = 1           &&
max_sz = 1           &&
*------------------------------------------------------------------------
*
*�������������������������������������������������������������Ŀ
*� �� ���ᨬ���� ࠧ��� ��娢���� 䠩�� ������������������Ŀ �
*� � ( ) ����࠭�祭  ( ) 1.4M  ( ) 1.2M  ( ) 700K  ( ) 360K � �
*� ����������������������������������������������������������� �
*�              < OK Ctrl-W > < �⪠������ Esc >               �
*���������������������������������������������������������������


PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 65, IIF(TYPE("ttl")="C", ttl, "�������� ������")
DO Sun_Bord WITH 2, 3, 4, 61
@ 2, 5 SAY " ���ᨬ���� ࠧ��� ��娢���� 䠩�� "

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3,  5 GET max_sz PICTURE "@*RH ����࠭�祭;1.4M;1.2M;700K;360K"
@ 5, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 1
  PRIVATE a_dir, l_dir, i, n, w
  
  l_dir = ADIR(a_dir, pth+arch_n+".A*")   && ����塞 ��娢�� 䠩��
  IF l_dir # 0                            &&         �᫨ ��� 㦥 ����...
    FOR i = 1 TO l_dir
      w = RIGHT(a_dir(i,1), 2)
      IF w = "RJ"
        DELETE FILE (pth+a_dir(i,1))
      ENDIF
      IF RIGHT(w,1) $ "0123456789J" .AND. ;
         LEFT(w,1) $ "0123456789R"
        DELETE FILE (pth+a_dir(i,1))
      ENDIF
    ENDFOR
  ENDIF
  *
  * ��������...
  *
  PRIVATE l, c, ss, sr
  ACTIVATE SCREEN
  SAVE SCREEN TO ss
  cr = SET("CURSOR") = "ON"
  SET CURSOR OFF
  c = FLOOR((WCOLS("")-16)/2)
  l = CEILING((WROWS("")-3)/2)
  @ l+1, c+2 FILL TO l+3, c+17 COLOR (SCHEME(1,8))
  @ l,   c SAY "                " COLOR SCHEME 5
  @ l+1, c SAY "   ���㥬...    " COLOR SCHEME 5
  @ l+2, c SAY "                " COLOR SCHEME 5
  
  DO CASE
  CASE max_sz = 1
    w = ""
  CASE max_sz = 2
    w = " -V1400000"
  CASE max_sz = 3
    w = " -V1200000"
  CASE max_sz = 4
    w = " -V700000"
  CASE max_sz = 5
    w = " -V360000"
  ENDCASE
  w = "! ARJ A"+w+" -Y -E "+pth+arch_n+" "+pth+" > nul"
  &w
  RESTORE SCREEN FROM ss
  IF cr
    SET CURSOR ON
  ENDIF
ENDIF
*--------------------------------------------------------------------------

RETURN ex = 1