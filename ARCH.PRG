*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ �璢│�牀����� え爛�皰爬�.                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Arch
PARAMETER pth, arch_n, ttl

*   pth    - �璢│�珮ガ�� え爛�皰爬�;
*   arch_n - ━� 甌Г���ガ��� ����� �璢│�;
*   ttl    - ��く�瘡 �� 筮珥�腦�
*
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

PRIVATE ex, max_sz   &&
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex     = 1           &&
max_sz = 1           &&
*------------------------------------------------------------------------
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 ���瓱���讚覃 ��Кム �璢│���� ����� 陳陳陳陳陳陳陳陳陳朕 �
*� � ( ) �ギ�����腑�  ( ) 1.4M  ( ) 1.2M  ( ) 700K  ( ) 360K � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�              < OK Ctrl-W > < �皖���碎瘴 Esc >               �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�


PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 65, IIF(TYPE("ttl")="C", ttl, "�����→� ����諷")
DO Sun_Bord WITH 2, 3, 4, 61
@ 2, 5 SAY " ���瓱���讚覃 ��Кム �璢│���� ����� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  5 GET max_sz PICTURE "@*RH �ギ�����腑�;1.4M;1.2M;700K;360K"
@ 5, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 1
  PRIVATE a_dir, l_dir, i, n, w
  
  l_dir = ADIR(a_dir, pth+arch_n+".A*")   && ����錺� �璢│�襯 �����
  IF l_dir # 0                            &&         メ�� ��� 礒� メ碎...
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
  * �����→�...
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
  @ l+1, c SAY "   ���礇�...    " COLOR SCHEME 5
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
