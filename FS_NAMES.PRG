*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� FS_Names     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�      蹍������ 甌Г���� �������� 筰�Κ�� か� 甎�瓷�� (�����き諷).      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 03/27/1999 様�
PROCEDURE FS_Names

PRIVATE n_flags, fs_names, i

IF File_O(base_path+"FS_FLAGS.MEM")
  RESTORE FROM (base_path+"FS_FLAGS.MEM") ADDITIVE
ELSE
  DIMENSION fs_names(16)
  FOR i = 1 TO 16
    fs_names(i) = SPACE(16)
  ENDFOR
ENDIF
n_flags = 0
FOR i = 1 TO 8
  IF EMPTY(fs_names(i))
    EXIT
  ELSE
    n_flags = n_flags+1
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�      1. ....:....!....:.       �
*�      2. ....:....!....:.       �
*�      3. ....:....!....:.       �
*�      4. ....:....!....:.       �
*�      5. ....:....!....:.       �
*�      6. ....:....!....:.       �
*�      7. ....:....!....:.       �
*�      8. ....:....!....:.       �
*�                                �
*�< OK Ctrl-W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 13, 36, "ｯ����┘ 筰�Κ��"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  2, 7 SAY " 1." GET fs_names( 1) VALID V_Flg( 1) WHEN n_flags >=  0
@  3, 7 SAY " 2." GET fs_names( 2) VALID V_Flg( 2) WHEN n_flags >=  1
@  4, 7 SAY " 3." GET fs_names( 3) VALID V_Flg( 3) WHEN n_flags >=  2
@  5, 7 SAY " 4." GET fs_names( 4) VALID V_Flg( 4) WHEN n_flags >=  3
@  6, 7 SAY " 5." GET fs_names( 5) VALID V_Flg( 5) WHEN n_flags >=  4
@  7, 7 SAY " 6." GET fs_names( 6) VALID V_Flg( 6) WHEN n_flags >=  5
@  8, 7 SAY " 7." GET fs_names( 7) VALID V_Flg( 7) WHEN n_flags >=  6
@  9, 7 SAY " 8." GET fs_names( 8) VALID V_Flg( 8) WHEN n_flags >=  7
*@ 10, 7 SAY " 9." GET fs_names( 9) VALID V_Flg( 9) WHEN n_flags >=  8
*@ 11, 7 SAY "10." GET fs_names(10) VALID V_Flg(10) WHEN n_flags >=  9
*@ 12, 7 SAY "11." GET fs_names(11) VALID V_Flg(11) WHEN n_flags >= 10
*@ 13, 7 SAY "12." GET fs_names(12) VALID V_Flg(12) WHEN n_flags >= 11
*@ 14, 7 SAY "13." GET fs_names(13) VALID V_Flg(13) WHEN n_flags >= 12
*@ 15, 7 SAY "14." GET fs_names(14) VALID V_Flg(14) WHEN n_flags >= 13
*@ 16, 7 SAY "15." GET fs_names(15) VALID V_Flg(15) WHEN n_flags >= 14
*@ 17, 7 SAY "16." GET fs_names(16) VALID V_Flg(16) WHEN n_flags >= 15

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  SAVE ALL LIKE fs_names TO (base_path+"FS_FLAGS.MEM")

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� V_Flg        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      �諷�� ├ ���� ━キ� 筰�Κ�.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03/27/1999 陳�
PROCEDURE V_Flg
PARAMETERS i0
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF i0 <= n_flags .AND. EMPTY(fs_names(i0))   && ���Ξ� 礒� °� ������キ 
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰 ━� 礒� �カ讌� �腮痰�碎!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF i0 = n_flags+1 .AND. .NOT. EMPTY(fs_names(i0))   && ���Ξ� ��錢┼瘴!
  n_flags = n_flags+1
ENDIF
RETURN .T.
