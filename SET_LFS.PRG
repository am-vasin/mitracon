*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Set_LFS      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�       蹍罐ゃ�� �腮痰��/竅����→� 筰�Κ�� か� 甎�瓷� (�牀��Θ).        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 27.03.1999 様�
PROCEDURE Set_LFS
PRIVATE fs_names, n_flags, vl, i
DIMENSION vl(16)

IF File_O(base_path+"FS_FLAGS.MEM")
  RESTORE FROM (base_path+"FS_FLAGS.MEM") ADDITIVE
ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �え� 筰�Ξ� �� ������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
n_flags = 0
FOR i = 1 TO 8
  IF EMPTY(fs_names(i))
    EXIT
  ELSE
    n_flags = n_flags+1
  ENDIF
ENDFOR
IF n_flags = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �え� 筰�Ξ� �� ������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

FOR i = 1 TO 16
  vl(i) = .NOT. EMPTY(SUBSTR(LIST_TIT.LST_FLAGS,i,1))
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
*�      [ ] ....:....!....:.      �
*�      [ ] ....:....!....:.      �
*�      [ ] ....:....!....:.      �
*�      [ ] ....:....!....:.      �
*�< OK Ctrl-W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH n_flags+4, 36, "���Κ� か� 甎�瓷�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
FOR i = 1 TO n_flags
  @ 1+i, 8 GET vl(i) PICTURE "@*C "+fs_names(i)
ENDFOR

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  ex = ""
  FOR i = 1 TO 16
    ex = ex +IIF(vl(i), "*", " ")
  ENDFOR
  REPLACE LIST_TIT.LST_FLAGS WITH ex
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
