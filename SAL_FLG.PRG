*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Sal_Flg      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�           ��玻オ罔� 筰�Κ�� �ク�痼イ痰▲��� �ムイ �牀��Ε�.            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 13.05.1999 様�
PROCEDURE Sal_Flg

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*
PRIVATE fs_names
IF .NOT. File_O(base_path+"FS_FLAGS.MEM")
  RETURN
ENDIF
RESTORE FROM (base_path+"FS_FLAGS.MEM") ADDITIVE
IF EMPTY(fs_names(1))
  RETURN
ENDIF

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
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� [ ] ....:....!....:.  [ ] ....:....!....:. �
*� [ ] ....:....!....:.  [ ] ....:....!....:. �
*� [ ] ....:....!....:.  [ ] ....:....!....:. �
*� [ ] ....:....!....:.  [ ] ....:....!....:. �
*�                                            �
*�      < OK Ctrl-W > < �皖���碎瘴 Esc >      �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, v_flags, i, s_sav
ex = 1
DIMENSION v_flags(16)
FOR i = 1 TO 16
  v_flags(i) = .NOT. EMPTY(SUBSTR(LIST_TIT.LST_FLAGS,i,1))
ENDFOR

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 52, "���Κ�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
FOR i = 1 TO 4
  IF EMPTY(fs_names(i))
    @ i+1, 3 GET v_flags(i) PICTURE "@*C �ェム�" DISABLE
  ELSE
    @ i+1, 3 GET v_flags(i) PICTURE "@*C "+fs_names(i)
  ENDIF
ENDFOR
FOR i = 1 TO 4
  IF EMPTY(fs_names(i+4))
    @ i+1, 25 GET v_flags(i+4) PICTURE "@*C �ェム�" DISABLE
  ELSE
    @ i+1, 25 GET v_flags(i+4) PICTURE "@*C "+fs_names(i+4)
  ENDIF
ENDFOR
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  ex = ""
  FOR i = 1 TO 16
    ex = ex+IIF(v_flags(i), "*", " ")
  ENDFOR
  s_sav = SELECT()
  SELECT LIST_TIT
  REPLACE LST_FLAGS WITH ex
  SELECT (s_sav)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
