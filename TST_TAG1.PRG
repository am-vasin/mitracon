*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Tst_Tag1     ��о�｀砒┴ ��むォ ��瓱�           20.11.97 19:00:04 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   �襦�� ����� か� �牀▲爲� ┃ぅ���.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Tst_Tag1

PRIVATE s_path, f_name, t_list, t_numb, t_choose, i, ln

DO Set_Lwr

s_path = base_path
DO WHILE .T.

  f_name = Int_File(s_path, "DBF", "�襦ム�皀 ���� か� �牀▲爲�")
  IF EMPTY(f_name)
    EXIT
  ENDIF
  USE (f_name)
  t_numb = 0
  s_path = LEFT(f_name,RAT("\",f_name))
  DO WHILE .T.
    IF EMPTY(TAG(t_numb+1))
      EXIT
    ENDIF
    t_numb = t_numb+1
    DIMENSION t_list(t_numb)
    t_list(t_numb) = TAG(t_numb)
  ENDDO
  IF t_numb = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�襦����覃 ���� ���� �� ━ゥ� �� �き��� ┃ぅ���!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    LOOP
  ENDIF
  ln = 0
  FOR i = 1 TO t_numb
    ln = MAX(ln, LEN(t_list(i)))
  ENDFOR
  ln = ln+2
  FOR i = 1 TO t_numb
    t_list(i) = PADR(t_list(i),ln)
  ENDFOR
  t_choose = 1
  DO WHILE .T.
    t_choose = Get_Tag(t_choose,t_list)
    IF EMPTY(t_choose)
      EXIT
    ENDIF
    DO Tst_Tag WITH f_name, ALLTRIM(t_list(t_choose))
  ENDDO
ENDDO

CLOSE DATABASES
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                             �襦�� ┃ぅ���.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 21.11.97 11:33:23 夕
PROCEDURE Get_Tag
PARAMETER i_choose, lst
EXTERNAL ARRAY lst
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
*�     敖陳陳陳陳陳陳陳陳陳陳朕     �
*�     �                      �     �
*�     �                      �     �
*�     �                      �     �
*�     �                      �     �
*�     �                      �     �
*�     �                      �     �
*�     青陳陳陳陳陳陳陳陳陳陳潰     �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, ln, wd, c1, c2
ex = 1
wd = MAX(38, LEN(lst(1))+6)
c1 = FLOOR((wd-LEN(lst(1))-2)/2)
c2 = FLOOR((wd-32)/2)
ln = MIN(ALEN(lst), 15)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln+6, wd, "��ぅ�瘠"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, c1 GET i_choose FROM lst SIZE ln+2, LEN(lst(1))+2
@ WROWS()-2, c2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

i_choose = IIF(ex = 1, i_choose, 0)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN i_choose
