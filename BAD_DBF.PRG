*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          蹍������ 筮爼�牀����� 甎�瓷� �甎�瑯キ諷 DBF-������.         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Bad_Dbf

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

PRIVATE ex, nm_w     &&
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
nm_w = PADR(base_path,40)   &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 69, "蹍▲爲� DBF-������..."
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                                                 �
*� 蹍▲瑙ガ�� え爛�皰爬� ....:....1....:....2....:....3....:....4 �
*�                                                                 �
*�                < OK Ctrl-W > < �皖���碎瘴 Esc >                 �
*�                                                                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�


*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  3 SAY "蹍▲瑙ガ�� え爛�皰爬�" GET nm_w PICTURE "@K"
@ 5, 18 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "


READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 2 .OR. EMPTY(nm_w)
  RETURN
ENDIF
*--------------------------------------------------------------------------

nm_w = ALLTRIM(nm_w)

PRIVATE sav_int, bad_list, bad_len, dbf_list, err_c, dbf_len, mss
DIMENSION bad_list(1)
dbf_len = ADIR(dbf_list,nm_w+"*.DBF")
IF dbf_len = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��爛�皰爬� "+nm_w+" �� 甌ぅ爨�� �� �き��� DBF-�����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

sav_int = ON("ERROR")
err_c = 0
ON ERROR err_c = ERROR()
bad_len = 0
FOR ex = 1 TO dbf_len
  WAIT WINDOW dbf_list(ex, 1) NOWAIT
  err_c = 0
  USE (nm_w+dbf_list(ex, 1))
  IF err_c # 0
    bad_len = bad_len+1
    DIMENSION bad_list(bad_len)
    bad_list(bad_len) = PADR(dbf_list(ex, 1),13)+STR(err_c,5)
  ENDIF
ENDFOR
USE
ON ERROR &sav_int

IF bad_len = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�荐｀� �モ!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  RETURN
ENDIF  

PRIVATE view_l

view_l = MIN(WROWS("")-8, bad_len)

DO D_Win_N WITH view_l+3, 22, "�荐！�..."

ex = 1
DO WHILE .T.
  ex = MAX(ex,1)
  ex = MIN(ex, bad_len)
  @ 1,1 MENU bad_list, bad_len, view_l
  READ MENU TO ex
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
ENDDO

RELEASE WINDOW (win_name)

RETURN
