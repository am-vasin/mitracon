*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� NY_Mail      ��о�｀砒┴ �. ��瓱�               20.12.96 14:56:58 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      蹍������ ��〓��きォ ��艪�.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                  �甅�↓�� �牀������ ��〓��きォ ��艪�.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 20.12.96 14:58:06 夕
PROCEDURE NY_Mail

IF .NOT. File_O(base_path+"NY_Mail.DBF") .OR. .NOT. File_O(base_path+"NY_Mail.FPT")
  RETURN
ENDIF

USE NY_Mail

DO View_M       && ����щ��ガ ��艪�...

IF Want_Add()   && 踳か���ガ ぎ��※碎 艪�-�─磴� �� 瓮￥ �...
  DO Add_M      &&      ...ぎ��←錺�!
ENDIF

USE

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            蹍甃�矗 ��艪�.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 20.12.96 15:03:24 夕
PROCEDURE View_M
PRIVATE s, win_name, ln, wd

win_name = PROGRAM()
s = ""
SCAN FOR .NOT. EMPTY(NY_MESS)
  s = s+NY_MESS+CHR(13)+REPLICATE("�",74)+CHR(13)
ENDSCAN

ln = WROWS("")-6
wd = 78

DO D_Win_N WITH ln, wd, "��〓��き闖 ��艪�!"

@ 2,2 EDIT s SIZE ln-3, wd-4 NOMODIFY
READ

RELEASE WINDOW (win_name)

RETURN
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 ���牀�: ぎ��※� �� 艪�-�─磴� �� 瓮￥?                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 20.12.96 15:44:51 夕
PROCEDURE Want_Add
PRIVATE s, win_name, ln, wd

win_name = PROGRAM()
*                    ��������!
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*楽陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖                                               崖
*崖    �� ��Ε皀 ぎ��※碎 艪�-�─磴� �� 瓮￥. 踸 崖
*崖 轤�� ��茱 ��Г��←キ┘ 磚┐閧 ≡� 皀, �皰 ��- 崖
*崖 ｀��モ � �牀�������� ������� � ( ┼� ) �����! 崖
*崖                                               崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*� < �� ぎ��←�! Ctrl_W > < � 甄イ竡薑� ���. Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

ln = 11
wd = 53
s = 1

DO D_Win_N WITH ln, wd, "��������!"
DO Sun_Bord WITH 2, 2, 8, 50
@ 4, 4 SAY "   �� ��Ε皀 ぎ��※碎 艪�-�─磴� �� 瓮￥. 踸"
@ 5, 4 SAY "轤�� ��茱 ��Г��←キ┘ 磚┐閧 ≡� 皀, �皰 ��-"
@ 6, 4 SAY "｀��モ � �牀�������� ������� � ( ┼� ) �����!"

@ 9, 3 GET s PICTURE "@*HT \! �� ぎ��←�! Ctrl_W ;\? � 甄イ竡薑� ���. Esc "
READ CYCLE

RELEASE WINDOW (win_name)

RETURN s = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                                ����瘡.                                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 20.12.96 15:03:24 夕
PROCEDURE Add_M
PRIVATE s, win_name, ln, wd, ex

win_name = PROGRAM()
s = ""

ln = 14
wd = 78
ex = 1

DO D_Win_N WITH ln, wd, "�ム���皀 艪�-�─磴�..."
SCROLL 2, 1, WROWS()-1, WCOLS()-1, 1

@ 2,2 EDIT s SIZE ln-4, wd-4
@ WROWS()-1, 20 GET ex PICTURE "@*HT \! ������碎 Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

RELEASE WINDOW (win_name)

IF ex = 1 .AND. .NOT. EMPTY(s)
  WAIT WINDOW "�������瑟 ���!" NOWAIT
  APPEND BLANK
  REPLACE WHO WITH user, WHERE WITH arm, NY_MESS WITH s
ENDIF

RETURN
