*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Edit         ��о�｀砒┴ ��むォ ��瓱�           11.02.97 14:50:58 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           �イ��皰� 皀�痰��.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Edit

PRIVATE f_name

f_name = Int_File("", "*", "�イ��皋珮ガ覃 ����")
IF EMPTY(f_name)
  RETURN
ENDIF

DELETE FILE (tmpo_path+"EDI_TMP.TXT")

COPY FILE (f_name) TO (tmpo_path+"EDI_TMP.TXT")


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


*PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
*DO Prp_Nav_2
DO D_Win_N WITH WROWS("")-3, WCOLS("")-4, f_name
DEFINE WINDOW W_Ed FROM WLROW()+2, WLCOL()+2 TO WLROW()+WROWS()-2, WLCOL()+WCOLS()-3 ;
              NONE COLOR SCHEME 13

PRIVATE what_do
what_do = "Edit"
DO WHILE .T.
  DO CASE
  CASE what_do = "Edit"
    SET SYSMENU AUTOMATIC
    HIDE MENU _msysmenu
    ON KEY LABEL Escape DO Ab_Proc
    MODIFY COMMAND (tmpo_path+"EDI_TMP.TXT") WINDOW W_Ed
    ON KEY LABEL Escape
    what_do = IIF(what_do = "Edit", "Exit", what_do)
  CASE what_do = "Repeat"
     what_do = "Edit"
  CASE what_do = "Exit"
    DELETE FILE (f_name)
    COPY FILE (tmpo_path+"EDI_TMP.TXT") TO (f_name)
    EXIT
  OTHERWISE
    EXIT
  ENDCASE
ENDDO  
DELETE FILE (tmpo_path+"EDI_TMP.TXT")

*POP KEY
RELEASE WINDOW W_Ed
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ �÷�｀砒┴ ���※荐 Esc.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Ab_Proc
PRIVATE mss
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  � 艪�, ぅ�痰※皀�讚� ≡� ��＜ぅ�? [ ��, ��＜ぅ�! ] [ �モ, ▲爿ガ瘴! ]"
mss(3) = ""
DO WHILE INKEY() # 0   &&&&&&&&&&&&
ENDDO

IF Out_Mess(7, "mss") = 1
*  ACTIVATE WINDOW W_Ed
  KEYBOARD "{Ctrl+W}"
  what_do = "Abort"
ELSE
*  ACTIVATE WINDOW W_Ed
  KEYBOARD "{Ctrl+W}"
  what_do = "Repeat"
ENDIF
RETURN
