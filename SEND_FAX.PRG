*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Send_Fax     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  蹍������ �≒������� �皴��→� �����.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 16.10.1998 様�
PROCEDURE Send_Fax

PRIVATE w_code, w_phone, w_to, w_from, s_name, f_type
PRIVATE w_subj, w_name, w_exitto
PRIVATE s_sav, i

s_sav = SELECT()
w_phone = SPACE(20)
w_to    = SPACE(25)
s_name  = ""
SELECT 0
USE (send_log+"SENDMESS")
w_code  = 0
w_from  = PADR("��� ������", 50)
w_name  = ""
w_exitto= 1

IF .NOT. Fax_Scr()
  USE
  SELECT (s_sav)
  RETURN
ENDIF
*
*  ��爼�珮ガ ━� �����
*
w_name = SUBSTR("ABCDEFJHIJKLMNOPQRSTUVWXYZ", YEAR(DATE())-1997,1)+ ;
         SUBSTR("0123456789AB", MONTH(DATE()),1)+ ;
         RIGHT(STR(100+DAY(DATE()),3),2)
SELECT 0
USE (base_path+"PARMS")
DO WHILE .NOT. LOCK()
ENDDO
IF LEFT(LAST_SEND,4) = w_name
  w_name = w_name+RIGHT(STR(10000+VAL(RIGHT(LAST_SEND,4))+1,5),4)
ELSE
  w_name = w_name+"0000"
ENDIF
REPLACE LAST_SEND WITH w_name
UNLOCK
USE (send_log+"MATERS")
DO WHILE .NOT. LOCK()
ENDDO
w_code = SEND_MT
REPLACE SEND_MT WITH SEND_MT+1
UNLOCK
USE
w_name = send_path+w_name+RIGHT(s_name,4)
IF 
COPY FILE (s_name), (w_name)
DO Copy_D_W WITH s_name, w_name
SELECT SENDMESS

DO CASE
CASE w_exitto = 1
  w_exitto = " "
CASE w_exitto = 2
  w_exitto = "1"
CASE w_exitto = 3
  w_exitto = "2"
ENDCASE

APPEND BLANK
REPLACE CODE    WITH w_code,  ;
        PHONE   WITH w_phone, ;
        TO      WITH w_to, ;
        FROM    WITH w_from, ;
        SUBJECT WITH w_subj, ;
        NAME    WITH w_name, ;
        EXITTO  WITH w_exitto, ;
        FAX     WITH "*", ;
        DATE_ON WITH DATE(), ;
        WHO     WITH user, ;
        WHERE   WITH arm
USE
SELECT (s_sav)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�            ��皀���皋↓�� ������キ┘ ┃筮爼�罔� か� �矚覊��.            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 15.09.98 13:33:42 夕
PROCEDURE Fax_Scr
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 �ガ� 甌�♂キ�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �....:....!....:....!....:....!....:....!....:....!....:....!....� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� 敖 �� ���� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �       ....:....!....:....!....:....!....:....!....:....!       � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� 敖 ���� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �                   ....:....!....:....!....                     � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� 敖 ���� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �       ( ) ��牀め���  ( ) �ウゃ��牀き.  ( ) �ウゃ��牀き.        � �
*� �                   ���ム ....:....!....:....!                   � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�                 < OK Ctrl-W > < �皖���碎瘴 Esc >                   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 17, 72, "�矚覊�� �����"
DO Sun_Bord WITH  2, 3,  4, 68, " �ガ� 甌�♂キ�� "
DO Sun_Bord WITH  5, 3,  7, 68, " �� ���� "
DO Sun_Bord WITH  8, 3, 10, 68, " ���� "
DO Sun_Bord WITH 11, 3, 14, 68, " ���� "
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  9, 23 GET w_to
@ 12, 11 GET w_exitto PICTURE "@*RH ��牀め��� ;�ウゃ��牀き. ;�ウゃ��牀き."
@ 13, 23 SAY "���ム" GET w_phone
@ 15, 19 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
@  3,  4 GET w_subj
@  6, 11 GET w_from
READ CYCLE VALID Tst_Phone()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      蹍▲爲� ��玻オ皚�痰� 筮爼�.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 15.09.98 16:56:23 夕
PROCEDURE Tst_Phone
PRIVATE p_w, i, c

IF ex = 2
  RETURN .T.
ENDIF
p_w = ""
FOR i = 1 TO LEN(w_phone)
  c = SUBSTR(w_phone,i,1)
  IF ISDIGIT(c)
    p_w = p_w+c
  ENDIF
ENDFOR

IF EMPTY(p_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���ム ����� 礫���� �オ�玻オ皚� ┼� �� 礫���� 〓≡�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
w_phone = p_w

RETURN .T.
