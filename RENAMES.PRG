*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Renames      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     �窶��� ��玻オ罔� ���キ���矣琺.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 11/03/1998 様�
PROCEDURE Renames
PARAMETER context

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
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

s_sav = SELECT()
SELECT 0

DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P8B03 AGAIN

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS A8B03 AGAIN

SELECT 0
USE (base_path+"DEVELOP") ORDER TAG CODE ALIAS D18B03 AGAIN

SELECT 0
USE (base_path+"DEVELOP") ORDER TAG CODE ALIAS D28B03 AGAIN

SELECT 0
USE (base_path+"STO_LOG") ALIAS S8B03 AGAIN
SET RELATION TO OLD_PRO  INTO D18B03
SET RELATION TO NEW_PRO  INTO D28B03 ADDITIVE
SET RELATION TO WHO_CORR INTO P8B03  ADDITIVE
SET RELATION TO WHERE_C  INTO A8B03  ADDITIVE
GO BOTTOM

*
*   �����モ琺 か� ���瓷�
*
PRIVATE s_pattern, s_dir, s_all
PRIVATE choose, ret_v
ret_v = 0
IF TYPE("context") = "C"
  choose = .T.
  s_pattern = context
ELSE
  s_pattern = ""
  choose = .F.
ENDIF
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF choose
  DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"
ENDIF

DEFINE PAD Seek_1 OF (menu_name) PROMPT "���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek_1 OF (menu_name) DO Sw_Mode WITH "Seek_1"

DEFINE PAD Seek_2 OF (menu_name) PROMPT "蹍ぎ�Θ碎 ���瓷" KEY Ctrl-A, "Ctrl-F7"
ON SELECTION PAD Seek_2 OF (menu_name) DO Sw_Mode WITH "Seek_2"

DEFINE PAD Cnt_M OF (menu_name) PROMPT "��め腑� �ムエ�キ����┤" KEY Ctrl-A, "F1"
ON SELECTION PAD Cnt_M OF (menu_name) DO Sw_Mode WITH "Cnt_M"

IF choose
  DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ELSE
  DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ENDIF
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

s_dir  = 1
s_all  = 1

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, cnt_sav
ln = WROWS("")-9   && ����腑痰〓 ※え�諷 痰牀� BROWSE
snt_sav = SET("CENTURY") = "OFF"
SET CENTURY ON
wd = IIF(SET("CENTURY") = "ON", 72, 70)
*
*敖����陳陳陳陳���牀� ��━キ����┘陳陳陳陳陳陳��〓� ��━キ����┘陳陳陳陳陳�
*���.��.���� 臼臼 臼臼臼臼臼臼臼臼臼臼臼臼� 臼臼 臼臼臼臼臼臼臼臼臼臼臼臼嘘
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*敖�皰 ��玻オ皋牀���陳陳陳陳陳���｀腑� �メ皰陳陳��爼�陳陳��爼�陳陳�爛�鐵陳�
*咳臼臼臼臼臼臼臼臼臼臼臼臼臼� 臼臼臼臼臼臼臼臼 臼臼臼臼 臼臼臼臼 臼臼臼臼�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

DO D_Wins WITH ln, wd, "�窶��� ��玻オ罔� ���キ���矣琺", 0, 1
@ 1, 2 SAY "�����陳陳陳陳���牀� ��━キ����┘陳陳陳陳陳陳��〓� ��━キ����┘"
@ WROWS()-3, 2 SAY "��皰 ��玻オ皋牀���陳陳陳陳陳���｀腑� �メ皰陳陳��爼�陳陳��爼�陳陳�爛��"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    IF choose
      ON KEY LABEL Enter KEYBOARD CHR(23)
    ENDIF
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek_1"
    ON KEY LABEL Ctrl+F7 DO Sw_Mode WITH "Seek_2"
    ON KEY LABEL F1 DO Sw_Mode WITH "Cnt_M"
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM

*
*    BROWSE - �キ�
*
    BROWSE FIELDS DAT_CORR:P="@D":H="", ;
                  OLD_PRF:4:H="",  ;
                  OLD_NAM:25:H="", ;
                  NEW_PRF:4:H="",  ;
                  NEW_NAM:25:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      IF choose
        what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
      ELSE
        what_do = "Exit"
      ENDIF
    ENDIF

  CASE what_do = "Choose"    && �襦��

    ret_v = CODE
    EXIT

  CASE what_do = "Seek_1"    && ���瓷

    DO Seek_1
    what_do = "List"

  CASE what_do = "Seek_2"    && 蹍ぎ�Ε�┘ ���瓷�

    DO Seek_2 WITH s_pattern, s_dir, 2
    what_do = "List"

  CASE what_do = "Cnt_M"     && ��ょモ �ムエ�キ����┤

    DO Cnt_Move
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT P8B03
USE
SELECT A8B03
USE
SELECT D18B03
USE
SELECT D28B03
USE
SELECT S8B03
USE
SELECT (s_sav)
IF snt_sav
  SET CENTURY OFF
ENDIF

RETURN ret_v

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
PRIVATE u_name, arm_name
u_name = ""
arm_name = A8B03.ARM_N
IF S8B03.WHERE_C = "***"
  arm_name = "��痰�∫┴"
ELSE
  IF WHO_CORR = 0
    u_name = "��� ��玻オ皋牀���!"
  ELSE
    u_name = ALLTRIM(P8B03.FAMILY)+" "+LEFT(P8B03.NAME,1)+" "+LEFT(P8B03.S_NAME,1)
  ENDIF
ENDIF
u_name = PADR(u_name,28)
arm_name = PADR(arm_name, FSIZE("ARM_N", "A8B03"))

@ WROWS()-2, 2 SAY u_name COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY arm_name COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY LEFT(D18B03.B_NAME,8) COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY LEFT(D28B03.B_NAME,8) COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY TIM_CORR COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_1       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ������キ┘ ��゛�肓 ���瓷�.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.11.1998 陳�
PROCEDURE Seek_1

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 ��゛�� か� ���瓷� 陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �                                             � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 �����モ琺 ���瓷� 陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �  ( ) �� ≡ガ 甎�瓷�  ( ) � 皀�竕ォ ��Ж罔�  � �
*� � ( ) �� ����� � ������ ( ) �� ������ � ���聶 � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE w_pattern, w_dir, w_all
PRIVATE ex
w_pattern = s_pattern
w_dir = s_dir
w_all = s_all
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 11, 53, "���皀�痰�覃 ���瓷"
DO Sun_Bord WITH  2,  3,  4, 49, " ��゛�� か� ���瓷� "
DO Sun_Bord WITH  5,  3,  8, 49, " �����モ琺 ���瓷� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 5 EDIT w_pattern SIZE 1, 43 PICTURE "@K" COLOR (SCHEME(1,1))
@ 6, 6 GET w_all PICTURE "@*RH �� ≡ガ 甎�瓷� ;� 皀�竕ォ ��Ж罔�"
@ 7, 5 GET w_dir PICTURE "@*RH �� ����� � ������;�� ������ � ���聶"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  s_pattern = w_pattern
  s_dir = w_dir
  s_all = w_all
  DO Seek_2 WITH s_pattern, s_dir, s_all
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)PRETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_2       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��＝癶キ�� ���瓷.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.11.1998 陳�
PROCEDURE Seek_2
PARAMETERS patt, dir, all

PRIVATE r_sav, dir_w
r_sav = RECNO()
dir_w = IIF(dir = 1, -1, 1)

DO Wt_Mess WITH "���瓷..."
IF all = 1
  IF dir_w = 1
    GO TOP
  ELSE
    GO BOTTOM
  ENDIF
ELSE
  SKIP dir_w
ENDIF

DO WHILE .NOT. BOF() .AND. .NOT. EOF()
  IF patt $ OLD_PRF+" "+OLD_NAM .OR. ;
     patt $ NEW_PRF+" "+NEW_NAM
    EXIT
  ENDIF
  SKIP dir_w
ENDDO

DO Wt_Mess

IF EOF() .OR. BOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��゛�� �� ���ぅ�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

RETURN
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Cnt_Move     ��о�｀砒┴ ���┃� ���矗┘���                 �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ����腑痰〓 �ムエ�キ����┤ ��Ж罔�.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 19.04.2002 陳�
PROCEDURE Cnt_Move
PRIVATE s_sav, d01, d02, mss, cnt_s

s_sav = SELECT()
d01 = DATE() - 30
d02 = DATE()

DO G_Period WITH d01, d02
SELECT S8B03
COUNT TO cnt_s FOR DAT_CORR >= d01 .AND. DAT_CORR <= d02

IF cnt_s = 0
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �ム┏� � "+DTOC(d01)+" �� "+DTOC(d02)+" �ムエ�キ������ ��Ж罔�"
  mss(3) = CHR(0)+" �� �牀〓え��瘡."
  mss(4) = ""
  DO Out_Mess WITH 5, "mss"
ELSE
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �ム┏� � "+DTOC(d01)+" �� "+DTOC(d02)+" °�� �牀▲ぅ�� "+ALLTRIM(STR(cnt_s))
  mss(3) = CHR(0)+" �ムエ�キ����┤ ��Ж罔�."
  mss(4) = ""
  DO Out_Mess WITH 5, "mss"
ENDIF  

SELECT(s_sav)
RETURN