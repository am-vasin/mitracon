*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Rev_Tit      ��о�｀砒┴ ��むォ ��瓱�           06.05.97 09:39:47 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�          ��爼�牀���┘ ����皰→� �爬ガ� �� ���祚�皀��.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Rev_Tit
PRIVATE w_code, w_name
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2, s_tag
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"
f_c000 = def_firm
f_n000 = ""
f_c_old = def_firm
w_code = 0
w_name = SPACE(50)

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�        踳く爬閧┘ 999 9999999999999999 (F1)         �
*� 敖 [ ] ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �....:....!....:....!....:....!....:....!....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�           < OK Ctrl-W > < �皖���碎瘴 Esc >           �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, c_sw
ex = 1
c_sw = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 58, "�������皀 ゛���"
DO Sun_Bord WITH  3,  3,  5, 54, "            "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO Get_Firm WITH 2, 10, enab_firm
@ 3, 6 GET c_sw PICTURE "@*C ��┘��" VALID C_Cust()
@ 4, 4 GET w_name WHEN .F.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_B()
USE
SELECT INC_TIT

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  IF RECCOUNT("TMP_INC") # 0
    SELECT TMP_INC
    ZAP
    SELECT INC_TIT
  ENDIF
  IF RECCOUNT("TMP_NEW") # 0
    SELECT TMP_NEW
    ZAP
    SELECT INC_TIT
  ENDIF

  SCATTER TO arr_w BLANK
  SET DELETED OFF
  LOCATE FOR DELETED()
  IF FOUND()
    RECALL
    GATHER FROM arr_w
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE FIRM      WITH f_c000,   ;
          SUP_CODE  WITH w_code,   ;
          SUP_NAME  WITH w_name,   ;
          FILE_TYPE WITH 1,        ;
          BUY       WITH 1,        ;
          IS_PRICE  WITH "�"       ;
          LET_INC   WITH .T.       ;
          LOCKED    WITH "***"
  SELECT INC_LIST
  m.s_tag = TAG()
  SET ORDER TO DEL
  GO TOP
  IF DELETED()
    RECALL
    BLANK
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE LIST_NO WITH STR(RECNO("INC_TIT"),5)
  SET DELETED ON
  SET ORDER TO TAG (m.s_tag)
  SELECT 0
  USE (log_path+"DUE_LOG")
  COPY STRU TO (tmp_path+"DL"+ALLTRIM(STR(RECNO("INC_TIT"))))
  USE
  SELECT INC_TIT
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� C_Cust       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             �襦�� ��┘���.                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10.02.1999 陳�
PROCEDURE C_Cust

IF EMPTY(w_code)
  GO TOP
ELSE
  SET ORDER TO TAG CUS_CODE
  SEEK w_code
  SET ORDER TO TAG CUS_NAME
ENDIF

IF Custs("", .T., .F., .T.)
  w_code = CUS_CODE
  w_name = CUS_NAME
  SHOW GET w_name
ENDIF

c_sw = .F.
SHOW GET c_sw

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_B        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �諷�� ├ ゛����.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10.02.1999 陳�
PROCEDURE Tst_B

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� �爛く爬閧┘..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(w_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� ��┘���..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
RETURN .T.
