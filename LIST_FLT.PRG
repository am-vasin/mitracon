*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� List_Flt     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     �痰���→� 筥�赳�� か� 甎�瓷��.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 10.05.2005 様�
PROCEDURE List_Flt
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 蹍���� 陳陳陳陳陳陳陳陳陳陳朕 �
*� � [ ] 蹍����   [ ] �� �牀����  � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 �腑� 陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � [ ] �覩����   [ ] �� �覩����  � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 ｯ���� 瘍モ� 陳陳陳陳陳陳陳陳� �
*� � [ ] ｯ��腑�   [ ] �� ����腑�  � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 �牀�� 陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � [ ] �牀�� [ ] �ェム� [ ] �痰. � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�  < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
PRIVATE sl, n_sl, bl, n_bl, pay, n_pay, br, res, rst
STORE .T. TO sl, n_sl, bl, n_bl, pay, n_pay, br, res, rst
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 16, 39, "�┼赳�"
DO Sun_Bord WITH   2,  3,  4, 35, " 蹍���� "
DO Sun_Bord WITH   5,  3,  7, 35, " �腑� "
DO Sun_Bord WITH   8,  3, 10, 35, " ｯ���� 瘍モ� "
DO Sun_Bord WITH  11,  3, 13, 35, " �牀�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@   3,  5 GET sl    PICTURE "@*C 蹍����"
@   3, 19 GET n_sl  PICTURE "@*C �� �牀����"
@   6,  5 GET bl    PICTURE "@*C �覩����"
@   6, 19 GET n_bl  PICTURE "@*C �� �覩����"
@   9,  5 GET pay   PICTURE "@*C ｯ��腑�"
@   9, 19 GET n_pay PICTURE "@*C �� ����腑�"
@  12,  5 GET br    PICTURE "@*C �牀��"
@  12, 15 GET res   PICTURE "@*C �ェム�"
@  12, 26 GET rst   PICTURE "@*C �痰."

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  PRIVATE s_sav, cur_v
  *
  * �矗���硅��ガ ゛���
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"LIST_DET") SHARED AGAIN ORDER TAG LIST ALIAS LLL_10505
  SELECT 0
  USE (base_path+"BILLS") SHARED AGAIN ORDER TAG DOC_NUM ALIAS BBB_10505
  SELECT 0
  USE (base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_ARM ALIAS TTT_10505
  SET RELATION TO BILL_FIRM+BILL_YEAR+BILL_NUM INTO BBB_10505
  SEEK arm
  SCAN REST WHILE LST_ARM = arm
    cur_v = ""
    DO CASE
    CASE .NOT. sl .AND. .NOT. EMPTY(SAL_NUM)
      cur_v = "*"
    CASE .NOT. n_sl .AND. EMPTY(SAL_NUM)
      cur_v = "*"
    CASE .NOT. bl .AND. .NOT. EMPTY(BILL_NUM)
      cur_v = "*"
    CASE .NOT. n_bl .AND. EMPTY(BILL_NUM)
      cur_v = "*"
    CASE .NOT. pay .AND. "ｯ��腑��" $ BBB_10505.STATUS
      cur_v = "*"
    CASE .NOT. n_pay .AND. .NOT. "ｯ��腑��" $ BBB_10505.STATUS
      cur_v = "*"
    CASE .NOT. br .OR. .NOT. res .OR. .NOT. rst .AND. EMPTY(SAL_NUM)
      IF Br_Exist()
        IF EMPTY(REQ_DATE)
          IF .NOT. m.res
            cur_v = "*"
          ENDIF
        ELSE
          IF .NOT. m.br
            cur_v = "*"
          ENDIF
        ENDIF
      ELSE
        IF .NOT. m.rst
          cur_v = "*"
        ENDIF
      ENDIF
    ENDCASE
    REPLACE FILTER WITH cur_v
  ENDSCAN
  USE IN BBB_10505
  USE IN TTT_10505
  USE IN LLL_10505
  SELECT (s_sav)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Br_Exist     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ����腮� ÷��� (爛Д燿�) � 甎�瓷�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 25.01.2008 陳�
PROCEDURE Br_Exist

PRIVATE l_n, ret_v
m.l_n = STR(TTT_10505.LST_NUM, 6)
m.ret_v = .F.

SELECT LLL_10505
SEEK m.l_n
SCAN REST WHILE LIST_NUM = m.l_n
  IF .NOT. EMPTY(QNT_REQ)
    m.ret_v = .T.
    EXIT
  ENDIF
ENDSCAN

SELECT TTT_10505

RETURN m.ret_v
