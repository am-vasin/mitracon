*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   ��ぃ�皰→� �����モ牀� �ムイ�腮.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Parm_TD
PARAMETER id_c    && ��鈑 - 皋� �゜キ�+��� ��玻メ���ぅ���

*
*   �┓� �゜キ��
*  SM   - �ムイ��� �� �甅�↓�� 瓷���
*  SF   - �ムイ��� � 筥����
*  RM   - �爬ガ � �甅�↓��� 瓷����
*  RF   - �爬ガ ├ 筥�����
*

PRIVATE i, sw       && ����!
*
*  ���珮Θ �. �. ��爛ぅ�キ�:
*    s_date - �����讚�� ���� ( 皰�讓� か� �ムイ�腮 ), �爬 √�ぅ - �遏��;
*    t_num  - ���ム ���モ� ( 皰�讓� か� �ムイ�腮 ), �爬 √�ぅ - �遏��.
*

USE TR_LOG ORDER TAG TR_NUM DESCENDING ALIAS TMP000 AGAIN
IF SEEK(id_c)                 && �薀� ��甄イ�銕 (!) �ムイ�艫!
  t_num = TR_NUM+1
  sw = .T.
  FOR i = 1 TO n_over
    SKIP
    IF EOF() .OR. TR_CODE+ID_CORR # id_c  && �� 磴�モ瘴 �皙���碎 �� n_over ���モ��...
      sw = .F.
      EXIT
    ENDIF
  ENDFOR
  IF sw
    s_date = TR_DATE
  ELSE
    USE PARMS ALIAS TMP000 AGAIN
    s_date = DAT_START
  ENDIF
ELSE
  USE PARMS ALIAS TMP000 AGAIN
  s_date = DAT_START
  t_num = 1
ENDIF
USE

RETURN
