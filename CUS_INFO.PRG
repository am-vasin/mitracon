*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Cus_Info     ��о�｀砒┴ ��むォ ��瓱�           22.05.97 15:50:15 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�    ��┘��: ┃筮爼�罔� � ぅ��Ж皀, �爛え皀, ��ぅ爨��� � �爬�ョ���閻.    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Cus_Info
PARAMETER c_c     && ��� ��┘���

IF c_c = 0
  RETURN
ENDIF

PRIVATE s_sav, is_acc, r_sav, t_sav

s_sav = SELECT()
is_acc = USED("ACCOUNT")
IF is_acc
  SELECT ACCOUNT
  r_sav = RECNO()
  t_sav = TAG()
  SET ORDER TO TAG CUS_CODE
ELSE
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
ENDIF

SEEK c_c
DO V_Note

IF is_acc
  SET ORDER TO (t_sav)
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ELSE
  USE
ENDIF

SELECT (s_sav)

RETURN
