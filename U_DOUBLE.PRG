*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� U_Double     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    ｡螢え�キ┘ �え�����諷 ��Ж罔�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 28.12.1998 様�
PROCEDURE U_Double
PARAMETERS l_num

PRIVATE s_sav, q0, q_s, q_d

s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_CODE ALIAS _8C28 AGAIN
PRIVATE c_sav, r_sav
SEEK l_num
c_sav = 0
SCAN REST WHILE l_num = LIST_NUM FOR CODE # 0
  IF CODE = c_sav
    q0  = QNT
    q_s = QNT_REQ
    q_d = QNT_DUE
    DELETE
    GO r_sav
    REPLACE QNT WITH QNT+q0, QNT_REQ WITH QNT_REQ+q_s, QNT_DUE WITH QNT_DUE+q_d
    LOOP
  ENDIF
  c_sav = CODE
  r_sav = RECNO()
ENDSCAN
USE
SELECT (s_sav)

RETURN
