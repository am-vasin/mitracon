*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  U_Double      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                    ก๊ฅคจญฅญจฅ ฎคจญ ชฎข๋ๅ ฏฎงจๆจฉ.                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 28.12.1998 ออผ
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
