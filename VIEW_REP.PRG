*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� View_Rep     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         ����→� �� 爛��├�罔�                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 01.04.2006 様�
PROCEDURE View_Rep
PARAMETERS c_code

PRIVATE qnt_i, qnt_s, qnt_sr, qnt_r, qnt_rp

STORE 0 TO m.qnt_i, m.qnt_s, m.qnt_sr, m.qnt_r, m.qnt_rp
DO Q_Sale
DO Q_Inc
DO Q_Rep

? "ｯ爬絎ぎ����     "+STR(m.qnt_i)
? "�瓮�� �牀����    "+STR(m.qnt_s)
? "�� 〓茫� � �砒モ "+STR(m.qnt_s-qnt_sr)
? "��茫� � �砒モ    "+STR(m.qnt_rp)
? "�痰�皰�          "+STR(m.qnt_r)


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Q_Sale       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                 �瓮�� �牀���� ├ ▼閧��� �� 爛��├�罔�                 �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01.04.2006 陳�
PROCEDURE Q_Sale

PRIVATE s_sav, tmp_rep

m.s_sav = SELECT()
SELECT 0

USE (m.base_path+"SALE") SHARED AGAIN ALIAS TMP_6401 ORDER TAG CODE
USE (m.base_path+"ACT_S_T") SHARED AGAIN ALIAS PRT_6401 ORDER TAG DOC_NUM IN 0
SET RELATION TO ACT_FIRM+LEFT(DTOS(ACT_DATE), 4)+ACT_NUM INTO PRT_6401
m.tmp_rep = 0
SEEK m.c_code
SCAN REST WHILE CODE = m.c_code FOR PRT_6401.FLG_BUY = "R"
  m.tmp_rep = m.tmp_rep+QNT_REP
  m.qnt_s   = m.qnt_s+QNT
ENDSCAN

m.qnt_sr = m.qnt_s-m.tmp_rep
USE
USE IN PRT_6401
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Q_Inc        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      �瓮�� �爬�閧� �� 爛��├�罔�                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01.04.2006 陳�
PROCEDURE Q_Inc

PRIVATE s_sav, tmp_rep

m.s_sav = SELECT()
SELECT 0

USE (m.base_path+"ACT_S") SHARED AGAIN ALIAS TMP_6401 ORDER TAG CODE

SEEK m.c_code
SCAN REST WHILE CODE = m.c_code FOR FLG_BUY = "R"
  m.qnt_i = m.qnt_i+QNT_INC
  m.qnt_r = m.qnt_r+REST_REP
ENDSCAN

USE
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Q_Rep        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �瓮�� � �砒モ��                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01.04.2006 陳�
PROCEDURE Q_Rep
PRIVATE s_sav, tmp_rep

m.s_sav = SELECT()
SELECT 0

USE (m.base_path+"REPORT") SHARED AGAIN ALIAS TMP_6401 ORDER TAG CODE

SEEK STR(m.c_code)
SCAN REST WHILE CODE = m.c_code
  m.qnt_rp   = m.qnt_rp+QNT
ENDSCAN

USE
SELECT (m.s_sav)

RETURN

