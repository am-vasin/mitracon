*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� RemAdd       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  �エ�ぅ���罔� ����� ぎ�. ���キ���矣琺                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 23.03.2008 様�
PROCEDURE RemAdd
PARAMETER rst

PRIVATE wrk_path, err_sav, s_sav, nf, mss, err, mss1
SET TALK OFF
SET EXCLU OFF

m.mss = "OK"

m.err = 0
m.err_sav = ON("ERROR")
ON ERROR m.err = ERROR()
m.wrk_path = SYS(2004)+"BASE\"

DELETE FILE (m.wrk_path+"RemAdd.tmp")
DELETE FILE (m.wrk_path+"RemAdd.log")
m.s_sav = SELECT()
SELECT 0
USE (m.wrk_path+"ADD_STO") EXCLUSIVE

ON ERROR &err_sav
IF m.err # 0
  m.mss = "ERROR "+ALLTRIM(STR(m.err))
ELSE
  REINDEX
ENDIF
USE
SELECT (m.s_sav)

IF m.rst
  DIMENSION mss1[3]
  IF "ERROR" $ mss
    mss1[1] = ""
    mss1[2] = CHR(0)+"�� 磴�モ瘴 ��紜�皋碎 ��゛�聶 か� 爛┃ぅ���罔�. ��≒�爬皀 ��Е�."
    mss1[3] = ""
    DO Out_Mess WITH 7, "mss1"
  ELSE
    mss1[1] = ""
    mss1[2] = CHR(0)+"�エ�ぅ���罔� �牀茫� 竅�ヨ��"
    mss1[3] = ""
    DO Out_Mess WITH 5, "mss1"
  ENDIF
  RETURN
ELSE
  m.nf = FCREATE(m.wrk_path+"RemAdd.tmp")
  = FPUT(m.nf, m.mss)
  = FCLOSE(m.nf)
  RENAME (m.wrk_path+"RemAdd.tmp") TO (m.wrk_path+"RemAdd.log")
  WAIT WINDOW "���牀�皀 轤� ����" NOWAIT
  QUIT
ENDIF
