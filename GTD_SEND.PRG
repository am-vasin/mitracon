*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Gtd_Send     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        �ムイ��� ��� � ��������.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 08/24/2000 様�
PROCEDURE Gtd_Send

PRIVATE cnt
DELETE FILE (tmpo_path+"GTD_SND0.DBF")

CREATE DBF (tmpo_path+"GTD_SND0.DBF") ;
  ( CODE     N( 7),  ;
    GTD_NUM  C(26),  ;
    GTD_LINE C( 4),  ;
    COUNTRY  C(16)   )

SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE DESCENDING

SELECT 0
USE (base_path+"STOCK")

cnt = 0

DO Wt_Mess WITH "��ぃ�皰→� ����諷"
SCAN
  cnt = cnt+1
  IF cnt % 100 = 0
    @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)), WCOLS())
  ENDIF
  SELECT ACT_S
  SEEK STOCK.CODE
  SCAN REST WHILE CODE = STOCK.CODE
    IF EMPTY(GTD_NUM)
      LOOP
    ELSE
      SELECT GTD_SND0
      APPEND BLANK
      REPLACE CODE     WITH STOCK.CODE,     ;
              GTD_NUM  WITH ACT_S.GTD_NUM,  ;
              GTD_LINE WITH ACT_S.GTD_LINE, ;
              COUNTRY  WITH ACT_S.COUNTRY
      EXIT
    ENDIF
  ENDSCAN
  SELECT STOCK
ENDSCAN

DO Wt_Mess

USE
SELECT ACT_S
USE
SELECT GTD_SND0

PRIVATE tmpPth

tmpPth = Int_Dir("C:\", "�磴� ��茱� ����襯?")
IF .NOT. EMPTY(tmpPth)
  DELETE FILE (tmpPth+"GTD_SEND.DBF")
  COPY TO (tmpPth+"GTD_SEND")
ENDIF

USE

DELETE FILE (tmpo_path+"GTD_SND0.DBF")

RETURN
