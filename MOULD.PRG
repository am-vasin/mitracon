*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Mould     ��о�｀砒┴ ��むォ + ����             16.04.97 20:20:43 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     蹍������ 筮爼�牀����� ��゛���.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Mould
PARAMETERS tmpMONTH, tmpYEAR

**tmpMONTH - N(2), ���ム �メ閹�
**tmpYEAR  - N(4), ���
	
PRIVATE s_date, st

tmpDate = {}

s_date  = "01."+STR(tmpMONTH,2)+"." +STR(tmpYEAR,4)
s_date	= CTOD(s_date)

***����瓱� � �� 
	
st = ""
DO WHILE tmpMONTH = MONTH(s_date)
  st = st+IIF(INLIST(DOW(s_date), 1, 7), "�", "�")
  s_date = s_date + 1
ENDDO
APPEND BLANK
REPLACE MONTH WITH tmpMONTH, ;
        YEAR  WITH tmpYEAR,  ;
        DAYS  WITH st
RETURN
