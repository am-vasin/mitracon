*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ - ぎ��←キ�� 痰牀�� � 甎牀�.                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Dem_Line
PARAMETER item

PRIVATE ex, tit, tmpCode, tmpProd, s_sav

s_sav = SELECT()
SELECT 0
USE base_path+"STOCK" SHARED AGAIN ORDER TAG CODE ALIAS ST_4619
SELECT 0
USE base_path+"DEMAND" SHARED AGAIN ALIAS DM_4619
SCATTER MEMVAR BLANK
m.qty   = 1
m.who   = user
m.date  = DATE()
m.where = arm
m.producer = "        "
m.pro_code = 1

IF TYPE("item") = "N"
  = SEEK(m.item, "ST_4619")
  m.sto_code = ST_4619.CODE
  m.producer = ST_4619.PRODUCER
  m.pro_code = ST_4619.PRO_CODE
  m.prefix   = ST_4619.PREFIX
  m.name     = ST_4619.NAME
ELSE
  m.name = PADR(m.item, LEN(m.name))
ENDIF
tmpCode = m.pro_code
tmpProd = m.producer
  
ex = 1

*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*楽� ��Ж罔� 陳陳陳陳陳陳陳陳陳陳陳笥� ��爼� 陳陳笥���-〓審
*崖 旭旭 旭旭碓旭葦旭旭碓旭葦旭旭� 崖  旭旭碓旭  崖旭旭碓崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳拈陳陳陳陳陳陳拈陳陳陳抒
*�  �祗��. ��Л�腑�┘  旭旭碓旭葦旭旭碓旭葦旭旭碓旭葦旭   �
*�                                                        �
*�       ��ぅ�� ��������  旭旭碓旭葦旭旭碓旭葦旭�         �
*�                                                        �
*�           < OK Ctrl-W > < �皖���碎瘴 Esc >             �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
tit = "����←キ┘ 痰牀�� � 甎牀�"
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
DO D_W WITH "Demand", 11, 60, tit, 1
@ 2,  2 TO 4, 35
@ 2,  4 SAY " ��Ж罔� "
@ 2, 36 TO 4, 49
@ 2, 38 SAY " ��爼� "
@ 2, 50 TO 4, 57
@ 2, 51 SAY "���-〓"
@ 5,  4 SAY "�祗��. ��Л�腑�┘"
@ 7,  9 SAY "��ぅ�� ��������"
@ 3,  4 GET m.prefix WHEN m.sto_code = 0
@ 3,  9 GET m.name   WHEN m.sto_code = 0
@ 3, 39 GET tmpProd WHEN m.sto_code = 0 VALID Develop(tmpCode)
@ 3, 51 GET m.qty PICTURE "@Z 999999"
@ 5, 23 GET m.function
@ 7, 26 GET m.amodel
@ 9, 13 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID Sav_Bas()

RELEASE WINDOW Demand
USE IN DM_4619
USE IN ST_4619
SELECT (s_sav)

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ �諷��� ├ ゛����.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sav_Bas
PRIVATE mss
IF ex = 2
  RETURN .T.
ENDIF
IF m.sto_code = 0 .AND. EMPTY(m.name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��Ж罔� � �竅硅� ��Б��┘�? ��� �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
APPEND BLANK
m.pro_code = tmpCode
m.producer = tmpProd
GATHER MEMVAR
RETURN .T.
