*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                �ணࠬ�� - ���������� ��ப� � ���.                 �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
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
*��������������������������������������������������������Ŀ
*��� ������ ���������������������Ŀ�� ��ଠ ���Ŀڊ��-����
*�� ���� ������������������������� ��  ��������  ����������
*��������������������������������������������������������ٳ
*�  �㭪�. �����祭��  ��������������������������������   �
*�                                                        �
*�       ������ ������  �����������������������         �
*�                                                        �
*�           < OK Ctrl-W > < �⪠������ Esc >             �
*����������������������������������������������������������
tit = "���������� ��ப� � ���"
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
DO D_W WITH "Demand", 11, 60, tit, 1
@ 2,  2 TO 4, 35
@ 2,  4 SAY " ������ "
@ 2, 36 TO 4, 49
@ 2, 38 SAY " ��ଠ "
@ 2, 50 TO 4, 57
@ 2, 51 SAY "���-��"
@ 5,  4 SAY "�㭪�. �����祭��"
@ 7,  9 SAY "������ ������"
@ 3,  4 GET m.prefix WHEN m.sto_code = 0
@ 3,  9 GET m.name   WHEN m.sto_code = 0
@ 3, 39 GET tmpProd WHEN m.sto_code = 0 VALID Develop(tmpCode)
@ 3, 51 GET m.qty PICTURE "@Z 999999"
@ 5, 23 GET m.function
@ 7, 26 GET m.amodel
@ 9, 13 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
READ CYCLE VALID Sav_Bas()

RELEASE WINDOW Demand
USE IN DM_4619
USE IN ST_4619
SELECT (s_sav)

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                    �ணࠬ�� ��室� �� ������.                        �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Sav_Bas
PRIVATE mss
IF ex = 2
  RETURN .T.
ENDIF
IF m.sto_code = 0 .AND. EMPTY(m.name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������ � ����� ���������? ��� �� �뢠��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
APPEND BLANK
m.pro_code = tmpCode
m.producer = tmpProd
GATHER MEMVAR
RETURN .T.