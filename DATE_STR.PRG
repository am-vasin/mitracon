********************************************************************************
*    ���� Date_Str       ���ࠡ��稪 ���३ ��ᨭ                22.03.2012    *
********************************************************************************
*                                                                              *
*                                ��� �ய����                                 *
*                                                                              *
********************************************************************************
PROCEDURE Date_Str
PARAMETERS prmDate
*
* prmDate - ���
*

PRIVATE dd, mm, yy

m.dd = DAY(m.prmDate)
m.mm = MONTH(m.prmDate)
m.yy = YEAR(m.prmDate)

DO CASE
CASE m.mm = 1
  m.mm = "������"
CASE m.mm = 2
  m.mm = "���ࠫ�"
CASE m.mm = 3
  m.mm = "����"
CASE m.mm = 4
  m.mm = "��५�"
CASE m.mm = 5
  m.mm = "���"
CASE m.mm = 6
  m.mm = "���"
CASE m.mm = 7
  m.mm = "���"
CASE m.mm = 8
  m.mm = "������"
CASE m.mm = 9
  m.mm = "�������"
CASE m.mm = 10
  m.mm = "������"
CASE m.mm = 11
  m.mm = "�����"
CASE m.mm = 12
  m.mm = "�������"
ENDCASE

RETUR '"'+STR(m.dd, 2)+'" '+m.mm+STR(m.yy, 5)+"�."
