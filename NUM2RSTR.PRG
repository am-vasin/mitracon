*=========================================================================
FUNCTION Num2RStr	&& ��᫮ �ய����
*-------------------------------------------------------------------------
PARAMETERS num_p
*
*  s   - ��ப� � ���ன �����頥��� १����;
*  t   - ��।����� த - ( 0 - ����, 1 - ����), �����頥���
*        ����稭� ��� �離�: 0 - �����, 1 - �����, 2 - �����;
*  num_p - �᫮.
*
PRIVATE s, num, t, t_w, s_tmp
PRIVATE n1, n2, n3

t = 0
num = ABS(num_p)

IF num = 0
  s = "����"
  t = 0
  RETURN s
ENDIF

n3 = num

s_tmp  = FLOOR (n3/1000)		&& 1 - 999
n1 = n3 - s_tmp*1000

n3 = s_tmp
s_tmp  = FLOOR (n3/1000)		&& 1,001 - 999,999
n2 = n3 - s_tmp*1000

n3 = s_tmp				&& 1,000,001 - 999,999,999

s_tmp = ""
IF n1 # 0
  DO Pr_3 WITH s_tmp, t, n1
  s_tmp = s_tmp
ELSE
  t = 0
ENDIF
s = ALLTRIM (Millions (n3)) + IIF (n2 = 0, "", " " +;
             Thousands (n2)) + " " + s_tmp

RETURN IIF(num_p<0, "����� ", "")+ALLTRIM (s)

*-------------------------------------------------------------------------
PROCEDURE Pr_3
*-------------------------------------------------------------------------
PARAMETERS s, t, num
PRIVATE s_tmp, n1, n2, n3

t_w   = 0
n3    = num
s_tmp = FLOOR (n3/10)			&& �������
n1    = n3 - s_tmp*10

n3    = s_tmp
s_tmp = FLOOR (n3/10)			&& ����⪨
n2    = n3 - s_tmp*10

n3    = s_tmp				&& �⭨

****	��⭨

DO CASE
  CASE n3 = 0
    s_tmp = ""
  CASE n3 = 1
    s_tmp = "��"
  CASE n3 = 2
    s_tmp = "�����"
  CASE n3 = 3
    s_tmp = "����"
  CASE n3 = 4
    s_tmp = "������"
  CASE n3 = 5
    s_tmp = "������"
  CASE n3 = 6
    s_tmp = "������"
  CASE n3 = 7
    s_tmp = "ᥬ���"
  CASE n3 = 8
    s_tmp = "��ᥬ���"
  CASE n3 = 9
    s_tmp = "��������"
ENDCASE

IF n2 = 1		&& ��ன ����⮪
  s_tmp = s_tmp + " "
  DO CASE
    CASE n1 = 0
      s_tmp = s_tmp + "������"
    CASE n1 = 1
      s_tmp = s_tmp + "����������"
    CASE n1 = 2
      s_tmp = s_tmp + "���������"
    CASE n1 = 3
      s_tmp = s_tmp + "�ਭ�����"
    CASE n1 = 4
      s_tmp = s_tmp + "���ୠ����"
    CASE n1 = 5
      s_tmp = s_tmp + "��⭠����"
    CASE n1 = 6
      s_tmp = s_tmp + "��⭠����"
    CASE n1 = 7
      s_tmp = s_tmp + "ᥬ������"
    CASE n1 = 8
      s_tmp = s_tmp + "��ᥬ������"
    CASE n1 = 9
      s_tmp = s_tmp + "����⭠����"
  ENDCASE
  t = t_w
  s = s_tmp
  RETURN
ELSE

  ****	����⪨

  IF n2 > 1
    s_tmp = s_tmp + " "
    DO CASE
      CASE n2 = 2
        s_tmp = s_tmp + "�������"
      CASE n2 = 3
        s_tmp = s_tmp + "�ਤ���"
      CASE n2 = 4
        s_tmp = s_tmp + "�ப"
      CASE n2 = 5
        s_tmp = s_tmp + "���줥���"
      CASE n2 = 6
        s_tmp = s_tmp + "���줥���"
      CASE n2 = 7
        s_tmp = s_tmp + "ᥬ줥���"
      CASE n2 = 8
        s_tmp = s_tmp + "��ᥬ줥���"
      CASE n2 = 9
        s_tmp = s_tmp + "���ﭮ��"
    ENDCASE
  ENDIF

  ****	�������

  IF n2 # 1 AND n1 # 0
    s_tmp = IIF (EMPTY (s_tmp), "", s_tmp + " ")
    DO CASE
      CASE n1 = 1
        s_tmp = s_tmp + IIF (t = 0, "����", "����")
        t_w = 1
      CASE n1 = 2
        s_tmp = s_tmp + IIF (t = 0, "���",  "���")
        t_w = 2
      CASE n1 = 3
        s_tmp = s_tmp + "��"
        t_w = 2
      CASE n1 = 4
        s_tmp = s_tmp + "����"
        t_w = 2
      CASE n1 = 5
        s_tmp = s_tmp + "����"
      CASE n1 = 6
        s_tmp = s_tmp + "����"
      CASE n1 = 7
        s_tmp = s_tmp + "ᥬ�"
      CASE n1 = 8
        s_tmp = s_tmp + "��ᥬ�"
      CASE n1 = 9
        s_tmp = s_tmp + "������"
    ENDCASE
  ENDIF
ENDIF
t = t_w
s = s_tmp
RETURN

*-------------------------------------------------------------------------
PROCEDURE Millions
*-------------------------------------------------------------------------
PARAMETERS num
PRIVATE t, s_tmp

t     = 0
s_tmp = ""
DO Pr_3 WITH s_tmp, t, num

IF NOT EMPTY (s_tmp)
  DO CASE
    CASE t = 0
      s_tmp = s_tmp + " ���������"
    CASE t = 1
      s_tmp = s_tmp + " �������"
    OTHERWISE
      s_tmp = s_tmp + " ��������"
  ENDCASE
ENDIF
RETURN s_tmp

*-------------------------------------------------------------------------
PROCEDURE Thousands
*-------------------------------------------------------------------------
PARAMETERS num
PRIVATE t, s_tmp

t     = 1
s_tmp = ""
DO Pr_3 WITH s_tmp, t, num

IF NOT EMPTY (s_tmp)
  DO CASE
    CASE t = 0
      s_tmp = s_tmp + " �����"
    CASE t = 1
      s_tmp = s_tmp + " �����"
    OTHERWISE
      s_tmp = s_tmp + " �����"
  ENDCASE
ENDIF
RETURN s_tmp

*=========================================================================
FUNCTION  Capital	&& ����⠫����� 1-� �㪢� ��ப�
*-------------------------------------------------------------------------
PARAMETER tmpStr
PRIVATE tmpStr

*RETURN UPPER (LEFT (tmpStr, 1)) + SUBSTR (tmpStr, 2)

RETURN RUpper (LEFT (tmpStr, 1)) + SUBSTR (tmpStr, 2)

*=========================================================================
FUNCTION RUpper		&& ��ॢ�� ��ப� � ���孨� ॣ����
*-------------------------------------------------------------------------
PARAMETER tmpStr
PRIVATE tmpStr, tmpLower, tmpUpper

tmpLower = "abcdefghijklmnopqrstuvwxyz��������������������������������"
tmpUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ��������������������������������"

RETURN CHRTRAN (tmpStr, tmpLower, tmpUpper)

