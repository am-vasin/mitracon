*===================================================================
FUNCTION RetPrice	&& ��������� ���� ��� �������
*-------------------------------------------------------------------
*	�������� �� 24.11.1999
*	����������� 13.04.2000	- ������ ���� ������� (N) ����� ���� ��� ������ (C)
*	����������� 04.05.2000	- �������� ��������� ����� �� +-3%
*	����������� 06.07.2000	- ������ ������� ���������� �� 50 ���. ����� � ���. ���� >= 1 ���
*	����������� 14.08.2001 (�������)	- ������ 12 ����������� ������������� - ������� 10%
*	����������� 24.10.2001 (�������)	- ������ 12 (����������� �������������) - �� ������ ���������
*	����������� 12.03.2002 (��������)	- ������ 72 (�����) ����� 20.2 (����� ��� �����������) - ������� 11.8%
*-------------------------------------------------------------------
PARAMETERS tmpName, tmpPrice, is_import, d_code
* tmpName			&& ������������ �������
* tmpPrice			&& ������� ����
* is_import         && ��������� �������!
* d_code			&& ��� ������� (N) ��� ������ (C)

IF PARAMETERS() < 4
	d_code = 0
ENDIF

PRIVATE tmpMin, tmpMax	&& ������� �� ������ ��������� [%]
PRIVATE tmpMid			&& ������� � �������� (1.0) ��������� [%]
PRIVATE tmpSovok		&& ������������� ����������� ��� �����
PRIVATE tmpDev			&& �������� ��������� ������� [%]

PRIVATE tmpRet			&& ��������� ��������� ����
PRIVATE tmpRandom		&& ������ ��������������� ���������
PRIVATE tmp, tmpCount		

DIMENSION tmpRandom [13]

****	��������� ����������
tmpSovok = 0.96
tmpMin =  19*0.01*IIF(is_import, 1.0, tmpSovok)	&& ��� � -> 00  �/� -> 1+tmpMin
tmpMax = 130*0.01*IIF(is_import, 1.0, tmpSovok)	&& ��� � -> 0   �/� -> 1+tmpMax
tmpMid =  65*0.01*IIF(is_import, 1.0, tmpSovok)	&& ��� � =  1   �/� =  1+tmpMid
tmpDev =   3*0.01	&& ��������� ��������� � ��������� +- tmpDev %

tmpMid = (tmpMid - tmpMin)/(tmpMax - tmpMid)

****	���������� ������� ��������� ��������� � ����������� �� ������
=RAND (INT ((DATE() - CTOD ('01.01.' + STR (YEAR (DATE()),4)))/14))
* =RAND (INT ((DATE() - CTOD ('01.01.' + STR (YEAR (DATE()),4)))/7))
&&    (INT (30*MONTH(DATE()) + DAY(DATE())))	&& ����� = ������ ������
FOR tmp = 1 TO 13
  tmpRandom [tmp] = RAND()
ENDFOR

****	���������� "����������" ��������� ���� �� �������
****	retPrice = price + price*[min + mid*(max - min)/(price + mid)]
tmpRet = tmpPrice*(1 + tmpMin + tmpMid*(tmpMax-tmpMin)/(tmpPrice +tmpMid))

****	���������� "����������" ��������� ���� ��� ������� ������� ��� ������� �����
IF Is_Import AND tmpPrice < 0.1
*	tmpRet = tmpPrice*(1 + 7.0 - 20*(7.0 - 1.23)*tmpPrice)
	PRIVATE a
	DIMENSION a(7,2)
	a[1,1] = 0.000
	a[1,2] = 6.0
	a[2,1] = 0.010
	a[2,2] = 6.0
	a[3,1] = 0.015
	a[3,2] = 4.5
	a[4,1] = 0.020
	a[4,2] = 4.0
	a[5,1] = 0.035
	a[5,2] = 2.8
	a[6,1] = 0.050
	a[6,2] = 2.5
	a[7,1] = 0.100
	a[7,2] = 2.16
	FOR tmp = 2 TO ALEN(a,1)
		IF tmpPrice < a[tmp,1]
			tmpRet = tmpPrice*(;
			((a[tmp,2] - a[tmp-1,2])*tmpPrice - (a[tmp,2]*a[tmp-1,1] - a[tmp-1,2]*a[tmp,1]))/;
			(a[tmp,1] - a[tmp-1,1]))
			EXIT
		ENDIF
	ENDFOR
ENDIF

*	����. ������� ��� ��������� ��������
IF TYPE("d_code") = "N"
	DO CASE
	CASE d_code = 27 OR d_code = 52			&& ���������; ������������
		tmpRet = tmpPrice*(1 + 0.15)
	CASE d_code = 63						&& �������� ����.
		tmpRet = tmpPrice*(1 + 0.10)
*!*		CASE d_code = 12 AND !is_import			&& ����������� �������������
*!*			tmpRet = tmpPrice*(1 + 0.10)
	CASE d_code = 68						&& ������.������ (KIT)
		tmpRet = tmpPrice*(1 + 0.27)
	*	12.03.2002 (��������)
	CASE d_code = 72						&& �����
		tmpRet = tmpPrice*(1 + 0.118)
	ENDCASE
*	����. ������� ��� ��������� �������	&&  ****13.04.2000 ****
ELSE
	DO CASE
	CASE d_code = "0704"					&& �������� ����������
		tmpRet = tmpPrice*(1 + 0.10)
*!*		CASE d_code = "2601" AND !is_import		&& ����������� (�������������)
*!*			tmpRet = tmpPrice*(1 + 0.10)
	*	12.03.2002 (��������)
	CASE d_Code = "2002"					&& ����� ��� �����������
		tmpRet = tmpPrice*(1 + 0.118)
	CASE d_code = "24"						&& ���������� ��������� ����
		tmpRet = tmpPrice*(1 + 0.15)
	CASE d_code = "29"						&& ����������� ������ � ������
		tmpRet = tmpPrice*(1 + 0.27)
	ENDCASE
ENDIF

****	��������� ���� �� ������������ �� ������ 13
tmpCount = 0
FOR tmp = 1 TO LEN (ALLTRIM (tmpName))
  tmpCount = tmpCount + ASC (SUBSTR (tmpName, tmp))
ENDFOR
tmpCount = tmpCount % 13

****	��������� ���������� ��������� �� ��������� ����
IF TYPE("d_code") = "N"
	DO CASE
	CASE d_code = 12 AND !is_import			&& ����������� �������������
	OTHERWISE
		tmpRet = tmpRet * (1 + 2*tmpDev*(tmpRandom[tmpCount+1]-0.5))
	ENDCASE
ELSE ****13.04.2000 *******************************************
	DO CASE
	CASE d_code = "2601" AND !is_import		&& ����������� (�������������)
*!*			tmpRet = tmpPrice*(1 + 0.00)
	OTHERWISE
		tmpRet = tmpRet * (1 + 2*tmpDev*(tmpRandom[tmpCount+1]-0.5))
	ENDCASE
ENDIF

****06.07.2000 - ���������� �� 50 ��� ����� � ��� ���� >= 1 ���

* ����� ���� ������� � �������� ���������!
* �� � ��� �� ������� - ����� ����� ���� �� ������ ������������
* �������� ����, �.�. � ������ �����

****06.07.2000 end ********************************************

RETURN tmpRet

