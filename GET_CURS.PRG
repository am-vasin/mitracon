*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Get_Curs     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�              ����祭�� ⥪�饣� (��� �� 㪠������ �६�)               �
*�                   ���祭�� ��������� ���� ������.                    �
*�                                                                        �
*����������������������������������������������������������� 18.02.1999 �ͼ
PROCEDURE Get_Curs
PARAMETERS c_type, als, dt, tm
*
*  c_type - ��� ⨯� ����;
*  als    - ����� ��� ����� ������ ⠡���� CURS_SET, �᫨ ���
*           ����� � 㯮�冷祭�, ��� ���� (""), �᫨ ���;
*  dt     - ���, �᫨ �� 㪠���� - ⥪��� (��᫥����);
*  tm     - �६� � �ଠ� SECONDS() ��� TIME() - �᫨ �� 㪠����, �
*           ����� ���.
*
PRIVATE tm_w, dt_w, als_w, k, s_sav

IF TYPE("dt") = "D"
  dt_w = DTOS(dt)
ELSE
  dt_w = "99999999"
ENDIF

DO CASE
CASE TYPE("tm") = "N"
  k = tm
  tm_w = ":"+RIGHT(STR(100+k%60),2)
  k = FLOOR(k/60)
  tm_w = ":"+RIGHT(STR(100+k%60),2)+tm_w
  k = FLOOR(k/60)
  tm_w = RIGHT(STR(100+k),2)+tm_w
CASE TYPE("tm") = "C"
  tm_w = tm
OTHERWISE
  tm_w = ""
ENDCASE

s_sav = SELECT()
IF EMPTY(als)
  SELECT 0
  USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CRS_9218 AGAIN
ELSE
  SELECT (als)
ENDIF

k = PADR(c_type,10)+dt_w+tm_w
IF .NOT. SEEK(k)
  k = RECNO(0)
  IF BETWEEN(k, 1, RECCOUNT())
    GO k
  ENDIF
ENDIF

k = IIF(BOF() .OR. EOF(), 0, CURS)

IF EMPTY(als)
  USE
ENDIF

SELECT (s_sav)

RETURN k