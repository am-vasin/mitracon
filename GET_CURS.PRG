*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Get_Curs     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�              ���竍キ┘ 皀�竕ィ� (┼� �� 礫������� ∇ガ�)               �
*�                   Л�腑��� ��������� �窶�� ぎ�����.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 18.02.1999 様�
PROCEDURE Get_Curs
PARAMETERS c_type, als, dt, tm
*
*  c_type - ��� 皋�� �窶��;
*  als    - ����� ┼� ���ム �゛�痰� ��゛�肓 CURS_SET, メ�� ���
*           �皖琺�� � 祚�瑙ぎ腑��, ┼� �竅皰 (""), メ�� �モ;
*  dt     - ����, メ�� �� 礫����� - 皀�竕�� (��甄イ�闖);
*  tm     - ∇ガ� � 筮爼�皀 SECONDS() ┼� TIME() - メ�� �� 礫�����, 皰
*           ���ユ き�.
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
