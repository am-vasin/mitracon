*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� S_F1_        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  ������珥�覃 √�� � ���� ���� 筥爼�.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 15.01.1999 様�
PROCEDURE S_F1_
PARAMETER ex_proc
f_c_old = f_c000
IF EMPTY(ex_proc)
  ON KEY LABEL F1 DO Choose_F
ELSE
  PRIVATE n
  n = 'WITH "'+ex_proc+'"'
  ON KEY LABEL F1 DO Choose_F &n
ENDIF
RETURN .T.
