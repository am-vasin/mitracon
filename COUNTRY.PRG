*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Country      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                                �矗���.                                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 14.02.2000 様�
PROCEDURE Country
PARAMETERS startVal

PRIVATE tmpPoint, tmpList, tmpLen, s_sav

s_sav = SELECT()
SELECT 0
USE (path_comm+"COUNTRY") ORDER TAG B_NAME ALIAS C_0214 AGAIN
tmpLen = 0
SCAN
  tmpLen = tmpLen+1
  DIMENSION tmpList(tmpLen)
  tmpList(tmpLen) = B_NAME
ENDSCAN
USE
SELECT (s_sav)
IF tmpLen = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���甌� 痰��� �竅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN startVal
ENDIF

tmpPoint = MAX(ASCAN(tmpList, startVal), 1)

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

PRIVATE ln
ln = MIN(tmpLen+3, 20)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln, LEN(tmpList)+4, "�矗���"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO WHILE .T.
  tmpPoint = MAX(1, tmpPoint)
  tmpPoint = MIN(tmpLen, tmpPoint)
  @ 1, 1 MENU tmpList, tmpLen, ln-3
  READ MENU TO tmpPoint
  IF READKEY() % 256 = 12
    tmpPoint = 0
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN IIF(tmpPoint = 0, startVal, tmpList(tmpPoint))
