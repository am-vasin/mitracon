*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Country      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                                ��࠭�.                                 �
*�                                                                        �
*����������������������������������������������������������� 14.02.2000 �ͼ
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
  mss(2) = CHR(0)+"���᮪ ��࠭ ����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN startVal
ENDIF

tmpPoint = MAX(ASCAN(tmpList, startVal), 1)

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

PRIVATE ln
ln = MIN(tmpLen+3, 20)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH ln, LEN(tmpList)+4, "��࠭�"

*------------------------------------------------------------------------
*      ���� ����� ������
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