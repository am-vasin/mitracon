*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� C_Bank       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                            ���竍�碎 ����.                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 06.07.2000 様�
PROCEDURE C_Bank

PARAMETERS b_code, b_name, f_code

PRIVATE point, codeList, nameList, nList, s_sav, mss

s_sav = SELECT()
SELECT 0
USE (base_path+"BANKS") ORDER TAG NAME ALIAS BANK0706 AGAIN

nList = 0
point = 1
SEEK f_code
SCAN REST WHILE f_code = FIRM_CODE
  nList = nList+1
  DIMENSION codeList(nList), nameList(nList)
  codeList(nList) = BANK_CODE
  nameList(nList) = B_NAME
  IF b_code = BANK_CODE
    point = nList
  ENDIF
ENDSCAN

USE
SELECT (s_sav)

IF nList = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ぎ痰祚キ �� �え� ����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF nList = 1
  b_code = codeList(1)
  b_name = nameList(1)
  RETURN
ENDIF

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
PRIVATE ln, wd
ln = MIN(ALEN(nameList)+4, WROWS("")-9)
wd = LEN(nameList(1))+6

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, "�����"
@ 1, 1 CLEAR TO WROWS()-1, WCOLS()-1
@ WROWS()-1, 0 SAY PADC("Enter-�襦��,Esc-�皖��", WCOLS())

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO WHILE .T.
  point = MAX(point,1)
  point = MIN(point,ALEN(nameList))
  @ 1, 2 MENU nameList, ALEN(nameList), WCOLS()-4
  READ MENU TO point
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    b_code = codeList(point)
    b_name = nameList(point)
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
