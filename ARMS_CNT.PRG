*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�          �஢�ઠ � ���४�� ���稪� ��⨢��� ࠡ��� ����.         �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Arms_Cnt

PRIVATE pic, mss, mode_ex, k, ok

USE PARMS
SELECT 0

USE ARM_LST
COUNT FOR .NOT. EMPTY(SYS_NAM) TO k

mode_ex = 1
ok = k = PARMS.EXCLU

IF ok
  mss = "���稪 � ���浪�!"
  pic = "@*HT \? �������� Esc ;\\ ����⠭����� Ctrl-W ; ������ ��! "
ELSE
  mss = " ���稪 ����襭! "
  pic = "@*HT \? �������� Esc ;\! ����⠭����� Ctrl-W ; ������ ��! "
ENDIF

DO D_W WITH "Arms_Cnt", 8, 65, "�஢�ઠ ���稪� ��⨢��� ࠡ��� ����", 1

@ 3, 24 SAY mss
@ 5,  3 GET mode_ex PICTURE pic
READ CYCLE

IF mode_ex = 2
  SELECT PARMS
  REPLACE EXCLU WITH k
ENDIF

IF mode_ex = 3
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��������� �⤠�� ᥡ� ���� � ⮬, �� ᮡ�ࠥ��� ᤥ����?"
  mss(3) = CHR(0)+"  [ �� ]         [ ��� ]"
  mss(4) = ""
  IF Out_Mess(7, "mss") = 1
    REPLACE ALL SYS_NAM WITH " ", ;
                SYS_DAT WITH {},  ;
                SYS_TIM WITH " "
    SELECT PARMS
    REPLACE EXCLU WITH 0
    RELEASE WINDOW Arms_Cnt
    CLOSE DATABASES
    CLEAR
    SET DEFAULT TO (sav_dir)
    s = "! "+LEFT(sav_dir,2)
    &s
    s = "! CD "+SUBSTR(sav_dir,3)
    s = LEFT(s,LEN(s)-1)
    &s
    QUIT
  ENDIF

ENDIF

RELEASE WINDOW Arms_Cnt
CLOSE DATABASES

RETURN