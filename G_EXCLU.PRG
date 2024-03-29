*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      蹍������ ��紜��� ��щ.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE G_Exclu
PRIVATE k, mss

USE PARMS
SELECT 0

USE ARM_LST
COUNT FOR .NOT. EMPTY(SYS_NAM) TO k

IF k # PARMS.EXCLU
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 磴�モ瘴 ���竍�碎 �瓷�鈑�皀�讚覃 ぎ痰祚 � ����覓:"
  mss(3) = CHR(0)+"��珮茱� 瘍モ腮� ��皋↓諷 ��｀腮� �メ�."
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN .F.
ENDIF

DO WHILE .T.
  IF PARMS.EXCLU # 1
    IF View_Arms()
      LOOP
    ELSE
      CLOSE DATABASES
      RETURN .F.
    ENDIF
  ELSE
    SELECT PARMS
    REPLACE EXCLU WITH -1
    CLOSE DATABASES
    RETURN .T.
  ENDIF
ENDDO

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      蹍������ ��紜��� ��щ.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE View_Arms

PRIVATE ln, tt, ft

PUSH KEY CLEAR

DEFINE WINDOW H000 FROM 0, 0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW H000

DO Use_Dummy

SELECT 0
USE USERS ORDER TAG USER_CODE

SELECT ARM_LST
COUNT FOR .NOT. EMPTY(SYS_NAM) .AND. RECNO() # arm_rec TO ln

ln = MIN(ln, WROWS("")-10)

DIMENSION tt(1), ft(2)

tt(1) = PADC(" ���. �メ皰 陳�", FSIZE("ARM_N"), "�")+"�"+ ;
        PADC(" ��痰ガ� �", FSIZE("SYS_NAM"), "�")+"�"+ ;
        PADC(" �祗�罔� 陳陳陳陳陳", FSIZE("FUN_NAM"), "�")

ft(1) = PADC(" Enter - ����� ���謔��, Esc - �皖���碎瘴 ", LEN(tt(1)), "�")
ft(2) = PADR("  �皰 ��｀��モ: ", LEN(tt(1)))

DO Def_Win WITH "Arms_M", "Arms_E", "Arms_I", ln, LEN(tt(1)), ;
                "���甌� ��皋↓諷 ��｀腮� �メ�", tt, ft

SET RELATION TO US_CODE INTO USERS
GO TOP
ACTIVATE WINDOW Arms_E

ON KEY LABEL Enter KEYBOARD CHR(23)

BROWSE FOR .NOT. EMPTY(SYS_NAM) .AND. RECNO() # arm_rec  ;
       FIELDS ARM_N:H="",    ;
              SYS_NAM:H="",  ;
              FUN_NAM:H="",  ;
              DUMMY.F:H=""   ;
       FREEZE DUMMY.F NOMENU NOMODIFY  ;
       WHEN Show_U()                   ;
       WINDOW Arms_I IN WINDOW Arms_E

RELEASE WINDOW Arms_M
RELEASE WINDOW Arms_E
RELEASE WINDOW Arms_I
RELEASE WINDOW H000

SET RELATION TO

SELECT DUMMY
USE
SELECT USERS
USE
POP KEY

RETURN READKEY() % 256 # 12

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      ����щ��ガ �皰 ��｀��モ.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Show_U

PRIVATE s

DO CASE
CASE US_CODE = 0
  s = "���!"
CASE .NOT. FOUND("USERS")
  s = ""
OTHERWISE
  s = USERS.USER_NAME
ENDCASE

s = PADR(s, FSIZE("USER_NAME", "USERS"))

ACTIVATE WINDOW ARMS_M
@ WROWS()-1, 18 SAY s

ACTIVATE WINDOW (ALIAS())

RETURN .T.
