*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Set_Ctry     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                     ���������� ����� ��࠭ ��� ���                     �
*�                                                                        �
*����������������������������������������������������������� 27.12.2012 �ͼ
PROCEDURE Set_Ctry
PARAMETERS l_number

PRIVATE s_sav, tmpName

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"OKSM") SHARED AGAIN ORDER TAG B_NAME ALIAS FIL_OKSM
SELECT 0
USE (m.base_path+"INC_LIST") SHARED AGAIN ORDER TAG L_CODE ALIAS SRC_LIST
SELECT 0
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	LWR_NAME	C(FSIZE("COUNTRY", "SRC_LIST")),	;
		SRC_NAME	C(FSIZE("COUNTRY", "SRC_LIST")),	;
		COUNTRYC	C( 3)								)
INDEX ON LWR_NAME TAG NAME
USE (m.tmpo_path+m.tmpName) ALIAS TMP_LIST ORDER TAG NAME

DO Fil_Tmp

IF RECCOUNT("TMP_LIST") = 0
  USE IN FIL_OKSM
  USE IN TMP_LIST
  USE IN SRC_LIST
  DELETE FILE (m.tmpo_path+m.tmpName+".DBF")
  DELETE FILE (m.tmpo_path+m.tmpName+".CDX")
  SELECT (m.s_sav)
  RETURN
ENDIF

IF .NOT. Fil_Auto()
  DO Fil_M
ENDIF

DO Set_Res

USE IN FIL_OKSM
USE IN TMP_LIST
USE IN SRC_LIST
DELETE FILE (m.tmpo_path+m.tmpName+".DBF")
DELETE FILE (m.tmpo_path+m.tmpName+".CDX")
SELECT (m.s_sav)
RETURN


*������������������������������������������������������������������������Ŀ
*�   ��楤�� Fil_Tmp      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ���������� �६������ 䠩��                       �
*�                                                                        �
*����������������������������������������������������������� 27.12.2012 ���
PROCEDURE Fil_Tmp

PRIVATE tmpLwr

SELECT SRC_LIST
SEEK m.l_number
SCAN REST WHILE LIST_NO = m.l_number ;
              FOR .NOT. EMPTY(COUNTRY) .AND. EMPTY(COUNTRYC)
  m.tmpLwr = SYS(15, m.lwr, ALLTRIM(COUNTRY))
  IF SEEK(m.tmpLwr, "TMP_LIST")
    LOOP
  ENDIF
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE LWR_NAME WITH m.tmpLwr, ;
          SRC_NAME WITH SRC_LIST.COUNTRY
  SELECT SRC_LIST
ENDSCAN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Fil_Auto     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    ��⮬���᪮� ���������� �����                     �
*�                                                                        �
*����������������������������������������������������������� 27.12.2012 ���
PROCEDURE Fil_Auto

PRIVATE retVal

SELECT TMP_LIST
m.retVal = .T.
SCAN
  IF SEEK(LWR_NAME, "FIL_OKSM")
    REPLACE COUNTRYC WITH FIL_OKSM.CODE
  ELSE
    m.retVal = .F.
  ENDIF
ENDSCAN

RETURN m.retVal

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Fil_M        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ��筮� ����������                            �
*�                                                                        �
*����������������������������������������������������������� 27.12.2012 ���
PROCEDURE Fil_M

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
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!
PRIVATE mss
PRIVATE tmpCode

DIMENSION mss[3]
mss[1] = ""
mss[2] = CHR(0)+"H� ��� ��� ��࠭ ������ ���"
mss[3] = ""

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"OKSM") SHARED AGAIN ORDER TAG CODE ALIAS REL_OKSM
SELECT TMP_LIST

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "������" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"
DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = FSIZE("SRC_NAME")+FSIZE("COUNTRYC")+1

DO D_Wins WITH m.ln, m.wd, "�����ઠ � ����", 0, 1
m.what_do = "List"
@ 1, 2 SAY PADR("H�������", FSIZE("SRC_NAME")+1, "�")+"���"
GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS SRC_NAME:H="",     ;
                  COUNTRYC:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_C()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && �����ઠ

    m.tmpCode = Seek_C(ALLTRIM(SRC_NAME))
    IF .NOT. EMPTY(m.tmpCode)
      REPLACE COUNTRYC WITH m.tmpCode
    ENDIF
    m.what_do = "List"

  OTHERWISE
    
    LOCATE FOR EMPTY(COUNTRYC)
    IF FOUND()
      DO Out_Mess WITH 7, "mss"
      m.what_do = "List"
      EXIT  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
    ELSE
      EXIT
    ENDIF

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
USE IN REL_OKSM
SELECT (m.s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Show_C       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE Show_C
PRIVATE tmpV

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
IF EMPTY(COUNTRYC)
  m.tmpV = SPACE(m.wd)
ELSE
  = SEEK(COUNTRYC, "REL_OKSM")
  m.tmpV = PADR(REL_OKSM.B_NAME, m.wd)
ENDIF
@ WROWS()-2, 2 SAY m.tmpV

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Set_Res      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    ����������� ���⠭���� �����                    �
*�                                                                        �
*����������������������������������������������������������� 28.12.2012 ���
PROCEDURE Set_Res
PRIVATE tmpLwr

SELECT SRC_LIST
SEEK m.l_number
SCAN REST WHILE LIST_NO = m.l_number ;
              FOR .NOT. EMPTY(COUNTRY) .AND. EMPTY(COUNTRYC)
  m.tmpLwr = SYS(15, m.lwr, ALLTRIM(COUNTRY))
  IF SEEK(m.tmpLwr, "TMP_LIST")
    REPLACE COUNTRYC WITH TMP_LIST.COUNTRYC
  ENDIF
ENDSCAN