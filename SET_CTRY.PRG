*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Set_Ctry     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ������キ┘ ��ぎ� 痰��� か� ���                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 27.12.2012 様�
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


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fil_Tmp      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ������キ┘ ∇ガキ���� �����                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.12.2012 陳�
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fil_Auto     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    �≒���皋腑瓷�� ������キ┘ ��ぎ�                     �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.12.2012 陳�
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fil_M        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �竍��� ������キ┘                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.12.2012 陳�
PROCEDURE Fil_M

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
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!
PRIVATE mss
PRIVATE tmpCode

DIMENSION mss[3]
mss[1] = ""
mss[2] = CHR(0)+"H� か� ≡ュ 痰��� ���ぅ� ���"
mss[3] = ""

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"OKSM") SHARED AGAIN ORDER TAG CODE ALIAS REL_OKSM
SELECT TMP_LIST

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "��ぁ��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"
DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
m.wd = FSIZE("SRC_NAME")+FSIZE("COUNTRYC")+1

DO D_Wins WITH m.ln, m.wd, "��ぁ�爲� � ����", 0, 1
m.what_do = "List"
@ 1, 2 SAY PADR("H�Б��┘", FSIZE("SRC_NAME")+1, "�")+"���"
GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
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

  CASE m.what_do = "Choose"    && ��ぁ�爲�

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
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
USE IN REL_OKSM
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Show_C       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Show_C
PRIVATE tmpV

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Res      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    �ェ祀赳�珮鉗�� ��瘁����→� ��ぎ�                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 28.12.2012 陳�
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
