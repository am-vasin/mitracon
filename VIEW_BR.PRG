*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� View_Br      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                            �牀�� �� ��Ж罔�                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 28.09.2005 様�
PROCEDURE View_Br
PARAMETERS c_code
*
*  c_code - ��� ��Ж罔�
*

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

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
PRIVATE tmpName, tmpTit, qnt_tot
PRIVATE hRez, hWork, hBron
STORE 0 TO m.hRez, m.hWork, m.hBron
m.tmpName = SYS(3)
m.qnt_tot = 0
m.tmpTit = ""

s_sav = SELECT()
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
  ( QNT			N( 8), ;
  	QNT_REZ		N( 8), ;
  	QNT_WORK	N( 8), ;
  	QNT_BRON	N( 8), ;
    PRICE		C(14), ;
    CUS_NAME	C(50), ;
    LIST_N		C( 6), ;
    ARM_NAME	C(16), ;
    MAN_NAME	C(40)  )
USE (m.tmpo_path+m.tmpName) ALIAS TMP_BRON
DO Prp_Bron
SELECT TMP_BRON
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 轤�� ��Ж罔� ��腑�� �� ��÷���牀����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  SELECT (m.s_sav)
  RETURN
ENDIF
GO TOP
  
SELECT 0
DO Use_Dummy
SELECT TMP_BRON

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(RECCOUNT(), WROWS("")-11)   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 74

*    �牀��        �キ�    ��┘��
*!99999999 99999999999999 ....:....!....:....!....:....!....:....!....:....!!

*   �瓮��  ��｀腑� �メ皰    �キイΕ�                                 ���甌�
*!99999999 ....:....!....:. ....:....!....:....!....:....!....:....! 999999 !
DO D_Wins WITH ln, wd, m.tmpTit, 0, 4
@ 1,  5 SAY "�牀��"
@ 1, 18 SAY "�キ�"
@ 1, 26 SAY "��┘��"
@ WROWS()-6,  4 SAY "�瓮��"
@ WROWS()-6, 11 SAY "��｀腑� �メ皰"
@ WROWS()-6, 28 SAY "�キイΕ�"
@ WROWS()-6, 69 SAY "���甌�"
@ WROWS()-5,  2 SAY STR(m.qnt_tot, 8) COLOR SCHEME 1
@ WROWS()-4,  3 SAY "�ェム�:   �瓮�� "
@ WROWS()-4, COL() SAY STR(m.hRez, 6) COLOR SCHEME 1
@ WROWS()-4, COL() SAY " � ����Д"
@ WROWS()-3,  3 SAY "� ��｀皀: �瓮�� "
@ WROWS()-3, COL() SAY STR(m.hWork, 6) COLOR SCHEME 1
@ WROWS()-3, COL() SAY " � ����Д"
@ WROWS()-2,  3 SAY "�牀��:    �瓮�� "
@ WROWS()-2, COL() SAY STR(m.hBron, 6) COLOR SCHEME 1
@ WROWS()-2, COL() SAY " � ����Д"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS QNT:H="":P="99999999",  ;
                  PRICE:H="",             ;
                  CUS_NAME:H="",          ;
           DUMMY.F:H="" FREEZE DUMMY.F    ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
USE IN TMP_BRON
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-5, 11 SAY ARM_NAME COLOR SCHEME 1
@ WROWS()-5, 28 SAY MAN_NAME COLOR SCHEME 1
@ WROWS()-5, 69 SAY LIST_N   COLOR SCHEME 1
@ WROWS()-4, 35 SAY STR(QNT_REZ, 6) COLOR SCHEME 1
@ WROWS()-3, 35 SAY STR(QNT_WORK, 6) COLOR SCHEME 1
@ WROWS()-2, 35 SAY STR(QNT_BRON, 6) COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Bron     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        ��ぃ�皰→� 甎�瓷� ÷���                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 28.09.2005 陳�
PROCEDURE Prp_Bron

PRIVATE s_sav, tmpPrice, tmpMan
PRIVATE tmpRez, tmpWork, tmpBron

m.s_sav = SELECT()

SELECT 0
USE m.base_path+"STOCK" SHARED AGAIN ORDER TAG CODE ALIAS STO_5928
SEEK m.c_code
m.tmpTit = ALLTRIM(PREFIX)+" "+ALLTRIM(NAME)+ ;
           IIF(EMPTY(PRODUCER), "", " /"+ALLTRIM(PRODUCER)+"/")

USE m.base_path+"ACCOUNT" SHARED AGAIN ORDER TAG CUS_CODE ALIAS CUS_5928

SELECT 0
USE m.base_path+"PERSONS" SHARED AGAIN ORDER TAG CODE ALIAS MAN_5928

SELECT 0
USE m.base_path+"ARM_LST" SHARED AGAIN ORDER TAG ARM_C ALIAS ARM_5928

SELECT 0
USE m.base_path+"ORD_T" SHARED AGAIN ORDER TAG DOC_NUM ALIAS OT_5928

SELECT 0
USE m.base_path+"ORD_D" SHARED AGAIN ORDER TAG DOC_NUM ALIAS OD_5928

SELECT 0
USE m.base_path+"LIST_TIT" SHARED AGAIN ORDER TAG LST_NUM ALIAS TIT_5928
SET RELATION TO WHO_CR   INTO MAN_5928, ;
                LST_ARM  INTO ARM_5928, ;
                CLI_CODE INTO CUS_5928

SELECT 0
USE m.base_path+"LIST_DET" SHARED AGAIN ORDER TAG BRON ALIAS DET_5928
SET RELATION TO VAL(LIST_NUM) INTO TIT_5928

SEEK c_code
SCAN REST WHILE CODE = m.c_code
  IF .NOT. FOUND("TIT_5928")
    LOOP
  ENDIF
  IF .NOT. EMPTY(TIT_5928.SAL_NUM)
    LOOP
  ENDIF
  m.tmpPrice = TRANSFORM(DET_5928.SAL_PRICE, "99 999 999.99") + ;
               IIF(TIT_5928.IS_USD, "$", "�")
  m.tmpMan = ALLTRIM(MAN_5928.FAMILY)+" "+LEFT(MAN_5928.NAME, 1)+" "+LEFT(MAN_5928.S_NAME, 1)
  STORE 0 TO m.tmpRez, m.tmpWork, m.tmpBron
  SELECT OD_5928
  SEEK DET_5928.LIST_NUM
  SCAN REST WHILE DET_5928.LIST_NUM = LIST_NUM FOR CODE = DET_5928.CODE
    IF SEEK(LIST_NUM+STR(ORD_NUM, 3), "OT_5928")
      DO CASE
      CASE .NOT. EMPTY(OT_5928.STO_MARK)
        m.tmpBron = m.tmpBron+QNT
        m.hBron = m.hBron+QNT
      CASE .NOT. EMPTY(OT_5928.REQ_DATE)
        m.tmpWork = m.tmpWork+QNT
        m.hWork = m.hWork+QNT
      OTHERWISE
        m.tmpRez = m.tmpRez+QNT
        m.hRez = m.hRez+QNT
      ENDCASE
    ENDIF
  ENDSCAN
  SELECT TMP_BRON
  APPEND BLANK
  REPLACE QNT		WITH DET_5928.QNT_REQ,  ;
  		  QNT_REZ	WITH m.tmpRez,			;
  		  QNT_WORK	WITH m.tmpWork,			;
  		  QNT_BRON	WITH m.tmpBron,			;
          PRICE		WITH m.tmpPrice,        ;
          CUS_NAME	WITH CUS_5928.CUS_NAME, ;
          LIST_N	WITH DET_5928.LIST_NUM, ;
          ARM_NAME	WITH ARM_5928.ARM_N,    ;
          MAN_NAME	WITH m.tmpMan

  SELECT DET_5928
  m.qnt_tot = m.qnt_tot+QNT_REQ
ENDSCAN

USE IN DET_5928
USE IN TIT_5928
USE IN MAN_5928
USE IN CUS_5928
USE IN ARM_5928
USE IN OT_5928
USE IN OD_5928

SELECT (m.s_sav)

RETURN
