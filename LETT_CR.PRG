*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Lett_Cr      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           ��Г��┘ 甌�♂キ��                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 21.10.2005 様�
PROCEDURE Lett_Cr
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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*------------------------------------------------------------------------
*    �皖琺��ガ � 甌Г�ガ 矗ァ礇�襯 ��゛�肓
*
PRIVATE tmpPers, tmpArm, tmpType, tmpTit, tmpText, tmpDate, tmpH, tmpM, s_sav
m.s_sav = SELECT()
SELECT 0
USE base_path+"LETT_OUT" SHARED AGAIN ALIAS LOUT_5A21
SELECT 0
m.tmpPers = SYS(3)
m.tmpArm = SYS(3)
DO WHILE m.tmpPers = m.tmpArm
  m.tmpArm = SYS(3)
ENDDO
CREATE DBF (m.tmpo_path+m.tmpPers) ;
	(	CODE	N( 5), ;
		MARK	C( 1), ;
		NAME	C(30)  )
INDEX ON SYS(15, m.lwr, NAME) TAG NAME FOR .NOT. EMPTY(MARK)
INDEX ON CODE TAG CODE
USE (m.tmpo_path+m.tmpPers) EXCLUSIVE ALIAS TMP_PERS ORDER TAG CODE

SELECT 0
CREATE DBF (m.tmpo_path+m.tmpArm) ;
	(	CODE	C( 3), ;
		MARK	C( 1), ;
		NAME	C(30)  )
INDEX ON SYS(15, m.lwr, NAME) TAG NAME FOR .NOT. EMPTY(MARK)
INDEX ON CODE TAG CODE
USE (m.tmpo_path+m.tmpArm) EXCLUSIVE ALIAS TMP_ARM ORDER TAG CODE
m.tmpType = 1
m.tmpTit  = SPACE(32)
m.tmpText = ""
m.tmpDate = DATE()
m.tmpH    = LEFT(TIME(), 2)
m.tmpM    = SUBSTR(TIME(), 4, 2)

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�             �ガ� ....:....!....:....!....:....!..             �
*� ( ) ���� ....:....!....:....!....:....!....:....!....:....!  �
*� ( ) �磴� ....:....!....:....!....:....!....:....!....:....!  �
*�               蹍腮��碎 ��.��.���� F1 99� 99�                 �
*楽陳�オ痰 甌�♂キ�鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖                                                             崖
*崖                                                             崖
*崖                                                             崖
*崖                                                             崖
*崖                                                             崖
*崖                                                             崖
*崖                                                             崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�               < OK Ctrl-W > < �皖���碎瘴 Esc >                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, tmpRec
m.ex = 1
m.tmpRec = 0
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 17, 67, "��Г��┘ 甌�♂キ��"
DO Sun_Bord WITH  6,  2, 14, 64, " �オ痰 甌�♂キ�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  2, 15 SAY "�ガ�" GET m.tmpTit
@  3,  3 GET m.tmpType PICTURE "@*RV ����;�磴�" VALID Get_Addr()
@  5, 17 SAY "蹍腮��碎" GET tmpDate PICTURE "@D"
@  5, COL()+1 SAY "(F1)" GET tmpH PICTURE "99"
@  5, COL() SAY "�"
@  5, COL()+1 GET tmpM PICTURE "99"
@  5, COL() SAY "�"
@  7,  3 EDIT m.tmpText SIZE 7, 61
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

ON KEY LABEL F1 DO Ch_Date
READ CYCLE VALID Tst_Mess()
ON KEY LABEL F1

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  DO Add_Lett
ENDIF

USE IN LOUT_5A21
USE IN TMP_ARM
USE IN TMP_PERS
DELETE FILE (tmpo_path+m.tmpPers+".dbf")
DELETE FILE (tmpo_path+m.tmpPers+".cdx")
DELETE FILE (tmpo_path+m.tmpArm+".dbf")
DELETE FILE (tmpo_path+m.tmpArm+".cdx")
SELECT (m.s_sav)

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.tmpRec

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tmp_Addr     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            ���� ┼� �磴�...                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.10.2005 陳�
PROCEDURE Get_Addr
PRIVATE tmpLin

m.tmpLin = SPACE(52)
IF m.tmpType = 1
  @ 4, 12 SAY m.tmpLin
  SELECT TMP_PERS
  SET ORDER TO TAG CODE
  DO Lst_Pers WITH "TMP_PERS"
  SET ORDER TO TAG NAME
  GO TOP
  IF .NOT. EOF() .AND. .NOT. BOF()
    m.tmpLin = PADR(NAME, 50)
    SKIP
    m.tmpLin = m.tmpLin+IIF(.NOT. EOF(), " ", "  ")
  ENDIF
  @ 3, 12 SAY m.tmpLin COLOR SCHEME 1
ELSE
  @ 3, 12 SAY m.tmpLin
  SELECT TMP_ARM
  SET ORDER TO TAG CODE
  DO Lst_Arm WITH "TMP_ARM"
  SET ORDER TO TAG NAME
  GO TOP
  IF .NOT. EOF() .AND. .NOT. BOF()
    m.tmpLin = PADR(NAME, 50)
    SKIP
    m.tmpLin = m.tmpLin+IIF(.NOT. EOF(), " ", "  ")
  ENDIF
  @ 4, 12 SAY m.tmpLin COLOR SCHEME 1
ENDIF  

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Mess     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                蹍▲爲� ��玻オ皚�痰� ������キ�� ゛����                 �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.10.2005 陳�
PROCEDURE Tst_Mess

PRIVATE mss

IF m.ex = 2
  RETURN .T.
ENDIF

IF EMPTY(m.tmpTit)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"�ガ� 甌�♂キ�� 甄イ礇� 礫���碎..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

m.tmpH = CHRTRAN(PADL(ALLTRIM(m.tmpH), 2), " ", "0")
m.tmpM = CHRTRAN(PADL(ALLTRIM(m.tmpM), 2), " ", "0")
IF .NOT. BETWEEN(m.tmpH, "00", "23") .OR. ;
   .NOT. BETWEEN(m.tmpM, "00", "59")
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"�アム�� 礫����� ∇ガ� ぎ痰�→� 甌�♂キ��..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF m.tmpType = 1
  SELECT TMP_PERS
  SET ORDER TO TAG NAME
  GO TOP
  IF EOF() .OR. BOF()
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"�� ��°�� 礫���碎 �むメ���..."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ELSE
  SELECT TMP_ARM
  SET ORDER TO TAG NAME
  GO TOP
  IF EOF() .OR. BOF()
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"�� ��°�� 礫���碎 �むメ���..."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Add_Lett     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          ����←キ┘ 甌�♂キ��                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 21.10.2005 陳�
PROCEDURE Add_Lett

PRIVATE s_sav, tmp_ID, tmpTime

m.s_sav = SELECT()

SELECT 0
USE base_path+"LETT_OUT" SHARED AGAIN ALIAS OUT_5A21A
m.tmp_ID = DOC_NUM("LETT_ID", "", m.arm, DATE())
m.tmp_ID = SUBSTR(DTOS(DATE()), 3, 2)+STR(m.tmp_ID, 10)
APPEND BLANK
REPLACE MESS_ID		WITH m.tmp_ID, ;
        TITLE		WITH m.tmpTit, ;
        MESS_TEXT	WITH m.tmpText,;
        CR_DATE		WITH DATE(),   ;
        CR_TIME		WITH TIME(),   ;
        WHO			WITH m.user
m.tmpRec = RECNO()
USE base_path+"LETT_IN" SHARED AGAIN ALIAS IN_5A21A ORDER TAG MESS_ID
m.tmpTime = m.tmpH+":"+m.tmpM+":00"
IF m.tmpType = 1
  SELECT TMP_PERS
  SET ORDER TO TAG NAME
  SCAN
    SELECT IN_5A21A
    GO TOP
    IF .NOT. EMPTY(MESS_ID)
      APPEND BLANK
    ENDIF
    REPLACE MESS_ID		WITH m.tmp_ID, ;
            ADDRESS_T	WITH " ",      ;
            ADDRESS		WITH STR(TMP_PERS.CODE, 5), ;
            SHOW_DATE	WITH m.tmpDate,;
            SHOW_TIME	WITH m.tmpTime
    SELECT TMP_PERS
  ENDSCAN
ELSE
  SELECT TMP_ARM
  SET ORDER TO TAG NAME
  SCAN
    SELECT IN_5A21A
    GO TOP
    IF .NOT. EMPTY(MESS_ID)
      APPEND BLANK
    ENDIF
    REPLACE MESS_ID		WITH m.tmp_ID, ;
            ADDRESS_T	WITH "*",      ;
            ADDRESS		WITH TMP_ARM.CODE, ;
            SHOW_DATE	WITH m.tmpDate,;
            SHOW_TIME	WITH m.tmpTime
    SELECT TMP_ARM
  ENDSCAN
ENDIF

USE IN IN_5A21A

SELECT (m.s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Date      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �襷�� ���キ��瑙                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 22.10.2005 陳�
PROCEDURE Ch_Date

m.tmpDate = Get_Date(m.tmpDate)

SHOW GET m.tmpDate

RETURN
