*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Act_GTD      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                �痰���→� ��� か� ��珥┤ �爬ガ���� ����.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 27.02.2000 様�
PROCEDURE Act_GTD
PARAMETERS pFirm, pDoc, pDate

PRIVATE s_sav, doc_d, tmpVal
s_sav = SELECT()
SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM ALIAS INC_0227 AGAIN

doc_d = STR(YEAR(DATE()),4)
DO CASE
CASE TYPE("pDate") = "C"
  doc_d = pDate
CASE TYPE("pDate") = "D"
  doc_d = STR(YEAR(pDate),4)
CASE TYPE("pDate") = "N"
  doc_d = STR(pDate,4)
ENDCASE

IF .NOT. SEEK(pFirm+doc_d+pDoc)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� 礫�������� ���� ぎ�祠キ�� �� ���ぅ�� �� �き�� ��珥┬!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

PRIVATE swGTD, swQnt, swRus, swManual

swGTD = .F.
swRus = .F.
swQnt = .T.
swManual = .T.

IF .NOT. GetMode()
  USE
  SELECT (s_sav)
  RETURN
ENDIF
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_0227 AGAIN
SELECT 0

DELETE FILE (tmpo_path+"GTD_LINK.CDX")
DELETE FILE (tmpo_path+"GTD_LINK.DBF")

CREATE DBF (tmpo_path+"GTD_LINK") ;
  ( LINK     N(10), ;
    CODE     N( 7), ;
    MARK     L,     ;
    PREFIX   C( 4), ;
    NAME     C(78), ;
    PRODUCER C( 8), ;
    GTD_NAME C(29), ;
    GTD_NUM  C(26), ;
    GTD_LINE C( 4), ;
    COUNTRY  C(16), ;
    COUNTRYC C( 3), ;
    B_GTD    L  )

SELECT INC_0227
SET RELATION TO CODE INTO STO_0227
DO Wt_Mess WITH "��ぃ�皰→� 甎�瓷� ��珥┤"
SCAN REST WHILE FIRM = pFirm .AND. DOC_NUM = pDoc .AND. STR(YEAR(DOC_DATE),4) = doc_d
  IF .NOT. swRus .AND. STO_0227.STOP_PUR = "S"
    LOOP
  ENDIF
  IF .NOT. swGTD .AND. .NOT. EMPTY(GTD_NUM)
    LOOP
  ENDIF
  IF .NOT. swQnt .AND. EMPTY(QNT_REST)
    LOOP
  ENDIF
  SELECT GTD_LINK
  APPEND BLANK
  REPLACE LINK     WITH RECNO("INC_0227"), ;
          CODE     WITH STO_0227.CODE,     ;
          MARK     WITH .T.,               ;
          PREFIX   WITH STO_0227.PREFIX,   ;
          NAME     WITH STO_0227.NAME,     ;
          PRODUCER WITH STO_0227.PRODUCER
  SELECT INC_0227
ENDSCAN
DO Wt_Mess

SELECT STO_0227
USE
SELECT GTD_LINK
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ���ぅ�� �� �き�� ��ゅ�わ薀� ��珥┬!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"GTD_LINK.CDX")
  DELETE FILE (tmpo_path+"GTD_LINK.DBF")
  SELECT INC_0227
  USE
  SELECT (s_sav)
  RETURN
ENDIF

tmpVal = GTD_Link(swManual)
SELECT GTD_LINK
IF .NOT. swManual
  IF tmpVal = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� 磴���瘡 ��ぎ÷�碎 �� �き�� ���!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    DELETE FILE (tmpo_path+"GTD_LINK.CDX")
    DELETE FILE (tmpo_path+"GTD_LINK.DBF")
    SELECT INC_0227
    USE
    SELECT (s_sav)
    RETURN
  ELSE
    PRIVATE mss
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"�����瘡 ��ぎ÷�碎 ��� か� "+ALLTRIM(STR(tmpVal))+" ��珥┤ ├ "+ALLTRIM(STR(RECCOUNT()))+"."
    mss(3) = CHR(0)+"  蹍痰�※碎 ���ぅ��襯 ���? [ �� ] [ �モ ]"
    mss(4) = ""
    IF Out_Mess(5, "mss") = 2
      USE
      DELETE FILE (tmpo_path+"GTD_LINK.CDX")
      DELETE FILE (tmpo_path+"GTD_LINK.DBF")
      SELECT INC_0227
      USE
      SELECT (s_sav)
      RETURN
    ENDIF
  ENDIF
ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"踸痰祚�ガ � 竅����→� ���? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 2
    USE
    DELETE FILE (tmpo_path+"GTD_LINK.CDX")
    DELETE FILE (tmpo_path+"GTD_LINK.DBF")
    SELECT INC_0227
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
ENDIF

DO Wt_Mess WITH "�痰���→� ���"
SELECT INC_0227
SET ORDER TO
SELECT GTD_LINK
SCAN
  IF .NOT. EMPTY(GTD_NUM)
    GO GTD_LINK.LINK IN INC_0227
    REPLACE INC_0227.GTD_NUM  WITH GTD_LINK.GTD_NUM, ;
            INC_0227.COUNTRY  WITH GTD_LINK.COUNTRY, ;
            INC_0227.COUNTRYC WITH GTD_LINK.COUNTRYC,;
            INC_0227.GTD_LINE WITH GTD_LINK.GTD_LINE,;
            INC_0227.B_GTD    WITH GTD_LINK.B_GTD
  ENDIF
ENDSCAN
DO Wt_Mess

USE
DELETE FILE (tmpo_path+"GTD_LINK.CDX")
DELETE FILE (tmpo_path+"GTD_LINK.DBF")
SELECT INC_0227
USE
SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� GetMode      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �痰���→� 爛Θ��.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.02.2000 陳�
PROCEDURE GetMode

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 � �. �. か� ��珥┤ 陳陳陳陳朕 �
*� � [ ] � �竅硅� �痰�皖��        � �
*� � [ ] � �ク竅硅� ���           � �
*� � [ ] �皀�. ��Ж罔�            � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�       [ ] �竍��� ���矗���        �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 38, "�痰���→� ���"
DO Sun_Bord WITH  2,  3,  6, 34, " � �. �. か� ��珥┤ "
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3, 5 GET swQnt PICTURE "@*C � �竅硅� �痰�皖��"
@ 4, 5 GET swGTD PICTURE "@*C � �ク竅硅� ���"
@ 5, 5 GET swRus PICTURE "@*C �皀�. ��Ж罔�"
@ 7, 9 GET swManual PICTURE "@*C �竍��� ���矗���"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
