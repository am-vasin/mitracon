*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Act_GTD      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                ��⠭���� ��� ��� ���⨩ �ਥ����� ���.                �
*�                                                                        �
*����������������������������������������������������������� 27.02.2000 �ͼ
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
  mss(2) = CHR(0)+"��� 㪠������� ���� ���㬥�� �� ������� �� ����� ���⨨!"
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
DO Wt_Mess WITH "�����⮢�� ᯨ᪠ ���⨩"
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
  mss(2) = CHR(0)+"�� ������� �� ����� ���室�饩 ���⨨!"
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
    mss(2) = CHR(0)+"�� 㤠���� �������� �� ����� ���!"
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
    mss(2) = CHR(0)+"������� �������� ��� ��� "+ALLTRIM(STR(tmpVal))+" ���⨩ �� "+ALLTRIM(STR(RECCOUNT()))+"."
    mss(3) = CHR(0)+"  ���⠢��� �������� ���? [ �� ] [ ��� ]"
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
  mss(2) = CHR(0)+"����㯠�� � ��⠭���� ���? [ �� ] [ ��� ]"
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

DO Wt_Mess WITH "��⠭���� ���"
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� GetMode      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ��⠭���� ०���.                            �
*�                                                                        �
*����������������������������������������������������������� 27.02.2000 ���
PROCEDURE GetMode

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

*����������������������������������Ŀ
*� �� � �. �. ��� ���⨩ ��������Ŀ �
*� � [ ] � ����� ���⪮�        � �
*� � [ ] � ������� ���           � �
*� � [ ] ���. ����権            � �
*� �������������������������������� �
*�       [ ] ��筮� ����஫�        �
*� < OK Ctrl-W > < �⪠������ Esc > �
*������������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 10, 38, "��⠭���� ���"
DO Sun_Bord WITH  2,  3,  6, 34, " � �. �. ��� ���⨩ "
*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 3, 5 GET swQnt PICTURE "@*C � ����� ���⪮�"
@ 4, 5 GET swGTD PICTURE "@*C � ������� ���"
@ 5, 5 GET swRus PICTURE "@*C ���. ����権"
@ 7, 9 GET swManual PICTURE "@*C ��筮� ����஫�"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1