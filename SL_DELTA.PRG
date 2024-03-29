*****************************************************
*                                                   *
*           ����� �࣮��� ��業��                 *
*                                                   *
*****************************************************
PROCEDURE Sl_Delta
PRIVATE s_date

s_date = Gt_Dt()		&& ��砫쭠� ��� ����.

IF EMPTY(s_date)
  RETURN
ENDIF

DO Prep_Fl


*****************************************************
*                                                   *
*             ��砫쭠� ��� ����                 *
*                                                   *
*****************************************************
PROCEDURE Gt_Dt
PARAMETERS sValue, prmTit

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
*
*����������������������������������������Ŀ
*�                                        �
*� ������ ��砫��� ���� ��.��.���� (F1) �
*�                                        �
*�    < OK Ctrl-W > < �⪠������ Esc >    �
*������������������������������������������

PRIVATE ex, tmpDate
tmpDate = DTOC(DATE())
tmpDate = CTOD("01"+SUBSTR(tmpDate,3))

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 44, "��࣮��� ��業��"

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 3,  3 SAY "������ ��砫��� ����            (F1)"
@ 3, 26 GET tmpDate PICTURE "@ D"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

ON KEY LABEL F1 DO GDt
READ CYCLE
ON KEY LABEL F1

IF ex # 1
  *
  * ��ࠡ��뢠�� �����
  tmpDate = {}
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN tmpDate

************************************************************
PROCEDURE GDt
tmpDate = Get_Date(tmpDate)
SHOW GET tmpDate
RETURN

*****************************************************
*                                                   *
*                ��ନ஢���� 䠩��                 *
*                                                   *
*****************************************************
PROCEDUR Prep_Fl
PRIVATE r_n, tmpFile, mss, tmpD

DELETE FILE (tmpo_path+"SL_DELTA.DBF")
CREATE DBF (tmpo_path+"SL_DELTA.DBF") ;
  (  CODE      N( 7),    ;
     PREFIX    C( 4),    ;
     NAME      C(35),    ;
     PRODICER  C( 8),    ;
     FIRM      C( 3),    ;
     DOC_NUM   C(10),    ;
     DOC_DATE  D,        ;
     CUS_CODE  N( 8),    ;
     CUS_NAME  C(50),    ;
     QNT       N(10),    ;
     PRICE_R   N(15, 4), ;
     PRICE_D   N(15, 4), ;
     PRICE_B_R N(15, 4), ;
     PRICE_B_D N(15, 4)  )

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
SELECT 0
USE (base_path+"REPORT") ORDER TAG CODE
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE
SELECT 0
USE (base_path+"ACT_S") ORDER TAG PARTY
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM
SET RELATION TO PARTY INTO ACT_S, ;
                CODE INTO STOCK, ;
                STR(CODE)+FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM+ACT_FIRM+LEFT(DTOS(ACT_DATE),4)+ACT_NUM INTO REPORT
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON
SET RELATION TO CUS_CODE INTO ACCOUNT, ;
                FIRM+LEFT(DTOS(DOC_DATE), 4)+DOC_NUM INTO SALE
SET SKIP TO SALE

IF .NOT. SEEK(DTOS(s_date))
  r_n = RECNO(0)
  IF r_n # 0
    GO r_n
  ENDIF
ENDIF
tmpD = DOC_DATE
DO Wt_Mess WITH "�����⮢�� ������..."
SCAN REST
  IF tmpD # DOC_DATE
    tmpD = DOC_DATE
    WAIT WIND NOWAIT DTOC(tmpD)
  ENDIF
  SELECT SL_DELTA
  APPEND BLANK
  REPLACE CODE      WITH SALE.CODE,      ;
          PREFIX    WITH STOCK.PREFIX,   ;
          NAME      WITH STOCK.NAME,     ;
          PRODICER  WITH STOCK.PRODUCER, ;
          FIRM      WITH SALE.FIRM,      ;
          DOC_NUM   WITH SALE.DOC_NUM,   ;
          DOC_DATE  WITH SALE.DOC_DATE,  ;
          CUS_CODE  WITH SALE_TIT.CUS_CODE,;
          CUS_NAME  WITH ACCOUNT.CUS_NAME, ;
          QNT       WITH SALE.QNT,       ;
          PRICE_R   WITH SALE.PRICE_R,   ;
          PRICE_D   WITH SALE.PRICE_D,   ;
          PRICE_B_R WITH ACT_S.PRICE_R,  ;
          PRICE_B_D WITH ACT_S.PRICE_D
  IF FOUND("REPORT")
    REPLACE PRICE_B_R WITH REPORT.PRICE_R,  ;
            PRICE_B_D WITH REPORT.PRICE_D
  ELSE
    IF ACT_S.FLG_BUY = "R" .AND. EMPTY(PRICE_B_R) .AND. EMPTY(PRICE_B_D)
      REPLACE PRICE_B_R WITH STOCK.PRICE_B_R,  ;
              PRICE_B_D WITH STOCK.PRICE_B
    ENDIF
  ENDIF
  SELECT SALE_TIT
ENDSCAN

CLOSE DATA

DO Wt_Mess

DO WHILE .T.
  tmpFile = New_File(tmpo_path, "dbf", "�㤠 �������� १����")
  IF EMPTY(tmpFile)
    EXIT
  ENDIF
  
  IF RIGHT(tmpFile, 1) = "\"
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� �� 㪠���� ��� 䠩��..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF
  IF File_O(tmpFile)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ����� 䠩� 㦥 �������. ����塞? [ �� ] [ ��� ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      LOOP
    ENDIF
    DELETE FILE (tmpFile)
  ENDIF
  COPY FILE (tmpo_path+"SL_DELTA.DBF") TO (tmpFile)
  EXIT
ENDDO
DELETE FILE (tmpo_path+"SL_DELTA.DBF")

RETURN
