*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�         ��ぃ�皰→� ����諷 か� 蹣甎�珥� � ＜紕��皀爬�.                 �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Export_B

PRIVATE dat_0, dat_1, dat_v0, dat_v1, ex, c_name, p_numb, p_line
PRIVATE mss

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
PRIVATE ln, ln1, l_ex, wk_list
USE PLACES
p_numb = RECCOUNT()
p_line = ""
FOR dat_1 = 1 TO p_numb
  p_line = p_line+CHR(ASC("A")+dat_1-1)
ENDFOR

USE PARMS
dat_0  = DAT_START
dat_v0 = DAT_START
USE
dat_1  = DATE()
dat_v1 = DATE()

DIMENSION wk_list(p_numb)
FOR ex = 1 TO p_numb
  wk_list(ex) = CHR(ASC("A")+ex-1) $ p_line
ENDFOR

ex   = 1

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��皀燿�� 陳陳陳陳陳陳陳陳� 敖 ç��お� 陳陳陳陳陳陳陳陳陳朕 �
*� �                           � �                              � �
*� � �����讚�� ���� ��.��.���� � � ( ) ....:....!....:....!.... � �
*� �                           � �                              � �
*� � ���ョ��� ����  ��.��.���� � �                              � �
*� �                           � �                              � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�               < OK Ctrl-W > < �皖���碎瘴 Esc >                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

ln1  = p_numb+4
ln   = ln1+4
l_ex = 6+p_numb

DO D_Win_N WITH ln, 68, "�┼赳�"

DO Sun_Bord WITH 2, 3, 8, 31
@  2, 5 SAY " ��皀燿�� "

DO Sun_Bord WITH 2, 32, 2+ln1-1, 64
@  2, 34 SAY " ç��お� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 4,  5 SAY "�����讚�� ����" GET dat_0 PICTURE "@D"
@ 6,  5 SAY "���ョ��� ���� " GET dat_1 PICTURE "@D"
DO G_Place WITH 4, 35, wk_list

@ l_ex, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID V_Dat()

POP KEY
RELEASE WINDOW (win_name)
IF ex = 2
  RETURN
ENDIF
PRIVATE mss

DO Wt_Mess WITH "��ぁ�爲� ����諷..."

IF File_O(tmpo_path+"EXPORT_B.DBF")
  DELETE FILE (tmpo_path+"EXPORT_B.DBF")
ENDIF
SET EXCLUSIVE ON
IF .F.    &&&&&&&&&&&&&&&&&&& �� ���※�讚�� ▲珀┬ �������������
  CREATE DBF (tmpo_path+"EXPORT_B.DBF") ;
    ( OPER_NUMB  N( 6), ;
      WORK_PLACE N( 2), ;
      DATE       D,     ;
      DOCUMENT   C(15), ;
      COMMENT   C(250), ;
      DEBIT      C(20), ;
      CREDIT     C(20), ;
      SUM        N(15,2), ;
      VALUE      N(15,3)  )
ELSE
  CREATE DBF (tmpo_path+"EXPORT_B.DBF") ;
    ( OPER_NUMB  N( 6), ;
      WORK_PLACE N( 2), ;
      DATE       D,     ;
      DOCUMENT   C( 8), ;
      COMMENT_1  C(48), ;
      COMMENT_2  C(48), ;
      DEBIT      C(20), ;
      CREDIT     C(20), ;
      SUM        N(15,2), ;
      VALUE      N(15,3)  )
ENDIF

USE (tmpo_path+"EXPORT_B.DBF")
SET EXCLUSIVE OFF

SELECT 0
USE ACCOUNT ORDER TAG CUS_CODE

SELECT 0
USE INCMONEY ORDER TAG DOC_NUM

SELECT 0
USE SALE_TIT ORDER TAG HRON

SEEK DTOS(dat_0)
IF .NOT. FOUND()
  ex = RECNO(0)
  IF ex > 0 .AND. ex <= RECCOUNT()
    GO ex
  ELSE
    GO BOTTOM
    ex = 0
  ENDIF
ELSE
  ex = RECNO()
ENDIF
IF ex # 0
  SCAN REST WHILE DOC_DATE <= dat_1 FOR LEFT(DOC_NUM,1) $ p_line
    IF NEW_TYPE # " " .OR. TOTAL_R = 0
      LOOP
    ENDIF
    c_name = IIF(SEEK(CUS_CODE,"ACCOUNT"),  ;
             ACCOUNT.CUS_NAME, CUS_NAME)
    SELECT EXPORT_B
    APPEND BLANK
    IF .F.    &&&&&&&&&&&&&&&&&&& �� ���※�讚�� ▲珀┬ �������������
      REPLACE OPER_NUMB  WITH RECNO(),               ;
              WORK_PLACE WITH 1,                     ;
              DOCUMENT   WITH IIF(SALE_TIT.NEW_TYPE="0", "*"," ")+  ;
          LEFT(SALE_TIT.NEW_DOC,1)+ALLTRIM(SUBSTR(SALE_TIT.NEW_DOC,2)), ;
              DATE       WITH SALE_TIT.DOC_DATE,     ;
              COMMENT    WITH "�皴竕キ�:"+CHR(13)+"   "+c_name,  ;
              DEBIT      WITH "62", ;
              CREDIT     WITH "46/���", ;
              SUM        WITH SALE_TIT.TOTAL_R
    ELSE
      REPLACE OPER_NUMB  WITH RECNO(),               ;
              WORK_PLACE WITH 1,                     ;
              DOCUMENT   WITH IIF(SALE_TIT.NEW_TYPE="0", "*"," ")+  ;
          LEFT(SALE_TIT.NEW_DOC,1)+ALLTRIM(SUBSTR(SALE_TIT.NEW_DOC,2)), ;
              DATE       WITH SALE_TIT.DOC_DATE,     ;
              COMMENT_1  WITH "�皴竕キ� �����オ矣鉗┘",  ;
              COMMENT_2  WITH "   "+c_name,  ;
              DEBIT      WITH "62", ;
              CREDIT     WITH "46/���", ;
              SUM        WITH SALE_TIT.TOTAL_R
    ENDIF
    SELECT SALE_TIT
  ENDSCAN
ENDIF

SELECT INCMONEY
SCAN FOR BETWEEN(ENTER_DAT, dat_0, dat_1) .AND. DOC_TYPE = "1" .AND. LEFT(DOC_NUM,1) $ p_line
  c_name = IIF(SEEK(CUS_CODE,"ACCOUNT"),  ;
           ACCOUNT.CUS_NAME, CUS_NAME)
  SELECT EXPORT_B
  APPEND BLANK
  IF .F.    &&&&&&&&&&&&&&&&&&& �� ���※�讚�� ▲珀┬ �������������
    REPLACE OPER_NUMB  WITH RECNO(),             ;
            DATE       WITH INCMONEY.ENTER_DAT,  ;
            WORK_PLACE WITH 1,                     ;
            DOCUMENT   WITH ALLTRIM(INCMONEY.ENTER_DOC), ;
            COMMENT    WITH "��痰. �� �/�:"+CHR(13)+"   "+c_name,  ;
            DEBIT      WITH "51", ;
            CREDIT     WITH "62", ;
            SUM        WITH INCMONEY.MONEY_R
  ELSE
    REPLACE OPER_NUMB  WITH RECNO(),             ;
            DATE       WITH INCMONEY.ENTER_DAT,  ;
            WORK_PLACE WITH 1,                     ;
            DOCUMENT   WITH ALLTRIM(INCMONEY.ENTER_DOC), ;
            COMMENT_1  WITH "��痰. �� �/� �� �����オ矣鉗┘",  ;
            COMMENT_2  WITH "   "+c_name,  ;
            DEBIT      WITH "51", ;
            CREDIT     WITH "62", ;
            SUM        WITH INCMONEY.MONEY_R
  ENDIF
  SELECT INCMONEY
ENDSCAN

SET ORDER TO TAG HRON
SEEK DTOS(dat_0)
IF .NOT. FOUND()
  ex = RECNO(0)
  IF ex > 0 .AND. ex <= RECCOUNT()
    GO ex
  ELSE
    GO BOTTOM
    ex = 0
  ENDIF
ELSE
  ex = RECNO()
ENDIF
IF ex # 0
  SCAN REST WHILE DOC_DATE <= dat_1 FOR LEFT(DOC_NUM,1) $ p_line .AND. DOC_TYPE = " "
    c_name = IIF(SEEK(CUS_CODE,"ACCOUNT"),  ;
             ACCOUNT.CUS_NAME, CUS_NAME)
    SELECT EXPORT_B
    APPEND BLANK
    IF .F.    &&&&&&&&&&&&&&&&&&& �� ���※�讚�� ▲珀┬ �������������
      REPLACE OPER_NUMB  WITH RECNO(),               ;
              DATE       WITH INCMONEY.DOC_DATE,     ;
              WORK_PLACE WITH 1,                     ;
              DOCUMENT   WITH LEFT(INCMONEY.DOC_NUM,1)+ALLTRIM(SUBSTR(INCMONEY.DOC_NUM,2)), ;
              COMMENT    WITH "��痰祚┼� � ��瘁�: "+CHR(13)+"   "+c_name, ;
              DEBIT      WITH "50", ;
              CREDIT     WITH "62", ;
              SUM        WITH INCMONEY.MONEY_R
    ELSE
      REPLACE OPER_NUMB  WITH RECNO(),               ;
              DATE       WITH INCMONEY.ENTER_DAT,    ;
              WORK_PLACE WITH 1,                     ;
              DOCUMENT   WITH LEFT(INCMONEY.DOC_NUM,1)+ALLTRIM(SUBSTR(INCMONEY.DOC_NUM,2)), ;
              COMMENT_1  WITH "踸絎� � ��瘁�: ぎ����� � ．Л���", ;
              COMMENT_2  WITH "   "+c_name, ;
              DEBIT      WITH "50", ;
              CREDIT     WITH "62", ;
              SUM        WITH INCMONEY.MONEY_R
    ENDIF
    SELECT INCMONEY
  ENDSCAN
ENDIF

DO Wt_Mess

IF RECCOUNT("EXPORT_B") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 礫�����覃 ���� �ム┏� �� ���ぅ�� �� �き�� ��ム�罔�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  CLOSE DATABASES
  DO To_Disk WITH tmpo_path+"EXPORT_B.DBF"
ENDIF

CLOSE DATABASES
DELETE FILE (tmpo_path+"EXPORT_B.DBF")

RETURN

*********************************************************************
PROCEDURE V_Dat

IF ex = 2
  RETURN .T.
ENDIF

IF dat_0 < dat_v0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��ゥ� ����襯 皰�讓� � "+TRANSFORM(dat_v0,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_0 = dat_v0
  SHOW GETS
  RETURN .F.
ENDIF

IF dat_1 > dat_v1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ィ�き� 皰�讓� "+TRANSFORM(dat_v1,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_1 = dat_v1
  SHOW GETS
  RETURN .F.
ENDIF
PRIVATE l_tmp, i

l_tmp = ""
FOR i = 1 TO p_numb
  IF wk_list(i)
    l_tmp = l_tmp+CHR(ASC("A")+i-1)
  ENDIF
ENDFOR

IF EMPTY(l_tmp)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�甄� ��� �� ┃皀爛痺モ �� �き� �����お�?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

p_line = l_tmp

RETURN .T.
