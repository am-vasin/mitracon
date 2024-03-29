*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          ����� �牀▲爲� ������� �� ��瘁�� �� ��｀腮� �メ���.          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE NEW_T_K
PARAMETER mode

PRIVATE d_s, ab, sch

sch = SCHEME(13,1)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,4)+","+ ;
      SCHEME(13,5)+","+ ;
      SCHEME(13,6)+","+ ;
      SCHEME(13,7)+","+ ;
      SCHEME(13,8)+","+ ;
      SCHEME(13,9)
IF mode = "c"
  PRIVATE log_rec, err_cnt

  log_rec = Use_Log("��瘁� �� ���. �メ���")   

  USE PARMS
  d_s = DAT_START

  DELETE FILE (log_path+"N_T_M.DBF")
  DELETE FILE (log_path+"N_T_M.CDX")

  CREATE DBF (log_path+"N_T_M")  ;
     (  ARM_C     C( 3),   ;
        ARM_N     C(16),   ;
        KASSA_USD N(13,4), ;
        S_KASSA_U N(13,4), ;
        KASSA     N(12,2), ;
        S_KASSA   N(12,2)  )
  USE (log_path+"N_T_M") EXCLUSIVE
  INDEX ON ARM_C TAG ARM_C
  APPEND FIELDS ARM_C, ARM_N, KASSA, S_KASSA, KASSA_USD, S_KASSA_U  ;
                             FROM ARM_LST

  *DO Wt_Mess WITH "��瘍モ ����膈�痰�..."
  *
  *   ��瘍モ �諤竍��
  *
  SELECT 0
  *USE SALE_TIT
  *SET FILTER TO DOC_DATE >= d_s
  *SCAN
  *  IF .NOT. SEEK(WHERE, "N_T_M")
  *    LOOP
  *  ENDIF
  *  SELECT N_T_M
  *  REPLACE S_KASSA WITH S_KASSA+SALE_TIT.KASSA
  *  SELECT SALE_TIT
  *ENDSCAN

  USE INCMONEY
  SET FILTER TO DOC_DATE >= d_s .AND. DOC_TYPE $ " 3�"
  SCAN
    IF .NOT. SEEK(WHERE, "N_T_M")
      LOOP
    ENDIF
    SELECT N_T_M
    IF INCMONEY.DOC_TYPE = "3"
      REPLACE S_KASSA_U WITH S_KASSA_U+INCMONEY.MONEY_D+INCMONEY.N_OF_SALE
    ELSE
      REPLACE S_KASSA WITH S_KASSA+INCMONEY.MONEY_R+INCMONEY.N_OF_SALE
    ENDIF
    SELECT INCMONEY
  ENDSCAN

  USE KASSA
  SET FILTER TO DOC_DATE >= d_s .AND. .NOT. EMPTY(WHO_GET)
  SCAN
    IF .NOT. SEEK(WHERE, "N_T_M")
      LOOP
    ENDIF
    SELECT N_T_M
    IF KASSA.VALUTA = "$"
      REPLACE S_KASSA_U WITH S_KASSA_U-KASSA.TOTAL
    ELSE
      REPLACE S_KASSA WITH S_KASSA-KASSA.TOTAL
    ENDIF
    SELECT KASSA
  ENDSCAN
  USE
  SELECT N_T_M
  COUNT FOR S_KASSA # KASSA .OR. S_KASSA_U # KASSA_USD TO err_cnt
  DO Term_Log WITH log_rec, err_cnt  

ELSE

  DO Use_Dummy
  *DO Wt_Mess

  SELECT 0
  USE (log_path+"N_T_M")

  PRIVATE ln, wd, tt, ft
  DIMENSION tt(3), ft(1)
  tt(1) = ;
  "陳陳陳陳陳陳陳陳堕陳陳陳陳陳賃陳陳陳陳陳陳堕陳陳陳陳陳賃陳陳陳陳陳陳堕陳"
  tt(2) = ;
  "  ���. �メ皰    �  USD �. �. �USD ��瘍モ�.�  �磧 �. �. ��磧 ��瘍モ�.���."
  tt(3) = ;
  "陳陳陳陳陳陳陳陳田陳陳陳陳陳津陳陳陳陳陳陳田陳陳陳陳陳津陳陳陳陳陳陳田陳"
  ft(1) = ;
  "陳陳陳陳陳陳陳陳祖陳陳陳陳陳珍陳陳陳陳陳陳祖陳陳陳陳陳珍陳陳陳陳陳陳祖陳"

  ln = WROWS("")-10
  wd = LEN(ft(1))
  DO Def_Win WITH "Tst_K_M", "Tst_K_E", "Tst_K_I", ln, wd, ;
                  "蹍▲爲� ��瘁", tt, ft

  @ 1, 1 SAY "�"
  @ 2, 1 SAY "�"
  @ 3, 1 SAY "�"
  @ WROWS()-1, 1 SAY "�"

  @ 1, WCOLS()-2 SAY "�"
  @ 2, WCOLS()-2 SAY "�"
  @ 3, WCOLS()-2 SAY "�"
  @ WROWS()-1, WCOLS()-2 SAY "�"

  ACTIVATE WINDOW Tst_K_E
  GO TOP
  BROWSE FIELDS ARM_N:H="",   ;
                KASSA_USD:H="":P="@Z 9999999.9999", ;
                S_KASSA_U:H="":P="@Z 9999999.9999", ;
                KASSA:H="":P="@Z 999999999.99",   ;
                S_KASSA:H="":P="@Z 999999999.99", ;
                f001 = IIF(S_KASSA # KASSA .OR. S_KASSA_U # KASSA_USD, " � ", "   "):H="",   ;
                DUMMY.F   FREEZE DUMMY.F  ;
                NOMENU WINDOW Tst_K_I IN WINDOW Tst_K_E COLOR (sch)
  RELEASE WINDOW Tst_K_I
  RELEASE WINDOW Tst_K_E
  RELEASE WINDOW Tst_K_M
ENDIF

CLOSE DATABASES
RETURN
