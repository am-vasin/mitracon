*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ               ฎแ๋ซช  ไ ฉซ  ญ ชซ คญฎฉ ฏฅเฅค ็จ ข ไจซจ ซ.               บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 15.04.98 11:31:54 อผ
PROCEDURE S_Inv
PARAMETERS n_doc, d_doc

PRIVATE s_sav, f_name

s_sav = SELECT()

SELECT 0
USE (base_path+"RYN_M_T") ORDER TAG DOC_NUM ALIAS S_INV_T0 AGAIN
IF .NOT. SEEK(STR(YEAR(d_doc),4)+STR(n_doc,5))
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ญ ชซ คญฎฉ แ ญฎฌฅเฎฌ "+ALLTRIM(STR(n_doc))+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN .T.
ENDIF

SELECT 0
DELETE FILE (tmpo_path+"RYN000_T.DBF")
CREATE DBF (tmpo_path+"RYN000_T") ;
  ( DOC_NUM  N( 5), ;
    DOC_DATE D,     ;
    TECHNTP  C( 1), ;
    TECHNID  C(20), ;
    ORD_NUM  N( 5), ;
    ORD_DATE D,     ;
    SUP_CODE N( 8), ;
    CUS_CODE N( 8), ;
    WHO      N( 3), ;
    WHERE    C( 3)  )
APPEND BLANK
REPLACE DOC_NUM  WITH S_INV_T0.DOC_NUM,  ;
        DOC_DATE WITH S_INV_T0.DOC_DATE, ;
        ORD_NUM  WITH VAL(ALLTRIM(S_INV_T0.ORD_NUM)), ;
        ORD_DATE WITH S_INV_T0.ORD_DATE, ;
        SUP_CODE WITH S_INV_T0.SUP_CODE, ;
        CUS_CODE WITH S_INV_T0.CUS_CODE, ;
        WHO      WITH S_INV_T0.WHO,      ;
        WHERE    WITH S_INV_T0.WHERE

USE
DELETE FILE (tmpo_path+"RYN000.DBF")
CREATE DBF (tmpo_path+"RYN000") ;
  ( DOC_NUM   N( 5), ;
    DOC_DATE  D,     ;
    GTD_NUM   C(26), ;
    COUNTRY   C(16), ;
    COUNTRYC  C( 3), ;
    GTD_LINE  C( 4), ;
    CODE      N( 7), ;
    PRO_CODE  N( 5), ;
    OBJ_CODE  N(10), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    NUM_PART  C(35), ;
    NAME_ADD  C(35), ;
    DIV_CODE  N( 5), ;
    QNT_ROUND N( 5), ;
    STOP_PUR  C( 1), ;
    DATE_ON   D,     ;
    WHO_ADD   N( 3), ;
    WHERE_ADD C( 3), ;
    WHO_LEG   N( 3), ;
    NOTE_S    M,     ;
    QNT_ORD   N( 6), ;
    QNT       N( 6), ;
    NOTE      C(12)  )

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_TTT AGAIN
SELECT S_INV_T0
USE (base_path+"RYN_M") ORDER TAG DOC_NUM ALIAS S_INV AGAIN
SET RELATION TO CODE INTO STO_TTT

SEEK STR(YEAR(d_doc),4)+STR(n_doc,5)
SCAN REST WHILE d_doc = DOC_DATE .AND. n_doc = DOC_NUM
  SELECT RYN000
  APPEND BLANK
  REPLACE DOC_NUM   WITH S_INV.DOC_NUM,     ;
          DOC_DATE  WITH S_INV.DOC_DATE,    ;
          CODE      WITH S_INV.CODE,        ;
          GTD_NUM   WITH S_INV.GTD_NUM,     ;
          GTD_LINE  WITH S_INV.GTD_LINE,    ;
          COUNTRY   WITH S_INV.COUNTRY,     ;
          COUNTRYC  WITH S_INV.COUNTRYC,    ;
          PRO_CODE  WITH STO_TTT.PRO_CODE,  ;
          OBJ_CODE  WITH STO_TTT.OBJ_CODE,  ;
          PREFIX    WITH STO_TTT.PREFIX,    ;
          NAME      WITH STO_TTT.NAME,      ;
          PRODUCER  WITH STO_TTT.PRODUCER,  ;
          NUM_PART  WITH STO_TTT.NUM_PART,  ;
          NAME_ADD  WITH STO_TTT.NAME_ADD,  ;
          DIV_CODE  WITH STO_TTT.DIV_CODE,  ;
          QNT_ROUND WITH STO_TTT.QNT_ROUND, ;
          STOP_PUR  WITH IIF(FSIZE("STOP_PUR","STO_TTT") = 0,STOP_PUR,STO_TTT.STOP_PUR),;
          DATE_ON   WITH STO_TTT.DATE_ON,   ;
          WHO_ADD   WITH STO_TTT.WHO_ADD,   ;
          WHERE_ADD WITH STO_TTT.WHERE_ADD, ;
          WHO_LEG   WITH STO_TTT.WHO_LEG,   ;
          NOTE_S    WITH STO_TTT.NOTE,      ;
          QNT_ORD   WITH S_INV.QNT_ORD,     ;
          QNT       WITH S_INV.QNT,         ;
          NOTE      WITH S_INV.NOTE
  SELECT S_INV
ENDSCAN

USE
SELECT RYN000
USE
SELECT STO_TTT
USE

SELECT (s_sav)

RETURN "RYN000"
