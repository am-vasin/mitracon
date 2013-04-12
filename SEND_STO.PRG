*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Send_Sto     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║             Прередача файла накладной для приема в STORE.              ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 12/08/1999 ══╝
PROCEDURE Send_Sto
PARAMETERS f_code, inv_n, inv_d
*
*  f_code - код предприятия;
*  inv_n  - номер накладной ( внутренний формат );
*  inv_d  - либо дата накладной либо год в символьном или числовом формате;
*

PRIVATE s_sav, d_year, w

s_sav = SELECT()

IF TYPE("inv_d") = "D"
  d_year = LEFT(DTOS(inv_d),4)
ENDIF
IF TYPE("inv_d") = "N"
  d_year = STR(inv_d,4)
ENDIF
IF TYPE("inv_d") = "C"
  d_year = inv_d
ENDIF

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS ST_9C08
IF .NOT. SEEK(f_code+d_year+inv_n)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет такой накладной!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

SELECT 0
DELETE FILE (tmpo_path+"S_TMP.DBF")
CREATE DBF (tmpo_path+"S_TMP") ;
  (   DOC_NUM  N( 5), ;
      DOC_DATE D,     ;
      ORD_NUM  N( 5), ;
      ORD_DATE D,     ;
      SUP_CODE N( 8), ;
      CUS_CODE N( 8), ;
      WHO      N( 3), ;
      WHERE    C( 3) )
APPEND BLANK
IF TYPE("sup_own") = "N"
  w = sup_own
ELSE
  w = 0
ENDIF
REPLACE DOC_NUM  WITH VAL(ALLTRIM(RIGHT(ST_9C08.DOC_NUM,5))), ;
        DOC_DATE WITH ST_9C08.DOC_DATE,     ;
        ORD_NUM  WITH VAL(ALLTRIM(ST_9C08.REQ_DOC)),;
        ORD_DATE WITH ST_9C08.REQ_DATE,     ;
        SUP_CODE WITH w,                    ;
        CUS_CODE WITH ST_9C08.CUS_CODE,     ;
        WHO      WITH ST_9C08.WHO,          ;
        WHERE    WITH ST_9C08.WHERE
USE
SELECT ST_9C08
w = FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_9C08
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS SL_9C08
SET RELATION TO CODE INTO ST_9C08
SELECT 0
DELETE FILE (tmpo_path+"SL_TMP.DBF")
DELETE FILE (tmpo_path+"SL_TMP.FPT")
CREATE DBF (tmpo_path+"SL_TMP") ;
  (  DOC_NUM     N( 5), ;
     DOC_DATE    D,     ;
     CODE        N( 7), ;
     PRO_CODE    N( 7), ;
     OBJ_CODE    N(10), ;
     PREFIX      C( 4), ;
     NAME        C(35), ;
     PRODUCER    C( 8), ;
     NUM_PART    C(35), ;
     NAME_ADD    C(35), ;
     DIV_CODE    N( 5), ;
     QNT_ROUND   N( 5), ;
     DATE_ON     D,     ;
     WHO_ADD     N( 3), ;
     WHERE_ADD   C( 3), ;
     WHO_LEG     N( 3), ;
     GTD_NUM     C(26), ;
     COUNTRY     C(16), ;
     COUNTRYC    C( 3), ;
     GTD_LINE    C( 4), ;
     NOTE_S      M,     ;
     QNT_ORD     N( 6), ;
     QNT         N( 6), ;
     NOTE        C(12) )
SELECT SL_9C08
SEEK w
SCAN REST WHILE FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM = w
  SELECT SL_TMP
  APPEND BLANK
  REPLACE DOC_NUM   WITH VAL(ALLTRIM(RIGHT(SL_9C08.DOC_NUM,5))), ;
          DOC_DATE  WITH SL_9C08.DOC_DATE,  ;
          CODE      WITH SL_9C08.CODE,      ;
          PRO_CODE  WITH ST_9C08.PRO_CODE,  ;
          OBJ_CODE  WITH ST_9C08.OBJ_CODE,  ;
          PREFIX    WITH ST_9C08.PREFIX,    ;
          NAME      WITH ST_9C08.NAME,      ;
          PRODUCER  WITH ST_9C08.PRODUCER,  ;
          NUM_PART  WITH ST_9C08.NUM_PART,  ;
          NAME_ADD  WITH ST_9C08.NAME_ADD,  ;
          DIV_CODE  WITH ST_9C08.DIV_CODE,  ;
          QNT_ROUND WITH ST_9C08.QNT_ROUND, ;
          DATE_ON   WITH ST_9C08.DATE_ON,   ;
          WHO_ADD   WITH ST_9C08.WHO_ADD,   ;
          WHERE_ADD WITH ST_9C08.WHERE_ADD, ;
          WHO_LEG   WITH ST_9C08.WHO_LEG,   ;
          QNT       WITH SL_9C08.QNT,       ;
          GTD_NUM   WITH SL_9C08.GTD_NUM,   ;
          COUNTRY   WITH SL_9C08.COUNTRY,   ;
          COUNTRYC  WITH SL_9C08.COUNTRYC,  ;
          GTD_LINE  WITH SL_9C08.GTD_LINE,  ;
          NOTE_S    WITH ST_9C08.NOTE
  SELECT SL_9C08
ENDSCAN
USE
SELECT ST_9C08
USE
SELECT SL_TMP
USE
SELECT (s_sav)
w = "C:\"
DO WHILE .T.
  w = Int_Dir(w, "Куда пишем результат?", 0)
  IF EMPTY(w)
    EXIT
  ENDIF
  IF File_O(w+"SAL_S.DBF") .OR. ;
     File_O(w+"SAL_S_T.DBF") .OR. ;
     File_O(w+"SAL_S.FPT")
    PRIVATE mss
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Там уже есть накладные! Что делать?"
    mss(3) = CHR(0)+"    [ Удалить ] [ Сменить путь] [ Отказаться]"
    mss(4) = ""
    s_sav = Out_Mess(7, "mss")
    DO CASE
    CASE s_sav = 1
      DELETE FILE (w+"SAL_S.DBF")
      DELETE FILE (w+"SAL_S_T.DBF")
      DELETE FILE (w+"SAL_S.FPT")
    CASE s_sav = 1
      LOOP
    OTHERWISE
      EXIT
    ENDCASE
  ENDIF
  COPY FILE (tmpo_path+"S_TMP.DBF") TO (w+"SAL_S_T.DBF")
  COPY FILE (tmpo_path+"SL_TMP.DBF") TO (w+"SAL_S.DBF")
  COPY FILE (tmpo_path+"SL_TMP.FPT") TO (w+"SAL_S.FPT")
  EXIT
ENDDO

DELETE FILE (tmpo_path+"S_TMP.DBF")
DELETE FILE (tmpo_path+"SL_TMP.DBF")
DELETE FILE (tmpo_path+"SL_TMP.FPT")

RETURN
