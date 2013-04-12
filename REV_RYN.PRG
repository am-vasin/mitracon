*
*     Возврат из всех филиалов
*
PROCEDURE Rev_Ryn
PRIVATE n_d

USE (base_path+"ryn_m")

SELECT 0
USE (base_path+"stock") ORDER TAG CODE

SELECT 0
USE (base_path+"ryn_m_t")

SELECT 0
USE (base_path+"ryn_l") ORDER TAG CUS_CODE
DELETE ALL FOR QNT = 0

SELECT 0
USE (base_path+"ryn_l_t")

scan
  WAIT WIND CUS_NAME NOWAIT
  IF .NOT. SEEK(CUS_CODE, "ryn_l")
    DELETE
    LOOP
  ENDIF
  n_d = 0
  SELECT ryn_l
  SCAN REST WHILE CUS_CODE = RYN_L_T.CUS_CODE
    IF EMPTY(n_d)
      n_d = Doc_Num("RYN_M", "   ", arm, DATE())
    ENDIF
    SELECT RYN_M
    APPEND BLANK
    REPLACE DOC_NUM  WITH n_d,        ;
            DOC_DATE WITH DATE(),     ;
            CODE     WITH RYN_L.CODE, ;
            QNT      WITH -RYN_L.QNT
    SELECT STOCK
    SEEK RYN_L.CODE
    REPLACE QNT      WITH QNT+RYN_L.QNT, ;
            QNT_REAL WITH QNT_REAL-RYN_L.QNT
    SELECT RYN_L
    DELETE
  ENDSCAN
  SELECT RYN_M_T
  APPEND BLANK
  REPLACE DOC_NUM  WITH n_d,      ;
          DOC_DATE WITH DATE(),   ;
          DOC_TIME WITH SECONDS(),;
          SUP_CODE WITH sup_own,  ;
          CUS_CODE WITH RYN_L_T.CUS_CODE, ;
          WHO      WITH user,     ;
          WHERE    WITH arm
  SELECT RYN_L_T
  DELETE
ENDSCAN

CLOSE DATABASES

*
*   Проверка корректности...
*
USE (base_path+"stock") ORDER TAG CODE
LOCATE FOR QNT_REAL # 0
IF FOUND()
  WAIT WINDOW PREFIX+NAME+PRODUCER+" ОСТАЛОСЬ В ФИЛИАЛЕ!!!!" 
ENDIF
USE
