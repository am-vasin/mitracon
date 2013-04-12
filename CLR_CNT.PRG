*
*  Очистка счетчиков документов.
*
PRIVATE lst, i, s
DIMENSION lst(2,23)
lst( 1, 1) = "PARMS"
lst( 1, 2) = "PARMS"
lst( 1, 3) = "PARMS"
lst( 1, 4) = "PARMS"
lst( 1, 5) = "PARMS"
lst( 1, 6) = "PARMS"
lst( 1, 7) = "PARMS"
lst( 1, 8) = "PARMS"
lst( 1, 9) = "PARMS"
lst( 1,10) = "PARMS"
lst( 1,11) = "PARMS"
lst( 1,12) = "PARMS"
lst( 1,13) = "PARMS"
lst( 1,14) = "PARMS"
lst( 1,15) = "ARM_LST"
lst( 1,16) = "ARM_LST"
lst( 1,17) = "ARM_LST"
lst( 1,18) = "ARM_LST"
lst( 1,19) = "ARM_LST"
lst( 1,20) = "ARM_LST"
lst( 1,21) = "ARM_LST"
lst( 1,22) = "ARM_LST"
lst( 1,23) = "KASSA_S"
lst( 2, 1) = "LAST_BN"
lst( 2, 2) = "LAST_BF"
lst( 2, 3) = "LAST_BF0"
lst( 2, 4) = "LAST_ORD"
lst( 2, 5) = "LAST_ORD0"
lst( 2, 6) = "LAST_ORDR"
lst( 2, 7) = "LAST_ORDD"
lst( 2, 8) = "LAST_ACT_S"
lst( 2, 9) = "LAST_RYN"
lst( 2,10) = "LAST_SALE"
lst( 2,11) = "LAST_INV"
lst( 2,12) = "LAST_LST"
lst( 2,13) = "LAST_REP0"
lst( 2,14) = "LAST_SPEC"
lst( 2,15) = "LAST_BILL"
lst( 2,16) = "LAST_BN"
lst( 2,17) = "LAST_IND"
lst( 2,18) = "LAST_PRS"
lst( 2,19) = "LAST_PAY"
lst( 2,20) = "LAST_INV"
lst( 2,21) = "LAST_ORD"
lst( 2,22) = "LAST_REP"
lst( 2,23) = "LAST_DOC"

FOR i = 1 TO 23
  IF .NOT. File_O(base_path+lst(1,i)+".DBF")
    WAIT WIND "Отсутствует файл "+lst(1,i)
    LOOP
  ENDIF
  USE (base_path+lst(1,i))
  IF FSIZE(lst(2,i),lst(1,i)) = 0
    WAIT WIND "В файле "+lst(1,i)+" отсутствует поле "+lst(2,i)
    LOOP
  ENDIF
  s = "REPLACE ALL "+lst(1,i)+"."+lst(2,i)+" WITH 0"
  &s
ENDFOR

USE
