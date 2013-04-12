*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º ” ©« Rem_acc       §à ¡®âç¨ª €­¤à¥© ‚ á¨­           14.01.98 09:29:43 º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                  ‚®ááâ ­®¢«¥­¨¥ ª áá ­  à ¡. ¬¥áâ å                    º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Rem_Arm

SET DELE ON
SET EXCLU OFF
DO Set_Lwr

USE (base_path+"PARMS")
d_s = DAT_START
USE
DELETE FILE TST_ARM.DBF
DELETE FILE TST_ARM.CDX

CREATE DBF TST_ARM  ;
   (  ARM_C   C( 3),  ;
      ARM_N   C(16),  ;
      S_KASSA N(10,2),  ;
      KASSA   N(10,2)  )
INDEX ON ARM_C TAG ARM_C
APPEND FIELDS ARM_C, ARM_N, S_KASSA, KASSA FROM (base_path+"ARM_LST")

SELECT 0
USE (base_path+"INCMONEY")
SET FILTER TO DOC_DATE >= d_s .AND. DOC_TYPE # "1"
SCAN
  @ 10, 10 SAY "INCMONEY "+DTOC(DOC_DATE)
  IF .NOT. SEEK(WHERE, "TST_ARM")
    LOOP
  ENDIF
  SELECT TST_ARM
  REPLACE S_KASSA WITH S_KASSA+INCMONEY.MONEY_R
  SELECT INCMONEY
ENDSCAN
CLEAR

USE (base_path+"KASSA")
SET FILTER TO DOC_DATE >= d_s .AND. .NOT. EMPTY(WHO_GET)
SCAN
  @ 10, 10 SAY "KASSA    "+DTOC(DOC_DATE)
  IF .NOT. SEEK(WHERE, "TST_ARM")
    LOOP
  ENDIF
  SELECT TST_ARM
  REPLACE S_KASSA WITH S_KASSA-KASSA.R_TOTAL
  SELECT KASSA
ENDSCAN
USE

CLOSE DATABASES
IF .T. &&&&&&&&&&&&&&&&&&&&&&&&&
USE TST_ARM ORDER TAG ARM_C
SELECT 0
USE (base_path+"ARM_LST")
SET RELA TO ARM_C INTO TST_ARM
REPLACE ALL S_KASSA WITH S_KASSA+KASSA-TST_ARM.S_KASSA
CLOSE DATA
ENDIF &&&&&&&&&&&&&&&&&&&&&&&&&&&

CLEAR

RETURN
