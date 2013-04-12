*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ St_Rec1       งเ กฎโ็จช €ญคเฅฉ  แจญ           27.06.97 14:58:55 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ             เจฅฌ แโ ญค เโญฎฃฎ แฏจแช  ไ ฉซฎข ญ  ฏซฎ้ คชฅ.              บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE St_Rec1
PRIVATE pth_w, crs, s_sav, l, c

crs = SET("CURSOR") = "ON"
DO Prep_TD    && ‘ฎงค ฅฌ จซจ ฎ็จ้ ฅฌ โเ ญงจโญใ๎ คจเฅชโฎเจ๎
pth_w = tmpo_path+"TRANSMIT\"

IF .NOT. Get_File(pth_w)
  RETURN
ENDIF

SET CURSOR OFF

ACTIVATE SCREEN
SAVE SCREEN TO s_sav

l = CEILING((WROWS()-4)/2)
c = FLOOR((WCOLS()-21)/2)
@ l+1, c+2 FILL TO l+4, c+22 COLOR (SCHEME(1,8))
@ l,   c SAY SPACE(21) COLOR SCHEME 5
@ l+1, c SAY PADC(" แฏ ชฎขช !",21) COLOR SCHEME 5
@ l+2, c SAY PADC("คฅฌ-แ...",21) COLOR SCHEME 5
@ l+3, c SAY SPACE(21) COLOR SCHEME 5
c = "! ARJ E -V -Y "+pth_w+"ST_SEND1 "+pth_w+" > nul"
&c
c = "!DEL "+pth_w+"ST_SEND1.A*"
&c
RESTORE SCREEN FROM s_sav

DO Get_Cus

DO Get_Per

DO Get_Arm

IF crs
  SET CURSOR ON
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ขขฎค  จ เ แฏ ชฎขชจ ARJ ไ ฉซ .                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_File
PARAMETER pth

PRIVATE get_pth, i, mss, mss1, nm_w, w, is_fdd

get_pth = FromDisk(pth+"ST_SEND1.ARJ")

IF EMPTY(get_pth)
  RETURN .F.
ENDIF

is_fdd = .F.
IF LEN(get_pth) = 3
  IF INLIST(UPPER(LEFT(get_pth,2)), "A:", "B:")
    is_fdd = .T.
  ENDIF
ENDIF
IF is_fdd
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"…้ฅ กใคฅโ คจแชฅโ ? [   ] [ ฅโ ]"
  mss(3) = ""
  i = 1
  DO WHILE .T.
    nm_w = "ST_SEND1.A"+RIGHT(STR(100+i,3),2)
    IF Out_Mess(5, "mss") = 2
      EXIT
    ENDIF
    w = get_pth
    CALL IsDisk WITH w
    IF w # get_pth
      DIMENSION mss1(3)
      mss1(1) = ""
      mss1(2) = CHR(0)+"๋ ญฅ ใแโ ญฎขจซจ คจแชฅโใ!"
      mss1(3) = ""
      DO Out_Mess WITH 7,"mss1"
      LOOP
    ENDIF
    IF .NOT. File_O(get_pth+nm_w)
      DIMENSION mss1(3)
      mss1(1) = ""
      mss1(2) = CHR(0)+"๋ ใแโ ญฎขจซจ ญฅ โใ คจแชฅโใ!"
      mss1(3) = ""
      DO Out_Mess WITH 7,"mss1"
      LOOP
    ENDIF
    COPY FILE (get_pth+nm_w) TO (pth+nm_w)
    i = i+1
  ENDDO
ELSE
  i = 1
  DO WHILE .T.
    nm_w = "ST_SEND1.A"+RIGHT(STR(100+i,3),2)
    w = get_pth
    IF .NOT. File_O(get_pth+nm_w)
      EXIT
    ENDIF
    DO Wt_Mess WITH "จ่ฅฌ "+nm_w
    COPY FILE (get_pth+nm_w) TO (pth+nm_w)
    i = i+1
  ENDDO
ENDIF

DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+"แฅ แ็จโ ญฎ ฏเ ขจซ์ญฎ?  ็จญ ฅฌ ฎกเ กฎโชใ?"
mss(3) = CHR(0)+"  [   ] [ ฅโ, ฎโช ฆฅฌแ๏! ]"
mss(4) = ""
RETURN Out_Mess(5, "mss") = 1

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                         ‘ขฅเช  ไ ฉซ  ชซจฅญโฎข.                         บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 27.06.97 15:15:20 อผ
PROCEDURE Get_Cus

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (pth_w+"ACCOUNT") ALIAS SOURCE

SCAN FOR SUBSYS = sys_char

  IF SEEK(CUS_CODE,"ACCOUNT")
    IF .NOT. EMPTY(ACCOUNT.CUS_NAME)
      SELECT ACCOUNT
      REPLACE CUS_NAME WITH SOURCE.CUS_NAME, CITY WITH SOURCE.CITY
    ENDIF
  ELSE
    SELECT ACCOUNT
    APPEND BLANK
    REPLACE SUBSYS   WITH SOURCE.SUBSYS,   ;
            CUS_CODE WITH SOURCE.CUS_CODE, ;
            CUS_NAME WITH SOURCE.CUS_NAME, ;
            CITY     WITH SOURCE.CITY,     ;
            ID_CODE  WITH SOURCE.ID_CODE,  ;
            DATE_ON  WITH SOURCE.DATE_ON,  ;
            WHO      WITH SOURCE.WHO,      ;
            WHERE    WITH SOURCE.WHERE
  ENDIF
  SELECT SOURCE

ENDSCAN

USE
SELECT ACCOUNT
USE

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                     กญฎขซฅญจฅ แฏจแช  แฎโเใคญจชฎข.                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 27.06.97 15:25:47 อผ
PROCEDURE Get_Per
PRIVATE arr_w

IF File_O(pth_w+"DEPS.DBF")
  SELECT 0
  USE (base_path+"DEPS")

  SELECT 0
  USE (pth_w+"DEPS") ALIAS SOURCE

  SCAN
    SCATTER TO arr_w
    SELECT DEPS
    IF EOF() .OR. BOF()
      APPEND BLANK
    ENDIF
    GATHER FROM arr_w
    SKIP
    SELECT SOURCE
  ENDSCAN
  USE
  SELECT DEPS
  DELETE REST
  USE
ENDIF

IF File_O(pth_w+"PERSONS.DBF")

  USE (base_path+"PERSONS")

  SELECT 0
  USE (pth_w+"PERSONS") ALIAS SOURCE

  SCAN
    SCATTER TO arr_w
    SELECT PERSONS
    IF EOF() .OR. BOF()
      APPEND BLANK
    ENDIF
    GATHER FROM arr_w
    SKIP
    SELECT SOURCE
  ENDSCAN
  USE
  SELECT PERSONS
  DELETE REST
  USE
ENDIF

IF File_O(pth_w+"GOODS.DBF")
  USE (base_path+"GOODS")

  SELECT 0
  USE (pth_w+"GOODS") ALIAS SOURCE

  SCAN
    SCATTER TO arr_w
    SELECT GOODS
    IF EOF() .OR. BOF()
      APPEND BLANK
    ENDIF
    GATHER FROM arr_w
    SKIP
    SELECT SOURCE
  ENDSCAN
  USE
  SELECT GOODS
  DELETE REST
  USE

ENDIF

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ‘ขฅเช  แฏจแช  เ กฎ็จๅ ฌฅแโ.                       บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 27.06.97 15:44:13 อผ
PROCEDURE Get_Arm

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (pth_w+"ARM_LST") ALIAS SOURCE

SCAN
  SELECT ARM_LST
  IF .NOT. SEEK(SOURCE.ARM_C)
    APPEND BLANK
  ENDIF
  REPLACE ARM_C WITH SOURCE.ARM_C, ;
          ARM_N WITH SOURCE.ARM_N, ;
          ARM_S WITH SOURCE.ARM_S
  SELECT SOURCE
ENDSCAN

USE
SELECT ARM_LST
USE

RETURN
