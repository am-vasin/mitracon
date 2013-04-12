*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  To_URM_N      §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º       ”®à¬¨à®¢ ­¨¥ ä ©«  ¨§ á¯¨áª  ¤«ï ã¤ «¥­­®£® à ¡®ç¥£® ¬¥áâ        º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 24.10.2007 ÍÍ¼
PROCEDURE To_URM_N
PARAMETERS l_num

PRIVATE tmpNm, s_sav, mss, s_list, n_f, tmpSeg, d_t, d_w, bf

d_t = Table_D()
w_t = Table_W()

m.s_list = Seg_Lst()
IF len(m.s_list) = 0
  RETURN
ENDIF
m.s_sav = SELECT()
SELECT 0
USE m.path_comm+"SUBJECT" SHARED AGAIN ORDER TAG STO_CODE ALIAS SJ_07A24
SELECT 0
USE m.base_path+"STOCK" SHARED AGAIN ORDER TAG CODE ALIAS ST_07A24
SELECT 0
USE m.base_path+"LIST_DET" SHARED AGAIN ORDER TAG L_NAME ALIAS LS_07A24
SET RELATION TO CODE INTO SJ_07A24, CODE INTO ST_07A24
SEEK m.l_num
LOCATE REST WHILE m.l_num = LIST_NUM FOR QNT # 0
IF .NOT. FOUND()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"‘¯¨á®ª ¯ãáâ..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN SJ_07A24
  USE IN ST_07A24
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF
m.tmpNm = New_File("", "TXT", "ƒ¤¥ á®åà ­¨âì § ª §")
IF EMPTY(m.tmpNm)
  USE IN SJ_07A24
  USE IN ST_07A24
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF
DELETE FILE (m.tmpNm)
m.n_f = FCREATE(m.tmpNm)

SCAN REST WHILE m.l_num = LIST_NUM FOR QNT # 0
&& .AND. CODE # 0 
  IF FOUND("SJ_07A24")
    m.tmpSeg = SJ_07A24.SEGMENT
  ELSE
    m.tmpSeg = SPACE(FSIZE("SEGMENT", "SJ_07A24"))
  ENDIF
  IF .NOT. m.tmpSeg $ m.s_list 
    LOOP
  ENDIF
  IF FOUND("ST_07A24") .AND. CODE > 0
    m.bf = ALLTRIM(ST_07A24.PREFIX)+ALLTRIM(ST_07A24.NAME)+CHR(9)++ALLTRIM(ST_07A24.PRODUCER)
  ELSE
    m.bf = ALLTRIM(LS_07A24.PREFIX)+ALLTRIM(LS_07A24.NAME)+CHR(9)++ALLTRIM(LS_07A24.PRODUCER)
  ENDIF
  m.bf = m.bf+CHR(9)+ALLTRIM(STR(LS_07A24.QNT))
  m.bf = CHRTRAN(m.bf, d_t, w_t)
  = FPUTS(m.n_f, m.bf)
ENDSCAN

= FCLOSE(m.n_f)
USE IN SJ_07A24
USE IN ST_07A24
USE
SELECT (m.s_sav)
RETURN
