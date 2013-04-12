*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  To_URM_O      §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º       ”®à¬¨à®¢ ­¨¥ ä ©«  ¨§ § ª §  ¤«ï ã¤ «¥­­®£® à ¡®ç¥£® ¬¥áâ        º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 24.10.2007 ÍÍ¼
PROCEDURE To_URM_O
PARAMETERS o_firm, o_date, o_num

PRIVATE tmpNm, s_sav, mss, s_list, n_f, tmpSeg, d_t, d_w, bf

d_t = Table_D()
w_t = Table_W()

*m.s_list = Seg_Lst()
*IF EMPTY(m.s_list)
*  RETURN
*ENDIF
m.s_sav = SELECT()
SELECT 0
USE m.base_path+"STOCK" SHARED AGAIN ORDER TAG CODE ALIAS ST_07A24
SELECT 0
USE m.base_path+"ORDER" SHARED AGAIN ORDER TAG ORD_NUM ALIAS LS_07A24
SET RELATION TO CODE INTO ST_07A24
IF .NOT. SEEK(m.o_firm+LEFT(DTOS(m.o_date), 4)+m.o_num)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"¥â â ª®£® § ª § ..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN ST_07A24
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF
m.tmpNm = New_File("", "TXT", "ƒ¤¥ á®åà ­¨âì § ª §")
IF EMPTY(m.tmpNm)
  USE IN ST_07A24
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF
DELETE FILE (m.tmpNm)
m.n_f = FCREATE(m.tmpNm)

SCAN REST WHILE FIRM = m.o_firm .AND. ORD_DATE = m.o_date .AND. ORD_NUM = m.o_num
&& .AND. CODE # 0 
  IF FOUND("ST_07A24") .AND. CODE > 0
    m.bf = ALLTRIM(ST_07A24.PREFIX)+ALLTRIM(ST_07A24.NAME)+CHR(9)+ALLTRIM(ST_07A24.PRODUCER)
  ELSE
    m.bf = ALLTRIM(LS_07A24.PREFIX)+ALLTRIM(LS_07A24.NAME)+CHR(9)+ALLTRIM(LS_07A24.PRODUCER)
  ENDIF
  m.bf = m.bf+CHR(9)+ALLTRIM(STR(LS_07A24.QNT))
  m.bf = CHRTRAN(m.bf, d_t, w_t)
  = FPUTS(m.n_f, m.bf)
ENDSCAN

= FCLOSE(m.n_f)
USE IN ST_07A24
USE
SELECT (m.s_sav)
RETURN
