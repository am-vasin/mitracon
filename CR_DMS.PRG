*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  Cr_Dms        §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                          ‘®§¤ ­¨¥ â ¡«¨æë „Œ‘                          º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 13.02.2009 ÍÍ¼
PROCEDURE Cr_Dms
PARAMETERS srcTable, srcTerra, srcMit, srcMega

PRIVATE s_sav, tmpName, newName, i, s_Name, err_sav, tmpArr, n, tmp_err

m.s_sav = SELECT()
m.tmpName = SYS(3)
SELECT 0
USE (m.srcTable) SHARED AGAIN ALIAS SRC_DMS
m.newName = "DMS_"+Date_Dig(DATE())
m.s_Name = m.newName
m.i = 0
DO WHILE File_O(m.base_path+m.newName+".dbf")
  m.i = m.i+1
  m.newName = m.s_name+RIGHT(STR(1000+m.i), 1)
ENDDO

COPY TO (m.base_path+m.tmpName)
USE (m.base_path+m.tmpName) EXCLUSIVE ALIAS TMP_DMS
DO Add_Terr WITH m.srcTerra
DO Add_Mit WITH m.srcMit
DO Add_Mega WITH m.srcMega
INDEX ON CODE TAG CODE FOR .NOT. EMPTY(CODE)
USE

RENAME (m.base_path+m.tmpName+".cdx") TO (m.base_path+m.newName+".cdx")
RENAME (m.base_path+m.tmpName+".dbf") TO (m.base_path+m.newName+".dbf")

m.n = ADIR(tmpArr, m.base_path+"DMS_*.DBF")
IF m.n > 3
  PRIVATE n_skip, d_skip
  = ASORT(tmpArr, 3, -1, 1)
  m.err_sav = ON("ERROR")
  m.tmp_err = 0
  ON ERROR m.tmp_err = 1
  m.d_skip = tmpArr[1, 3]
  m.n_skip = 1
  FOR m.i = 1 TO m.n
    IF m.n_skip <= 3
      IF m.d_skip # tmpArr[m.i, 3]
        m.d_skip = tmpArr[m.i, 3]
        m.n_skip = m.n_skip+1
      ENDIF
    ENDIF
    IF m.n_skip <= 3
      LOOP
    ENDIF
    m.tmp_err = 0
    m.tmpName = tmpArr[m.i, 1]
    DELETE FILE (m.base_path+m.tmpName)
    IF m.tmp_err # 0
      LOOP
    ENDIF
    m.tmpName = LEFT(m.tmpName, LEN(m.tmpName)-3)+"cdx"
    DELETE FILE (m.base_path+m.tmpName)
  ENDFOR
  ON ERROR &err_sav
ENDIF

SELECT (m.s_sav)

RETURN
