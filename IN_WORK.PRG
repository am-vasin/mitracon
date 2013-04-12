*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла In_Work      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                            Запись в работе.                            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 15.04.2006 ══╝
PROCEDURE In_Work
PARAMETERS f_name, rc_no, to_get
*
* f_name - имя таблицы;
* rc_no  - номер записи;
* to_get - T - захватить запись, иначе освободить
*

PRIVATE s_sav, mss, tmpKey, tmpBlank

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"IN_WORK") SHARED AGAIN ORDER TAG IN_WORK ALIAS IW_6415
m.tmpKey = PADR(UPPER(ALLTRIM(m.f_name)), FSIZE("FILENAME"))+STR(m.rc_no)
IF .NOT. m.to_get		&& освободить
  IF SEEK(m.tmpKey)
    SCATTER TO tmpBlank BLANK
    GATHER FROM tmpBlank
  ENDIF
  USE
  SELECT (m.s_sav)
  RETURN .T.
ENDIF

*
*  Пытаемся захватить
*
IF SEEK(m.tmpKey)			&& Запись уже в работе
  DIMENSION mss[6]
  mss[1] = ""
  mss[2] = CHR(0)+"Запись в работе с "+DTOC(WORK_DATE)+" "+WORK_TIME+"."
  mss[3] = CHR(0)+"Рабочее место: "
  mss[4] = CHR(0)+"Сотрудник: "
  mss[5] = CHR(0)+"  Как поступим? [ Пусть работает ] [ Захватить запись ]"
  mss[6] = ""
  USE (m.base_path+"ARM_LST") SHARED AGAIN ORDER TAG ARM_C ALIAS TTT_6415 IN 0
  = SEEK(WHERE, "TTT_6415")
  mss[3] = mss[3]+ALLTRIM(TTT_6415.ARM_N)
  USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS TTT_6415 IN TTT_6415
  = SEEK(WHO, "TTT_6415")
  mss[4] = mss[4]+ALLTRIM(TTT_6415.FAMILY)+" "+LEFT(TTT_6415.NAME, 1)+" "+LEFT(TTT_6415.S_NAME, 1)
  USE IN TTT_6415
  IF Out_Mess(7, "mss") = 1
    USE
    SELECT (m.s_sav)
    RETURN .F.
  ENDIF
  SCATTER TO tmpBlank BLANK
  GATHER FROM tmpBlank
ENDIF

GO TOP
IF .NOT. EMPTY(FILENAME) .OR. BOF() .OR. EOF()
  APPEND BLANK
ENDIF
REPLACE FILENAME	WITH UPPER(f_name),	;
        REC_NO		WITH m.rc_no,		;
        WHO			WITH m.user,		;
        WHERE		WITH m.arm,			;
        WORK_DATE	WITH DATE(),		;
        WORK_TIME	WITH TIME()
USE
SELECT (m.s_sav)
RETURN .T.
