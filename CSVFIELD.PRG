********************************************************************************
*    Файл CSVField       Разработчик Андрей Васин                11.12.2011    *
********************************************************************************
*                                                                              *
*                     Первое поле из строки в формате CSV                      *
*                                                                              *
********************************************************************************
PROCEDURE CSVField
PARAMETERS prmDelim
*
* prmDelim - символ - разделитель полей, по умолчанию точка с запятой.
*
PRIVATE dlm - разделитель полей
PRIVATE tmpPoint - указатель
PRIVATE tmpValue

IF EMPTY(m.tmpLine)
  RETURN .F.
ENDIF

m.dlm = ";"
IF TYPE("m.prmDelim") = "C"
  IF LEN(m.prmDelim) > 0
    m.dlm = LEFT(m.prmDelim, 1)
  ENDIF
ENDIF
*set step on
IF LEFT(m.tmpLine, 1) = '"'	&& Строка
  m.tmpLine = SUBSTR(m.tmpLine, 2)
  m.tmpValue = ""
  n = 0
  DO WHILE .T.
    n = n+1
    if n > 10
      WAIT WINDOW "LOOP"
      EXIT
    endif
    m.tmp_point = AT('"', m.tmpLine)
    IF m.tmp_point = 0
      m.tmpLine = ""
      RETURN .F.
    ENDIF
    IF SUBSTR(m.tmpLine, m.tmp_point+1, 1) == '"'
      m.tmpValue = m.tmpValue+LEFT(m.tmpLine, m.tmp_point)
      m.tmpLine = SUBSTR(m.tmpLine, m.tmp_point+2)
    ELSE
      m.tmpValue = m.tmpValue+LEFT(m.tmpLine, m.tmp_point-1)
      m.tmpLine = SUBSTR(m.tmpLine, m.tmp_point)
      EXIT
    ENDIF
  ENDDO
  m.tmp_point = AT(m.dlm, m.tmpLine)
  m.tmpLine = LTRIM(SUBSTR(m.tmpLine, m.tmp_point+1))
  RETURN m.tmpValue
ELSE						&& Число
  m.tmp_point = AT(m.dlm, m.tmpLine)
  IF m.tmp_point = 0
    m.tmpValue = ALLTRIM(m.tmpLine)
    m.tmpLine = ""
    IF .NOT. EMPTY(CHRTRAN(m.tmpValue, "1234567890.", ""))
      RETURN .F.
    ENDIF
    m.tmpValue = VAL(m.tmpValue)
    RETURN m.tmpValue
  ELSE
    m.tmpValue = ALLTRIM(LEFT(m.tmpLine, m.tmp_point-1))
    m.tmpLine = LTRIM(SUBSTR(m.tmpLine, m.tmp_point+1))
    IF .NOT. EMPTY(CHRTRAN(m.tmpValue, "1234567890.", ""))
      RETURN .F.
    ENDIF
    RETURN VAL(m.tmpValue)
  ENDIF
ENDIF
