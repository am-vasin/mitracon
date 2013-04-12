***********************************************************************************
*                                                                                 *
*                                Разборка CSV-файла                               *
*                                                                                 *
***********************************************************************************
PROCEDURE CSV_File
PARAMETER fName

PRIVATE cr, dlm, buff, fldVal, fldNum, nFile, st, pnt, crLen, nRec, tmpChar, isCr

m.cr = CHR(10)
m.crLen = 1
m.dlm = ";"
m.isCr = .T.

m.nFile = FOPEN(m.fName)
m.buff = FREAD(m.nFile, 2048)
m.nRec = 0
m.fldVal = ""
m.fldNum = 0
m.st = 0
m.pnt = 1
nn = 0
DO WHILE .T.
  m.tmpChar = nxtChar()
  IF LEN(m.tmpChar) = 0
    EXIT
  ENDIF
  IF m.st = 0	&& Начало записи
    m.st = 1
    m.nRec = m.nRec+1
    m.fldNum = 0
    = NewRec(m.nRec)
  ENDIF
  DO CASE
  CASE m.st = 1					&& Начало поля
    m.fldVal = ""
    m.fldNum = m.fldNum+1
    DO CASE
    CASE m.tmpChar = CHR(0)
      m.st = 0
    CASE m.tmpChar = m.dlm		&& Пустое поле
      = AddField(m.fldNum, m.fldVal)
    CASE m.tmpChar = '"'		&& Поле в кавычках
      m.isCr = .F.
      m.st = 21
    OTHERWISE					&& Поле без кавычек
      m.st = 20
      m.fldVal = m.fldVal+m.tmpChar
    ENDCASE
  CASE m.st = 20				&& Поле без кавычек очередной символ
    DO CASE	
    CASE m.tmpChar = CHR(0)
      = AddField(m.fldNum, m.fldVal)
      m.st = 0
    CASE m.tmpChar = m.dlm		&& Конец поля
      = AddField(m.fldNum, m.fldVal)
      m.st = 1
    OTHERWISE
      m.fldVal = m.fldVal+m.tmpChar
    ENDCASE
  CASE m.st = 21				&& Поле в кавычках очередной символ
    DO CASE
    CASE m.tmpChar = '"'
      m.st = 211
      m.isCr = .T.
    OTHERWISE
      m.fldVal = m.fldVal+m.tmpChar
    ENDCASE
  CASE m.st = 211
    DO CASE
    CASE m.tmpChar = '"'
      m.st = 21
      m.isCr = .F.
      m.fldVal = m.fldVal+m.tmpChar
    CASE m.tmpChar = m.dlm
      = AddField(m.fldNum, m.fldVal)
      m.st = 1
    CASE m.tmpChar = CHR(0)
      = AddField(m.fldNum, m.fldVal)
      m.st = 0
    ENDCASE
  ENDCASE
ENDDO
IF .NOT. EMPTY(m.fldVal)
  = AddField(m.fldNum, m.fldVal)
ENDIF

= FCLOSE(m.nFile)

RETURN

*********************************************************************
PROCEDURE nxtChar

IF m.pnt > LEN(m.buff)
  RETURN ""
ENDIF

IF m.pnt > 1024
  IF .NOT. FEOF(m.nFile)
    m.buff = SUBSTR(m.buff, m.pnt)+FREAD(m.nFile, 1024)
    m.pnt = 1
  ENDIF
ENDIF

IF SUBSTR(m.buff, m.pnt, m.crLen) == m.cr .AND. m.isCr
  m.pnt = m.pnt+m.crLen
  RETURN CHR(0)
ENDIF
m.pnt = m.pnt+1

RETURN SUBSTR(m.buff, m.pnt-1, 1)
