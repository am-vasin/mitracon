********************************************************************************
*    Файл Cls_Name       Разработчик Андрей Васин                13.03.2012    *
********************************************************************************
*                                                                              *
*                        Наименование товара для класса                        *
*                                                                              *
********************************************************************************
PROCEDURE Cls_Name
PARAMETERS prmCode, prmSelect
*
* prmCode   - Код класса
* prmSelect - Select область для таблицы классов
*
PRIVATE s_sav, retVal

m.s_sav = SELECT()
SELECT (prmSelect)
= SEEK(m.prmCode)
IF .NOT. EMPTY(GOOD_NAME)
  m.retVal = ALLTRIM(GOOD_NAME)
ELSE
  m.retVal = cNameOnl(ALLTRIM(NAME))
ENDIF

SELECT (m.s_sav)

RETURN m.retVal
