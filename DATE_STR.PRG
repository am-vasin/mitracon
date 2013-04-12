********************************************************************************
*    Файл Date_Str       Разработчик Андрей Васин                22.03.2012    *
********************************************************************************
*                                                                              *
*                                Дата прописью                                 *
*                                                                              *
********************************************************************************
PROCEDURE Date_Str
PARAMETERS prmDate
*
* prmDate - Дата
*

PRIVATE dd, mm, yy

m.dd = DAY(m.prmDate)
m.mm = MONTH(m.prmDate)
m.yy = YEAR(m.prmDate)

DO CASE
CASE m.mm = 1
  m.mm = "Января"
CASE m.mm = 2
  m.mm = "Февраля"
CASE m.mm = 3
  m.mm = "Марта"
CASE m.mm = 4
  m.mm = "Апреля"
CASE m.mm = 5
  m.mm = "Мая"
CASE m.mm = 6
  m.mm = "Июня"
CASE m.mm = 7
  m.mm = "Июля"
CASE m.mm = 8
  m.mm = "Августа"
CASE m.mm = 9
  m.mm = "Сентября"
CASE m.mm = 10
  m.mm = "Октября"
CASE m.mm = 11
  m.mm = "Ноября"
CASE m.mm = 12
  m.mm = "Декабря"
ENDCASE

RETUR '"'+STR(m.dd, 2)+'" '+m.mm+STR(m.yy, 5)+"г."
