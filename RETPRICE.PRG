*===================================================================
FUNCTION RetPrice	&& Розничная цена для оптовой
*-------------------------------------------------------------------
*	Алгоритм от 24.11.1999
*	Модификация 13.04.2000	- вместо кода раздела (N) может быть код класса (C)
*	Модификация 04.05.2000	- диапазон колебаний сужен до +-3%
*	Модификация 06.07.2000	- нельзя сделать округление до 50 коп. вверх и мин. цена >= 1 руб
*	Модификация 14.08.2001 (Кислова)	- раздел 12 справочники отечественные - наценка 10%
*	Модификация 24.10.2001 (Кислова)	- раздел 12 (справочники отечественные) - по общему алгоритму
*	Модификация 12.03.2002 (Светлова)	- раздел 72 (химия) класс 20.2 (Химия для электроники) - наценка 11.8%
*-------------------------------------------------------------------
PARAMETERS tmpName, tmpPrice, is_import, d_code
* tmpName			&& Наименование позиции
* tmpPrice			&& Оптовая цена
* is_import         && Импортная позиция!
* d_code			&& Код раздела (N) или класса (C)

IF PARAMETERS() < 4
	d_code = 0
ENDIF

PRIVATE tmpMin, tmpMax	&& Наценки на концах диапазона [%]
PRIVATE tmpMid			&& Наценка в середине (1.0) диапазона [%]
PRIVATE tmpSovok		&& Отстраивающий коэффициент для совка
PRIVATE tmpDev			&& Диапазон колебания наценки [%]

PRIVATE tmpRet			&& Расчетная розничная цена
PRIVATE tmpRandom		&& Массив псевдослучайных колебаний
PRIVATE tmp, tmpCount		

DIMENSION tmpRandom [13]

****	Установка параметров
tmpSovok = 0.96
tmpMin =  19*0.01*IIF(is_import, 1.0, tmpSovok)	&& При О -> 00  Р/О -> 1+tmpMin
tmpMax = 130*0.01*IIF(is_import, 1.0, tmpSovok)	&& При О -> 0   Р/О -> 1+tmpMax
tmpMid =  65*0.01*IIF(is_import, 1.0, tmpSovok)	&& При О =  1   Р/О =  1+tmpMid
tmpDev =   3*0.01	&& Случайные колебания в диапазоне +- tmpDev %

tmpMid = (tmpMid - tmpMin)/(tmpMax - tmpMid)

****	Заполнения массива случайных колебаний в зависимости от недели
=RAND (INT ((DATE() - CTOD ('01.01.' + STR (YEAR (DATE()),4)))/14))
* =RAND (INT ((DATE() - CTOD ('01.01.' + STR (YEAR (DATE()),4)))/7))
&&    (INT (30*MONTH(DATE()) + DAY(DATE())))	&& Серия = номеру недели
FOR tmp = 1 TO 13
  tmpRandom [tmp] = RAND()
ENDFOR

****	Вычисление "правильной" розничной цены по формуле
****	retPrice = price + price*[min + mid*(max - min)/(price + mid)]
tmpRet = tmpPrice*(1 + tmpMin + tmpMid*(tmpMax-tmpMin)/(tmpPrice +tmpMid))

****	Вычисление "правильной" розничной цены для дешевых позиций как ломаной линии
IF Is_Import AND tmpPrice < 0.1
*	tmpRet = tmpPrice*(1 + 7.0 - 20*(7.0 - 1.23)*tmpPrice)
	PRIVATE a
	DIMENSION a(7,2)
	a[1,1] = 0.000
	a[1,2] = 6.0
	a[2,1] = 0.010
	a[2,2] = 6.0
	a[3,1] = 0.015
	a[3,2] = 4.5
	a[4,1] = 0.020
	a[4,2] = 4.0
	a[5,1] = 0.035
	a[5,2] = 2.8
	a[6,1] = 0.050
	a[6,2] = 2.5
	a[7,1] = 0.100
	a[7,2] = 2.16
	FOR tmp = 2 TO ALEN(a,1)
		IF tmpPrice < a[tmp,1]
			tmpRet = tmpPrice*(;
			((a[tmp,2] - a[tmp-1,2])*tmpPrice - (a[tmp,2]*a[tmp-1,1] - a[tmp-1,2]*a[tmp,1]))/;
			(a[tmp,1] - a[tmp-1,1]))
			EXIT
		ENDIF
	ENDFOR
ENDIF

*	Спец. наценки для отдельных разделов
IF TYPE("d_code") = "N"
	DO CASE
	CASE d_code = 27 OR d_code = 52			&& батарейки; АККУМУЛЯТОРЫ
		tmpRet = tmpPrice*(1 + 0.15)
	CASE d_code = 63						&& ЗАРЯДНЫЕ УСТР.
		tmpRet = tmpPrice*(1 + 0.10)
*!*		CASE d_code = 12 AND !is_import			&& справочники отечественные
*!*			tmpRet = tmpPrice*(1 + 0.10)
	CASE d_code = 68						&& электр.наборы (KIT)
		tmpRet = tmpPrice*(1 + 0.27)
	*	12.03.2002 (Светлова)
	CASE d_code = 72						&& химия
		tmpRet = tmpPrice*(1 + 0.118)
	ENDCASE
*	Спец. наценки для отдельных классов	&&  ****13.04.2000 ****
ELSE
	DO CASE
	CASE d_code = "0704"					&& Зарядное устройство
		tmpRet = tmpPrice*(1 + 0.10)
*!*		CASE d_code = "2601" AND !is_import		&& Справочники (отечественные)
*!*			tmpRet = tmpPrice*(1 + 0.10)
	*	12.03.2002 (Светлова)
	CASE d_Code = "2002"					&& Химия для электроники
		tmpRet = tmpPrice*(1 + 0.118)
	CASE d_code = "24"						&& Химические источники тока
		tmpRet = tmpPrice*(1 + 0.15)
	CASE d_code = "29"						&& Электронные наборы и модули
		tmpRet = tmpPrice*(1 + 0.27)
	ENDCASE
ENDIF

****	Генерация кода из наименования по модулю 13
tmpCount = 0
FOR tmp = 1 TO LEN (ALLTRIM (tmpName))
  tmpCount = tmpCount + ASC (SUBSTR (tmpName, tmp))
ENDFOR
tmpCount = tmpCount % 13

****	Наложение случайного колебания на розничную цену
IF TYPE("d_code") = "N"
	DO CASE
	CASE d_code = 12 AND !is_import			&& справочники отечественные
	OTHERWISE
		tmpRet = tmpRet * (1 + 2*tmpDev*(tmpRandom[tmpCount+1]-0.5))
	ENDCASE
ELSE ****13.04.2000 *******************************************
	DO CASE
	CASE d_code = "2601" AND !is_import		&& Справочники (отечественные)
*!*			tmpRet = tmpPrice*(1 + 0.00)
	OTHERWISE
		tmpRet = tmpRet * (1 + 2*tmpDev*(tmpRandom[tmpCount+1]-0.5))
	ENDCASE
ENDIF

****06.07.2000 - округление до 50 коп вверх и мин цена >= 1 руб

* Нужен курс доллара в качестве аргумента!
* Но и это не поможет - нужно брать курс на момент формирования
* рублевой цены, т.е. в другом месте

****06.07.2000 end ********************************************

RETURN tmpRet

