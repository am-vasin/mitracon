*=========================================================================
PROCEDURE Break_Str	&& Pазбивка стpоки на две
*-------------------------------------------------------------------------
PARAMETERS sr, s1, s2, ln

*   sr - исходная строка
*   s1 - первая строка
*   s2 - вторая строка (остаток)
*   ln - ограничение по длине для первой строки
PRIVATE n

s1 = sr
IF EMPTY (SUBSTR (s1, ln, 1)) OR EMPTY (SUBSTR (s1, ln+1, 1))
  s2 = ALLTRIM (SUBSTR (s1, ln+1))
  s1 = ALLTRIM (LEFT (s1,ln))
ELSE
  n = ln - 1
  DO WHILE n > 0 AND NOT EMPTY (SUBSTR (s1, n, 1))
    n = n - 1
  ENDDO
  IF n = 0
    s2 = ALLTRIM (s1)
    s1 = ""
  ELSE
    s2 = ALLTRIM (SUBSTR (s1, n))
    s1 = ALLTRIM (LEFT (s1, n))
  ENDIF
ENDIF

s1 = PADR (s1, ln)
RETURN

*=========================================================================
FUNCTION Num2RStr	&& Число прописью
*-------------------------------------------------------------------------
PARAMETERS num
*
*  s   - строка в которой возвращается результат;
*  t   - передается род - ( 0 - один, 1 - одна), возвращается
*        величина для связки: 0 - тысяч, 1 - тысяча, 2 - тысячи;
*  num - число.
*
PRIVATE t, t_w, s_tmp
PRIVATE n1, n2, n3

t = 0

IF num = 0
  s = "ноль"
  t = 0
  RETURN s
ENDIF

n3 = num

s_tmp  = FLOOR (n3/1000)		&& 1 - 999
n1 = n3 - s_tmp*1000

n3 = s_tmp
s_tmp  = FLOOR (n3/1000)		&& 1,001 - 999,999
n2 = n3 - s_tmp*1000

n3 = s_tmp				&& 1,000,001 - 999,999,999

s_tmp = ""
IF n1 # 0
  DO Pr_3 WITH s_tmp, t, n1
  s_tmp = s_tmp
ELSE
  t = 0
ENDIF
s = ALLTRIM (Millions (n3)) + IIF (n2 = 0, "", " " +;
             Thousands (n2)) + " " + s_tmp

RETURN ALLTRIM (s)

*-------------------------------------------------------------------------
PROCEDURE Pr_3
*-------------------------------------------------------------------------
PARAMETERS s, t, num
PRIVATE s_tmp, n1, n2, n3

t_w   = 0
n3    = num
s_tmp = FLOOR (n3/10)			&& единицы
n1    = n3 - s_tmp*10

n3    = s_tmp
s_tmp = FLOOR (n3/10)			&& десятки
n2    = n3 - s_tmp*10

n3    = s_tmp				&& сотни

****	Сотни

DO CASE
  CASE n3 = 0
    s_tmp = ""
  CASE n3 = 1
    s_tmp = "сто"
  CASE n3 = 2
    s_tmp = "двести"
  CASE n3 = 3
    s_tmp = "триста"
  CASE n3 = 4
    s_tmp = "четыреста"
  CASE n3 = 5
    s_tmp = "пятьсот"
  CASE n3 = 6
    s_tmp = "шестьсот"
  CASE n3 = 7
    s_tmp = "семьсот"
  CASE n3 = 8
    s_tmp = "восемьсот"
  CASE n3 = 9
    s_tmp = "девятьсот"
ENDCASE

IF n2 = 1		&& Второй десяток
  s_tmp = s_tmp + " "
  DO CASE
    CASE n1 = 0
      s_tmp = s_tmp + "десять"
    CASE n1 = 1
      s_tmp = s_tmp + "одиннадцать"
    CASE n1 = 2
      s_tmp = s_tmp + "двенадцать"
    CASE n1 = 3
      s_tmp = s_tmp + "тринадцать"
    CASE n1 = 4
      s_tmp = s_tmp + "четырнадцать"
    CASE n1 = 5
      s_tmp = s_tmp + "пятнадцать"
    CASE n1 = 6
      s_tmp = s_tmp + "шестнадцать"
    CASE n1 = 7
      s_tmp = s_tmp + "семнадцать"
    CASE n1 = 8
      s_tmp = s_tmp + "восемнадцать"
    CASE n1 = 9
      s_tmp = s_tmp + "девятнадцать"
  ENDCASE
  t = t_w
  s = s_tmp
  RETURN
ELSE

  ****	Десятки

  IF n2 > 1
    s_tmp = s_tmp + " "
    DO CASE
      CASE n2 = 2
        s_tmp = s_tmp + "двадцать"
      CASE n2 = 3
        s_tmp = s_tmp + "тридцать"
      CASE n2 = 4
        s_tmp = s_tmp + "сорок"
      CASE n2 = 5
        s_tmp = s_tmp + "пятьдесят"
      CASE n2 = 6
        s_tmp = s_tmp + "шестьдесят"
      CASE n2 = 7
        s_tmp = s_tmp + "семьдесят"
      CASE n2 = 8
        s_tmp = s_tmp + "восемьдесят"
      CASE n2 = 9
        s_tmp = s_tmp + "девяносто"
    ENDCASE
  ENDIF

  ****	Единицы
  
  IF n2 # 1 AND n1 # 0
    s_tmp = IIF (EMPTY (s_tmp), "", s_tmp + " ")
    DO CASE
      CASE n1 = 1
        s_tmp = s_tmp + IIF (t = 0, "один", "одна")
        t_w = 1
      CASE n1 = 2
        s_tmp = s_tmp + IIF (t = 0, "два",  "две")
        t_w = 2
      CASE n1 = 3
        s_tmp = s_tmp + "три"
        t_w = 2
      CASE n1 = 4
        s_tmp = s_tmp + "четыре"
        t_w = 2
      CASE n1 = 5
        s_tmp = s_tmp + "пять"
      CASE n1 = 6
        s_tmp = s_tmp + "шесть"
      CASE n1 = 7
        s_tmp = s_tmp + "семь"
      CASE n1 = 8
        s_tmp = s_tmp + "восемь"
      CASE n1 = 9
        s_tmp = s_tmp + "девять"
    ENDCASE
  ENDIF
ENDIF
t = t_w
s = s_tmp
RETURN

*-------------------------------------------------------------------------
PROCEDURE Millions
*-------------------------------------------------------------------------
PARAMETERS num
PRIVATE t, s_tmp

t     = 0
s_tmp = ""
DO Pr_3 WITH s_tmp, t, num

IF NOT EMPTY (s_tmp)
  DO CASE
    CASE t = 0
      s_tmp = s_tmp + " миллионов"
    CASE t = 1
      s_tmp = s_tmp + " миллион"
    OTHERWISE
      s_tmp = s_tmp + " миллиона"
  ENDCASE
ENDIF
RETURN s_tmp

*-------------------------------------------------------------------------
PROCEDURE Thousands
*-------------------------------------------------------------------------
PARAMETERS num
PRIVATE t, s_tmp

t     = 1
s_tmp = ""
DO Pr_3 WITH s_tmp, t, num

IF NOT EMPTY (s_tmp)
  DO CASE
    CASE t = 0
      s_tmp = s_tmp + " тысяч"
    CASE t = 1
      s_tmp = s_tmp + " тысяча"
    OTHERWISE
      s_tmp = s_tmp + " тысячи"
  ENDCASE
ENDIF
RETURN s_tmp

*=========================================================================
FUNCTION  Capital	&& Капитализация 1-й буквы строки
*-------------------------------------------------------------------------
PARAMETER tmpStr
PRIVATE tmpStr

*RETURN UPPER (LEFT (tmpStr, 1)) + SUBSTR (tmpStr, 2)

RETURN RUpper (LEFT (tmpStr, 1)) + SUBSTR (tmpStr, 2)

*=========================================================================
FUNCTION RUpper		&& Перевод строки в верхний регистр
*-------------------------------------------------------------------------
PARAMETER tmpStr
PRIVATE tmpStr, tmpLower, tmpUpper

tmpLower = "abcdefghijklmnopqrstuvwxyzабвгдежзийклмнопрстуфхцчшщъыьэюя"
tmpUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"

RETURN CHRTRAN (tmpStr, tmpLower, tmpUpper)

