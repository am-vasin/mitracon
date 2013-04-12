PROCEDURE Locator
PARAMETERS l001, c001, ln

PRIVATE prf_flt, is_ins, c_flt, chr_type, c, c_c, c_t

is_ins = INSMODE()   && Режим вставка/замена

*
*   "*" - коды, соответствующие печатным символам
*
chr_type = SPACE(31)+     REPLICATE("*",126-31 )+ ;
           SPACE(127-126)+REPLICATE("*",175-127)+ ;
           SPACE(223-175)+REPLICATE("*",241-223)+ ;
           SPACE(255-223)
*
*   "I" - коды, соответствующие символам внутренней навигации
*
chr_type = STUFF(chr_type,   1, 1, "I")  &&  Home
chr_type = STUFF(chr_type,   4, 1, "I")  &&  Стрелка вправо
chr_type = STUFF(chr_type,   6, 1, "I")  &&  End
chr_type = STUFF(chr_type,   7, 1, "I")  &&  Del
chr_type = STUFF(chr_type,  13, 1, "I")  &&  Enter
chr_type = STUFF(chr_type,  19, 1, "I")  &&  Стрелка влево
chr_type = STUFF(chr_type,  22, 1, "I")  &&  Ins
chr_type = STUFF(chr_type, 127, 1, "I")  &&  BackSpace

*
*   "E" - коды, соответствующие символам внешней навигации ( по базе )
*

chr_type = STUFF(chr_type,  3, 1, "E")  &&  PgDn
chr_type = STUFF(chr_type,  5, 1, "E")  &&  Стрелка вверх
chr_type = STUFF(chr_type, 18, 1, "E")  &&  PgUp
chr_type = STUFF(chr_type, 24, 1, "E")  &&  Стрелка вниз

prf_flt = ""    && Префикс
c_flt   = 0     && Текущая позиция курсора

DO WHILE .T.
  @ l001, c001 SAY PADR(prf_flt, ln, "▒")
  @ l001, c001+c_flt SAY ""

  c_c = INKEY(0)                && Код символа
  IF c_c >= 0
    c = CHR(c_c)                && Символ
    c_t = SYS(15, chr_type, c)  && Тип символа
  ENDIF
  DO CASE
  CASE c_c < 0                  && Функциональная клавиша
  CASE c_t = "*"                && Печатный символ
    prf_flt = LEFT(prf_flt, c_flt)+c+  ;
              SUBSTR(prf_flt, c_flt+IIF(is_ins,1,2))
    prf_flt = LEFT(prf_flt, ln)
    c_flt = c_flt+1
    c_flt = MIN(c_flt, ln)
    LOOP
  CASE c_t = "E"                && Навигация по базе
  CASE c_t = "I"                && Внутренняя навигация
    DO CASE
    CASE c_c = 7                && Del
      prf_flt = LEFT(prf_flt, c_flt)+SUBSTR(prf_flt, c_flt+2)
    CASE c_c = 127              && BackSpace
      prf_flt = LEFT(prf_flt, c_flt-1)+SUBSTR(prf_flt, c_flt+1)
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 19               && Стрелка влево
      c_flt = MAX(c_flt-1,0)
    CASE c_c = 4                && Стрелка вправо
      c_flt = MIN(c_flt+1,LEN(prf_flt))
    CASE c_c = 22               && Ins
      is_ins = .NOT. is_ins
      = INSMODE(is_ins)
    CASE c_c = 13
      EXIT
    ENDCASE
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RETURN
