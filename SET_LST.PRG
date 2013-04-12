*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Set_LST       งเ กฎโ็จช €ญคเฅฉ + €ซซ           08.04.97 21:18:48 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                       แแโ ญฎขช  ญฅเ กฎ็จๅ คญฅฉ ข โ กฅซฅ.              บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_LST

PRIVATE point, col0, col1, col0_i, col1_i, s, n
PRIVATE t_month,t_year
point = 1

col0   = SCHEME(13,1)
col0_i = SCHEME(13,2)
col1   = SCHEME(14,2)
col1_i = SCHEME(14,1)
n = ALEN(h_sw)

DO WHILE .T.
  @ rw(point), cl(point) FILL TO rw(point), cl(point)+1  ;
               COLOR (IIF(h_sw(point), col1_i, col0_i))
  s = INKEY(0, "HM")
  @ rw(point), cl(point) FILL TO rw(point), cl(point)+1  ;
               COLOR (IIF(h_sw(point), col1, col0))

  DO CASE
  CASE s = 27    && Esc
    EXIT
  CASE s = 19    && Left
    point = MAX(point-7, 1)
  CASE s =  4    && Right
    point = MIN(point+7, n)
  CASE s =  5    && Up
    point = MAX(point-1, 1)
  CASE s = 24    && Down
    point = MIN(point+1, n)
  CASE s =  9 .AND.EMPTY(ls.where)    && Tab
    h_sw(point) = .NOT. h_sw(point)
    t_fild ="D_"+ALLTRIM(STR(point))
    IF EMPTY(&t_fild)
    replace &t_fild	with "*"
    ELSE
    replace &t_fild	with " "
    ENDIF
*** …• …’ €’“ ‘ ’ €
  CASE s = 18    && PageUp
  IF SEEK(STR(PS_CODE,5)+STR(MOULD.YEAR,4)+STR(MOULD.MONTH-1,2),"LS")
	DO Calendar	  
  ELSE	
    WAIT WINDOW "ฎแโจฃญใโฎ ญ ็ ซฎ โ กฅซ๏!!!" NOWAIT
  ENDIF  
  CASE s =  3    && PageDown  
  IF SEEK(STR(PS_CODE,5)+STR(MOULD.YEAR,4)+STR(MOULD.MONTH+1,2),"LS")
	DO Calendar
  ELSE		  
  WAIT WINDOW "ฎแโจฃญใโ ชฎญฅๆ โ กฅซ๏!!!" NOWAIT
  ENDIF  
    
  ENDCASE
ENDDO

RETURN
