*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл BD_Table     Разработчик А В и А С              28.10.97 12:30:16 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Работа с табелем.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE BD_Table
PARAMETERS tmpMONTH,tmpYEAR,tmpPers

**tmpMONTH - N(2),номер месяца
**tmpYEAR  - N(4),год
	
PRIVATE mss, sl, nm, tit_1, pic, s_date, days_sh, was_pay, days_w
PRIVATE ex, tmp_arr, i, l0, c0, n_week, y_on, m_on, d_on, y_off, m_off, d_off

pic = "@M "
sl = SELECT()
SELECT 0
USE (base_path+"MOULD") ORDER TAG MONTH ALIAS ML_TMP AGAIN
SEEK STR(tmpYEAR,4)+STR(tmpMONTH,2)
IF .NOT. FOUND() .OR. RECNO() = RECCOUNT()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Календарь на этот месяц не утвержден!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sl)
  RETURN
ENDIF
was_pay = ML_TMP.FLAG

days_sh = ALLTRIM(ML_TMP.DAYS)
s_date  = "01."+STR(tmpMONTH,2)+"." +STR(tmpYEAR,4)
s_date	= CTOD(s_date)

USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PS_TMP AGAIN
SEEK tmpPers
nm = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+". "+LEFT(S_NAME,1)+"."
y_on = YEAR(DATE_IN)
m_on = MONTH(DATE_IN)
d_on = DAY(DATE_IN)
y_off = YEAR(DATE_OFF)
m_off = MONTH(DATE_OFF)
d_off = DAY(DATE_OFF)
tit_1 = "Табель за "
DO CASE
CASE tmpMONTH = 1
  tit_1 = tit_1+"январь"
CASE tmpMONTH = 2
  tit_1 = tit_1+"февраль"
CASE tmpMONTH = 3
  tit_1 = tit_1+"март"
CASE tmpMONTH = 4
  tit_1 = tit_1+"апрель"
CASE tmpMONTH = 5
  tit_1 = tit_1+"май"
CASE tmpMONTH = 6
  tit_1 = tit_1+"июнь"
CASE tmpMONTH = 7
  tit_1 = tit_1+"июль"
CASE tmpMONTH = 8
  tit_1 = tit_1+"август"
CASE tmpMONTH = 9
  tit_1 = tit_1+"сентябрь"
CASE tmpMONTH = 10
  tit_1 = tit_1+"октябрь"
CASE tmpMONTH = 11
  tit_1 = tit_1+"ноябрь"
CASE tmpMONTH = 12
  tit_1 = tit_1+"декабрь"
ENDCASE
tit_1 = tit_1+STR(tmpYEAR,5)

USE (base_path+"LS_TABLE") ORDER TAG PS_CODE ALIAS LS_TMP AGAIN

IF tmpMONTH+12*tmpYEAR # m_on+12*y_on
  ex = .F.
  l0 = tmpMONTH-1
  IF l0 <= 0
    c0 = tmpYEAR-1
    l0 = 12
  ELSE
    c0 = tmpYEAR
  ENDIF
  IF .NOT. SEEK(STR(tmpPers,5)+STR(c0,4)+STR(l0,2))
    ex = .T.
  ELSE
    ex = EMPTY(WHO)
  ENDIF
  IF ex
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Не закрыт табель за предыдущий месяц!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (sl)
    RETURN
  ENDIF
ENDIF
***Проверяем,нет ли уже такой записи
IF .NOT. SEEK(STR(tmpPers,5)+STR(tmpYEAR,4)+STR(tmpMONTH,2))
  IF was_pay
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Табель для сотрудника отсутствует!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (sl)
    RETURN
  ENDIF
  days_w = days_sh

  DO CASE
  CASE tmpMONTH+12*tmpYEAR < m_on+12*y_on && Еще не было
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Он (она) в это время еще у нас не работал!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (sl)
    RETURN
  CASE tmpMONTH+12*tmpYEAR = m_on+12*y_on && Был част.
    IF d_on > 1
      days_w = REPLICATE("А", d_on-1)+SUBSTR(days_w, d_on)
    ENDIF
  ENDCASE

  IF m_off # 0
    DO CASE
    CASE tmpMONTH+12*tmpYEAR > m_off+12*y_off && Уже не было
      days_w = REPLICATE("А",LEN(days_w))
    CASE tmpMONTH+12*tmpYEAR = m_off+12*y_off && Был част.
      IF d_off < LEN(days_w)
        days_w = LEFT(days_w, d_off)+REPLICATE("А",LEN(days_w)-d_off+1)
      ENDIF
    ENDCASE
  ENDIF
  APPEND BLANK
  REPLACE MONTH   WITH tmpMONTH, ;
          YEAR    WITH tmpYEAR,  ;
          DAYS    WITH days_w,   ;
          PS_CODE WITH tmpPERS
ENDIF
IF .NOT. EMPTY(WHO) .OR. was_pay
  WAIT WINDOW "Деньги по табелю начислены, его нельзя корректировать" NOWAIT
ENDIF     

*
*  Переменные состояния для навигации
*

PRIVATE stat_type     && Тип навигации: 0 - нестандартный;
                                        1 - бланк;
                                        2 - BROWSE - таблица;
                                        3 - BROWSE - список.
PRIVATE what_do       && Имя режима.
PRIVATE menu_name     && Имя асинхронного меню.
PRIVATE last_mouse    && Время последнего нажатия звериной кнопки.
PRIVATE win_name      && Имя окна ( окон для BROWSE ).

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

ex   = 1
DIMENSION tmp_arr(LEN(ALLTRIM(DAYS)))
FOR i = 1 TO ALEN(tmp_arr)
  tmp_arr(i) = SUBSTR(DAYS,i,1)
ENDFOR

*------------------------------------------------------------------------

*          Фамилия И. О.
*┌────────────────────────────────────┐
*│      Табель за <месяц> <Год>       │
*│    ┌──┬────┬────┬────┬────┬────┐   │
*│    │Пн│    │ 5 ▒│12 ▒│19 ▒│26 ▒│   │
*│    │Вт│    │ 6 ▒│13 ▒│20 ▒│27 ▒│   │
*│    │Ср│    │ 7 ▒│14 ▒│21 ▒│28 ▒│   │
*│    │Чт│ 1 ▒│ 8 ▒│15 ▒│22 ▒│29 ▒│   │
*│    │Пт│ 2 ▒│ 9 ▒│16 ▒│23 ▒│30 ▒│   │
*│    │Сб│ 3 ▒│10 ▒│17 ▒│24 ▒│31 ▒│   │
*│    │Вс│ 4 ▒│11 ▒│18 ▒│25 ▒│    │   │
*│    └──┴────┴────┴────┴────┴────┘   │
*│< OK ^W > < Начисл. > < Отказ. Esc >│
*│┌─Типы─────────────────────────────┐│
*││                                  ││
*││                                  ││
*││                                  ││
*││                                  ││
*│└──────────────────────────────────┘│
*└────────────────────────────────────┘

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
SELECT 0
USE (base_path+"DAYS_T")
l0 = CEILING(RECCOUNT()/2)
DO D_Win_N WITH 16+l0, 42, nm
DO Sun_Bord WITH 13, 2, WROWS()-2, WCOLS()-3, "Типы"
SCAN
  pic = pic+CODE+","
  IF RECNO() <= l0
    @ 13+RECNO(), 4 SAY CODE+"-"+NAME
  ELSE
    @ 13+RECNO()-l0, 22 SAY CODE+"-"+NAME
  ENDIF
ENDSCAN
pic = LEFT(pic,LEN(pic)-1)
USE
SELECT LS_TMP

@ 2, 2 SAY PADC(tit_1, WCOLS()-4)
*
*  Рисуем сетку
n_week = DOW(s_date)
n_week = IIF(n_week = 1, 7, n_week-1)
n_week = CEILING((ALEN(tmp_arr)+n_week-1)/7)
l0     = 3
c0     = FLOOR((WCOLS()-4-5*n_week)/2)
@ l0,   c0 SAY+"┌──"+REPLICATE("┬────",n_week)+"┐"
@ l0+1, c0 SAY+"│Пн"+REPLICATE("│    ",n_week)+"│"
@ l0+2, c0 SAY+"│Вт"+REPLICATE("│    ",n_week)+"│"
@ l0+3, c0 SAY+"│Ср"+REPLICATE("│    ",n_week)+"│"
@ l0+4, c0 SAY+"│Чт"+REPLICATE("│    ",n_week)+"│"
@ l0+5, c0 SAY+"│Пт"+REPLICATE("│    ",n_week)+"│"
@ l0+6, c0 SAY+"│Сб"+REPLICATE("│    ",n_week)+"│"
@ l0+6, c0+1 FILL TO l0+6, c0+2 COLOR (SCHEME(14,2))
@ l0+7, c0 SAY+"│Вс"+REPLICATE("│    ",n_week)+"│"
@ l0+7, c0+1 FILL TO l0+7, c0+2 COLOR (SCHEME(14,2))
@ l0+8, c0 SAY+"└──"+REPLICATE("┴────",n_week)+"┘"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

c0 = c0+4
n_week = DOW(s_date)
n_week = IIF(n_week = 1, 7, n_week-1)
PRIVATE mac

FOR i = 1 TO ALEN(tmp_arr)
  @ l0+n_week, c0 SAY STR(i,2) COLOR (IIF(SUBSTR(days_sh,i,1)="Р", ;
                                        SCHEME(13,1), SCHEME(14,2)) )
  mac = STR(i)
  @ l0+n_week, c0+3 GET tmp_arr(i) PICTURE pic VALID Jmp(&mac) COLOR SCHEME 1
  n_week = n_week+1
  IF n_week = 8
    n_week = 1
    c0 = c0+5
  ENDIF
ENDFOR

IF .NOT. EMPTY(WHO) .OR. was_pay
  CLEAR GETS
  c0 = FLOOR((WCOLS()-6)/2)
  @ 12, c0 GET ex PICTURE "@*HT \ OK "
ELSE
  c0 = FLOOR((WCOLS()-36)/2)
  @ 12, c0 GET ex PICTURE "@*HT \! OK ^W ; Начисл. ;\? Отказ. Esc "
ENDIF

READ CYCLE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF ex # 3 .AND. EMPTY(WHO) .AND. .NOT. was_pay
  pic = ""
  FOR i = 1 TO ALEN(tmp_arr)
    pic = pic+tmp_arr(i)
  ENDFOR
  REPLACE DAYS WITH pic
  IF ex = 2
    DO Tbl_Pay WITH tmpPers, tmpMONTH, tmpYEAR
  ENDIF
ENDIF
USE
SELECT(sl)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Jmp          Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Движение влево-вправо.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.09.1999 ──┘
PROCEDURE Jmp
PARAMETERS flt_n

PRIVATE s

s = READKEY() % 256
IF s = 0
  s = MIN(flt_n, 7)
  RETURN -s
ENDIF
IF s = 1
  s = MIN(ALEN(tmp_arr)-flt_n+1, 7)
  RETURN s
ENDIF

RETURN .T.

