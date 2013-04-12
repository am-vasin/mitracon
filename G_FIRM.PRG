*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла G_Firm       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Получить предприятие.                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 06.07.2000 ══╝
PROCEDURE G_Firm

PARAMETERS f_code, f_name, acc

PRIVATE point, codeList, nameList, nList, s_sav, mss
PRIVATE df_chList, lt_chList

s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG NAME ALIAS FIRM0706 AGAIN

nList = 0
point = 1
SCAN
  IF TYPE("acc") = "N"
    IF .NOT. Leg_Firm(acc, FIRM_CODE)
      LOOP
    ENDIF
  ENDIF
  nList = nList+1
  DIMENSION codeList(nList), nameList(nList), df_chList(nList), lt_chList(nList)
  codeList(nList) = FIRM_CODE
  nameList(nList) = B_NAME
  df_chList(nList) = DEF_WCHK
  lt_chList(nList) = LET_WCHK
  IF f_code = FIRM_CODE
    point = nList
  ENDIF
ENDSCAN

USE
SELECT (s_sav)

IF nList = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не доступно ни одного предприятия!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF nList = 1
  f_code = codeList(1)
  f_name = nameList(1)
  lt_wchk=lt_chList(1)
  df_wchk=df_chList(1)
  w_check = df_wchk
  RETURN
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
PRIVATE ln, wd
ln = MIN(ALEN(nameList)+4, WROWS("")-9)
wd = LEN(nameList(1))+6

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, "Предприятия"
@ 1, 1 CLEAR TO WROWS()-1, WCOLS()-1
@ WROWS()-1, 0 SAY PADC("Enter-выбор,Esc-отказ", WCOLS())

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  point = MAX(point,1)
  point = MIN(point,ALEN(nameList))
  @ 1, 2 MENU nameList, ALEN(nameList), WCOLS()-4
  READ MENU TO point
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    f_code = codeList(point)
    f_name = nameList(point)
    lt_wchk = lt_chList(point)
    df_wchk = df_chList(point)
    w_check = df_wchk
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF Sale_Mode = 'MEGA'
  IF WONTOP('Corr_Tit')  && update w_check checkbox only if it is present
    SHOW GET w_check
  ENDIF
ENDIF
RETURN
