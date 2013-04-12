*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла C_Bank       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                            Получить банк.                              ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 06.07.2000 ══╝
PROCEDURE C_Bank

PARAMETERS b_code, b_name, f_code

PRIVATE point, codeList, nameList, nList, s_sav, mss

s_sav = SELECT()
SELECT 0
USE (base_path+"BANKS") ORDER TAG NAME ALIAS BANK0706 AGAIN

nList = 0
point = 1
SEEK f_code
SCAN REST WHILE f_code = FIRM_CODE
  nList = nList+1
  DIMENSION codeList(nList), nameList(nList)
  codeList(nList) = BANK_CODE
  nameList(nList) = B_NAME
  IF b_code = BANK_CODE
    point = nList
  ENDIF
ENDSCAN

USE
SELECT (s_sav)

IF nList = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не доступен ни один банк!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF nList = 1
  b_code = codeList(1)
  b_name = nameList(1)
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
DO D_Win_N WITH ln, wd, "Банки"
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
    b_code = codeList(point)
    b_name = nameList(point)
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
