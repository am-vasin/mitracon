*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла RASNO__      Разработчик Дмитриенко                        ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                    Бланк для накладной разногласий                     ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 21.10.1999 ══╝
PROCEDURE RASNO__

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
PRIVATE cod_0, nam_0, qnt_0, s_sav, p
p = 1
qnt_0 = 0
s_sav = SELECT()

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"S_RIGHTS") ORDER TAG RIGHT

SELECT 0
USE (base_path+"USERS") ORDER TAG USER_NAME
SET RELATION TO S_RIGHTS INTO S_RIGHTS 
SET RELATION TO USER_CODE INTO PERSONS ADDITIVE

SCAN FOR S_RIGHTS.R_FLAGS < "2" 
  qnt_0 = qnt_0 + 1
  DIMENSION cod_0(qnt_0), nam_0(qnt_0)
  cod_0(qnt_0) = USER_CODE
  nam_0(qnt_0) = ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
ENDSCAN

USE 

SELECT S_RIGHTS
USE 

SELECT PERSONS
USE 

SELECT (s_sav)
IF qnt_0 = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = " "
  mss(2) = chr(0)+"Нет того, кто разрешит передачу деталей!"
  mss(3) = " "
  DO OUT_MESS WITH 7, "mss"
  RETURN .F.
ENDIF
*------------------------------------------------------------------------
*    Содержательная часть программы:
*
*┌─────────────────────────────────────────────┐
*│    Предприятие AAA AAAAAAAAAAAAAAAA (F1)    │
*│              ┌────────────────────────────┐ │
*│ Кто разрешил │AAAAAAAAAAAAAAAAAAAAAAAAAAAA│ │
*│              └────────────────────────────┘ │
*│         Примечание AAAAAAAAAAAAAAAA         │ 
*│                                             │
*│            [ ] Печать накладной             │
*│                                             │
*│    < OK Ctrl-W > < Отказаться Esc >         │
*└─────────────────────────────────────────────┘
PRIVATE ex
ex = 1
*------------------------------------------------------------------------
PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 12, 49, "Заполните бланк"
@ 4,3 SAY "Кто разрешил"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO Get_Firm WITH 2, 6, enab_firm
@ 3, 16 GET p PICTURE "@^" SIZE 1,30 FROM nam_0
@ 6, 11 SAY "Примечание" GET nt
@ 8, 14 GET to_print PICTURE "@*C Печать накладной"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_F()
w_l = cod_0(p)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
