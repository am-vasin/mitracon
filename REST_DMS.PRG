*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Rest_DMS     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                             Наличие в ДМС                              ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 28.05.2009 ══╝
PROCEDURE Rest_DMS
PARAMETERS prmCode

PRIVATE s_sav, tmpKey

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS DR_STO
SELECT 0
DO Use_DMS WITH "DR_DMS"
SET ORDER TO TAG CODE
= SEEK(m.prmCode, "DR_STO")
IF TYPE("CODE") = "C"
  m.tmpKey = ALLTRIM(STR(m.prmCode))
  m.tmpKey = PADR(m.tmpKey, FSIZE("CODE"))
ELSE
  m.tmpKey = m.prmCode
ENDIF
IF .NOT. SEEK(m.tmpKey, "DR_DMS")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Этой позиции нет в ДМС"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN DR_STO
  USE IN DR_DMS
  SELECT(m.s_sav)
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
PRIVATE s_sav         && Номер рабочей области для сохранения и восстановления!

*
*   Заполняем значениями переменные состояния...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()


SELECT 0
DO Use_Dummy
SELECT DR_DMS

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = FSIZE("QNT")+FSIZE("SUP_DATE")+12

DO D_Wins WITH m.ln, m.wd, "Наличие в ДМС", 1, 0
m.what_do = "List"
@ 1, 2 SAY ALLTRIM(DR_STO.PREFIX)+ALLTRIM(DR_STO.NAME)+" "
IF .NOT. EMPTY(DR_STO.PRODUCER)
  @ 1, COL() SAY "/"+ALLTRIM(DR_STO.PRODUCER)+"/"
ENDIF
@ 2, 2 SAY PADC("Кол-во", FSIZE("QNT"), "─")+"─"+ ;
           PADC("Цена", 10, "─")+"─"+ ;
           PADC("Срок поставки", FSIZE("SUP_DATE"), "─")
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE KEY m.tmpKey	;
           FIELDS QNT:H="",    ;
           PR = PRICE1*m.kurs:H="":P="@Z 9999999.99",   ;
           SUP_DATE:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*           WHEN Draw_Foot()    

    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

*  CASE m.what_do = "Choose"    && Просмотр списка

*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    m.what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*

USE IN DR_STO
USE IN DR_DMS
SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
*PROCEDURE Draw_Foot

*ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*

*ACTIVATE WINDOW (ALIAS()) SAME

*RETURN .T.
