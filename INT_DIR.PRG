*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Int_Dir      Разработчик Андрей Васин           12.02.97 16:31:04 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                    Интерактивный выбор директории.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Int_Dir

PARAMETER st_path, titl, sz
&& Начальное значение для пути, заголовок окна и требуемое пространство.

PRIVATE drv, wk_path, mss, drv_list, i, titl_w, sz_w
PRIVATE sp_f, sp_h

sp_f = 0
sp_h = 0

*
*    Список устройств
*
drv = Drv_Lst()
i = 0
DO WHILE LEN(drv) > 0
  i = i+1
  DIMENSION drv_list(i)
  drv_list(i) = LEFT(drv,2)
  drv = SUBSTR(drv,3)
ENDDO

*
*    Формируем имя диска и путь
*
IF TYPE("titl") = "C"
  titl_w = ALLTRIM(titl)
ELSE
  titl_w = "Выбираем директорию..."
ENDIF

IF TYPE("sz") = "N"
  sz_w = sz
ELSE
  sz_w = 0
ENDIF

IF TYPE("st_path") = "C"
  wk_path = ALLTRIM(st_path)
ELSE
  wk_path = "\"
ENDIF

IF EMPTY(wk_path)
  wk_path = "\"
ELSE
  wk_path = IIF(RIGHT(wk_path,1) = "\", wk_path, wk_path+"\")
ENDIF

IF .NOT. Disk_OK(wk_path)     && Не в порядке дискета...
  wk_path = "\"
ENDIF

DO WHILE .T.
  wk_path = FULLPATH(wk_path)   && Получаем полный путь и...
  drv = LEFT(wk_path, 2)        &&    выделяем драйв 
  wk_path = SUBSTR(wk_path,3)   &&      и собственно путь

  IF LEN(wk_path) > 1            && Какой то путь указан...
    IF .NOT. Is_Dir(drv+wk_path) &&       проверяем его наличие
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"Указанной Вами директории не существует!"
      mss(3) = CHR(0)+"Берем корневую."
      mss(4) = ""
      DO Out_Mess WITH 7, "mss"
      wk_path = "\"
      LOOP
    ENDIF
  ENDIF
  IF ASCAN(drv_list, drv) = 0
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Не существует устройства "+drv+"!"
    mss(3) = CHR(0)+"Берем корневую директорию текущего устройства."
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
    wk_path = "\"
    LOOP
  ENDIF
  EXIT
ENDDO

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
DEFINE WINDOW No_Space FROM 0, WCOLS("")-50 TO 1, WCOLS("")-5 NONE SHADOW COLOR SCHEME 7
ACTIVATE WINDOW No_Space NOSHOW

@ 0, 0 SAY PADC("ВНИМАНИЕ! Требуется "+  ;
             ALLTRIM(TRANSFORM(sz_w, "99 999 999 999"))+" байт.", WCOLS())
@ 1, 0 SAY PADC("На этом устройстве столько нет!", WCOLS())

ACTIVATE SCREEN

*         Выбираем директорию...
*┌──────────────────────────────────────────────┐
*│ ┌─ Текущая директория ─────────────────────┐ │
*│ │ ....:....1....:....2....:....3....:....4 │ │
*│ └──────────────────────────────────────────┘ │
*│     Директории                               │
*│ ┌────────────────┐  ┌─ Текущий диск ───────┐ │
*│ │ 12345678 12345 │  │     ┌──────────┐     │ │
*│ │ 12345678 12345 │  │     │          │     │ │
*│ │ 12345678 12345 │  │     └──────────┘     │ │
*│ │ 12345678 12345 │  │ Всего 99 999 999 999 │ │
*│ │ 12345678 12345 │  │ Своб. 99 999 999 999 │ │
*│ │ 12345678 12345 │  └──────────────────────┘ │
*│ │ 12345678 12345 │                           │
*│ │ 12345678 12345 │     <   OK Ctrl-W    >    │
*│ │ 12345678 12345 │                           │
*│ │ 12345678 12345 │     < Отказаться Esc >    │
*│ └────────────────┘                           │
*└──────────────────────────────────────────────┘

&&   Объявляем и заполняем поля бланка
PRIVATE wk_list, nm_w, list_p, drv_p, ex

DIMENSION wk_list(1)

drv_p = ASCAN(drv_list, drv)
list_p = 1
nm_w = drv_list(drv_p)+wk_path
DO Dir_List WITH nm_w
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 19, 50, titl_w
DO Sun_Bord WITH 2, 3, 4, 46
@ 2, 5 SAY " Текущая директория "
DO Sun_Bord WITH 6, 23, 12, 46
@ 6, 25 SAY " Текущий диск "
@ 10,25 SAY "Всего"
@ 11,25 SAY "Своб."
@ 5,  7 SAY "Директории"
DO Show_Space

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  5 EDIT nm_w SIZE 1, 40 WHEN .F. COLOR (SCHEME(13,2))
@  7, 30 GET  drv_p  FROM drv_list PICTURE "@^" SIZE 1,  10 VALID Exit_D()
@  6,  4 GET  list_p FROM wk_list  PICTURE "@&" SIZE 12, 18 VALID Exit_P()  ;
              WHEN ALEN(wk_list) > 1 .OR. .NOT. EMPTY(wk_list(1))
@ 13, 25 GET  ex PICTURE "@*VT \!   OK Ctrl-W    ;\? Отказаться Esc " SIZE 1, 16, 1

READ CYCLE VALID Enough_Sp()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RELEASE WINDOW No_Space

RETURN IIF(ex = 1, drv+wk_path, "")

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Проверка наличия пространства на диске.                ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.02.97 16:38:28 ═╝
PROCEDURE Enough_Sp

PRIVATE mss
IF ex # 1 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF .F.  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
IF sp_f < sz_w
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"На диске не достаточно места!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
ENDIF  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Вывод пространства на диске.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.02.97 16:38:28 ═╝
PROCEDURE Show_Space

DO Disk_Space WITH drv_list(drv_p), sp_f, sp_h
@ 10, 31 SAY sp_h PICTURE "99 999 999 999"
@ 11, 31 SAY sp_f PICTURE "99 999 999 999"
IF .F.  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
IF sp_f < sz_w
  SHOW WINDOW No_Space TOP
ELSE
  HIDE WINDOW No_Space
ENDIF
ENDIF   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Программа выхода из списка директорий.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.02.97 13:43:53 ═╝
PROCEDURE Exit_P

PRIVATE prmpt, s

s = READKEY() % 256

IF s # 15
  RETURN .T.
ENDIF

prmpt = ALLTRIM(LEFT(wk_list(list_p),8))

DO CASE
CASE EMPTY(prmpt)      && Драйв не содержит директорий
  RETURN .T.
CASE prmpt == "."      && Домой!
  wk_path = "\"
CASE prmpt == ".."     && Вверх!
  wk_path = LEFT(wk_path, RAT("\", wk_path, 2))
OTHERWISE
  wk_path = wk_path+prmpt+"\"
ENDCASE

nm_w = drv_list(drv_p)+wk_path
SHOW GET nm_w
DO Dir_List WITH nm_w
list_p = 1
SHOW GET list_p
DO Show_Space

RETURN .F.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Программа выхода из драйва.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.02.97 13:59:53 ═╝
PROCEDURE Exit_D

IF drv_list(drv_p) $ "A:B:"
  IF .NOT. Disk_OK(drv_list(drv_p))
    drv_p = ASCAN(drv_list, drv)
    SHOW GET drv_p
    RETURN .F.
  ENDIF
ENDIF

IF drv # drv_list(drv_p)
  drv = drv_list(drv_p)
  list_p = 1
  wk_path = "\"
  nm_w = drv_list(drv_p)+wk_path
  SHOW GET nm_w
  DO Dir_List WITH nm_w
  list_p = 1
  SHOW GET list_p
ENDIF

DO Show_Space

RETURN .T.
