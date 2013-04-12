*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл St_Send1     Разработчик Андрей Васин           27.06.97 10:02:27 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║         Пересылка стандартного списка файлов с осн. площадки.          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE St_Send1
PRIVATE mss, wk_char, pthw

IF (sys_char # "A" .AND. sale_mode # "Mitin")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Это не основная площадка!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

wk_char = ""
DO Ch_Place WITH wk_char

DO Prep_TD    && Создаем или очищаем транзитную директорию
pthw = tmpo_path+"transmit\"

DO Prep_St1 WITH wk_char, pthw

DO Arch WITH pthw, "ST_SEND1"

PRIVATE out_path
n = ADIR(arr_w,pthw+"*.*")
FOR i = 1 TO n
  IF LEFT(arr_w(i,1),10) # "ST_SEND1.A")
    DELETE FILE (pthw+arr_w(i,1))
  ENDIF
ENDFOR

out_path = "\"
out_path = Int_Dir(out_path, "Куда пишем данные?", 0)

IF .NOT. EMPTY(out_path)
  DO Copy_A WITH pthw+"ST_SEND1", out_path+"ST_SEND1"  && Сохранение архива
ENDIF

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Выбор площадки.                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 27.06.97 10:28:47 ═╝
PROCEDURE Ch_Place
PARAMETER w_code
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
DO Use_Dummy
SELECT 0
USE (base_path+"PLACES")
SET FILTER TO P_CHAR # "A"
GO TOP
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"У Вас нет ни одной дополнительной площадки!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = FSIZE("P_NAME")

DO D_Wins WITH ln, wd, "Площадки", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS P_NAME,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    w_code = P_CHAR
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║  Подготовка стандартного списка файлов для передачи на доп. площадку.  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 27.06.97 10:52:22 ═╝
PROCEDURE Prep_St1
PARAMETERS w_ch, pth
PRIVATE arr_w, p, i

*
*     Клиенты
*
DO Wt_Mess WITH "Клиенты"
USE (base_path+"ACCOUNT")
COPY STRUCTURE TO (pth+"ACCOUNT")
SELECT 0
USE (pth+"ACCOUNT") ALIAS DEST
SELECT ACCOUNT
SET ORDER TO TAG PL_NAME
SEEK w_ch
SCAN REST WHILE SUBSYS = w_ch
  SCATTER TO arr_w
  SELECT DEST
  APPEND FROM ARRAY arr_w
  SELECT ACCOUNT
ENDSCAN

SET ORDER TO TAG EMP_NAME
SEEK w_ch
SCAN REST WHILE SUBSYS = w_ch
  SCATTER TO arr_w
  SELECT DEST
  APPEND FROM ARRAY arr_w
  SELECT ACCOUNT
ENDSCAN
DO Wt_Mess
DO Wt_Mess WITH "Список рабочих мест"

*
*     Список рабочих мест
*
USE 
SELECT DEST
USE (base_path+"ARM_LST")
COPY TO (pth+"ARM_LST")

*
*     Список отделов предприятия
*
DO Wt_Mess
DO Wt_Mess WITH "Отделы предприятия"
USE (base_path+"DEPS")
COPY TO (pth+"DEPS")

*
*     Список типов товара
*
DO Wt_Mess
DO Wt_Mess WITH "Отделы предприятия"
USE (base_path+"GOODS")
COPY TO (pth+"GOODS")

*
*     Список сотрудников предприятия
*
DO Wt_Mess
DO Wt_Mess WITH "Список сотрудников предприятия"
USE (base_path+"PERSONS")
COPY STRUCTURE TO (pth+"PERSONS")
SELECT 0
USE (pth+"PERSONS") ALIAS DEST
SELECT PERSONS
i = 1
p = 0
DO WHILE .NOT. EMPTY(FIELD(i))
  IF FIELD(i) == "SALARY"
    p = i
    EXIT
  ENDIF
  i = i+1
ENDDO
SCAN
  SCATTER TO arr_w
  IF p # 0
    arr_w(p) = ""
  ENDIF
  SELECT DEST
  APPEND FROM ARRAY arr_w
  SELECT PERSONS
ENDSCAN
DO Wt_Mess

CLOSE DATABASES

RETURN
