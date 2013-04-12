*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл OrdTrans     Разработчик Андрей Васин           26.02.97 13:29:25 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║         Программа обработки заказа в свободном DBF - формате.          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE OrdTrans

PRIVATE s_file, arr_f, arr_f1, arr_w, f, what_do, p_, n, i

*
*    Открываем и исследуем исходный файл
*
s_file = Int_File("", "DBF", "Укажите исходный DBF-файл")

IF EMPTY(s_file)
  RETURN
ENDIF

USE (s_file) ALIAS S_FILE

DIMENSION p_(5)
p_(1) = 1
p_(2) = 1
p_(3) = 1
p_(4) = 1
p_(5) = 1

n = AFIELD(arr_w)
DIMENSION arr_f(n+1), arr_f1(n+1)
arr_f(1) = SPACE(10)
arr_f1(1) = SPACE(10)
FOR i = 1 TO n
  arr_f(i+1)  = IIF(arr_w(i,2) = "C", "", "\")+PADR(arr_w(i,1), 10)
  arr_f1(i+1) = IIF(arr_w(i,2) $ "NC", "", "\")+PADR(arr_w(i,1), 10)
ENDFOR

SELECT 0
SET EXCLUSIVE ON
DELETE FILE (tmpo_path+"T_ORD.DBF")
CREATE DBF (tmpo_path+"T_ORD")  ;
   (  DOC_NUM   N( 1),  ;
      DOC_DATE  D,      ;
      PREFIX    C( 4),  ;
      NAME      C(35),  ;
      MODEL     C(10),  ;
      FUNC      C(20),  ;
      QNT       N( 6)  )
SET EXCLUSIVE OFF


PRIVATE what_do
what_do = "Prep"

DO WHILE .T.

  DO CASE

  CASE what_do = "Prep"    && Подготовка временного файла

    what_do = IIF(Prep(), "List", "Exit")

  CASE what_do = "List"    && Просмотр списка

    DO V_List
    what_do = "Save"

  CASE what_do = "Save"

    IF .NOT. Term_F()
      what_do = "Prep"
    ELSE
      what_do = "Exit"
    ENDIF

  OTHERWISE
  
    EXIT

  ENDCASE

ENDDO

CLOSE DATABASES
DELETE FILE (tmpo_path+"T_ORD.DBF")

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Программа сопоставления полей.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 26.02.97 14:04:08 ═╝
PROCEDURE Prep
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

PRIVATE ex, p_w, n_w
n_w = 1
ex  = 1

*
*┌──────────────────────────────────────────────────────────────────┐
*│┌────────────┬────────────┬────────────┬────────────┬────────────┐│
*││  Префикс   │Наименование│Модель апп. │Функц. назн.│ Количество ││
*│├────────────┼────────────┼────────────┼────────────┼────────────┤│
*││            │            │            │            │            ││
*││            │            │            │            │            ││
*││            │            │            │            │            ││
*│└────────────┴────────────┴────────────┴────────────┴────────────┘│
*│                                                                  │
*│                 < OK Ctrl_W > < Отказаться Esc >                 │
*│                                                                  │
*└──────────────────────────────────────────────────────────────────┘


*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 13, 70, "Сопоставьте поля"
p_w = p_(1)

@ 2, 2 SAY "┌────────────┬────────────┬────────────┬────────────┬────────────┐"
@ 3, 2 SAY "│  Префикс   │Наименование│Модель апп. │Функц. назн.│ Количество │"
@ 4, 2 SAY "├────────────┼────────────┼────────────┼────────────┼────────────┤"
@ 5, 2 SAY "│            │            │            │            │            │"
@ 6, 2 SAY "│            │            │            │            │            │"
@ 7, 2 SAY "│            │            │            │            │            │"
@ 8, 2 SAY "└────────────┴────────────┴────────────┴────────────┴────────────┘"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@  5,  3 GET p_(1) PICTURE "@^" FROM arr_f SIZE 1, 12
@  5, 16 GET p_(2) PICTURE "@^" FROM arr_f SIZE 1, 12
@  5, 29 GET p_(3) PICTURE "@^" FROM arr_f SIZE 1, 12
@  5, 42 GET p_(4) PICTURE "@^" FROM arr_f SIZE 1, 12
@  5, 55 GET p_(5) PICTURE "@^" FROM arr_f1 SIZE 1, 12
@ 10, 19 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Проверка полей.                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 26.02.97 16:45:47 ═╝
PROCEDURE Tst_Blank

PRIVATE mss, f_p, f_n, f_q, f_a, f_f, a_w, i

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

SELECT T_ORD
ZAP

SELECT S_FILE
SCAN
  SCATTER TO a_w
  i = p_(1)
  f_p = IIF(i = 1, "", a_w(i-1))
  i = p_(2)
  f_n = IIF(i = 1, "", a_w(i-1))
  i = p_(3)
  f_a = IIF(i = 1, "", a_w(i-1))
  i = p_(4)
  f_f = IIF(i = 1, "", a_w(i-1))
  i = p_(5)
  f_q = IIF(i = 1, 0, a_w(i-1))
  IF TYPE("f_q") # "N"
    f_q = VAL(ALLTRIM(f_q))
  ENDIF
  f_p = PADL(ALLTRIM(f_p),4)
  SELECT T_ORD
  APPEND BLANK
  REPLACE PREFIX WITH f_p, ;
          NAME   WITH f_n, ;
          MODEL  WITH f_a, ;
          FUNC   WITH f_f, ;
          QNT    WITH f_q
  SELECT S_FILE
ENDSCAN

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                Просмотр и коррекция созданного списка.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 26.02.97 16:55:38 ═╝
PROCEDURE V_List

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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

SELECT T_ORD
*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 100   && Количество видимых строк BROWSE
wd = FSIZE("PREFIX")+FSIZE("NAME")+FSIZE("QNT")+2

DO D_Wins WITH ln, wd, "Просмотр и коррекция заявки", 0, 0
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - меню
*
    BROWSE FIELDS PREFIX:H="", NAME:H="", QNT:H=""   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Выход из экранной формы.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 26.02.97 17:04:33 ═╝
PROCEDURE Term_F

PRIVATE mss, i, sz, pt

DIMENSION mss(3)

mss(1) = ""
mss(2) = CHR(0)+"   [ Сохраним заявку ] [ Вернемся в начало ] [ Все забудем ]?"
mss(3) = ""

i = Out_Mess(5, "mss")
IF i = 2
  RETURN .F.
ENDIF

IF i = 3
  RETURN .T.
ENDIF

SELECT T_ORD
sz = HEADER()+RECNO()*RECSIZE()

pt = ""
DO WHILE .T.
  
  pt = Int_Dir(pt, "Выберите раздел для заявки...", sz)
  
  IF EMPTY(pt)
    EXIT
  ENDIF
  
  IF File_O(pt+"ORD.DBF")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  А там уже есть файл с заявкой! [ Удалить! ] [ Пишем в другое место ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      LOOP
    ENDIF
    DELETE FILE (pt+"ORD.DBF")
  ENDIF
  
  GO TOP
  COPY TO (pt+"ORD.DBF")
  EXIT

ENDDO

RETURN .T.
