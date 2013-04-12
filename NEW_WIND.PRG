*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                     Коррекция формата документов.                     ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE NEW_WIND
PRIVATE _file,_d_file, mss, mn_w, ye_w

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

DEFINE PAD Corr OF (menu_name) PROMPT IIF(RECCOUNT() = RECNO(), ;
               "Корр.", "Просмотр") KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

IF RECCOUNT() = 1

  DEFINE PAD Aff OF (menu_name) PROMPT "Утвердить" KEY Ctrl-A, "F5"
  ON SELECTION PAD Aff OF (menu_name) Do Sw_Mode WITH "Aff"

ELSE

  DEFINE PAD Ls_Tab OF (menu_name) PROMPT "Табель" KEY Ctrl-A, "F3"
  ON SELECTION PAD Ls_Tab OF (menu_name) Do Sw_Mode WITH "Ls_tab"

ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"


*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

***Определяем есть ли уже такой файл  
_file = base_path + "MOULD"
d_file = _file +".dbf"
 
 IF NOT FILE_O (d_file)
	DO MOULD WITH MONTH(DATE()),YEAR(DATE())
 ENDIF
 
 IF NOT USED("MOULD")
   SELECT 0
   USE (base_path+"MOULD") ORDER TAG MONTH
 ENDIF
 IF RECCOUNT() = 0
   DO Ad_Month
 ENDIF

 IF NOT USED("DUMMY")
 DO USE_DUMMY
 ENDIF

 SELECT MOULD
 GO BOTTOM
*
*   Расчет размеров окна
*
PRIVATE ln, wd, f_name, cl
*ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
ln = 20   && Количество видимых строк BROWSE
wd = 13

DO D_Wins WITH ln, wd, "Шаблоны ", 0, 0
what_do = "list"
@ 1, 2 SAY "─Месяц───Год"
cl = SCHEME(13,1)+","+SCHEME(13,2)+",,,,,"+SCHEME(14,2)

f_name = ""

DO WHILE .T.
  DO CASE

  CASE what_do = "list"    && Просмотр списка

	statys_type = 3    
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1    Do Sw_Mode WITH "Parms"
	ON KEY LABEL F5    Do Sw_Mode WITH "Aff"
*    ON KEY LABEL F6    Do Sw_Mode WITH "Add"
	ON KEY LABEL F3    Do Sw_Mode WITH "Ls_tab"

  SELECT MOULD
*
*    BROWSE - меню
*
    BROWSE FIELDS M000 = Month_R(MONTH) :8 :W=.F.:H="", ;
                  YEAR:H="",      ;
                  DUMMY.F FREEZE DUMMY.F ;
           WHEN Modi_Menu()       ;
           NOAPPEND NODELETE NOMODIFY NORGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")  &&COLOR (cl)
    ON KEY

    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Corr")
    ENDIF

  CASE what_do = "Corr"      && Выбор интересующего месяца
    ye_w = YEAR
    mn_w = MONTH
    DO Mould_A WITH ye_w, mn_w
    SEEK STR(ye_w,4)+STR(mn_w,2)
    what_do = "list"

  CASE what_do = "Ls_tab"    && Список сотрудников

    IF RECNO() # RECCOUNT()
      DO Ls_Table 
    ENDIF
    what_do = "list"

  CASE what_do = "Aff"    && Утвердить шаблон
    IF RECNO() = RECCOUNT()
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0) + "  Утверждаем шаблон? [ Да ] [ Нет ]"
      mss(3) = ""
      d = OUT_Mess(7,"mss")
      IF d = 1
        DO Ad_Month
      ENDIF
    ENDIF
    what_do = "list"

  OTHERWISE
	
    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN
*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Коррекция первого пункта меню.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 25.09.97 18:44:21 ═╝
PROCEDURE Modi_Menu

IF RECCOUNT() = RECNO()
  DEFINE PAD Corr OF (menu_name) PROMPT "Корр." KEY Ctrl-A, "Enter"
  ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"
  DEFINE PAD Aff OF (menu_name) BEFORE Exit PROMPT "Утвердить" KEY Ctrl-A, "F5"
  ON SELECTION PAD Aff OF (menu_name) Do Sw_Mode WITH "Aff"
  RELEASE PAD Ls_Tab OF (menu_name)
ELSE
  DEFINE PAD Ls_Tab OF (menu_name) AFTER Corr PROMPT "Табель" KEY Ctrl-A, "F3"
  ON SELECTION PAD Ls_Tab OF (menu_name) Do Sw_Mode WITH "Ls_tab"

  DEFINE PAD Corr OF (menu_name) PROMPT "Просмотр" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"
  RELEASE PAD Aff OF (menu_name)
ENDIF

ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS("")-11) COLOR (SCHEME(1,2))
SHOW MENU (menu_name)
ACTIVATE WINDOW (ALIAS()) SAME
RETURN
