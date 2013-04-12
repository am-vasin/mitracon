*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                     СПРАВОЧНИК ПО МЕСЯЦАМ.                            ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE INF_CLN
PRIVATE _file,_d_file,tmpMonth,tmpYEAR 

*DIMENSION M(12)
* M(1) = "Январь"
* M(2) = "Февраль"
* M(3) = "Март"
* M(4) = "Апрель"
* M(5) = "Май"
* M(6) = "Июнь"
* M(7) = "Июль"
* M(8) = "Август"
* M(9) = "Сентябрь"
* M(10) = "Октябрь"
* M(11) = "Ноябрь"
* M(12) = "Декабрь"

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
DEFINE MENU (menu_name) IN SCREEN && COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

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
 USE (base_path+"MOULD")
 ENDIF

 IF NOT USED("DUMMY")
 DO USE_DUMMY
 ENDIF

 SELECT MOULD
 tmpMonth = MOULD.MONTH
 tmpYEAR = MOULD.YEAR


*
*   Расчет размеров окна
*
PRIVATE ln, wd, f_name, cl
*ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
ln = 20   && Количество видимых строк BROWSE
wd =15

DO D_Wins WITH ln, wd, " Шаблоны", 0, 0
what_do = "list"
*@ 1, 2 SAY SPACE(FSIZE("MONTH")+FSIZE("YEAR")+FSIZE("FLAG"))+" Шаблоны"
@ 1, 2 SAY PADR("Месяц    ",(FSIZE("MONTH")+FSIZE("YEAR")+FSIZE("FLAG")))+" Год Утв"
cl = SCHEME(13,1)+","+SCHEME(13,2)+",,,,,"+SCHEME(14,2)

f_name = ""

DO WHILE .T.
  DO CASE

  CASE what_do = "list"    && Просмотр списка

	statys_type = 3    
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

 SELECT MOULD
*
*    BROWSE - меню
*
    BROWSE FIELDS M00 = Month_R(MONTH) :8 :W=.F.:H="",     ;
           YEAR		:H = "Год", ;
           F_N = IIF(EMPTY(FLAG),  " ", "░"):H="" ,;
           DUMMY.F FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NORGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")  &&COLOR (cl)
    ON KEY

    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Corr")
    ENDIF

  CASE what_do = "Corr"      && Выбор месяца для просмотра
*    DO LS_TABLE 
   what_do = "list"
	EXIT
  OTHERWISE
	SEEK STR(tmpYEAR,4)+STR(tmpMonth,2)

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW(win_name+"_I")
RELEASE WINDOW(win_name+"_E")
RELEASE WINDOW(win_name+"_M")

RETURN

