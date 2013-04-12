*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл To_Dev       Разработчик Андрей Васин           02.09.98 12:05:59 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                          Письма разработчику.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE To_Dev
PRIVATE stack, titles
DIMENSION titles(1)
titles(1) = "Перечень основных тем"

*DO Dev_Mess

stack = ""

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

DEFINE PAD View OF (menu_name) PROMPT "Просмотр" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Return OF (menu_name) PROMPT "Вернуться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Return OF (menu_name) DO Sw_Mode WITH "Return"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy
SELECT 0
USE (base_path+"T_LIST")

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = FSIZE("P_NAME")+FSIZE("TERM")

DO D_Wins WITH ln, wd, "", 0, 0
what_do = IIF(RECCOUNT("T_LIST") > 1, "List", "View")

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 0, 0 SAY PADC(titles(ALEN(titles)), WCOLS()) COLOR SCHEME 5
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS F000 = P_NAME+TERM,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           FOR LEFT(LEVEL,LEN(stack)) == stack .AND. EMPTY(RIGHT(LEVEL,6-LEN(stack))) ;
               .AND. .NOT. EMPTY(SUBSTR(LEVEL,LEN(stack)+1,3)) ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           REST ;
           WHEN Md_Menu()  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Return", "View")
    ENDIF

  CASE what_do = "View"    && Просмотр списка

    IF EMPTY(TERM)
      DO Mss_List WITH T_LIST.LEVEL, T_LIST.P_NAME, user
    ELSE
      DIMENSION titles(ALEN(titles)+1)
      titles(ALEN(titles)) = ALLTRIM(P_NAME)
      stack = stack+SUBSTR(LEVEL,LEN(stack)+1,3)
    ENDIF
    what_do = IIF(RECCOUNT("T_LIST") > 1, "List", "Return")

  OTHERWISE

    IF ALEN(titles) = 1
      EXIT
    ELSE
      DIMENSION titles(ALEN(titles)-1)
      LOCATE FOR LEVEL = PADR(stack,9)
      stack = LEFT(stack,LEN(stack)-3)
      what_do = "List"
    ENDIF
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
*║              Коррекция меню в зависимости от типа пункта.              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 02.09.98 13:54:11 ═╝
PROCEDURE Md_Menu

IF EMPTY(TERM)
  DEFINE PAD View OF (menu_name) PROMPT "Просмотр" KEY Ctrl-A, "Enter" BEFORE Return
  ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"
ELSE
  DEFINE PAD View OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter" BEFORE Return
  ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"
ENDIF

ACTIVATE SCREEN
@ 0,0 SAY SPACE(WCOLS()-15) COLOR SCHEME 13
ACTIVATE WINDOW (ALIAS())
SHOW MENU (menu_name)

RETURN .T.
