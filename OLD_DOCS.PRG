*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Old_Docs     Разработчик Андрей Васин           03.02.97 23:48:28 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Просмотр и печать документов.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Old_Docs
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
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy

SELECT 0
USE PATTERN ORDER TAG CODE

PRIVATE key_l, doc_l, f_len
f_len = 1
DIMENSION key_l(f_len), doc_l(f_len)
key_l(f_len) = "     "
doc_l(f_len) = PADR("Все документы", FSIZE("DOC_PROMPT"))
SCAN
  f_len = f_len+1
  DIMENSION key_l(f_len), doc_l(f_len)
  key_l(f_len) = STR(DOC_CODE,5)
  doc_l(f_len) = DOC_PROMPT
ENDSCAN

SELECT 0
USE TXT_DOCS
SET RELATION TO DOC_CODE INTO PATTERN
GO BOTTOM

IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного документа!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать документа" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) Do Sw_Mode WITH "Print"

DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр" KEY Ctrl-A, "F1"
ON SELECTION PAD Filter OF (menu_name) Do Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, set_f, pic
ln = WROWS("")-9   && Количество видимых строк BROWSE
wd = FSIZE("DOC_PROMPT", "PATTERN")+ ;
     FSIZE("VISUAL_L", "TXT_DOCS")+ ;
     FSIZE("DOC_TIME", "TXT_DOCS")+ ;
     FSIZE("DOC_NUM", "TXT_DOCS")+ ;
     IIF(SET("CENTURY") = "ON", 14, 12)
pic = "@S"+ALLTRIM(STR(FSIZE("DOC_PROMPT", "PATTERN")+  ;
                      FSIZE("VISUAL_L", "TXT_DOCS")+1))
set_f = key_l(1)

DEFINE WINDOW Filter FROM 1, WCOLS("")-24 TO  ;
                          3, WCOLS("")-4 SHADOW COLOR SCHEME 5
ACTIVATE WINDOW Filter NOSHOW
@ 0, 1 SAY "Фильтр установлен"
ACTIVATE SCREEN

DO D_Wins WITH ln, wd, "Список документов", 1, 1
@ 1, 2 SAY PADC("Дата", IIF(SET("CENTURY") = "ON", 10, 8))+" "+  ;
           PADC("Время", FSIZE("DOC_TIME", "TXT_DOCS"))+" "+     ;
           PADC("N док", FSIZE("DOC_NUM", "TXT_DOCS"))+" "+      ;
           "Тип документа"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1    Do Sw_Mode WITH "Filter"

*
*    BROWSE - меню
*
    IF EMPTY(set_f)
      SET ORDER TO 0
      BROWSE FIELDS DOC_DATE:H="",     ;
                    DOC_TIME:H="",     ;
                    DOC_NUM:H="",      ;
                    F001 = ALLTRIM(PATTERN.DOC_PROMPT)+" "+ ;
                           ALLTRIM(VISUAL_L):H="":P=pic, ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Visual_F()             ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      SHOW WINDOW Filter TOP
      SET ORDER TO TAG DOC_NUM
      BROWSE KEY set_f                 ;
             FIELDS DOC_DATE:H="",     ;
                    DOC_TIME:H="",     ;
                    DOC_NUM:H="",      ;
                    F001 = ALLTRIM(PATTERN.DOC_PROMPT)+" "+ ;
                           ALLTRIM(VISUAL_L):H="":P=pic, ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Visual_F()             ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    HIDE WINDOW Filter
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Print")
    ENDIF

  CASE what_do = "Print"    && Печать документа

    DO P_Doc_T
    what_do = "List"

  CASE what_do = "Filter"   && Установка/снятие фильтра

    set_f = Set_Fl(set_f)
    what_do = "List"

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
*║                  Программа установки/снятия фильтра.                   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 04.02.97 00:40:41 ═╝
PROCEDURE Set_Fl
PARAMETER old_v
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

PRIVATE i, ln, wd, s
wd = LEN(doc_l(1))+4
ln = MIN(WROWS("")-10, f_len+3)
i = MAX(1, ASCAN(key_l, old_v))

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, "Фильтр"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  i = MAX(i, 1)
  i = MIN(i, f_len)
  @ 1, 1 MENU doc_l, f_len, wd-4
  READ MENU TO i
  s = READKEY() % 256
  IF s = 12
    i = old_v
    EXIT
  ENDIF
  IF s = 15
    i = key_l(i)
    EXIT
  ENDIF
ENDDO

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN i

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Печать визуализируемого поля внизу таблицы.               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 23.02.97 12:05:43 ═╝
PROCEDURE Visual_F

ACTIVATE WINDOW (win_name+"_M") BOTTOM
@ WROWS()-2, 2 SAY PADC(VISUAL_F,WCOLS()-4)
ACTIVATE WINDOW (ALIAS())

RETURN .T.
