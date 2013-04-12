*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Get_GTD      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Выбор ГТД для позиции.                         ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 26.02.2000 ══╝
PROCEDURE Get_GTD
PARAMETERS pPref, pName, pProd

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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
USE (path_comm+"GTD_TIT") ORDER TAG GTD_NUM ALIAS TIT_0226 AGAIN

SELECT 0
USE (path_comm+"GTD") ORDER TAG NAME DESCENDING ALIAS DET_0226 AGAIN
SET RELATION TO GTD_NUM INTO TIT_0226
PRIVATE GTD_name, tmpName, retValue
GTD_name = PADR(ALLTRIM(pPref)+ALLTRIM(pName), FSIZE("NAME"))
tmpName  = IIF(EMPTY(pPref), "", ALLTRIM(pPref)+" ")+ALLTRIM(pName)+ ;
           IIF(EMPTY(pProd), "", " /"+ALLTRIM(pProd)+"/")
retValue = ""

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Clear OF (menu_name) PROMPT "Очистить" KEY Ctrl-A, "Пробел"
ON SELECTION PAD Clear OF (menu_name) DO Sw_Mode WITH "Clear"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, tmpValue
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 65
*┌──Дата─────Номер ГТД──────────────────Страна─────────────Кол-во──┐
*│ДД.ММ.ГГГГ GTL----------------------- -----Страна----- 99999999 *│
*└─────────────────────────────────────────────────────────────────┘
DO D_Wins WITH ln, wd, "Выбор ГТД", 2
@ 3, 2 SAY "──Дата─────Номер ГТД──────────────────Страна─────────────Кол-во"
what_do = "List"
PRIVATE lTmp
lTmp = LEN(ALLTRIM(GTD_name))
DO WHILE lTmp > 2
  IF SEEK(GTD_name)
    EXIT
  ENDIF
  lTmp = lTmp-1
  GTD_name = PADR(LEFT(GTD_name,lTmp), FSIZE("NAME"))
ENDDO

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 2 SAY "Позиция    "+tmpName
    @ 2, 2 SAY "Имя по ГТД "+GTD_name
    
    DO Prp_Nav_1
    ON KEY LABEL Enter    KEYBOARD CHR(23)
    ON KEY LABEL F7       DO Sw_Mode WITH "Seek"
    ON KEY LABEL SpaceBar DO Sw_Mode WITH "Clear"
    IF .NOT. SEEK(GTD_name)
      WAIT WINDOW "В ГТД нет подходящей позиции!" NOWAIT
    ENDIF

*
*    BROWSE - меню
*
    BROWSE KEY GTD_name   ;
           FIELDS GTD_DATE:10:H="",  ;
                  GTD_NUM:26:H="",   ;
                  COUNTRY:16:H="",   ;
                  QNT:H="":P="@Z 99999999", ;
                  TIT_0226.GTD_TYPE:1:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    IF .NOT. BOF() .AND. .NOT. EOF()
      retValue = GTD_NUM+CHR(13)+COUNTRY+CHR(13)+GTD_LINE+CHR(13)+COUNTRC
    ENDIF
    EXIT
    
  CASE what_do = "Clear"     && Очистить

*    IF .NOT. BOF() .AND. .NOT. EOF()
      retValue = " "+CHR(13)+" "
*    ENDIF
    EXIT
    
  CASE what_do = "Seek"      && Поиск

    tmpValue = GTD_Sto(pPref, pName, pProd)
    IF .NOT. EMPTY(tmpValue)
      GTD_name = tmpValue
    ENDIF
    what_do = "List"

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
USE
SELECT TIT_0226
USE
SELECT (s_sav)

RETURN retValue
