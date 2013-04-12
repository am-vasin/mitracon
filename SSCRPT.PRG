*******************************************************************
*                                                                 *
*               Подписка на внутренюю почту                       *
*                                                                 *
*******************************************************************
PROCEDURE Sscrpt
PARAMETERS prmUser

PRIVATE tmpUser, tmpCUser, tmpNUser
IF TYPE("m.prmUser") = "N"
  m.tmpUser = m.prmUser
ELSE
  m.tmpUser = m.User
ENDIF
m.tmpCUser = STR(m.tmpUser)

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

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
m.s_sav = SELECT()
SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"SSCRPT") SHARED AGAIN ORDER TAG SS_CODE ALIAS TMP_LIST
SELECT 0
USE (m.base_path+"SSCRPT") SHARED AGAIN ORDER TAG SS_CODE ALIAS TMP_MODIFY
SELECT 0
USE (m.base_path+"SSCRPT_T") SHARED AGAIN ORDER TAG SS_NAME ALIAS TMP_TYPE
*SET RELATION TO STR(SS_CODE)+m.tmpCUser INTO TMP_LIST
USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS TMP_PRS IN 0
= SEEK(m.tmpCUser, "TMP_PRS")
m.tmpNUser = ALLTRIM(TMP_PRS.FAMILY)+" "+LEFT(TMP_PRS.NAME, 1)+" "+LEFT(TMP_PRS.S_NAME, 1)
USE IN TMP_PRS


*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD modify OF (menu_name) PROMPT "Вставить/удалить" KEY Ctrl-A, "Enter"
ON SELECTION PAD modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = FSIZE("SS_NAME", "TMP_TYPE")+2

DO D_Wins WITH m.ln, m.wd, "Почта SALE: подписка", 1, 0
@ 1, 2 SAY "Сотрудник "+m.tmpNUser
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    SELECT TMP_TYPE
    GO RECNO()
*    WAIT WINDOW IIF(FOUND("TMP_LIST"), "√", "N")

*
*    BROWSE - меню
*
    BROWSE FIELDS SS_NAME:H="",     ;
           MRK = IIF(SEEK(STR(SS_CODE)+m.tmpCUser, "TMP_LIST"), "√", " "):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY

    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE m.what_do = "Modify"    &&

    SELECT TMP_MODIFY
    IF SEEK(STR(TMP_TYPE.SS_CODE)+m.tmpCUser)
      REPLACE SS_CODE WITH 0, SS_USER WITH 0
    ELSE
      GO TOP
      IF .NOT. EMPTY(SS_USER) .OR. BOF() .OR. EOF()
        APPEND BLANK
      ENDIF
      REPLACE SS_CODE WITH TMP_TYPE.SS_CODE, SS_USER WITH m.tmpUser
    ENDIF
    SELECT TMP_TYPE
    =SEEK(STR(TMP_TYPE.SS_CODE)+m.tmpCUser, "TMP_LIST")
    m.what_do = "List"

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
USE IN TMP_LIST
USE IN TMP_TYPE
USE IN TMP_MODIFY
SELECT (m.s_sav)

RETURN
