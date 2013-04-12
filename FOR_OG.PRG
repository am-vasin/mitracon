*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла For_OG       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                          Продажи по клиентам.                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 12.11.2001 ══╝
PROCEDURE For_OG

PRIVATE m1, y1, m2, y2, c_code, c_name, tmpName, dep_c, yTmp, mTmp
m1 = MONTH(GOMONTH(DATE(),-3))
y1 = YEAR(GOMONTH(DATE(),-3))
m2 = MONTH(DATE())
y2 = YEAR(DATE())
dep_c = 2
c_code = 0
c_name = SPACE(50)
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ;
   (  CUS_CODE  N( 8),   ;
      CUS_NAME  C(50),   ;
      MONTH     N( 2),   ;
      YEAR      N( 4),   ;
      SUM_OWN   N(13,4), ;
      SUM_OTHER N(13,4)  )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_LIST
INDEX ON STR(CUS_CODE)+STR(YEAR)+STR(MONTH) TAG TMP
SET ORDER TO

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
SELECT TMP_LIST
SET ORDER TO

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Calc OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Enter"
ON SELECTION PAD Calc OF (menu_name) DO Sw_Mode WITH "Calc"

DEFINE PAD Save OF (menu_name) PROMPT "Сохранить" KEY Ctrl-A, "F2"
ON SELECTION PAD Save OF (menu_name) DO Sw_Mode WITH "Save"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 71
*┌─Клиент──────────────────────────────Год─Мц────────ОП───────Ост. отделы┐
*│кккккккккКкккккккккКкккккккккКккккк ГГГГ ММ 99999999.9999 99999999.9999│
*└───────────────────────────────────────────────────────────────────────┘
DO D_Wins WITH ln, wd, "Продажи по клиентам", 0, 1
@ 1, 2 SAY "─Клиент──────────────────────────────Год─Мц────────ОП───────Ост. отделы"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F2    DO Sw_Mode WITH "Save"

*
*    BROWSE - меню
*
    BROWSE FIELDS CUS_NAME:35:H="",    ;
                  MONTH:H="",          ;
                  YEAR:H="",           ;
                  SUM_OWN:H="",        ;
                  SUM_OTHER,           ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Calc")
    ENDIF

  CASE what_do = "Calc"    && Рассчет

    DO C_Form
    what_do = "List"

  CASE what_do = "Save"    && Сохранение

    DO Save_DBF
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
SELECT TMP_LIST
tmpName = DBF()
USE
DELETE FILE (tmpName)
DELETE FILE (LEFT(tmpName, LEN(tmpName)-3), "cdx")
SELECT (s_sav)

RETURN
*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-2, 5 SAY CUS_NAME

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура C_Form       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Форма для расчета                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.11.2001 ──┘
PROCEDURE C_Form

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

*┌──────────────────────────────────────────────────────┐
*│ ┌─ Период ─────────────────────────────────────────┐ │
*│ │     С 99 месяц 9999 год по 99 месяц 9999 год     │ │
*│ └──────────────────────────────────────────────────┘ │
*│ ┌─ [ ] Клиент ─────────────────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!....:....!│ │
*│ └──────────────────────────────────────────────────┘ │
*│           < OK Ctrl-W > < Отказаться Esc >           │
*└──────────────────────────────────────────────────────┘

PRIVATE ex, sw_cus
ex = 1
sw_cus = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 58, "Расчет продаж"
DO Sun_Bord WITH  2,  3,  4, 54, " Период "
DO Sun_Bord WITH  5,  3,  7, 54, " [ ] Клиент "
@ 6, 4 SAY c_name

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  9 SAY "С 99 месяц 9999 год по 99 месяц 9999 год"
@ 3, 11 GET m1 RANGE 1, 12 PICTURE "99"
@ 3, 20 GET y1 RANGE YEAR(DATE())-10 PICTURE "9999"
@ 3, 32 GET m2 RANGE 1, 12 PICTURE "99"
@ 3, 41 GET y2 RANGE YEAR(DATE()) PICTURE "9999"
@ 5,  6 GET sw_cus PICTURE "@*C Клиент" VALID Get_C()

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * Отрабатываем бланк
  yTmp = y1
  mTmp = m1
  SELECT TMP_LIST
  DO WHILE yTmp*12+mTmp <= y2*12+m2
    APPEND BLANK
    REPLACE CUS_CODE WITH c_code, ;
            CUS_NAME WITH c_name, ;
            YEAR     WITH yTmp,   ;
            MONTH    WITH mTmp
    mTmp = mTmp+1
    IF mTmp > 12
      yTmp = yTmp+1
      mTmp = 1
    ENDIF
  ENDDO
  SET ORDER TO TAG TMP
  SELECT 0
  USE (base_path+"PERSONS") ORDER TAG CODE
  SELECT 0
  USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE
  SET RELATION TO WHO INTO PERSONS
  mTmp = y1*12+m1
  yTmp = y2*12+m2
  SEEK c_code
  SCAN REST WHILE CUS_CODE = c_code
    IF SEEK(STR(c_code)+STR(YEAR(DOC_DATE))+STR(MONTH(DOC_DATE)), "TMP_LIST")
      IF PERSONS.DEP_NO = dep_c
        REPLACE TMP_LIST.SUM_OWN WITH TMP_LIST.SUM_OWN+TOTAL_D
      ELSE
        REPLACE TMP_LIST.SUM_OTHER WITH TMP_LIST.SUM_OTHER+TOTAL_D
      ENDIF
    ENDIF
  ENDSCAN
  USE
  SELECT PERSONS
  USE
  SELECT TMP_LIST
  SET ORDER TO
  GO BOTTOM
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_C        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выбор клиента                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.11.2001 ──┘
PROCEDURE Get_C

IF .NOT. sw_cus
  RETURN .T.
ENDIF
sw_cus = .F.
SHOW GET sw_cus
DO Customer WITH c_code, c_name, "i"
@ 6, 4 SAY c_name

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка правильности заполнения бланка.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.11.2001 ──┘
PROCEDURE Tst_Blank

PRIVATE r_sav, mss

IF ex # 1
  RETURN .T.
ENDIF

IF y2*12+m2 < y1*12+m1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы некорректно указали период..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(c_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали клиента..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

SELECT TMP_LIST
r_sav = RECNO()
SET ORDER TO TAG TMP
IF SEEK(STR(c_code))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Этот клиент уже есть в спсике..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SET ORDER TO
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
  RETURN .F.
ENDIF

SET ORDER TO
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Save_DBF     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Сохранение файла.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.11.2001 ──┘
PROCEDURE Save_DBF

PRIVATE tmp_dir, mss, tmpName, r_sav

tmp_dir = IIF(Is_Dir("D:"), "D:\", "C:\")

DO WHILE .T.
  tmpName = New_File(tmp_dir, "dbf", "Укажите файл")
  IF EMPTY(tmpName)
    RETURN
  ENDIF
  IF File_O(tmpName)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Такой файл уже существует. Удалим? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      DELETE FILE (tmpName)
    ELSE
      LOOP
    ENDIF
  ENDIF
  EXIT
ENDDO

r_sav = RECNO()
COPY TO (tmpName)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN
