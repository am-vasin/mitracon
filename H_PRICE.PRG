*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура H_Price      Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                 Программа ввода "дороговизны" деталей                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 19.11.1999 ──┘
PROCEDURE H_Price  
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
*   Определяем асинхронное меню
*                                                              

DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr" 

DEFINE PAD Add  OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add  OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del  OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Del  OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
DELETE FILE (tmpo_path+"H_PRICE.DBF")
DELETE FILE (tmpo_path+"H_PRICE.CDX")
CREATE DBF (tmpo_path+"H_PRICE.DBF") ;
     ( CHAR_C     C( 1),   ;
       NAME_C     C(24),   ;
       USD0       C( 8),   ;
       USD1       C( 8))
INDEX ON USD1 TAG USD1       
SELECT 0
IF FILE_O(base_path+"O_Bound.DBF")
  USE (base_path+"O_Bound") ORDER TAG H_BOUND 
ELSE
  CREATE DBF (base_path+"O_Bound");
             (CHAR_C  C(  1), ;
              NAME_C  C( 24), ;
              H_BOUND N(8,2)) 
  INDEX ON H_BOUND TAG H_BOUND
  USE (base_path+"O_Bound") ORDER TAG H_BOUND 
  APPEND BLANK
  REPLACE NAME_C  WITH "Ну очень дорогие!", ;                
          H_BOUND WITH 99999.99
ENDIF          
  
prv = ""
SCAN
  SELECT H_Price 
  APPEND BLANK
  REPLACE CHAR_C  WITH O_Bound.CHAR_C,;
          NAME_C  WITH O_Bound.NAME_C,;
          USD0    WITH prv,;
          USD1    WITH IIF(O_Bound.H_BOUND = 99999.99,"И выше",STR(O_Bound.H_BOUND,8,2))
  prv = STR(O_Bound.H_BOUND+0.01,8,2)       
  SELECT O_Bound
ENDSCAN         
SELECT H_Price
*
*   Расчет размеров окна
*
*   Градация "дороговизны" деталей для заявок
* ┌С─Название───────────────────────От────До───┐
* │X XXXXXXXXXXXXXXXXXXXXXXXX     0.00 99999.99│
* │X XXXXXXXXXXXXXXXXXXXXXXXX 99999.99 99999.99│
* │X XXXXXXXXXXXXXXXXXXXXXXXX 99999.99 99999.99│
* │X XXXXXXXXXXXXXXXXXXXXXXXX 99999.99 99999.99│
* │X XXXXXXXXXXXXXXXXXXXXXXXX 99999.99 И Выше  │
* └────────────────────────────────────────────┘
PRIVATE ln, wd
ln = WROWS("")-7   && Количество видимых строк BROWSE
wd = 44

DO D_Wins WITH ln, wd, 'Градация "дороговизны" деталей для заявок', 0, 0
what_do = "List"
@ 1,2 SAY "Симв─Название─────────────────От───────До──"  
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins DO Sw_Mode WITH "Add"
    ON KEY LABEL Del DO Sw_Mode WITH "Del"
    
*
*    BROWSE - меню
*    
 BROWSE FIELDS CHAR_C:H="":4,  ;
    		   NAME_C:H="":21,  ;
    		   USD0:H="",    ;
    		   USD1:H="",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

   CASE what_do = "Corr"    && Корректировать

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Price
    ENDIF
    what_do = "List"

  CASE what_do = "Add"       && Добавить

    DO Price WITH .T.
    what_do = "List"

  CASE what_do = "Del"       && Удалить
    IF .NOT. BOF() .AND. .NOT. EOF()
      IF USD1 = "И выше  "
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Эту запись удалять нельзя!!!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
      ELSE
        DELETE 
      ENDIF
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

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+" Сохранить изменения? [ Да ] [ Нет ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 1
  DO END_H
ENDIF  
SELECT O_BOUND    
USE  
SELECT H_PRICE
USE
DELETE FILE (tmpo_path+"H_PRICE.DBF")
DELETE FILE (tmpo_path+"H_PRICE.CDX")
SELECT (s_sav)

    
*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура END_H        Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Процедура выхода                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 23.11.1999 ──┘
PROCEDURE END_H

SELECT O_BOUND
SET ORDER TO
RECALL ALL
GO TOP
SELECT H_PRICE
SET ORDER TO
SCAN
  SELECT O_BOUND
  IF EOF()   
    APPEND BLANK 
  ENDIF
  IF H_PRICE.USD1 = "И выше  "
    prv = 99999.99
  ELSE
    prv = VAL(ALLTRIM(H_PRICE.USD1))
  ENDIF  
  REPLACE  CHAR_C  WITH H_PRICE.CHAR_C, ;
           NAME_C  WITH H_PRICE.NAME_C, ;
           H_BOUND WITH prv
  SKIP
  SELECT H_PRICE            
ENDSCAN         
SELECT O_BOUND
DELETE REST
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура  Price                                                     │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│            Коррекция/добавление списка градации "дороговизны"          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 17.11.1999 ──┘
PROCEDURE Price  
PARAMETERS new
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

PRIVATE ex, c_char, c_name, c_bound, r_sav, v_sav, is_last
ex = 1
IF new
  c_char  = SPACE(1)
  c_name  = SPACE(24)
  c_bound = 0
  is_last = .F.  
ELSE
  c_char  = CHAR_C
  c_name  = NAME_C
  c_bound = IIF(USD1="И выше  ",99999.99,VAL(ALLTRIM(USD1)))
  is_last = USD1 = "И выше  "
    
ENDIF

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 52, "Заполните бланк!"
* ┌───────────────────────────────────────────────┐
* │ Символ X  Название XXXXXXXXXXXXXXXXXXXXXXXX   │
* │                                               │
* │            Значение (USD) 99999.99            │
* │                                               │
* │      < OK Ctrl-W > < Отказаться Esc >         │
* └───────────────────────────────────────────────┘

*      Ввод полей бланка
*

@ 2,5 SAY "Символ" GET c_char   
@ 2,15 SAY "Название " GET c_name 
@ 4,16 SAY "Значение (USD)" GET c_bound PICTURE "99999.99" WHEN .NOT. is_last
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE VALID Test_Blank()

IF ex = 1
  *
  * Отрабатываем бланк
  * 
  IF new 
    APPEND BLANK 
  ENDIF
  REPLACE CHAR_C WITH c_char,;
          NAME_C WITH c_name,;
          USD1   WITH IIF(c_bound = 99999.99,"И выше", STR(c_bound,8,2))
  r_sav = RECNO()             
  v_sav = ""
  SCAN
    REPLACE USD0 WITH v_sav
    v_sav = VAL(ALLTRIM(USD1))+0.01
    v_sav = STR(v_sav,8,2)
  ENDSCAN
  GO r_sav  
ENDIF

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Test_Blank   Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Проверка выхода из бланка                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 19.11.1999 ──┘
PROCEDURE Test_Blank
PRIVATE r_sav, s_sav

IF ex = 1
  s_sav = IIF(new,0,RECNO())
  IF c_bound => 99999.99 .AND. .NOT. is_last
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Нет такой цены!!!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF  
  SELECT H_PRICE
  r_sav = RECNO()
  SCAN FOR RECNO() # s_sav
    IF c_char == H_PRICE.CHAR_C 
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Такой символ уже есть! Выберите какой-нибудь другой..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      IF BETWEEN(r_sav,1,RECCOUNT()) 
        GO r_sav
      ENDIF    
      RETURN .F.
    ENDIF
    IF c_name == H_PRICE.NAME_C 
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Такое название уже есть! Выберите какое-нибудь другое..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss" 
      IF BETWEEN(r_sav,1,RECCOUNT()) 
        GO r_sav
      ENDIF    
      RETURN .F.
    ENDIF
    IF STR(c_bound,8,2) = H_PRICE.USD1
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Такое значение уже есть!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss" 
      IF BETWEEN(r_sav,1,RECCOUNT()) 
        GO r_sav
      ENDIF    
      RETURN .F.
    ENDIF
  ENDSCAN
  IF BETWEEN(r_sav,1,RECCOUNT()) 
    GO r_sav
  ENDIF    
ENDIF
RETURN .T.
