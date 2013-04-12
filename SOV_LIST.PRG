*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа подготовки данных для Л. А. и И. Е.            ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Sov_List

*
*  Переменные состояния для навигвции
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

PRIVATE ex, dat_1, dat_2, dat_1b, dat_2b, nm_w, nm_d, q, p, cnt     &&
                     &&   Объявляем и заполняем поля бланка
USE (base_path+"PARMS")
dat_1b = DAT_START
dat_2b = DATE()
dat_1  = MAX(dat_1b, dat_2b-91)
dat_2  = DATE()
ex   = 1             &&
nm_d = SPACE(40)     &&
nm_w = SPACE(40)     &&
IF File_O(prmo_path+"SOV_LIST.MEM")
  RESTORE FROM (prmo_path+"SOV_LIST.MEM") ADDITIVE
ENDIF
USE
*------------------------------------------------------------------------
*┌───────────────────────────────────────────────────────────────┐
*│    ┌─ Расход ─────────────────────────────────────────────┐   │
*│    │            C  ДД/ММ/ГГГГ  по  ДД/ММ/ГГГГ             │   │
*│    └──────────────────────────────────────────────────────┘   │
*│ Где список позиций   ....:....1....:....2....:....3....:....4 │
*│                                                               │
*│ Куда пишем результат ....:....1....:....2....:....3....:....4 │
*│                                                               │
*│              < OK Ctrl-W > < Отказаться Esc >                 │
*└───────────────────────────────────────────────────────────────┘
PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 11, 67, "Подборка расхода для Л. А. и И. Е."
DO Sun_Bord WITH 2, 6, 4, 61
@ 2, 8 SAY " Расход "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 19 SAY "C              по"
@ 3, 22 GET dat_1 PICTURE "@D"
@ 3, 38 GET dat_2 PICTURE "@D"
@ 5, 3 SAY "Где список позиций  " GET nm_d PICTURE "@K"
@ 7, 3 SAY "Куда пишем результат" GET nm_w PICTURE "@K"
@ 9, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Inp()

POP KEY
RELEASE WINDOW (win_name)

IF ex = 2
  RETURN
ENDIF
SAVE ALL LIKE nm_* TO (prmo_path+"SOV_LIST.MEM")
*--------------------------------------------------------------------------
nm_w = ALLTRIM(nm_w)
SET EXCLUSIVE ON
CREATE DBF (nm_w)   ;
    ( CODE   N( 7), ;
      PREFIX C( 4), ;
      NAME   C(35), ;
      PRODUCER C(8), ;
      PRICE  N(10,4), ;
      QNT    N( 6),  ;
      SUM    N(12,4),;
      BRON   N( 6),  ;
      SALED  N( 6),  ;
      MARK   C( 1)   )
USE (nm_w) ALIAS RESULT
SET EXCLUSIVE OFF


SELECT 0
USE (nm_d) ORDER TAG CODE ALIAS LIST

SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME
SET RELATION TO CODE INTO LIST
DO Wt_Mess WITH "Подготовка данных..."

cnt = 0
SCAN FOR FOUND("LIST")

  SELECT RESULT
  APPEND BLANK
  REPLACE CODE   WITH STOCK.CODE,  ;
          PREFIX WITH STOCK.PREFIX,;
          NAME   WITH STOCK.NAME,  ;
          PRODUCER WITH STOCK.PRODUCER, ;
          QNT    WITH STOCK.QNT,   ;
          PRICE  WITH STOCK.PRICE, ;
          SALED  WITH 0,           ;
          BRON   WITH STOCK.QNT_SAVE+STOCK.QNT_REZ

  SELECT STOCK
  cnt = cnt+1
  @ WROWS()-1, 0 SAY PADC("Номенклатура "+ALLTRIM(STR(cnt,6)), WCOLS())
ENDSCAN

USE (base_path+"SALE") ORDER TAG CODE

SELECT RESULT
cnt = 0
SCAN FOR SEEK(CODE, "SALE")
  q = 0
  p = 0
  SELECT SALE
  SCAN REST WHILE CODE = RESULT.CODE FOR PRICE_R # 0
    IF BETWEEN(DOC_DATE, dat_1, dat_2)
      q = q+QNT
      p = p+PRICE_D*QNT
    ENDIF
  ENDSCAN
  SELECT RESULT
  REPLACE SALED WITH q, SUM WITH p
  cnt = cnt+1
  @ WROWS()-1, 0 SAY PADC("Расход "+ALLTRIM(STR(cnt,6)), WCOLS())
ENDSCAN

CLOSE DATABASES

DO Wt_Mess

PRIVATE l00, c00

l00 = FLOOR((WROWS("")-3)/2)
c00 = FLOOR((WCOLS("")-10)/2)

DEFINE WINDOW www FROM l00, c00 TO l00+2, c00+9 NONE SHADOW COLOR SCHEME 5
ACTIVATE WINDOW www

@ 1, 3 SAY "ВСЕ!"

= INKEY(3, "M")
RELEASE WINDOW www

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа проверки выхода из бланка.                     ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Tst_Inp
PRIVATE mss, i

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF dat_1 > dat_2
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Начальная дата больше конечной: так не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_1 < dat_1b
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Неверная начальная дата: данные о расходе хранятся только с "+TRANSFORM(dat_1b,"@E")+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_2 > dat_2b
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Неверная конечная дата: сегодня еще только "+TRANSFORM(dat_2b,"@E")+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(nm_w)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Как-то не очень понятно, куда запишем результат? Давайте еще раз уточним!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
PRIVATE mmm, i, sw
mmm = ALLTRIM(nm_d)
IF .NOT. "." $ mmm
  mmm = mmm+".DBF"
ENDIF
IF .NOT. File_O(mmm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Файл "+ALLTRIM(mmm)+" не существует!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
USE (mmm)
i = 1
sw = .F.
DO WHILE .NOT. EMPTY(TAG(i))
  IF TAG(i) = "CODE"
    sw = .T.
    EXIT
  ENDIF
  i = i+1
ENDDO
USE
IF .NOT. sw
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Файл позиций не упорядочен по кодам!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF File_O(ALLTRIM(nm_w))
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Файл "+ALLTRIM(nm_w)+" уже есть! Что будем делать?"
  mss(3) = CHR(0)+"   [ Удалим ] [ Сменим ] [ Откажемся ]"
  mss(4) = ""
  i = Out_Mess(7, "mss")
  IF i = 1
    DELETE FILE (ALLTRIM(nm_w))
    RETURN .T.
  ENDIF
  IF i = 3
    ex = 2
    RETURN .T.
  ENDIF
  RETURN .F.
ENDIF

RETURN .T.
