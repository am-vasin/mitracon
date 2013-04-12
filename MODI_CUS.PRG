*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа коррекции/создания клиента.                    ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Modi_Cus
PARAMETERS mode

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

PRIVATE ex, nm_w, sw_i, sw_a, cd, rc_sav, tmpType, tmpINN, tmpKPP
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
nm_w = IIF(mode = "A", SPACE(FSIZE("CUS_NAME")), CUS_NAME)
sw_i = IIF(mode = "A",.T.,.F.)
sw_a = .F.
m.tmpINN = ""
m.tmpKPP = ""
IF mode = "A"
  tmpType = 0
  DO Get_INN WITH 0, m.tmpINN, m.tmpKPP
ELSE
  DO Get_INN WITH CUS_CODE, m.tmpINN, m.tmpKPP
  DO CASE
  CASE MARK1 = "1"
    tmpType = 2
  CASE MARK1 = "2"
    tmpType = 3
  OTHERWISE
    tmpType = 1
  ENDCASE
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
*
*┌────────────────────────────────────────────────────────┐
*│ ┌─ Имя клиента ──────────────────────────────────────┐ │
*│ │ ....:....!....:....!....:....!....:....!....:....! │ │
*│ └────────────────────────────────────────────────────┘ │
*│        ИНН ....:....!..    КПП ....:....!....:.        │
*│                                                        │
*│      [ ] Карточка клиента    [ ] Атрибуты клиента      │
*│                                                        │
*│     ( ) Юрид.  ( ) Предприниматель  ( ) Частн.лицо     │
*│                                                        │
*│            < OK Ctrl-W > < Отказаться Esc >            │
*└────────────────────────────────────────────────────────┘

DO Prp_Nav_2
DO D_Win_N WITH 13, 60, "Клиент"
DO Sun_Bord WITH 2, 3, 4, WCOLS()-4
@ 2, 5 SAY " Имя клиента "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  5 GET nm_w
@  5, 10 SAY "ИНН" GET m.tmpINN
@  5, 30 SAY "КПП" GET m.tmpKPP
@  7,  8 GET sw_i PICTURE "@*C Карточка клиента" VALID Tst_Inf()
@  7, 32 GET sw_a PICTURE "@*C Атрибуты клиента"
@  9,  7 GET tmpType PICTURE "@*RH Юрид. ;Предприниматель ;Частн. лицо"
@ 11, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Name()

tmpType = TRANSFORM(tmpType-1, "@Z 9")
IF ex = 1
  IF mode = "A"
    IF (sys_char = "A" .OR. sale_mode = "Mitin")   && основная площадка: добавляем запись

      cd = Doc_Num("CUS_CODE  ", "", arm, DATE())
      SELECT ACCOUNT
      APPEND BLANK
      REPLACE SUBSYS     WITH IIF(EMPTY(pl_wk), sys_char, pl_wk),  ;
              CUS_CODE   WITH cd,        ;
              CUS_NAME   WITH nm_w,      ;
              DATE_CORR  WITH DATE(),    ;
              MARK1      WITH tmpType,   ;
              WHO_CORR   WITH user,      ;
              WHERE_CORR WITH arm,       ;
              DATE_ON    WITH DATE(),    ;
              WHO        WITH user,      ;
              WHERE      WITH arm

    ELSE     && Не основная площадка: используем заготовку

      rc_sav = RECNO()
      SET ORDER TO TAG EMP_NAME
      GO TOP
      IF EOF() .OR. BOF()
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Вы исчерпали все заготовки!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        sw_i = .F.
        sw_a = .F.
      ELSE
        REPLACE SUBSYS     WITH sys_char,  ;
                CUS_NAME   WITH nm_w,      ;
                WHO_CORR   WITH user,      ;
                WHERE_CORR WITH arm,       ;
                DATE_CORR  WITH DATE(),    ;
                MARK1      WITH tmpType,   ;
                DATE_ON    WITH DATE(),    ;
                WHO        WITH user,      ;
                WHERE      WITH arm
        rc_sav = RECNO()
      ENDIF
      SET ORDER TO TAG CUS_NAME
      IF rc_sav > 0 .AND. rc_sav <= RECCOUNT()
        GO rc_sav
      ENDIF
    ENDIF

  ELSE

    REPLACE CUS_NAME   WITH nm_w, ;
            WHO_CORR   WITH user, ;
            MARK1      WITH tmpType,   ;
            WHERE_CORR WITH arm,  ;
            DATE_CORR  WITH DATE()

  ENDIF
  DO Put_INN WITH CUS_CODE, m.tmpINN, m.tmpKPP
  IF sw_i
    DO Cli_Info WITH "", .T.
  ENDIF
  IF sw_a
    DO Cus_Bil WITH ACCOUNT.CUS_CODE, ACCOUNT.CUS_NAME
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Проверка имени клиента.                            ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Tst_Name

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Пустое имя клиента? Так не пойдет!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF .NOT. INLIST(tmpType, 1, 2, 3)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы так и не указали тип клиента!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN Tst_INN(IIF(mode = "A", 0, CUS_CODE), m.tmpINN )

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║          Обязательный ввод страны при заведении нового клиента.        ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 26.03.2003 ═╝
PROCEDURE Tst_Inf

IF mode = "A" 
  sw_i = .T.
ENDIF

SHOW GET sw_i

RETURN
