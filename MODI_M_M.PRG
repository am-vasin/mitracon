*╔══════════════════════════════════════════════════════════════════════╗
*║                                                                      ║
*║           Программа создания основного меню приложения.              ║
*║                                                                      ║
*╚══════════════════════════════════════════════════════════════════════╝
PROCEDURE Modi_M_M
PARAMETERS m_file, a_level, sch_num
*
*   m_file  - имя ( и, возможно, путь ) файла описания меню;
*   a_level - уровень доступа;
*   sch_num - цветовая схема.
*
PRIVATE sch_w
PRIVATE pad_sav, bar_, popup_n
pad_sav  = ""
bar_     = 0
popup_n  = ""

*
*   Устанавливаем цветовую схему
*
sch_w = IIF(TYPE("sch_num") = "N", sch_num, 1)

*
*   Открываем файл описания меню и прячем пункты согласно санкциям
*
USE (m_file) ORDER TAG LEVELS
SET FILTER TO .NOT. H_FOR_ALL

*
*  Формируем меню...
*
DEFINE MENU Menu_T BAR MARK "√" COLOR SCHEME (sch_w)
PRIVATE pl1, bl2, bl3, mn2, mn3
SCAN
  DO CASE

  CASE EMPTY(LEVEL2)                 && Пункт первого уровня
    pl1 = "PAD1_"+ALLTRIM(STR(LEVEL1))
    DEFINE PAD (pl1) OF Menu_T PROMPT ALLTRIM(P_PROMPT)+" "

    IF EMPTY(COMM_STR)                   && Имеет место подменю
      mn2 = "POP"+ALLTRIM(STR(LEVEL1))
      ON PAD (pl1) OF Menu_T ACTIVATE POPUP (mn2)
      DEFINE POPUP (mn2) MARGIN MARK "√" RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl2 = 1
    ELSE                                && Установить/снять пометку
      SET MARK OF PAD (pl1) OF Menu_T TO SUBSTR(ACC_LABELS,a_level,1) # " " 
      ON SELECTION PAD (pl1) OF Menu_T DO Mark_P
    ENDIF

  CASE EMPTY(LEVEL3)                 && Пункт второго уровня
    DEFINE BAR bl2 OF (mn2) PROMPT ALLTRIM(P_PROMPT)+" "

    IF EMPTY(COMM_STR)                   && Имеет место подменю
      mn3 = "POP"+ALLTRIM(STR(LEVEL1))+"_"+ALLTRIM(STR(LEVEL2))
      ON BAR bl2 OF (mn2) ACTIVATE POPUP (mn3)
      DEFINE POPUP (mn3) MARGIN MARK "√" RELATIVE SHADOW COLOR SCHEME (sch_w)
      bl3 = 1
    ELSE                                && Установить/снять пометку
      SET MARK OF BAR bl2 OF (mn2) TO SUBSTR(ACC_LABELS,a_level,1) # " "
      ON SELECTION BAR bl2 OF (mn2) DO Mark_B
    ENDIF
    bl2 = bl2+1

  OTHERWISE    && Пункт третьего уровня и никаких подменю!
    DEFINE BAR bl3 OF (mn3) PROMPT ALLTRIM(P_PROMPT)+" "
    SET MARK OF BAR bl3 OF (mn3) TO SUBSTR(ACC_LABELS,a_level,1) # " "
    ON SELECTION BAR bl3 OF (mn3) DO Mark_B
    bl3 = bl3+1
  ENDCASE

ENDSCAN

*
*   Интерактивная установка пометок в меню
*
SET ESCAPE OFF
DO WHILE .T.
  IF EMPTY(bar_) .AND. EMPTY(popup_n) .AND. EMPTY(pad_sav)
    ACTIVATE MENU Menu_T
  ELSE
    IF EMPTY(bar_)
      ACTIVATE MENU Menu_T PAD (pad_sav)
    ELSE
      ACTIVATE POPUP (popup_n) BAR bar_
    ENDIF
  ENDIF
  IF MOD(READKEY(),256) = 12
    EXIT
  ENDIF
  pad_sav = PAD()
ENDDO

*
*  Установка пометок в базе данных по пометкам меню: первый этап
*                пометки для термальных пунктов
*
SCAN
  DO CASE

  CASE EMPTY(LEVEL2)                 && Пункт первого уровня
    pl1 = "PAD1_"+ALLTRIM(STR(LEVEL1))
    DEFINE PAD (pl1) OF Menu_T PROMPT ALLTRIM(P_PROMPT)+" "

    IF EMPTY(COMM_STR)                   && Имеет место подменю
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1," ")
      mn2 = "POP"+ALLTRIM(STR(LEVEL1))
      bl2 = 1
    ELSE                                && Установить/снять пометку
      bar_ = IIF(MRKPAD("Menu_T",pl1), "*", " ")
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
    ENDIF

  CASE EMPTY(LEVEL3)                 && Пункт второго уровня
    IF EMPTY(COMM_STR)                   && Имеет место подменю
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1," ")
      mn3 = "POP"+ALLTRIM(STR(LEVEL1))+"_"+ALLTRIM(STR(LEVEL2))
      bl3 = 1
    ELSE                                && Установить/снять пометку
      bar_ = IIF(MRKBAR(mn2,bl2), "*", " ")
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
    ENDIF
    bl2 = bl2+1

  OTHERWISE    && Пункт третьего уровня и никаких подменю!
    bar_ = IIF(MRKBAR(mn3,bl3), "*", " ")
    REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,bar_)
    bl3 = bl3+1
  ENDCASE

ENDSCAN

*
*  Установка пометок в базе данных по пометкам меню: второй этап
*                пометки для нетермальных пунктов
*

SET FILTER TO IIF(a_level=0, .T., EMPTY(SUBSTR(ACC_LABELS,a_level,1)))  ;
              .AND. .NOT. H_FOR_ALL

SET ORDER TO TAG LEVELS DESCENDING
PRIVATE lv1, lv2, lv3, ok
STORE 0 TO lv1, lv2, lv3

SCAN

  ok = .F.
  IF EMPTY(COMM_STR)        && не терм, надо разобраться
    IF EMPTY(LEVEL2)       && первый уровень
      IF .NOT. EMPTY(lv2) .AND. EMPTY(lv3)
        IF LEVEL1 = lv1
          ok = .T.
        ENDIF
      ENDIF
    ELSE                   && второй уровень
      IF .NOT. EMPTY(lv3)
        IF LEVEL1 = lv1 .AND. LEVEL2 = lv2
          ok = .T.
        ENDIF
      ENDIF
    ENDIF
    IF .NOT. ok
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS,a_level,1,"*")
      LOOP
    ENDIF
  ENDIF
  lv1 = LEVEL1
  lv2 = LEVEL2
  lv3 = LEVEL3
ENDSCAN

USE
RETURN  

PROCEDURE Mark_P

SET MARK OF PAD (PAD()) OF Menu_T TO .NOT. MRKPAD("Menu_T",PAD())
*popup_n = ""
popup_n = POPUP()
pad_sav = ALLTRIM(PROMPT())
bar_ = BAR()

RETURN

PROCEDURE Mark_B

SET MARK OF BAR BAR() OF (POPUP()) TO .NOT.MRKBAR(POPUP(),BAR())
popup_n = POPUP()
*pad_sav = ""
pad_sav = ALLTRIM(PROMPT())
bar_ = BAR()

RETURN
