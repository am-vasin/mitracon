*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Программа работы со списками.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Lists

PRIVATE mrkl, what_do, mv, i, price_age, mode_fil, mode_ord, mode_s_a
PRIVATE mode_d_i, mode_d_o, mode_d_r, mode_d_b, mode_firm, mode_tch, dn_buhg, mode_due, mode_pr0
PRIVATE opt_part, round_1, tmpRec, tmpArm, r_sav, r_sav1, sumBound

= CAPSLOCK(.T.)
= INSMODE(.F.)

opt_part = Get_Sys("OPT_PART")
IF EMPTY(opt_part)
  opt_part = 0.8
ELSE
  opt_part = VAL(ALLTRIM(opt_part))
ENDIF

round_1 = .F.   && Разовая отмена округления до нормы продажи

IF File_O(base_path+"BND.MEM")
  RESTORE FROM BND ADDITIVE
  IF TYPE("bound_l") = "N"
    bound_l = kurs_b*bound_l
  ENDIF
  IF TYPE("bound_r") = "N"
    bound_r = kurs_b*bound_r
  ENDIF
ENDIF

*max_ord = 0
*IF File_O(base_path+"MAX_ORD.MEM")
*  RESTORE FROM (base_path+"MAX_ORD.MEM") ADDITIVE
*ENDIF

mode_d_i  = .F.
mode_s_a  = .T.
mode_d_o  = .F.
mode_d_r  = .F.
mode_d_b  = .F.
mode_defA = .T.
mode_defD = .F.
mode_defO = .T.
mode_defS = .T.
mode_firm = .F.
mode_fil  = .F.
mode_rnd  = 1
mode_part = .05
mode_pri  = 1.
mode_pr0  = .F.
mode_tch = .F.
mode_ord = "B"
mode_spec = IIF(acc_level < 3, .T., .F.)
mode_due = .F.
IF File_O(prmo_path+"MODE.MEM")
  RESTORE FROM (prmo_path+"MODE") ADDITIVE
  IF TYPE("mode_rnd") = "L"
    mode_rnd = IIF(mode_rnd, 1, 2)
  ENDIF
ENDIF
mode_rnd = 2      &&  Включен автоматический режим
IF mode_spec .AND. acc_level > 2
  mode_spec = .F.
ENDIF

price_age = 0
sumBound  = 0

IF File_O(base_path+"P_AGE.MEM")
  RESTORE FROM (base_path+"P_AGE.MEM") ADDITIVE
ENDIF

mrkl = ""

*
*    PAD - меню первого уровня
*
DEFINE MENU List_M IN SCREEN COLOR SCHEME 4
DEFINE PAD Titl OF List_M PROMPT "Заголовок"
DEFINE PAD Work OF List_M PROMPT "Работа"
DEFINE PAD Docs OF List_M PROMPT "Документы"
DEFINE PAD Opt  OF List_M PROMPT "Настройки"
DEFINE PAD Exit OF List_M PROMPT "Выход" KEY Ctrl-A, "Esc"

ON PAD Titl OF List_M ACTIVATE POPUP Titl
ON PAD Work OF List_M ACTIVATE POPUP Work
ON PAD Docs OF List_M ACTIVATE POPUP Docs
ON PAD Opt  OF List_M ACTIVATE POPUP Opt
ON SELECTION PAD Exit OF List_M Do Ex_Proc WITH "List_M"

*
*    POPUP - меню второго уровня
*
DEFINE POPUP Titl MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Titl PROMPT "Корректировать " KEY Ctrl-A, "Tab"
DEFINE BAR  2 OF Titl PROMPT "Добавить " KEY Ctrl-A, "Ins"
DEFINE BAR  3 OF Titl PROMPT "Удалить " KEY Ctrl-A, "Del"
DEFINE BAR  4 OF Titl PROMPT "Флажки " KEY Ctrl-A, "+"
DEFINE BAR  5 OF Titl PROMPT "В общую область "
DEFINE BAR  6 OF Titl PROMPT "Из общей области "
DEFINE BAR 14 OF Titl PROMPT "Доступ к рабочей области"
DEFINE BAR 12 OF Titl PROMPT "Передать список "
DEFINE BAR 13 OF Titl PROMPT "Получить список "
DEFINE BAR  7 OF Titl PROMPT "Установить станд." KEY Ctrl-A, "Ctrl-S"
DEFINE BAR  8 OF Titl PROMPT "Добавить станд." KEY Ctrl-A, "Ctrl-A"
DEFINE BAR  9 OF Titl PROMPT "Примечание" KEY Ctrl-A, "F5"
*DEFINE BAR 10 OF Titl PROMPT "Адрес" KEY Ctrl-A, "Ctrl-F5"
DEFINE BAR 11 OF Titl PROMPT "Вес" KEY Ctrl-A, "Shift-F5"
ON SELECTION BAR 1 OF Titl DO Ch_Mode WITH "List_M","corrl"
ON SELECTION BAR 2 OF Titl DO Ch_Mode WITH "List_M","addl"
ON SELECTION BAR 3 OF Titl DO Ch_Mode WITH "List_M","del_list"
ON SELECTION BAR 4 OF Titl DO Ch_Mode WITH "List_M","set_lfs"
ON SELECTION BAR 5 OF Titl DO Ch_Mode WITH "List_M","to common"
ON SELECTION BAR 6 OF Titl DO Ch_Mode WITH "List_M","from common"
ON SELECTION BAR 7 OF Titl DO Ch_Mode WITH "List_M","st_corrl"
ON SELECTION BAR 8 OF Titl DO Ch_Mode WITH "List_M","st_addl"
ON SELECTION BAR 9 OF Titl DO Ch_Mode WITH "List_M","note"
*ON SELECTION BAR 10 OF Titl DO Ch_Mode WITH "List_M","address"
ON SELECTION BAR 11 OF Titl DO Ch_Mode WITH "List_M","weight"
ON SELECTION BAR 12 OF Titl DO Ch_Mode WITH "List_M","SendList"
ON SELECTION BAR 13 OF Titl DO Ch_Mode WITH "List_M","GetList"
ON SELECTION BAR 14 OF Titl DO Ch_Mode WITH "List_M","Let_List"

DEFINE POPUP Work MARGIN RELATIVE SHADOW COLOR SCHEME 4
i = 1
DEFINE BAR  i OF Work PROMPT "Коррекция " KEY Ctrl-A, "Enter"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","edit"
i = i+1
DEFINE BAR  i OF Work PROMPT "Список в альтернативной валюте " KEY Ctrl-A, "Ctrl+Enter"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","view_alt"
i = i+1
DEFINE BAR  i OF Work PROMPT "Продажа" KEY Ctrl-A, "Shift-Tab"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","sale"
i = i+1
DEFINE BAR  i OF Work PROMPT "Вынести отмеченные для продажи"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","to_sale"
i = i+1
DEFINE BAR  i OF Work PROMPT "Требование в бухгалтерию" && KEY Ctrl-A, "Tab"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","to_buhg"
i = i+1
DEFINE BAR  i OF Work PROMPT "Доставка" && KEY Ctrl-A, "Tab"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","Delivery"
i = i+1
DEFINE BAR  i OF Work PROMPT "Печать требований в бухгалтерию" && KEY Ctrl-A, "Tab"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","prn_to_buhg"
i = i+1
DEFINE BAR  i OF Work PROMPT "На реализацию" && KEY Ctrl-A, "Tab"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","to_ryn"
*i = i+1
*DEFINE BAR  i OF Work PROMPT "Собрать к... " KEY Ctrl-A, "Ctrl-F2"
*ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","to prep"
i = i+1
DEFINE BAR  i OF Work PROMPT "Поиск " KEY Ctrl-A, "F7"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","seek"
i = i+1
DEFINE BAR  i OF Work PROMPT "Поиск счета" KEY Ctrl-A, "Ctrl-F7"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","seek_b"
i = i+1
DEFINE BAR  i OF Work PROMPT "Поиск списка" KEY Ctrl-A, "Shift-F7"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","seek_l"
i = i+1
DEFINE BAR  i OF Work PROMPT "Поиск по номеру интернет-заказа" && KEY Ctrl-A, "Shift-F7"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","seek_I"
i = i+1
DEFINE BAR  i OF Work PROMPT "Поиск позиции" KEY Ctrl-A, "Ctrl-F"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","seek_pos"
i = i+1
DEFINE BAR  i OF Work PROMPT "Повтор поиска позиции" KEY Ctrl-A, "Ctrl-G"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","seek_again"
i = i+1
DEFINE BAR  i OF Work PROMPT "Поиск по примечанию к позиции"   && KEY Ctrl-A, "Ctrl-F7"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","seek_nt"
i = i+1
DEFINE BAR  i OF Work PROMPT "Фильтр" KEY Ctrl-A, "Ctrl-F6"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","filter"
i = i+1
DEFINE BAR  i OF Work PROMPT "Курс продажи для списка" KEY Ctrl-A, "F2"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","course_l"
i = i+1
DEFINE BAR  i OF Work PROMPT "Сумма с НСП для списка" KEY Ctrl-A, "F4"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","NSP"
i = i+1
DEFINE BAR  i OF Work PROMPT "Разность списков" && KEY Ctrl-A, "F2"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","delta_lst"
*i = i+1
*DEFINE BAR  i OF Work PROMPT "Разбиение "
*i = i+1
*DEFINE BAR  i OF Work PROMPT "Слияние "
i = i+1
IF acc_Level < 3
  DEFINE BAR  i OF Work PROMPT "Скидка "
  ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","discount"
  i = i+1
ENDIF
DEFINE BAR  i OF Work PROMPT "Расчет скидки по накладной" KEY Ctrl-A, "-"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","dis_inv"
i = i+1
DEFINE BAR  i OF Work PROMPT "Пометка " KEY Ctrl-A, "F3"
ON SELECTION BAR i OF Work DO Mrk_Lst
i = i+1
DEFINE BAR  i OF Work PROMPT "Утвердить " KEY Ctrl-A, "F6"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","let_sale"
*i = i+1
*DEFINE BAR  i OF Work PROMPT "Ввод заявки "
*ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","get_ord"
i = i+1
DEFINE BAR  i OF Work PROMPT "Ввод произвольной заявки"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","get_o_1"
i = i+1
DEFINE BAR  i OF Work PROMPT "Ввод заявки от филиала"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","get_f_ord"
i = i+1
DEFINE BAR  i OF Work PROMPT "Ввод заявки от филиала (новый формат)"
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","rd_ord"
i = i+1
DEFINE BAR  i OF Work PROMPT "Копировать список "
ON SELECTION BAR i OF Work DO Ch_Mode WITH "List_M","copy"
i = i+1
*DEFINE BAR  i OF Work PROMPT "Обновление "


DEFINE POPUP Docs MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Docs PROMPT "Список "
DEFINE BAR  2 OF Docs PROMPT "Список с нормой отпуска"
DEFINE BAR  4 OF Docs PROMPT "Заявка на склад "
DEFINE BAR 28 OF Docs PROMPT "Заявки на поставку "
DEFINE BAR 24 OF Docs PROMPT "Разбронировать список "
DEFINE BAR  5 OF Docs PROMPT "Гистограмма загрузки склада "
DEFINE BAR  6 OF Docs PROMPT "Требование на склад "
DEFINE BAR  7 OF Docs PROMPT "Утверждение заявок на склад " KEY Ctrl-A, "F1"
DEFINE BAR  8 OF Docs PROMPT "Счет "
DEFINE BAR  9 OF Docs PROMPT "Счет без списка"
DEFINE BAR 10 OF Docs PROMPT "Накладная "
DEFINE BAR 29 OF Docs PROMPT "Акт по организации доставки "
DEFINE BAR 17 OF Docs PROMPT "Электронная накладная для STORE"
DEFINE BAR 11 OF Docs PROMPT "Товарный чек "
DEFINE BAR 24 OF Docs PROMPT "Разбронировать список "
DEFINE BAR 26 OF Docs PROMPT "Список в формате Excel "
DEFINE BAR 12 OF Docs PROMPT "Счет-фактура "
DEFINE BAR 13 OF Docs PROMPT "Приходный ордер "
DEFINE BAR 25 OF Docs PROMPT "Заказ для УРМ "
DEFINE BAR 27 OF Docs PROMPT "Заказ для УРМ (нов. формат)"
*DEFINE BAR 14 OF Docs PROMPT "Чек" KEY Ctrl-A, "F8"
DEFINE BAR 15 OF Docs PROMPT "Информация " KEY Ctrl-A, "F9"
DEFINE BAR 16 OF Docs PROMPT "Список с заменами"
DEFINE BAR  3 OF Docs PROMPT "Дорогие позиции"
DEFINE BAR 18 OF Docs PROMPT "Печать доставки" KEY Ctrl-D
DEFINE BAR 19 OF Docs PROMPT "Список с доставками"
DEFINE BAR 20 OF Docs PROMPT 'Список "Отказы" в DBF-формате'
DEFINE BAR 21 OF Docs PROMPT 'Печать списка "Отказы"'
DEFINE BAR 22 OF Docs PROMPT 'Список "Дефицит" в DBF-формате'
DEFINE BAR 23 OF Docs PROMPT 'Печать списка "Дефицит"'
ON SELECTION BAR  1 OF Docs DO Ch_Mode WITH "List_M","p_list"
ON SELECTION BAR  2 OF Docs DO Ch_Mode WITH "List_M","p1_list"
ON SELECTION BAR  3 OF Docs DO Ch_Mode WITH "List_M","p2_list"
ON SELECTION BAR  4 OF Docs DO Ch_Mode WITH "List_M","sto_ord"
ON SELECTION BAR  5 OF Docs DO Ch_Mode WITH "List_M","ord_hist"
ON SELECTION BAR  6 OF Docs DO Ch_Mode WITH "List_M","sto_req"
ON SELECTION BAR  7 OF Docs DO Ch_Mode WITH "List_M","mark_ord"
ON SELECTION BAR  8 OF Docs DO Ch_Mode WITH "List_M","bill"
ON SELECTION BAR  9 OF Docs DO Ch_Mode WITH "List_M","z_bill"
ON SELECTION BAR 10 OF Docs DO Ch_Mode WITH "List_M","invoice"
ON SELECTION BAR 17 OF Docs DO Ch_Mode WITH "List_M","inv_sto"
ON SELECTION BAR 11 OF Docs DO Ch_Mode WITH "List_M","inv0"
ON SELECTION BAR 12 OF Docs DO Ch_Mode WITH "List_M","bill_f"
ON SELECTION BAR 13 OF Docs DO Ch_Mode WITH "List_M","order"
*ON SELECTION BAR 14 OF Docs DO Ch_Mode WITH "List_M","check"
ON SELECTION BAR 15 OF Docs DO Ch_Mode WITH "List_M","info"
ON SELECTION BAR 16 OF Docs DO Ch_Mode WITH "List_M","answer"
ON SELECTION BAR 18 OF Docs DO Ch_Mode WITH "List_M","prn_dlv"
ON SELECTION BAR 19 OF Docs DO Ch_Mode WITH "List_M","lst_dlv"
ON SELECTION BAR 20 OF Docs DO Ch_Mode WITH "List_M","dbf_ref"
ON SELECTION BAR 21 OF Docs DO Ch_Mode WITH "List_M","lst_ref"
ON SELECTION BAR 22 OF Docs DO Ch_Mode WITH "List_M","dbf_def"
ON SELECTION BAR 23 OF Docs DO Ch_Mode WITH "List_M","lst_def"
ON SELECTION BAR 24 OF Docs DO Ch_Mode WITH "List_M","bron_off"
ON SELECTION BAR 25 OF Docs DO Ch_Mode WITH "List_M","URM_Ord"
ON SELECTION BAR 26 OF Docs DO Ch_Mode WITH "List_M","XLS_List"
ON SELECTION BAR 27 OF Docs DO Ch_Mode WITH "List_M","URM_New"
ON SELECTION BAR 28 OF Docs DO Ch_Mode WITH "List_M","sup_ord"
ON SELECTION BAR 29 OF Docs DO Ch_Mode WITH "List_M","act_d"


DEFINE POPUP Opt  MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR  1 OF Opt PROMPT "Режим "
ON SELECTION BAR 1 OF Opt DO Ch_Mode WITH "List_M","set_mode"
DEFINE BAR  2 OF Opt PROMPT "Параметры округления до упаковки"
ON SELECTION BAR 2 OF Opt DO Ch_Mode WITH "List_M","pack_mode"
DEFINE BAR  3 OF Opt PROMPT "Тип очереди для заявок"
ON SELECTION BAR 3 OF Opt DO Ch_Mode WITH "List_M","queue_mode"

HIDE MENU Main_M
DO Entry_P WITH "", "List_M"

USE (base_path+"RYN_L") ORDER C_CODE ALIAS FILIAL

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS LST_FRM

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_ARM
SEEK arm
REPLACE REST WHILE LST_ARM = arm FILTER WITH ""
GO TOP
SET FILTER TO EMPTY(FILTER)

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"BILLS") ORDER TAG DOC_NUM

IF File_O(base_path+"DEMAND.DBF")
  SELECT 0
  USE (base_path+"DEMAND.DBF") ORDER TAG WHERE
  IF File_O(base_path+"DEM_LINK.DBF")
    SELECT 0
    USE (base_path+"DEM_LINK.DBF") ORDER TAG LIST
  ENDIF
ENDIF

IF File_O(base_path+"REFUSAL.DBF")
  SELECT 0
  USE (base_path+"REFUSAL.DBF")
ENDIF

DO Use_Dummy
DO Use_Link

SELECT LIST_TIT
SET RELATION TO CLI_CODE INTO ACCOUNT, ;
                BILL_FIRM+BILL_YEAR+BILL_NUM INTO BILLS, ;
                FIRM INTO LST_FRM
GO BOTTOM

*DO Prp_Fltl
*DO Prep_Lst

what_do = "list"
DO WHILE .T.
  DO CASE
  CASE what_do = "list"     && Списки
    DO K_Refr
    DO Prep_W_L
    ON KEY LABEL F3  DO Mrk_lst
    ON KEY LABEL F1  DO Ch_Mode WITH "List_M","mark_ord"
*    ON KEY LABEL F8  DO Ch_Mode WITH "List_M","check"
    ON KEY LABEL F9  DO Ch_Mode WITH "List_M","info"
    ON KEY LABEL F2  DO Ch_Mode WITH "List_M","course_l"
    ON KEY LABEL F4  DO Ch_Mode WITH "List_M","NSP"
    ON KEY LABEL F6  DO Ch_Mode WITH "List_M","let_sale"
    ON KEY LABEL F7  DO Ch_Mode WITH "List_M","seek"
    ON KEY LABEL Shift-F7  DO Ch_Mode WITH "List_M","seek_l"
    ON KEY LABEL F5  DO Ch_Mode WITH "List_M","note"
*    ON KEY LABEL Ctrl-F5 DO Ch_Mode WITH "List_M","address"
    ON KEY LABEL Shift-F5 DO Ch_Mode WITH "List_M","weight"
    ON KEY LABEL Ctrl-F2 DO Ch_Mode WITH "List_M","to prep"
    ON KEY LABEL BackTab DO Ch_Mode WITH "List_M","sale"
    ON KEY LABEL Ctrl-F7 DO Ch_Mode WITH "List_M","seek_b"
    ON KEY LABEL Ctrl-F6 DO Ch_Mode WITH "List_M","filter"
    ON KEY LABEL Ctrl-R DO Rem_Lst WITH LST_NUM
    ON KEY LABEL Ctrl-Enter DO Ch_Mode WITH "List_M","view_alt"
    ON KEY LABEL "+" DO Ch_Mode WITH "List_M","set_lfs"
    ON KEY LABEL "-" DO Ch_Mode WITH "List_M","dis_inv"
    ON KEY LABEL Tab DO Ch_Mode WITH "List_M","corrl"
    ON KEY LABEL Ins DO Ch_Mode WITH "List_M","addl"
    ON KEY LABEL Ctrl-S DO Ch_Mode WITH "List_M","st_corrl"
    ON KEY LABEL Ctrl-F DO Ch_Mode WITH "List_M","seek_pos"
    ON KEY LABEL Ctrl-G DO Ch_Mode WITH "List_M","seek_again"
    ON KEY LABEL Ctrl-A DO Ch_Mode WITH "List_M","st_addl"
    ON KEY LABEL Del DO Ch_Mode WITH "List_M","del_list"
    ON KEY LABEL Enter DO Ch_Mode WITH "List_M","edit"
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL Ctrl+P DO Ch_Mode WITH "List_M","P_Inv1"
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "List_M"
    ON KEY LABEL Ctrl-D DO Ch_Mode WITH "List_M","prn_dlv"
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("Меню F10 ",WCOLS()) COLOR SCHEME 13
    ON KEY LABEL F10 DO F10_Proc WITH "List_M"
    SHOW MENU List_M
    mv = 2-WLROW("Lst_M")
    IF mv # 0
      MOVE WINDOW Lst_M BY mv, 0
      MOVE WINDOW Lst_E BY mv, 0
      MOVE WINDOW Lst_I BY mv, 0
    ENDIF
    IF WEXIST("Client")
      SHOW WINDOW Client
    ENDIF
    ACTIVATE WINDOW Lst_E
*    IF BETWEEN(RECNO(), 1, RECCOUNT())
*      GO RECNO()
*    ENDIF
    BROWSE FIELDS f001 = IIF(STR(LIST_TIT.LST_NUM,6)+"!" $ mrkl, "√"," "):H="", ;
                  LST_FRM.FIRM_CODE:H="",  ;
                  LIST_TIT.LST_NUM:H="", LIST_TIT.CR_DAT:10:H="",  ;
                  LIST_TIT.LST_NAME:H="",      ;
                  f004 = STR(LIST_TIT.R_TOTAL, 13, IIF(LIST_TIT.IS_USD, 4, 2)):H="", ;
                  f003 = IIF(LIST_TIT.IS_USD, "$", "р"):1:H="", ;
                  f_002 = IIF(EMPTY(LIST_TIT.SAL_NUM), " ", "√"):1:H="", ;
                  f_003 = IIF(EMPTY(LIST_TIT.NOTE), " ", "√"):1:H="", ;
                  DUMMY.F:H="" ;
           FREEZE DUMMY.F KEY arm  ;
           WHEN V_Foot()         ;
           NOMENU NORGRID NOAPPEND NODELETE NOMODIFY NOCLEAR ;
           WINDOW Lst_I IN WINDOW Lst_E
    ON KEY
    IF what_do = "list"
      what_do = "exit"
    ENDIF
  CASE what_do = "discount"   && Скидка
    DO Discount WITH mrkl, "List_M"
    what_do = "list"
  CASE what_do = "edit"       && Редактирование списка
    RELEASE WINDOW Lst_M
    RELEASE WINDOW Lst_E
    RELEASE WINDOW Lst_I
    DO View_Note
    DO List_Det
    DO Prep_W_L
    what_do = "list"
  CASE what_do = "view_alt"   && Просмотр списка в альтернативной валюте
    RELEASE WINDOW Lst_M
    RELEASE WINDOW Lst_E
    RELEASE WINDOW Lst_I
    DO View_Note
    DO List_Det WITH .T., .T.
    DO Prep_W_L
    what_do = "list"
  CASE what_do = "P_Inv1"     && Печать якобы накладной
    DO P_Inv1 WITH LST_NUM
    what_do = "list"
  CASE what_do = "XLS_List"   && Список в формате Exel
    DO XLS_List WITH LST_NUM
    what_do = "list"
  CASE what_do = "Delivery"   && Доставка
    DO Dlv_S
    what_do = "list"
  CASE what_do = "note"       && Примечание
    DO ModiNote
    what_do = "list"
  CASE what_do = "Let_List"   && Доступ к рабочей области
    DO Let_List
    what_do = "list"
  CASE what_do = "GetList"    && Прием списка

    m.tmpArm = Imp_List()
    IF m.tmpArm # 0
      SET ORDER TO TAG LST_NUM
      SEEK m.tmpArm
      REPLACE LST_ARM WITH m.arm
      SET ORDER TO TAG LST_ARM
    ENDIF
    what_do = "list"
    
  CASE what_do = "SendList"   && Передача списка
    IF .NOT. EOF() .AND. .NOT. BOF()
      m.tmpArm = EnabList()
      IF .NOT. EMPTY(m.tmpArm)
        m.r_sav = RECNO()
        SKIP
        IF EOF() .OR. LST_ARM # m.arm
          m.r_sav1 = 0
        ELSE
          m.r_sav1 = RECNO()
        ENDIF
        GO m.r_sav
        REPLACE LST_ARM WITH m.tmpArm
        IF m.r_sav1 = 0
          GO BOTTOM
        ELSE
          GO m.r_sav1
        ENDIF
      ENDIF
    ENDIF
    what_do = "list"
  CASE what_do = "address"    && Адрес
    DO ModiAddr
    what_do = "list"
  CASE what_do = "weight"     && Вес
    DO Lst_Wght WITH LIST_TIT.LST_NUM
    what_do = "list"
  CASE what_do = "bron_off"   && Разбронировать список
    IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
      PRIVATE mss_sal
      DIMENSION mss_sal[3]
      mss_sal[1] = ""
      mss_sal[2] = CHR(0)+"Список уже продан..."
      mss_sal[3] = ""
      DO Out_Mess WITH 7, "mss_sal"
    ELSE
      DO Bron_Off WITH STR(LIST_TIT.LST_NUM, 6)
    ENDIF
    what_do = "list"
  CASE what_do = "to prep"    && Ответ клиенту
    DO Prep_Time
    what_do = "list"
  CASE what_do = "answer"     && Ответ клиенту
    DO Answer WITH LST_NUM
    what_do = "list"
  CASE what_do = "URM_Ord"     && Заказ для Урм
    DO To_URM WITH STR(LST_NUM, 6)
    what_do = "list"
  CASE what_do = "URM_New"     && Заказ для Урм
    DO To_URM_N WITH STR(LST_NUM, 6)
    what_do = "list"
  CASE what_do = "seek_pos"   && Поиск позиции
    m.tmpRec = Pos_Lst()
    IF m.tmpRec # 0
      GO m.tmpRec IN LIST_TIT
    ENDIF
    what_do = "list"
  CASE what_do = "seek_again" && Повтор поиска позиции
    m.tmpRec = Pos_Lst(.T.)
    IF m.tmpRec # 0
      GO m.tmpRec IN LIST_TIT
    ENDIF
    what_do = "list"
  CASE what_do == "seek"      && Поиск
    DO Seek_Lst WITH "List_M"
    what_do = "list"
  CASE what_do == "seek_l"    && Поиск списка
    PRIVATE tmpLNum
    m.tmpLNum = SeekList()
    IF m.tmpLNum # 0
      GO m.tmpLNum IN LIST_TIT
    ENDIF
    what_do = "list"
  CASE what_do == "seek_I"    && Поиск по номеру интернет-заказа
    PRIVATE tmpLNum
    m.tmpLNum = SeekInt()
    SET ORDER TO TAG LST_ARM ASCENDING IN LIST_TIT
    IF m.tmpLNum # 0
      GO m.tmpLNum IN LIST_TIT
    ENDIF
    what_do = "list"
  CASE what_do == "course_l"  && Курс для списка
    DO W_Course
    what_do = "list"
  CASE what_do == "seek_nt"   && Поиск по примечанию к позиции
    m.what_do = Seek_P_N()
    IF m.what_do < 0
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Это не ваш список..."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ENDIF
    IF m.what_do > 0
      GO m.what_do IN LIST_TIT
    ENDIF
    what_do = "list"
  CASE what_do == "NSP"       && Сумма с НСП
    DO W_NSP
    what_do = "list"
  CASE what_do = "seek_b"     && Поиск счета
    DO Seek_B
    what_do = "list"
  CASE what_do = "corrl"      && Коррекция заголовка списка
    DO Corr_Tit WITH "old"
    what_do = "list"
  CASE what_do = "st_corrl"   && Стандартные атрибуты списка
    DO St_List
    what_do = "list"
  CASE what_do = "set_lfs"    && Флажки
    DO Set_LFS
    SELECT LIST_TIT
    what_do = "list"
  CASE what_do = "sale"       && Продажа
    DO List_Sal WITH "List_M"
    SELECT LIST_TIT
    what_do = "list"
  CASE what_do = "to_sale"    && Вынести отмеченные для продажи
    IF .NOT. BOF() .AND. .NOT. EOF()
      IF Saled(LST_NUM)
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Список уже продан..."
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
      ELSE
        m.tmpRec = To_Sale(STR(LST_NUM, 6))
        IF .NOT. EMPTY(m.tmpRec)
          GO m.tmpRec
        ENDIF
      ENDIF
    ENDIF
    what_do = "list"
  CASE what_do = "to_buhg"    && Требование в бухгалтерию
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+" Приход денег будет связан с "+IIF(LIST_TIT.IS_CHECK, "чеком", "приходным ордером")+". [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(5, "mss") = 1
      dn_buhg = ""
      DO List_Sal WITH "List_M", .T.
      CLEAR
      IF .NOT. EMPTY(dn_buhg)   && Требование сформировано
        SELECT 0
        USE (base_path+"BUHG_REQ") AGAIN ORDER TAG DOC_NUM
        IF SEEK(LEFT(DTOS(DATE()),4)+dn_buhg)   && Требование существует!
          IF DEP_TYPE # 4 .AND. IS_CONV         && не связано с чеком
            PRIVATE tmpDeps
            tmpDeps = Get_Sys("AutoDeps")
            IF .NOT. FIRM+"!" $ tmpDeps         && фирма не запрещает...
              DIMENSION mss(3)
              mss(1) = ""
              mss(2) = CHR(0)+"Проводим конвертацию? [ Да ] [ Нет ]"
              mss(3) = ""
              IF Out_Mess(5, "mss") = 1
                DO Conv_Dep WITH CUS_CODE, .T.
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        SELECT BUHG_REQ
        USE
      ENDIF
    ENDIF
    SELECT LIST_TIT
    what_do = "list"
  CASE what_do = "prn_to_buhg" && Печать требований в бухгалтерию
    DO V_B_Req WITH arm
    SELECT LIST_TIT
    what_do = "list"
  CASE what_do = "to_ryn"     && На реализацию
    DO List_Ryn
    SELECT LIST_TIT  && Так надо!
    what_do = "list"
  CASE what_do = "inv_sto"    && Накладная для STORE
    DO Send_Inv
    what_do = "list"
  CASE what_do = "invoice"    && Накладная
    DO Prnt_Inv
    what_do = "list"
  CASE what_do = "act_d"      && Акт по организации доставки
    DO Prnt_Inv  WITH .F., .T.
    what_do = "list"
  CASE what_do = "inv0"       && Еще одна накладная (товарный чек?)

    IF .NOT. Saled(LST_NUM)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Список еще не продан..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
*    DO Prnt_Inv WITH "0"
      DO P_Check WITH FIRM, SAL_NUM, SAL_DATE
    ENDIF
    what_do = "list"
 CASE what_do = "check"      && Чек
    DO Set_Check
    what_do = "list"
  CASE what_do = "bill_f"     && Счет-фактура
    DO Prnt_BF WITH "List_M"
    what_do = "list"
  CASE what_do = "order"      && Приходный ордер
    DO Prnt_Ord WITH "List_M"
    what_do = "list"
  CASE what_do = "addl"       && Добавление заголовка списка
    DO Corr_Tit WITH "new"
    what_do = "list"
  CASE what_do = "st_addl"    && Добавление стандартного заголовка списка
    DO St_List WITH .T.
    DO Corr_Tit WITH "stand"
    what_do = "list"
  CASE what_do = "copy"       && Копирование списка
    DO Copy_Lst
    what_do = "list"
  CASE what_do = "pack_mode"  && Установка режима округления до упаковки
    DO Pack_M
    what_do = "list"
  CASE what_do = "queue_mode" && Тип очереди для заявок
    DO Get_Q
    what_do = "list"
  CASE what_do = "del_list"   && Удаление списка
    DO Del_List
    what_do = "list"
  CASE what_do = "delta_lst"  && Разность списков
    IF LEN(mrkl) < 7
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Вы не отметили старый список!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      what_do = "list"
      LOOP
    ENDIF
    IF LEN(mrkl) > 7
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"Вы отметили несколько списков:"
      mss(3) = CHR(0)+CHRTRAN(mrkl, "!", ",")
      mss(3) = LEFT(mss(3),LEN(mss(3))-1)+"!"
      mss(4) = ""
      DO Out_Mess WITH 7, "mss"
      what_do = "list"
      LOOP
    ENDIF
    IF LEFT(mrkl,6) = STR(LIST_TIT.LST_NUM,6)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Вы пытаетесь сравнить список с собой же!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      what_do = "list"
      LOOP
    ENDIF
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Вычитаем список "+ALLTRIM(STR(LIST_TIT.LST_NUM,6))+ ;
                    " из "+ALLTRIM(LEFT(mrkl,6))+"? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(5, "mss") = 2
      what_do = "list"
      LOOP
    ELSE
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"  Выделяем отказы в отдельный список ?"+" [ Да ] [ Нет ]"
      mss(3) = ""
      IF Out_Mess(5, "mss") = 2
        DO Dlt_Lst WITH LEFT(mrkl,6), STR(LIST_TIT.LST_NUM,6)
        what_do = "list"
      ELSE
        DO Dlt_Lst WITH LEFT(mrkl,6), STR(LIST_TIT.LST_NUM,6)
        DO Diff_Lst WITH LEFT(mrkl,6), STR(LIST_TIT.LST_NUM,6)
        what_do = "list"
      ENDIF
    ENDIF
  CASE what_do = "p_list"     && Список
    DO P_List WITH LIST_TIT.LST_NUM && WITH "List_M"
    what_do = "list"
  CASE what_do = "p2_list"     && Список
    DO P_List2 WITH LIST_TIT.LST_NUM && WITH "List_M"
    what_do = "list"
  CASE what_do = "p1_list"    && Список с нормой отпуска
    DO P_List1 WITH LIST_TIT.LST_NUM && WITH "List_M"
    what_do = "list"
  CASE what_do = "sto_req"    && Требование на склад
    IF .NOT. Saled(LIST_TIT.LST_NUM)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Список еще не продан!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DO P_Req WITH LIST_TIT.FIRM, LIST_TIT.SAL_NUM, LIST_TIT.SAL_DATE
    ENDIF
    what_do = "list"
  CASE what_do = "dis_inv"    && Расчет скидки по накладной
    IF .NOT. Saled(LIST_TIT.LST_NUM)
      DO Dis_List WITH LIST_TIT.LST_NUM
    ELSE
      DO Dis_Inv WITH LIST_TIT.FIRM, LIST_TIT.SAL_NUM, LIST_TIT.SAL_DATE
    ENDIF
    what_do = "list"
  CASE what_do = "sup_ord"    && Заявка на поставку
    IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Список уже продан!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DO Order_0 WITH LIST_TIT.LST_NUM
    ENDIF
    what_do = "list"
  CASE what_do = "sto_ord"    && Заявка на склад
    DO Prp_Ord WITH STR(LIST_TIT.LST_NUM, 6)
    what_do = "list"
  CASE what_do = "ord_hist"   && Гистограмма загрузки склада
    DO Ord_Hist
    what_do = "list"
  CASE what_do = "mark_ord"    && Утверждение заявок на склад
    DO Mark_Ord
    what_do = "list"
  CASE what_do = "bill"        && Счет
    DO Bill_0 WITH lst_num
    what_do = "list"
  CASE what_do = "z_bill"      && Счет без списка
    DO Bill
    what_do = "list"
  CASE what_do = "to common"   && Перенос в общую область
    DO To_Common WITH arm, "***"
    what_do = "list"
  CASE what_do = "from common"  && Перенос из общей области
    DO F_Common
    what_do = "list"
  CASE what_do = "info"         && Информация
    DO Lst_Info WITH "List_M"
    what_do = "list"
  CASE what_do = "get_o_1"     && Ввод произвольной заявки
    DO Get_O1 WITH "List_M"
    what_do = "list"
  CASE what_do = "get_ord"     && Ввод заявки
    DO Get_Ord WITH "List_M"
    what_do = "list"
  CASE what_do = "get_f_ord"   && Ввод заявки от филиала
    DO F_Ord WITH arm
    what_do = "list"
  CASE what_do = "rd_ord"      && Ввод заявки от филиала ( новый формат )
    DO Rd_Ord
    what_do = "list"
  CASE what_do = "set_mode"     && Установка режима
    DO Set_Mode WITH "List_M"
    what_do = "list"
  CASE what_do = "let_sale"     && Утверждение цен
    DO Let_Sale
    what_do = "list"
  CASE what_do = "prn_dlv"      && Печать доставки
    PRIVATE dlv_sav, dlv_num, dlv_date
    dlv_sav = SELECT()
    SELECT 0
    USE (base_path+"DELIVERY") ORDER TAG LST_NUM ALIAS DLV_2417 AGAIN
    IF SEEK(LIST_TIT.LST_NUM)
      dlv_num  = DOC_NUM
      dlv_date = DOC_DATE
      DO Delivery WITH dlv_num, dlv_date
    ELSE
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"По данному списку доставки нет!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ENDIF
    USE
    SELECT (dlv_sav)
    what_do = "list"
  CASE what_do = "lst_dlv"     && Список с доставками
    DO Prep_Dlv
    what_do = "list"
  CASE what_do = "dbf_ref"     && Список "Отказы" в DBF-формате
    DO Lst_Ref WITH LST_NUM
    what_do = "list"
  CASE what_do = "lst_ref"     && Печать списка "Отказы"
    DO P_Ref WITH LST_NUM
    what_do = "list"
  CASE what_do = "dbf_def"     && Список "Отказы" в DBF-формате
    DO Lst_Def WITH LST_NUM
    what_do = "list"
  CASE what_do = "filter"      && Установка фильтра
    DO List_Flt
    GO BOTTOM
    what_do = "list"
  CASE what_do = "lst_def"     && Печать списка "Отказы"
    DO P_Def WITH LST_NUM
    what_do = "list"

  OTHERWISE
    EXIT
  ENDCASE
ENDDO
RELEASE WINDOW Lst_M
RELEASE WINDOW Lst_E
RELEASE WINDOW Lst_I

CLOSE DATABASES

DO Exit_P WITH "", "List_M"
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                  Программа пометки списка.                            ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Mrk_Lst
PRIVATE s, i
s = STR(LST_NUM,6)+"!"
i = AT(s,mrkl)
IF i = 0
  mrkl = mrkl+s
ELSE
  mrkl = STUFF(mrkl,i,LEN(s),"")
ENDIF
DEACTIVATE MENU List_M

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║            Программа создания окон для перечня списков.               ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Prep_W_L

PARAMETER tit_

PRIVATE l, c, ln, wd, tit1, tit2, ft
DIMENSION tit2(1), ft(5)
ft(1) = "─────────────────────────────────────────────────────────────"
ft(2) = "Клиент                                                               "
ft(3) = "Почта [ ]   Счет                  Оплачен [ ]   Накладная            "
ft(4) = "  Скидка [ ]     Спец.цена [ ]   Утверждение                "
ft(5) = "Доставка [ ]  Бронь [ ]  Собрана [ ]  Согласована с клиентом [ ]"
*
*   Основной перечень полей для BROWSE
*
tit2(1) = ;
"──Фрм──Номер───Дата─────────Имя────────────────────────────────Цена──────М─П"


*         "──"+PADC("─Номер",FSIZE("LST_NUM" ,"LIST_TIT"),"─")+"─"+ ;
*                PADC("Дата",IIF(SET("CENTURY")="ON",10,8),"─")+"─"+ ;
*                PADC("Имя списка────────",FSIZE("LST_NAME" ,"LIST_TIT"),"─")+"─"+ ;
*                PADC("──Сумма",FSIZE("R_TOTAL", "LIST_TIT"),"─")
ln = WROWS("")-10

*
*  Корректируем с учетом установленного фильтра
*
IF TYPE("tit_") = "C"
  tit1 = tit_
ELSE
  tit1 = "Перечень списков"
ENDIF
RELEASE WINDOW Lst_M
RELEASE WINDOW Lst_E
RELEASE WINDOW Lst_I

PRIVATE l1, c1, wd1
l1  = WROWS("")-7
wd1 = LEN(tit2(1))+4
c1  = FLOOR((WCOLS("")-wd1-2)/2)
DO Def_Win WITH "Lst_M", "Lst_E", "Lst_I", ln, LEN(tit2(1)), tit1, tit2, ft
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║        Программа - обработчик мышки для списков.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE M_Proc_1
PARAMETER menu_nm
PRIVATE mc, mr

*
*  Асинхронное меню
*
IF MENU() == UPPER(menu_nm)
  RETURN
ENDIF

IF MROW("") = 0
  IF .NOT. MENU() == UPPER(menu_nm)
    ACTIVATE MENU (menu_nm)
  ENDIF
  RETURN
ENDIF

*
*  Окно BROWSE
*
mc = MCOL("Lst_E")
mr = MROW("Lst_E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  Основное окно
*
mc = MCOL("Lst_M")
mr = MROW("Lst_M")
IF mc >= 0 .AND. mr >= 0
  IF mr = 1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WROWS("Lst_M")-1
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║             Программа коррекции/добавления списка.                    ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Corr_Tit
PARAMETERS new_lst, e_menu

PRIVATE new_w, l_name, cli_tp, tit001, code_w, menu_w, mail_w, mail_l, p_mail
PRIVATE o_num, o_dat, w_usd, w_check, s0001, s_sav
PRIVATE f_c000, f_n000, b_c000, b_n000, b_sw000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"

IF TYPE("new_lst") = "C"
  new_w = new_lst = "new"
ELSE
  new_w = .T.
ENDIF
IF TYPE("e_menu") = "C"
  menu_w = e_menu
ELSE
  menu_w = "List_M"
ENDIF

cli_tp = .F.
IF new_w
  l_name = SPACE(FSIZE("LST_NAME","LIST_TIT"))
  tit001 = "Новый список"
  mail_w = .F.
  mail_l = .T.
  o_num  = SPACE(FSIZE("REQ_DOC","LIST_TIT"))
  o_dat  = {}
  w_usd = .F.
  f_c000 = def_firm
  b_c000 = def_bank
  f_c_old = def_firm
  w_check = .F.
ELSE
  *
  *     Проверяем: а можно ли его корректировать?
  *
  IF Saled(LIST_TIT.LST_NUM)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Продано! Ни о какой коррекции не может быть и речи."
    mss(3) = ""
    DO Out_Mess WITH 7, 'mss'
    RETURN
  ENDIF
  l_name = LIST_TIT.LST_NAME
  tit001 = "Список N "+ALLTRIM(STR(LIST_TIT.LST_NUM,15))+" от "+TRANSFORM(LIST_TIT.CR_DAT,"@D")
  mail_w = LIST_TIT.MAIL # " "
  mail_l = LIST_TIT.MAIL # "2"
  o_num  = LIST_TIT.REQ_DOC
  o_dat  = LIST_TIT.REQ_DATE
  w_usd  = LIST_TIT.IS_USD
  w_check= LIST_TIT.IS_CHECK
  f_c000 = LIST_TIT.FIRM
  b_c000 = LIST_TIT.BANK
  f_c_old = LIST_TIT.FIRM
ENDIF

f_n000 = SPACE(16)
b_n000 = SPACE(16)
b_sw000= .F.

s_sav = SELECT()
IF .NOT. EMPTY(f_c000)
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS TMP_9876 AGAIN
  IF SEEK(f_c000)
    f_n000 = B_NAME
    df_wchk = DEF_WCHK         && NEW 22.12.1999
    lt_wchk = LET_WCHK         && NEW 22.12.1999
    w_check = IIF(new_w, df_wchk, w_check)   && NEW 22.12.1999
    * (Если список создается - для чека берется умолчание,
    * если корректируется, то оставляется значение LIST_TIT.IS_CHECK)
    IF .NOT. EMPTY(b_c000)
      USE (base_path+"BANKS") ORDER TAG BANK ALIAS TMP_9876 AGAIN
      IF SEEK(f_c000+b_c000)
        b_n000 = B_NAME
      ELSE
        b_c000 = "   "
      ENDIF
    ENDIF
  ELSE
    f_c000 = "   "
    b_c000 = "   "
  ENDIF
  USE
  SELECT (s_sav)
ELSE
  f_c000 = "   "
  b_c000 = "   "
ENDIF

DO Entry_P WITH menu_w, ""

PRIVATE ln, wd, l, c, pic, pic1, wdn, wdt, wdtp, p_ex, ex, nm_w
PRIVATE cli_sw

*
*┌──────────────────────────────────────────────────────────────┐
*│                    [ ] Отправка по почте                     │
*│             ┌─ Заказ ──────────────────────────┐             │
*│             │  Номер ....:....! от ДД.ММ.ГГГГ  │             │
*│             └──────────────────────────────────┘             │
*│   ┌─ [ ] Покупатель ─────────────────────────────────────┐   │
*│   │  ....:....!....:....!....:....!....:....!....:....!  │   │
*│   └──────────────────────────────────────────────────────┘   │
*│Преприятие 999 9999999999999999 (F1) [ ] Банк 9999999999999999│
*│                  [ ] Цены в У Е    [ ] Чек                   │
*│   Имя списка ....:....!....:....!....:....!.. (Ctrl-Enter)   │
*│                                                              │
*│           < Сохранить Ctrl-W > < Отказаться Esc >            │
*└──────────────────────────────────────────────────────────────┘
IF new_lst = "stand"
  pic1 = " Сохранить Ctrl-W "
ELSE
  pic1 = " Сохранить Ctrl-W ;\?  Отказаться Esc  "
ENDIF
pic  = "Новый;Постоянный"
wdn  = FSIZE("CUS_NAME","ACCOUNT")
wdtp = LEN(pic)+8+4
wdt  = 24+FSIZE("LST_NAME","LIST_TIT")

wd   = 66
wdn  = FLOOR((wd-wdn)/2)
wdtp = FLOOR((wd-wdtp)/2)
wdt  = FLOOR((wd-wdt)/2)+11
p_ex = FLOOR((wd-LEN(pic1))/2)

ln = 14
p_mail = FLOOR((wd-21)/2)

l = CEILING((WROWS("")-ln)/2)
c = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW Corr_Tit FROM l, c TO l+ln-1, c+wd-1 SHADOW NONE COLOR SCHEME 13
ACTIVATE WINDOW Corr_Tit

@  0,  0 SAY PADC(tit001, WCOLS()) COLOR SCHEME 5
@  1,  1 TO WROWS()-1, WCOLS()-2
@ 11,  5 SAY "Имя списка"
@ 11, 49 SAY "(Ctrl-Enter)"
DO Sun_Bord WITH 6,  5, 8, 58, "     Покупатель "
DO Sun_Bord WITH 3, 15, 5, 48, " Заказ "
nm_w = SPACE(FSIZE("CUS_NAME","ACCOUNT"))
cli_sw = 0
IF .NOT. New_w
  nm_w = ACCOUNT.CUS_NAME
ENDIF
ex = 1
PRIVATE left_p, right_p
left_p  = LEFT(l_name, 24)
right_p = SUBSTR(l_name, 25)

@  6,  8    GET cli_tp PICTURE "@*C Покупатель " VALID Get_Cli()
@  7,  7    GET nm_w WHEN .F.
@  9,  2    SAY "Преприятие" GET f_c000 WHEN Ent_firm() VALID Exi_Firm()
@  9, COL()+1 GET f_n000 WHEN .F.
@  9, COL()+1 SAY "(F1)"
@  9, COL()+1 GET b_sw000 PICTURE "@*C Банк" WHEN .NOT. EMPTY(f_c000) VALID SetBank()
@  9, COL()+1 GET b_n000 WHEN .F.
*DO Get_Firm WITH 9, 2, enab_firm
*DO Get_Bank WITH 9, 39, enab_bank
@ 10, 20    GET w_usd PICTURE "@*C Цены в У Е" WHEN new_w
@ 10, 38    GET w_check PICTURE "@*C Чек" WHEN (kkm_exist AND lt_wchk) VALID C_Nm()
* && NEW 22.12.1999 (включено условие lt_wchk - можно ли у данного предпр.
*                    менять чековость продаж)
* Во время выбора предпр. df_wchk и lt_wchk обновляются в C_F1_()
@ 11, 16    GET left_p WHEN Set_F1() VALID Clr_F1()
@ 11, COL() GET right_p WHEN Set_F1() .AND. .NOT. w_check VALID Clr_F1()
@ 12, p_ex  GET ex PICTURE "@*HT \!"+pic1
@  2, 22    GET mail_w PICTURE "@*C Отправка по почте" WHEN mail_l
@  4, 18    SAY "Номер" GET o_num
@  4, 35    SAY "от" GET o_dat
READ CYCLE VALID Tst_NL()
=C_Nm()           && NEW 22.12.1999 Если чек уст.по умолчанию (а не выбран крестиком),
                  && то все равно в название списка ставим слово "Чек"
l_name = left_p+right_p
IF ex = 1
  PRIVATE c_01, c_02
  c_01 = ACCOUNT.CUS_CODE
  c_02 = ACCOUNT.ACC_TYPE
  IF new_w
    mail_w = IIF(mail_w, "1", " ")
    SELECT 0
    USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS W_LIST_0 AGAIN
    code_w = Doc_Num("LISTS", "", "", "")
    SELECT W_LIST_0
    USE
    SELECT LIST_TIT
    APPEND BLANK
    REPLACE LST_ARM  WITH arm,    ;
            LST_NUM  WITH code_w, ;
            IS_USD   WITH w_usd,  ;
            IS_CHECK WITH w_check,;
            CR_DAT   WITH DATE(), ;
            MAIL     WITH mail_w, ;
            LST_NAME WITH l_name, ;
            CLI_CODE WITH c_01,   ;
            CLI_TYPE WITH c_02,   ;
            REQ_DOC  WITH o_num,  ;
            REQ_DATE WITH o_dat,  ;
            FIRM     WITH f_c000, ;
            ORD_QUEUE WITH queuetype,;
            BANK     WITH b_c000, ;
            WHO_CR   WITH user,   ;
            WHERE_CR WITH arm
  ELSE
    PRIVATE www0
    s0001 = LIST_TIT.R_TOTAL
    IF IS_USD # w_usd
      s0001 = 0
      SELECT 0
      USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS LD8B26
      SEEK STR(LIST_TIT.LST_NUM,6)
      IF w_usd         && Были рубли, стали USD
        www0 = IIF(DEF_PRICE = SAL_PRICE, SAL_PRICE/kurs, DEF_PRICE/kurs)
        SCAN REST WHILE LIST_NUM = STR(LIST_TIT.LST_NUM,6)
          REPLACE USD_PRICE WITH IIF(SAL_PRICE = DEF_PRICE, USD_DEF, SAL_PRICE/kurs), ;
                  SAL_PRICE WITH 0
          s0001 = s0001+USD_PRICE*QNT
        ENDSCAN
      ELSE             && Были USD, стали рубли
        SCAN REST WHILE LIST_NUM = STR(LIST_TIT.LST_NUM,6)
          REPLACE SAL_PRICE WITH IIF(USD_PRICE = USD_DEF, DEF_PRICE, Clc_Rub(USD_PRICE)), ;
                  USD_PRICE WITH 0
          s0001 = s0001+ROUND(SAL_PRICE*QNT,2)
        ENDSCAN
      ENDIF
      USE
    ENDIF
*    IF IS_CHECK # w_check
*      s0001 = 0
*      SELECT 0
*      USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS LD8B26
*      SEEK STR(LIST_TIT.LST_NUM,6)
*      IF .NOT. w_usd
*        www0 = IIF(w_check, 0, nds_flt)
*        SCAN REST WHILE LIST_NUM = STR(LIST_TIT.LST_NUM,6)
*          REPLACE SAL_PRICE WITH Round_6(SAL_PRICE, www0)
*          s0001 = s0001+ROUND(SAL_PRICE*QNT,2)
*        ENDSCAN
*      ENDIF
*      USE
*    ENDIF
    SELECT LIST_TIT
    IF .NOT. EMPTY(MAIL)
      mail_w = IIF(mail_w, MAIL, " ")
    ELSE
      mail_w = IIF(mail_w, "1", " ")
    ENDIF
    REPLACE LST_NAME WITH l_name, ;
            CLI_CODE WITH c_01,   ;
            IS_USD   WITH w_usd,  ;
            IS_CHECK WITH w_check,;
            MAIL     WITH mail_w, ;
            REQ_DOC  WITH o_num,  ;
            REQ_DATE WITH o_dat,  ;
            R_TOTAL  WITH s0001,  ;
            FIRM     WITH f_c000, ;
            BANK     WITH b_c000, ;
            CLI_TYPE WITH c_02
  ENDIF
ENDIF

RELEASE WINDOW Corr_Tit
DO Exit_P WITH menu_w, ""
SELECT (s_sav)
IF .NOT. EOF() .AND. .NOT. BOF()
  GO RECNO()        && Так надо, эмпирика!
ENDIF
RETURN ex = 1

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Установка пометки "Чек".                           ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE C_Nm
IF w_check
  right_p = PADR("Чек", LEN(right_p))
  SHOW GET right_p
ENDIF
RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа проверки имени списка.                       ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Tst_NL
PRIVATE mss

mss = MOD(READKEY(),256)
IF READKEY(1) # 1
  mss = 15
ENDIF
IF ex = 2
  RETURN .T.
ENDIF
IF EMPTY(nm_w)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы забыли указать клиента..."
  mss(3) = ""
  DO Out_Mess WITH 7,"mss"
  RETURN .F.
ENDIF
IF EMPTY(left_p+right_p)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Уж как-нибудь список назовите..."
  mss(3) = ""
  DO Out_Mess WITH 7,"mss"
  RETURN .F.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали предприятие!"
  mss(3) = ""
  DO Out_Mess WITH 7,"mss"
  RETURN .F.
ENDIF
*PRIVATE s_sav
*s_sav = SELECT()
*SELECT 0
*USE (base_path+"CUS_LINK") ORDER TAG TYPE ALIAS TTT_0707 AGAIN
*IF .NOT. SEEK(STR(ACCOUNT.NEW_TYPE,3)+f_c000)
*  DIMENSION mss(3)
*  mss(1) = ""
*  mss(2) = CHR(0)+"Клиент и предприятие противопоказаны друг другу!"
*  mss(3) = ""
*  DO Out_Mess WITH 7,"mss"
*  USE
*  SELECT (s_sav)
*  RETURN .F.
*ENDIF
*USE
*SELECT (s_sav)
IF .NOT. Leg_Firm(ACCOUNT.CUS_CODE, f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Клиент и предприятие противопоказаны друг другу!"
  mss(3) = ""
  DO Out_Mess WITH 7,"mss"
  RETURN .F.
ENDIF

RETURN Is_Dubl(ACCOUNT.CUS_CODE)

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                   Программа ввода клиента.                            ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Get_Cli
PRIVATE sw, mss_w

IF .NOT. cli_tp
  RETURN .T.
ENDIF
cli_tp = .F.
SHOW GET cli_tp
mss_w = ""
HIDE WINDOW Corr_Tit
sw = Custs ("", .T., .F.)
IF sw
  SELECT 0
  USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE
  IF SEEK(ACCOUNT.CUS_CODE)
    IF .NOT. EMPTY(NOTE)
      mss_w = " "+ALLTRIM(NOTE)+" "
    ENDIF
  ENDIF
  USE
  SELECT LIST_TIT
  cli_sw = ACCOUNT.ACC_TYPE
  nm_w   = ACCOUNT.CUS_NAME
  IF EMPTY(left_p)
    left_p = LEFT(nm_w,LEN(left_p))
    right_p = SUBSTR(nm_w,LEN(left_p)+1, LEN(right_p))
  ENDIF
ENDIF
ACTIVATE WINDOW Corr_Tit
SHOW GET left_p
SHOW GET right_p
SHOW GET nm_w
IF .NOT. EMPTY(mss_w)
  WAIT WINDOW mss_w NOWAIT
ENDIF
RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                 Программа переноса в общую область.                   ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE To_Common
PARAMETERS arm_from, arm_to

GO RECNO()
IF EMPTY(mrkl)
  REPLACE LST_ARM WITH arm_to
  RETURN
ENDIF
PRIVATE st_w, num_w
DO WHILE .NOT. EMPTY(mrkl)
  st_w = LEFT(mrkl,7)
  mrkl = SUBSTR(mrkl,8)
  st_w = LEFT(st_w,6)
  SEEK arm_from+st_w
  REPLACE LST_ARM WITH arm_to
ENDDO

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа работы с общим списком.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE F_Common

PRIVATE what_do
*
*    PAD - меню первого уровня
*
DEFINE MENU F_Common IN SCREEN COLOR SCHEME 4
DEFINE PAD Info  OF F_Common PROMPT "Информация" KEY Ctrl-A, "F9"
DEFINE PAD Seek  OF F_Common PROMPT "Поиск" KEY Ctrl-A, "F7"
DEFINE PAD Carry OF F_Common PROMPT "Передать" KEY Ctrl-A, "Enter"
DEFINE PAD Abort OF F_Common PROMPT "Выход" KEY Ctrl-A, "Esc"

ON SELECTION PAD Info  OF F_Common DO Ch_Mode WITH "F_Common","info"
ON SELECTION PAD Seek  OF F_Common DO Ch_Mode WITH "F_Common","seek"
ON SELECTION PAD Carry OF F_Common DO Ch_Mode WITH "F_Common","carry"
ON SELECTION PAD Abort OF F_Common Do Ex_Proc WITH "F_common"

DO Entry_P WITH "List_M", "F_Common"

what_do = "list"

DO WHILE .T.
  DO CASE
  CASE what_do = "list"     && Списки
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    SHOW MENU F_Common
    ON KEY LABEL F10   ACTIVATE MENU F_Common
    ON KEY LABEL F9    DO Ch_Mode WITH "F_Common","info"
    ON KEY LABEL F7    DO Ch_Mode WITH "F_Common","seek"
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "F_Common"
    DO Prep_W_L WITH "Списки общей области"
    mv = 2-WLROW("Lst_M")
    IF mv # 0
      MOVE WINDOW Lst_M BY mv, 0
      MOVE WINDOW Lst_E BY mv, 0
      MOVE WINDOW Lst_I BY mv, 0
    ENDIF
    ACTIVATE WINDOW Lst_E
    BROWSE FIELDS f001 = IIF(STR(LIST_TIT.LST_NUM,6)+"!" $ mrkl, "√"," "):H="", ;
                  LST_FRM.FIRM_CODE:H="",  ;
                  LIST_TIT.LST_NUM:H="", LIST_TIT.CR_DAT:10:H="",  ;
                  LIST_TIT.LST_NAME:H="",   ;
                  f004 = STR(LIST_TIT.R_TOTAL, 13, IIF(LIST_TIT.IS_USD, 4, 2)):H="", ;
                  f003 = IIF(LIST_TIT.IS_USD, "$", "р"):1:H="", ;
                  f_002 = IIF(EMPTY(LIST_TIT.SAL_NUM), " ", "√"):H="", ;
                  f_003 = IIF(EMPTY(LIST_TIT.NOTE), " ", "√"):1:H="", ;
                  DUMMY.F:H="" ;
           FREEZE DUMMY.F KEY "***"   ;
           WHEN V_Foot() ;
           NOMENU NORGRID NOAPPEND NODELETE NOMODIFY NOCLEAR ;
           WINDOW Lst_I IN WINDOW Lst_E
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "carry")
    ENDIF
  CASE what_do = "seek"     && Поиск
    DO Seek_Lst WITH "F_Common"
    what_do = "list"
  CASE what_do = "carry"     && Перенос в общую область
    DO To_Common WITH "***", arm
    what_do = "list"
  CASE what_do = "info"      && Иформация
    DO Lst_Info WITH "F_Common"
    what_do = "list"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO
RELEASE WINDOW Lst_M
RELEASE WINDOW Lst_E
RELEASE WINDOW Lst_I

DO Exit_P WITH "List_M", "F_Common"

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║               Программа выдачи информации о списке.                   ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Lst_Info
PARAMETERS ext_menu

PRIVATE arr_info, n_l, st_w, u_1, u_2, a_1, a_2, s

DO Entry_P WITH ext_menu, ""
n_l = 1
DIMENSION arr_info(n_l)

arr_info(n_l) = "  Список N "+ALLTRIM(STR(LIST_TIT.LST_NUM,10))+  ;
                " от "+TRANSFORM(LIST_TIT.CR_DAT,"@D")+" "+     ;
                ALLTRIM(LIST_TIT.LST_NAME)+"  "
n_l = n_l+1
DIMENSION arr_info(n_l)
IF EMPTY(CLI_CODE)
  arr_info(n_l) = "  Покупатель: Новый  "
ELSE
  arr_info(n_l) = "  Покупатель: "+ALLTRIM(ACCOUNT.CUS_NAME)+" (^F1) "
  arr_info(n_l) = arr_info(n_l)+"(F1) (Shift-F1) "
ENDIF
IF .NOT. EMPTY(LIST_TIT.BILL_NUM)
  n_l = n_l+1
  DIMENSION arr_info(n_l)
  arr_info(n_l) = "  Счет N "+LEFT(BILLS.DOC_NUM,4)+"-"+  ;
                  ALLTRIM(RIGHT(BILLS.DOC_NUM,6))+"("+BILLS.FIRM+") от "+  ;
                  TRANSFORM(BILLS.DOC_DATE,"@D")+" на сумму "+ ;
                  ALLTRIM(STR(BILLS.BILL_SUM,15,2))+" "+ALLTRIM(BILLS.STATUS)
ENDIF
st_w = ""
IF .NOT. EMPTY(LIST_TIT.IS_DIS)
  st_w = st_w+"скидка"
  IF .NOT. EMPTY(LIST_TIT.PRESENT)
    st_w = st_w+"-подарок на сумму "+ALLTRIM(STR(LIST_TIT.PRESENT,10,2))
  ENDIF
ENDIF
IF .NOT. EMPTY(LIST_TIT.IS_SPEC)
  st_w = st_w+IIF(.NOT. EMPTY(st_w), ", ","")+"спеццены"
ENDIF
IF .NOT. EMPTY(st_w)
  IF .NOT. EMPTY(LIST_TIT.WHO_LET)
    st_w = st_w+", утверждены"
  ENDIF
  n_l = n_l+1
  DIMENSION arr_info(n_l)
  arr_info(n_l) = "  Цены: "+st_w+"  "
ENDIF
IF Saled(LIST_TIT.LST_NUM)
  n_l = n_l+1
  DIMENSION arr_info(n_l)
  arr_info(n_l) = "  Накладная N "+LEFT(LIST_TIT.SAL_NUM,4)+"-"+  ;
                   ALLTRIM(RIGHT(LIST_TIT.SAL_NUM,6))+" от "+  ;
                   TRANSFORM(LIST_TIT.SAL_DATE,"@D")
ELSE
  st_w = ""
  IF .NOT. EMPTY(LIST_TIT.IS_DUE)
    st_w = st_w+"долг"
  ENDIF
  IF .NOT. EMPTY(LIST_TIT.IS_DEF)
    st_w = st_w+IIF(.NOT. EMPTY(st_w), ", ","")+"дефицит"
  ENDIF
  IF .NOT. EMPTY(LIST_TIT.ORD_NUM)
    st_w = st_w+IIF(.NOT. EMPTY(st_w), ", ","")+"бронь"
  ENDIF
  IF .NOT. EMPTY(st_w)
    n_l = n_l+1
    DIMENSION arr_info(n_l)
    arr_info(n_l) = "  Позиции: "+st_w+"  "
  ENDIF
ENDIF
SELECT 0
USE (base_path+"ARM_LST") AGAIN
LOCATE FOR LIST_TIT.WHERE_CR = ARM_C
IF FOUND()
  a_1 = ARM_LST.ARM_N
ELSE
  a_1 = SPACE(FSIZE("ARM_N"))
ENDIF
IF LIST_TIT.LST_ARM = "***"
  a_2 = "Общая область   "
ELSE
  LOCATE FOR LIST_TIT.LST_ARM = ARM_C
  IF FOUND()
    a_2 = ARM_LST.ARM_N
  ELSE
    a_2 = SPACE(FSIZE("ARM_N"))
  ENDIF
ENDIF

USE (base_path+"USERS") ORDER TAG USER_CODE
IF LIST_TIT.WHO_CR = 0
  u_1 = "САМ создал"
ELSE
  IF SEEK(LIST_TIT.WHO_CR)
    u_1 = ALLTRIM(USER_NAME)
  ELSE
    u_1 = "Непонятно кто"
  ENDIF
ENDIF

IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
  IF LIST_TIT.WHO_SAL = 0
    u_2 = "САМ продал"
  ELSE
    IF SEEK(LIST_TIT.WHO_SAL)
      u_2 = ALLTRIM(USER_NAME)
    ELSE
      u_2 = "Непонятно кто"
    ENDIF
  ENDIF
ENDIF
USE
SELECT LIST_TIT

n_l = n_l+1
DIMENSION arr_info(n_l)
arr_info(n_l) = "  Создан: Раб. место "+a_1+" Продавец "+u_1+"  "
n_l = n_l+1
DIMENSION arr_info(n_l)
IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
  arr_info(n_l) = "  Продан: Раб. место "+a_2+" Продавец "+u_2+"  "
ELSE
  arr_info(n_l) = "  Обраб.: Раб. место "+a_2+"  "
ENDIF
*
*  Далее эту информацию следует выдать
*
PRIVATE ln, wd, i, n, l, c

wd = 0
n = ALEN(arr_info)
FOR i = 1 TO n
  wd = MAX(wd, LEN(arr_info(i)))
ENDFOR
wd = wd+4

ln = n+2
l = CEILING((WROWS("")-ln)/2)
c = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW Info FROM l, c TO l+ln-1, c+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW Info
@ 0,0 SAY PADC(arr_info(1),WCOLS()) COLOR SCHEME 5
@ 1,1 TO WROWS()-1, WCOLS()-2
FOR i = 2 TO n
  @ i, 2 SAY arr_info(i)
ENDFOR
@ WROWS()-1,2 SAY PADC(" Нажмите любую клавишу ",WCOLS()-4,"─")
i = SET("CURSOR") = "ON"
n = INKEY("M")
SET CURSOR OFF
DO WHILE .T.
  s = INKEY(0,"M")
  DO CASE
  CASE s = 28
    DO Cus_Info WITH ACCOUNT.CUS_CODE
  CASE s = 94
    DO Wk_Fill WITH "List_M"
  CASE s = 84
    DO Acc_Hist WITH ACCOUNT.CUS_CODE
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR (SCHEME(1,2))
    ACTIVATE WINDOW Info
  OTHERWISE
    EXIT
  ENDCASE
ENDDO
IF i
  SET CURSOR ON
ENDIF
IF n > 0
  KEYBOARD CHR(n)
ENDIF
RELEASE WINDOW Info
DO Exit_P WITH ext_menu, ""

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                   Программа поиска списка.                            ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Seek_Lst
PARAMETERS ext_menu

PRIVATE rc_sav, l, ln, wd, c, st, s, ord, st1, pic, menu_sav, all_arms

DO Entry_P WITH ext_menu
menu_sav = ""
*
*┌────────────────────────────────────────────────┐
*│           [ ] По всем рабочим местам           │
*│ [X] По имени заказа    [X] По имени покупателя │
*│    ....:....!....:....!....:....!....:....!    │
*│      < Искать Ctrl-W >< Отказаться Esc >       │
*└────────────────────────────────────────────────┘


st  = SPACE(40)
st1 = "[X] По имени заказа    [X] По имени покупателя"
ln = 7
wd = MAX(LEN(st), LEN(st1))+6
pic = " Искать Ctrl-W ;\? Отказаться Esc "
l = CEILING((WROWS("")-ln)/2)
c = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW Seek_Lst FROM l, c TO l+ln-1, c+wd-1 SHADOW NONE COLOR SCHEME 13
ACTIVATE WINDOW Seek_Lst
@ 1, 1 TO WROWS()-1, WCOLS()-2
@ 0, 0 SAY PADC("Введите шаблон для поиска", WCOLS()) COLOR SCHEME 5
PRIVATE seek1, seek2, c01, c02, c03, c04, ex, to_go
ex    = 1
seek1 = .T.
seek2 = .T.
all_arms = .F.
to_go = .F.
c01   = FLOOR((WCOLS()-LEN(st1))/2)
c02   = c01+23
c03   = FLOOR((WCOLS()-LEN(st))/2)
c04   = FLOOR((WCOLS()-LEN(pic))/2)

DO WHILE .T.
  @ 4, c03 GET st
  @ 5, c04 GET ex    PICTURE "@*HT \!"+pic
  @ 2, 13  GET all_arms PICTURE "@*C По всем рабочим местам"
  @ 3, c01 GET seek1 PICTURE "@*C По имени заказа"
  @ 3, c02 GET seek2 PICTURE "@*C По имени покупателя"
  = INKEY("M")
  READ CYCLE
  s = MOD(READKEY(),256)
  IF s = 12 .OR. ex = 2
    s = .F.
    EXIT
  ENDIF
  IF EMPTY(st)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Вы забыли указать шаблон для поиска."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF
  IF .NOT. seek1 .AND. .NOT. seek2
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Ни в имени покупателя, ни в заказе... А где же, собственно, искать?!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF
  s = .T.
  EXIT
ENDDO
RELEASE WINDOW Seek_Lst
IF s
  DO Wt_Mess WITH "Ищем..."
  rc_sav = RECNO("LIST_TIT")
  PRIVATE arm_w
  IF UPPER(ext_menu) = "F_COMMON"
    arm_w = "***"
  ELSE
    arm_w = arm
  ENDIF
  st = SYS(15,lwr,ALLTRIM(st))

  SELECT LINK
  ZAP
  SELECT LIST_TIT
  IF all_arms
    GO TOP
  ELSE
    SEEK(arm_w)
  ENDIF
  SCAN REST WHILE IIF(all_arms, .T., LST_ARM = arm_w)
    s = .F.
    IF seek1 .AND. st $ SYS(15,lwr,LST_NAME)
      s = .T.
    ENDIF
    IF seek2 .AND. .NOT. s
      IF FOUND("ACCOUNT")
        s = st $ SYS(15,lwr,ACCOUNT.CUS_NAME)
      ENDIF
      IF .NOT. s .AND. FOUND("BILLS")
        s = st $ SYS(15,lwr,BILLS.CUS_NAME)
      ENDIF
    ENDIF
    IF s
      SELECT LINK
      APPEND BLANK
      REPLACE LNK WITH RECNO("LIST_TIT")
      SELECT LIST_TIT
    ENDIF
  ENDSCAN
  DO Wt_Mess
  IF RECCOUNT("LINK") = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Нет таких списков!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    IF BETWEEN(rc_sav, 1, RECCOUNT())
      GO rc_sav
    ENDIF
  ELSE
    PRIVATE what_do
    what_do = "List"
    DEFINE MENU Seek_Lst IN SCREEN COLOR SCHEME 4
    DEFINE PAD Info   OF Seek_Lst PROMPT "Информация"    KEY Ctrl-A, "F9"
    DEFINE PAD Choose OF Seek_Lst PROMPT "Выбрать"    KEY Ctrl-A, "Enter"
    DEFINE PAD Abort  OF Seek_Lst PROMPT "Отказаться" KEY Ctrl-A, "Esc"

    ON SELECTION PAD Info OF Seek_Lst Do Ch_Mode WITH "Seek_Lst","Info"
    ON SELECTION PAD Choose OF Seek_Lst Do Ex_Proc WITH "Seek_Lst",13
    ON SELECTION PAD Abort  OF Seek_Lst Do Ex_Proc WITH "Seek_Lst",27
    menu_sav = "Seek_Lst"
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("Меню F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU Seek_Lst
    ON KEY LABEL F10 DO F10_Proc WITH "Seek_Lst"
    DO Prep_W_L
    mv = 2-WLROW("Lst_M")
    @ 0, 0 SAY PADC("То, что удалось найти",WCOLS()) COLOR SCHEME 5
    IF mv # 0
      MOVE WINDOW Lst_M BY mv, 0
      MOVE WINDOW Lst_E BY mv, 0
      MOVE WINDOW Lst_I BY mv, 0
    ENDIF
    ord = SET("ORDER")
    SELECT LIST_TIT
    SET ORDER TO 0
    SELECT LINK
    SET RELATION TO LNK INTO LIST_TIT
    GO TOP
    DO WHILE .T.
      ACTIVATE WINDOW Lst_E
      ON KEY LABEL Home GO TOP
      ON KEY LABEL End  GO BOTTOM
      ON KEY LABEL F9   DO Ch_Mode WITH "Seek_Lst","Info"
      ON KEY LABEL ENTER KEYBOARD CHR(23)
      ON KEY LABEL LeftMouse DO M_Proc_1 WITH "Seek_Lst"
      IF seek2
        BROWSE FIELDS f001 = " ":H="", ;
                      LST_FRM.FIRM_CODE:H="",  ;
                      LIST_TIT.LST_NUM:H="", LIST_TIT.CR_DAT:10:H="",   ;
                      LIST_TIT.LST_NAME:H="",    ;
                      f004 = STR(LIST_TIT.R_TOTAL, 13, IIF(LIST_TIT.IS_USD, 4, 2)):H="", ;
                      f003 = IIF(LIST_TIT.IS_USD, "$", "р"):1:H="", ;
                      f_002 = IIF(EMPTY(LIST_TIT.SAL_NUM), " ", "√"):H="", ;
                      f_003 = IIF(EMPTY(LIST_TIT.NOTE), " ", "√"):1:H="", ;
                      DUMMY.F:H="" ;
               FREEZE DUMMY.F  ;
               WHEN V_Foot() ;
               NOMENU NORGRID NOAPPEND NODELETE NOMODIFY NOCLEAR ;
               WINDOW Lst_I IN WINDOW Lst_E
      ELSE
        BROWSE FIELDS f001 = " ":H="", ;
                      LST_FRM.FIRM_CODE:H="",  ;
                      LIST_TIT.LST_NUM:H="", LIST_TIT.CR_DAT:10:H="",   ;
                      LIST_TIT.LST_NAME:H="",     ;
                      f004 = STR(LIST_TIT.R_TOTAL, 10, IIF(LIST_TIT.IS_USD, 4, 2)):H="", ;
                      f003 = IIF(LIST_TIT.IS_USD, "$", "р"):1:H="",     ;
                      f_002 = IIF(EMPTY(LIST_TIT.SAL_NUM), " ", "√"):H="", ;
                      f_003 = IIF(EMPTY(LIST_TIT.NOTE), " ", "√"):1:H="", ;
                      DUMMY.F:H="" ;
               FREEZE DUMMY.F  ;
               NOMENU NORGRID NOAPPEND NODELETE NOMODIFY NOCLEAR ;
               WINDOW Lst_I IN WINDOW Lst_E
      ENDIF
      IF what_do = "Info"
        what_do = "List"
        SELECT LIST_TIT
        DO Lst_Info WITH "Seek_Lst"
        SELECT LINK
        LOOP
      ENDIF
      EXIT
    ENDDO
    ON KEY
    SET RELATION TO
    SELECT LIST_TIT
    SET ORDER TO &ord
    to_go = MOD(READKEY(),256) # 12
    IF LST_ARM # arm_w .AND. to_go
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Это не ваш список!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      to_go = .F.
    ENDIF
    IF .NOT. to_go
      GO rc_sav
    ENDIF
    RELEASE WINDOW Lst_M
    RELEASE WINDOW Lst_E
    RELEASE WINDOW Lst_I
  ENDIF
ENDIF

DO Exit_P WITH "List_M", menu_sav

what_do = "list"
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                      Программа установки режима.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Set_Mode
PARAMETERS ext_menu

DO Entry_P WITH ext_menu, ""

PRIVATE ln, wd, l, c, pic, pic1, ex, l0
PRIVATE sw1, sw2, sw3, sw4, sw5, sw6, sw7, sw8, sw9, sw10, swd1, swd2, swd3, swd4, swd5, sw11
PRIVATE c1, c2, c3

sw1 = mode_spec
sw2 = mode_defA
sw3 = mode_defS
sw4 = mode_defD
sw5 = mode_defO
sw6 = IIF(mode_fil, 2, 1)
sw7 = mode_rnd
sw8 = mode_firm
sw10 = mode_due
sw11 = mode_pr0
sw9 = IIF(mode_ord = "B", 1, 2)
swd1 = mode_d_i
swd2 = mode_d_b
swd3 = mode_d_o
swd4 = mode_d_r
swd5 = mode_tch

pic1 = " Сохранить Ctrl-W ;\? Отказаться Esc "

wd   = 47
ln = 21
l0 = 3
*
*│                 [ ] Долг                  │
*┌───────────────────────────────────────────┐
*│        [ ] Спеццены     [ ] Долг          │
*│ ┌─ Дефецит ─────────────────────────────┐ │
*│ │ [ ] Немедленная обработка             │ │
*│ │ [ ] Спрос                             │ │
*│ │ [ ] Долг                              │ │
*│ │ [ ] Другие фирмы                      │ │
*│ └───────────────────────────────────────┘ │
*│ ┌─ Документы при продаже ───────────────┐ │
*│ │ [ ] Накладная    [ ] Счет-фактура     │ │
*│ │ [ ] Прих. ордер  [ ] Треб. на склад   │ │
*│ └───────────────────────────────────────┘ │
*│    ( ) Упаковки ( ) Автомат ( ) Выкл.     │
*│       [ ] Норма отпуска отключена         │
*│       ( ) Продажа ( ) Реализация          │
*│       [ ] Система обучения включена       │
*│ Тип заказа ( ) Покупка   ( ) Реализация   │
*│  [ ] Показывать позиции с пустыми ценами  │
*│      < OK Ctrl-W > < Отказаться Esc >     │
*└───────────────────────────────────────────┘

l = CEILING((WROWS("")-ln)/2)
c = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW Set_Mode FROM l, c TO l+ln-1, c+wd-1 SHADOW NONE COLOR SCHEME 13
ACTIVATE WINDOW Set_Mode

@ 0, 0 SAY PADC("Режим", WCOLS()) COLOR SCHEME 5
@ 1, 1 TO WROWS()-1, WCOLS()-2
@ WROWS()-11, 3 TO WROWS()-8, WCOLS()-3
IF acc_level < 3
  @ 2, 10 GET sw1 PICTURE "@*C Спеццены "
  @ 2, 27 GET sw10 PICTURE "@*C Долг "
ELSE
  @ 2, 19 GET sw10 PICTURE "@*C Долг "
ENDIF

c1 = FLOOR((WCOLS()-12)/2)
c2 = FLOOR((WCOLS()-25)/2)
c3 = FLOOR((WCOLS()-39)/2)

@ l0, c2-2 TO l0+5, c2+26
@ l0, c2 SAY " Дефицит "

ex = 1
@ l0+1, c2 GET sw2 PICTURE "@*C Немедленная обработка"
@ l0+2, c2 GET sw3 PICTURE "@*C Спрос"
@ l0+3, c2 GET sw4 PICTURE "@*C Долг"
@ l0+4, c2 GET sw8 PICTURE "@*C Другие фирмы"
@ WROWS()-11,  5 GET swd1 PICTURE "@*C Накладная"
@ WROWS()-11, 22 GET swd2 PICTURE "@*C Счет-фактура"
@ WROWS()-10,  5 GET swd3 PICTURE "@*C Прих. ордер"
@ WROWS()-10, 22 GET swd4 PICTURE "@*C Треб. на склад"
@ WROWS()-8, c3 GET sw7 PICTURE "@*RH \Упаковки ;Автомат ;Выкл."
@ WROWS()-7,  9 GET round_1 PICTURE "@*C Норма отпуска отключена"
@ WROWS()-6, c3+6 GET sw6  PICTURE "@*RH \Продажа;Реализация"
@ WROWS()-5,  9 GET swd5 PICTURE "@*C Система обучения включена"
@ WROWS()-4,  3 SAY "Тип заказа"
@ WROWS()-4, 15 GET sw9 PICTURE "@*RH \Покупка  ;Реализация"
@ WROWS()-3, 4 GET sw11  PICTURE "@*C Показывать позиции с пустыми ценами"
@ WROWS()-2, c3 GET ex  PICTURE "@*HT \!"+pic1

READ CYCLE

IF ex = 1      && .AND. MOD(READKEY(),256) # 12
  mode_spec = sw1
  mode_defA = sw2
  mode_defS = sw3
  mode_defD = sw4
  mode_fil  = sw6 = 2
  mode_rnd  = sw7
  mode_firm = sw8
  mode_due  = sw10
  mode_d_i = swd1
  mode_d_b = swd2
  mode_d_o = swd3
  mode_d_r = swd4
  mode_pr0 = sw11
  mode_ord = IIF(sw9 = 1, "B", "R")
  mode_tch = swd5
  SAVE TO (prmo_path+"MODE") ALL LIKE mode_*
ENDIF

RELEASE WINDOW Set_Mode
DO Exit_P WITH ext_menu, ""

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                      Пeчать накладной.                                ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Prnt_Inv
PARAMETER d_tp, is_act
PRIVATE mss

IF EMPTY(LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Прежде надо это продать..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

PUSH KEY CLEAR
DEFINE WINDOW W_H_000 FROM 0,0 TO 0,WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW W_H_000

IF is_act
  DO P_Act_D WITH LIST_TIT.FIRM, LIST_TIT.SAL_NUM, LIST_TIT.SAL_DATE
ELSE 
  IF EMPTY(LEFT(LIST_TIT.SAL_NUM,1))
    DO P_Inv_F WITH VAL(ALLTRIM(LIST_TIT.SAL_NUM)), LIST_TIT.SAL_DATE
  ELSE
  *  DO P_Inv WITH LIST_TIT.FIRM, LIST_TIT.SAL_NUM, LIST_TIT.SAL_DATE, d_tp
    DO Prn_Inv WITH LIST_TIT.FIRM, LIST_TIT.SAL_NUM, LIST_TIT.SAL_DATE	&&, d_tp, (Sale_Mode = 'MEGA') && NEW 02.04.2002
  ENDIF
ENDIF
CLEAR WINDOW W_H_000
POP KEY

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                      Пeчать приходного ордера.                        ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Prnt_Ord
PARAMETERS ext_menu

PRIVATE mss, sel_rem, t_ord, n_ord, d_ord, dummy_cli, dummy_sd, dummy_sm

dummy_cli = ""

IF EMPTY(LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Прежде надо это продать..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

DO Entry_P WITH ext_menu

sel_rem = SELECT()
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS SAL_0 AGAIN
IF .NOT. SEEK(LIST_TIT.FIRM+LEFT(DTOS(LIST_TIT.SAL_DATE),4)+LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Доступ к приходному ордеру ( если он и был ) утрачен."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  DO Exit_P WITH ext_menu
  RETURN
ENDIF

IF EMPTY(SAL_0.PAY_DOC)          && KASSA = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Оплата с депозита..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  DO Exit_P WITH ext_menu
  RETURN
ENDIF

t_ord = SAL_0.PAY_TYPE
n_ord = SAL_0.PAY_DOC
d_ord = SAL_0.PAY_DATE
USE
DO P_Money WITH LIST_TIT.FIRM, t_ord, n_ord, d_ord

SELECT (sel_rem)
DO Exit_P WITH ext_menu
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                      Пeчать счета - фактуры.                          ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Prnt_BF
PARAMETERS ext_menu

PRIVATE mss, sel_rem, t_ord, n_ord, d_ord, dummy_cli, dummy_sd, dummy_sm

dummy_cli = ""

IF EMPTY(LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Прежде надо это продать..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

DO Entry_P WITH ext_menu

sel_rem = SELECT()
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS SAL_0 AGAIN
IF .NOT. SEEK(LIST_TIT.FIRM+LEFT(DTOS(LIST_TIT.SAL_DATE),4)+LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Доступ к документу ( если он и был ) утрачен."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  DO Exit_P WITH ext_menu
  RETURN
ENDIF

t_ord = SAL_0.NEW_TYPE
n_ord = SAL_0.NEW_DOC
d_ord = SAL_0.DOC_DATE
USE
*DO P_Bill_F WITH LIST_TIT.FIRM, n_ord, d_ord, t_ord
DO P_Bill_F WITH LIST_TIT.FIRM, n_ord, d_ord, t_ord, (Sale_Mode = 'MEGA') && NEW 02.04.2002

SELECT (sel_rem)
DO Exit_P WITH ext_menu
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                 Установить код для клиента.                           ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Mk_Cli0

PRIVATE ln, wd, is_new, ex, c, s1

ln = 8
wd = LEN(tmp_name)+8
is_new = .T.
ex = 1

DO D_W WITH "Cli002", ln, wd, "Покупатель", 1
c = FLOOR((WCOLS()-32)/2)
@ 3, 4 GET tmp_name
@ 5, c GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE VALID Tst_Cli0()

RELEASE WINDOW Cli002

RETURN ex = 1

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа выхода из экранной формы.                    ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Tst_Cli0

PRIVATE mss, ln

IF EMPTY(tmp_name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Клиент с пустым именем? Не может быть!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF TYPE("bound_l") = "N" .AND. TYPE("bound_r") = "N"
  IF BETWEEN(SAL_0.KASSA,bound_l,bound_r)
    dummy_cli = tmp_name
  ENDIF
ENDIF

RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа установки обработчика F1.                    ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Set_F1

ON KEY LABEL Ctrl+Enter DO Fill_Nm

RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа очистки обработчика F1.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Clr_F1

ON KEY LABEL Ctrl+Enter

RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║          Программа переноса имени клиента в имя списка.               ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Fill_Nm

left_p = LEFT(nm_w,LEN(left_p))
right_p = SUBSTR(nm_w,LEN(left_p)+1, LEN(right_p) )
SHOW GET left_p
SHOW GET right_p
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Программа копирования списка.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Copy_Lst
PRIVATE s, old_usd, new_usd, wd, wa

IF File_O(tmpo_path+"COPY_L.DBF")
  DELETE FILE (tmpo_path+"COPY_L.DBF")
ENDIF

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_CODE
SEEK STR(LIST_TIT.LST_NUM,6)
COPY REST FOR .NOT. DELETED() WHILE LIST_NUM = STR(LIST_TIT.LST_NUM,6) ;
     TO (tmpo_path+"COPY_L.DBF")
SELECT LIST_TIT
old_usd = IS_USD
IF Corr_Tit("new")
  new_usd = IS_USD
  SELECT 0
  USE (tmpo_path+"COPY_L.DBF")
  s = 0
  SCAN
    REPLACE LIST_NUM WITH STR(LIST_TIT.LST_NUM,6), QNT_REQ WITH 0, QNT_DUE WITH 0
    IF old_usd # new_usd
      IF new_usd
        IF SAL_PRICE = DEF_PRICE
          STORE ROUND(DEF_PRICE/kurs,4) TO wd, wa
        ELSE
          wd = ROUND(DEF_PRICE/kurs,4)
          wa = ROUND(SAL_PRICE/kurs,4)
        ENDIF
        REPLACE SAL_PRICE WITH 0, USD_DEF WITH wd, USD_PRICE WITH wa
        s = s+QNT*USD_PRICE
      ELSE
        IF USD_PRICE = USD_DEF
          STORE Clc_Rub(USD_PRICE) TO wd, wa
        ELSE
          wd = Clc_Rub(USD_DEF)
          wa = Clc_Rub(USD_PRICE)
        ENDIF
        REPLACE USD_PRICE WITH 0, DEF_PRICE WITH wd, SAL_PRICE WITH wa
        s = s+ROUND(QNT*SAL_PRICE,2)
      ENDIF
    ENDIF
  ENDSCAN
  USE
  SELECT LIST_DET
  APPEND FROM (tmpo_path+"COPY_L.DBF")
  USE
  SELECT LIST_TIT
  REPLACE R_TOTAL WITH s
  DELETE FILE (tmpo_path+"COPY_L.DBF")
ELSE
  SELECT LIST_DET
  USE
  SELECT LIST_TIT
ENDIF

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║          Программа установки/снятия пометки о наличии чека.           ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Set_Check
PRIVATE sel_sav, mss, k

IF EMPTY(LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Давайте прежде продадим..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF LIST_TIT.SAL_DATE # DATE()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Давно продали..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

sel_sav = SELECT()
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM
IF .NOT. SEEK(LIST_TIT.FIRM+LEFT(DTOS(LIST_TIT.SAL_DATE),4)+LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не удается найти записи о продаже..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
  RETURN
ENDIF

IF PAY_TYPE # " "
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Для этой продажи нельзя пробивать чек..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
  RETURN
ENDIF

k = FIRM+PAY_TYPE+LEFT(DTOS(PAY_DATE),4)+PAY_DOC
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM AGAIN ALIAS WWW_9217
IF .NOT. SEEK(k)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не удается найти записи о поступлении денег..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
  RETURN
ENDIF
IF ALLTRIM(ENTER_DOC) # "Чек"
  REPLACE ENTER_DOC WITH "Чек"
  k = LEFT(LIST_TIT.LST_NAME,FSIZE("LST_NAME","LIST_TIT")-1)+"Ч"
ELSE
  REPLACE ENTER_DOC WITH "   "
  k = LEFT(LIST_TIT.LST_NAME,FSIZE("LST_NAME","LIST_TIT")-1)+" "
ENDIF
USE
SELECT LIST_TIT
REPLACE LST_NAME WITH k
SELECT (sel_sav)
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║        Программа выдачи на экран примечания из файла CLI_INFO.        ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE View_Note
PRIVATE sel_sav, win_name

IF LIST_TIT.CLI_CODE = 0
  RETURN
ENDIF
win_name = PROGRAM()

sel_sav = SELECT()
SELECT 0
USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE ALIAS V_NOTE AGAIN
IF SEEK(LIST_TIT.CLI_CODE)
  IF .NOT. EMPTY(NOTE)
    DO D_Win_N WITH 4, 70, ALLTRIM(ACCOUNT.CUS_NAME)
    @ 1, 3 SAY " Примечание "
    @ 2, 3 SAY NOTE
    = INKEY(0,"MH")
  ENDIF
ENDIF
USE
RELEASE WINDOW (win_name)

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Программа вывода доп. информации.                  ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE V_Foot
PRIVATE vl0, vl1, vl2, vl31, vl32, vl4, vl5, vl6, vl7, vl8, vl9, vla, vld

ACTIVATE WINDOW Lst_M BOTTOM

*
*    Готовность
IF EMPTY(LIST_TIT.DATE_REQ)
  vl0 = " Не собирать! "
ELSE
  vl0 = " Собрать к "+DTOC(LIST_TIT.DATE_REQ)+" "+LIST_TIT.TIME_REQ+" "
ENDIF
*
*    Клиент
IF FOUND("ACCOUNT")
  vl1 = ACCOUNT.CUS_NAME
ELSE
  vl1 = SPACE(50)
ENDIF
*
*  Почта
vl2 = IIF(EMPTY(LIST_TIT.MAIL), " ", "X")

*
*  Счет
IF EMPTY(LIST_TIT.BILL_NUM)
  vl31 = ""
  vl32 = " "
ELSE
  vl31 = LEFT(LIST_TIT.BILL_NUM,4)+"-"+ALLTRIM(SUBSTR(LIST_TIT.BILL_NUM,5))+ ;
         "("+LIST_TIT.BILL_FIRM+")"
  vl32 = IIF(BILLS.STATUS = "Оплачено  ", "X", " ")
ENDIF
vl31 = PADR(vl31,16)

*
*  Накладная
IF EMPTY(LIST_TIT.SAL_NUM)
  vl4 = ""
ELSE
  vl4 = LEFT(LIST_TIT.SAL_NUM,4)+"-"+ALLTRIM(SUBSTR(LIST_TIT.SAL_NUM,5))
ENDIF
vl4 = PADR(vl4,11)

*
*  Скидка
vl5 = IIF(EMPTY(LIST_TIT.IS_DIS), " ", "X")

*
*  Спец.цена
vl6 = IIF(EMPTY(LIST_TIT.IS_SPEC), " ", "X")

*
*    Утверждение
IF .NOT. EMPTY(LIST_TIT.SAL_NUM) .OR. ;
         EMPTY(LIST_TIT.IS_DIS) .AND. EMPTY(LIST_TIT.IS_SPEC)
  vl7 = "Не требуется   "
ELSE
  IF EMPTY(LIST_TIT.WHO_LET)
    vl7 = "Нет            "
  ELSE
    vl7 = "Есть           "
  ENDIF
ENDIF

*
*     Доставка
PRIVATE d_sav
vld = " "
d_sav = SELECT()
SELECT 0
USE (base_path+"DELIVERY") ORDER TAG LST_NUM ALIAS DLV_2419 AGAIN
IF SEEK(LIST_TIT.LST_NUM)
  vld = "X"
ENDIF
USE
SELECT (d_sav)

*
*     Бронь
vl8 = IIF(.NOT. EMPTY(LIST_TIT.SAL_NUM) .OR. EMPTY(LIST_TIT.ORD_NUM), " ", "X")

*
*     Собрана
vl9 = IIF(EMPTY(LIST_TIT.SAL_NUM) .AND. EMPTY(LIST_TIT.NOTE_STO), "X", " ")
IF EMPTY(vl8)
  vl9 = " "
ENDIF

*
*     Согласована
vla = IIF(EMPTY(LIST_TIT.SAL_NUM) .AND. EMPTY(LIST_TIT.NOTE_SAL), "X", " ")
IF EMPTY(vl8)
  vla = " "
ENDIF

*Клиент ____:____!____:____!____:____!____:____!____:____!"
*Почта [ ]   Счет ____:____!_(---) Оплачен [ ]   Накладная ____:____!_"
*  Скидка [ ]     Спец.цена [ ]   Утверждение _______________"
*Доставка [ ]  Бронь [ ]  Собрана [ ]  Согласована с клиентом [ ]"

@ WROWS()-5,  3 SAY PADC(vl0, WCOLS()-6, "─")
@ WROWS()-4,  2 SAY vl1+" Адр. "+ListAddr(LIST_TIT.LST_NUM, 18)
@ WROWS()-3, 12 SAY vl2
@ WROWS()-3, 22 SAY vl31
@ WROWS()-3, 48 SAY vl32
@ WROWS()-3, 63 SAY vl4
@ WROWS()-2, 20 SAY vl5
@ WROWS()-2, 38 SAY vl6
@ WROWS()-2, 55 SAY vl7
@ WROWS()-1, 18 SAY vld
@ WROWS()-1, 29 SAY vl8
@ WROWS()-1, 42 SAY vl9
@ WROWS()-1, 70 SAY vla

ACTIVATE WINDOW (ALIAS())

RETURN .T.
*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Is_Dubl      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Проверка: нет ли такого клиента.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12/02/1998 ──┘
PROCEDURE Is_Dubl
PARAMETERS c_code

PRIVATE s_save, mss, l_n, a_n, dt, l_list, n_list
IF .NOT. new_w .OR. c_code = 0
  RETURN .T.
ENDIF

s_save = SELECT()
SELECT 0
USE (base_path+"ARM_LST") AGAIN ORDER TAG ARM_C ALIAS A8C02
SELECT 0
USE (base_path+"LIST_TIT") AGAIN ORDER TAG LST_NUM DESCENDING ALIAS L8C02
LOCATE FOR CLI_CODE = c_code
n_list = 0
DO WHILE FOUND()
  n_list = n_list+1
  DIMENSION l_list(n_list)
  IF LST_ARM = "***"
    l_list(n_list) = "Общая область   "
  ELSE
    = SEEK(LST_ARM, "A8C02")
    l_list(n_list) = A8C02.ARM_N
  ENDIF
  l_list(n_list) = STR(LST_NUM,6)+" "+l_list(n_list)
  CONTINUE
ENDDO
USE
SELECT A8C02
USE
SELECT (s_save)
RETURN V_List()

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура W_Course     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Расчет курса продажи.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12/03/1998 ──┘
PROCEDURE W_Course

PRIVATE s_sav, k_w

s_sav = SELECT()
DO CASE
CASE EMPTY(LIST_TIT.SAL_NUM)
  k_w = ALLTRIM(STR(kurs,9,4))
  k_w = "Текущий курс "+k_w
CASE .NOT. EMPTY(LIST_TIT.SAL_NUM) .AND. .NOT. LIST_TIT.IS_USD
  SELECT 0
  USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS W_COURSE AGAIN
  SEEK LEFT(DTOS(LIST_TIT.SAL_DATE),4)+LIST_TIT.SAL_NUM
  k_w = Get_Curs("CURS", "", LIST_TIT.SAL_DATE, DOC_TIME)
  k_w = ALLTRIM(STR(k_w,9,3))
  k_w = "Курс в момент продажи "+k_w
  USE
OTHERWISE
  SELECT 0
  USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS W_COURSE AGAIN
  SEEK LIST_TIT.FIRM+LEFT(DTOS(LIST_TIT.SAL_DATE),4)+LIST_TIT.SAL_NUM
  IF TOTAL_D # 0
    k_w = ROUND(TOTAL_R/TOTAL_D,3)
  ELSE
    k_w = 0
  ENDIF
  k_w = ALLTRIM(STR(k_w,9,3))
  USE
  k_w = "Курс продажи "+k_w
ENDCASE

SELECT (s_sav)

PRIVATE wd, l0, c0
wd = MAX(LEN(k_w),21)+8
l0 = FLOOR((WROWS("")-6)/2)
c0 = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW W_COURSE FROM l0, c0 TO l0+5, c0+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW W_COURSE
DO Sun_Bord WITH 0, 1, WROWS()-1, WCOLS()-2
@ 2, 2 SAY PADC(k_w, WCOLS()-4)
@ 4, 2 SAY PADC("Нажмите что-нибудь...", WCOLS()-4)
= INKEY(0, "MH")
RELEASE WINDOW W_COURSE

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_List       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Просмотр перечня списков.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12/17/1998 ──┘
PROCEDURE V_List

IF n_list = 0
  RETURN .T.
ENDIF

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

*┌─────────────────────────────────────────────────┐
*│                                                 │
*│                                                 │
*│                                                 │
*│                                                 │
*│                                                 │
*│                                                 │
*│                                                 │
*│                                                 │
*│                                                 │
*│                                                 │
*│                                                 │
*└─────────────────────────────────────────────────┘

PRIVATE ex, ln, wd
ex = 1
ln = MIN(ALEN(l_list)+4, 20)
wd = LEN(l_list(1))+4
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, "Клиент уже есть в списках:"
@ WROWS()-1, 0 SAY PADC("Enter-Выход, Esc-Отказ", WCOLS())

*------------------------------------------------------------------------
*      Ввод полей бланка
*
ln = WROWS()-4
ex = 1
DO WHILE .T.
  ex = MAX(1, ex)
  ex = MIN(ALEN(l_list), ex)
  @ 1, 1 MENU l_list, ALEN(l_list), ln
  READ MENU TO ex
  IF READKEY() % 256 = 12
    ex = 2
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    ex = 1
    EXIT
  ENDIF
ENDDO

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура W_NSP        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Сумма с НСП                                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12/03/1998 ──┘
PROCEDURE W_NSP

PRIVATE k_w

k_w = ROUND(LIST_TIT.R_TOTAL*(100+GetNalog(LIST_TIT.FIRM))/100, 3)
k_w = "Сумма с НСП "+ALLTRIM(STR(k_w,15,4))+IIF(LIST_TIT.IS_USD, " У Е", " руб")
PRIVATE wd, l0, c0
wd = LEN(k_w)+8
l0 = FLOOR((WROWS("")-6)/2)
c0 = FLOOR((WCOLS("")-wd)/2)
DEFINE WINDOW W_COURSE FROM l0, c0 TO l0+5, c0+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW W_COURSE
DO Sun_Bord WITH 0, 1, WROWS()-1, WCOLS()-2
@ 2, 2 SAY PADC(k_w, WCOLS()-4)
@ 4, 2 SAY PADC("Нажмите что-нибудь...", WCOLS()-4)
= INKEY(0, "MH")
RELEASE WINDOW W_COURSE

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Пeредача накладной для STORE.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Send_Inv
PRIVATE mss

IF EMPTY(LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Прежде надо это продать..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

PUSH KEY CLEAR
IF EMPTY(LEFT(LIST_TIT.SAL_NUM,1))
  f_name = S_Inv(VAL(ALLTRIM(LIST_TIT.SAL_NUM)), LIST_TIT.SAL_DATE)
  IF TYPE("f_name") = "C"
    path_w = ""
    path_w = Int_Dir(path_w, "Укажите путь", 0)
    DELETE FILE (path_w+f_name+"_T.DBF")
    DELETE FILE (path_w+f_name+".DBF")
    DELETE FILE (path_w+f_name+".FPT")
    COPY FILE (tmpo_path+f_name+"_T.DBF") TO (path_w+"RYN_M_T.DBF")
    COPY FILE (tmpo_path+f_name+".DBF") TO (path_w+"RYN_M.DBF")
    COPY FILE (tmpo_path+f_name+".FPT") TO (path_w+"RYN_M.FPT")
    DELETE FILE (path_w+f_name+"_T.DBF")
    DELETE FILE (path_w+f_name+".DBF")
    DELETE FILE (path_w+f_name+".FPT")
  ENDIF
ELSE
  DO Send_Sto WITH LIST_TIT.FIRM, LIST_TIT.SAL_NUM, LIST_TIT.SAL_DATE
ENDIF
POP KEY

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Time    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Собрать к...                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10.01.2000 ──┘
PROCEDURE Prep_Time

WAIT WIND "Операция отменена" NOWAIT
RETURN

IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Какая подборка? Список уже продан!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

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

*┌──────────────────────────────────┐
*│ Собрать к ДД.ММ.ГГГГ(F1) ЧЧч ММм │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, p, d, h, m, f1_butt, clr_butt, tm
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
DO Get_Sto WITH LIST_TIT.ORD_QUEUE
IF EMPTY(LIST_TIT.DATE_REQ) .AND. LIST_TIT.ORD_NUM = 0
  IF t_stock = 0
    d = {}
    tm = ""
  ELSE
    d = DATE()
    tm = TIME()
    DO Add_T WITH d, tm, t_stock+2, d, tm
  ENDIF
  h = VAL(ALLTRIM(SUBSTR(tm,1,2)))
  m = VAL(ALLTRIM(SUBSTR(tm,4,2)))
ELSE
  d = LIST_TIT.DATE_REQ
  h = VAL(ALLTRIM(SUBSTR(LIST_TIT.TIME_REQ,1,2)))
  m = VAL(ALLTRIM(SUBSTR(LIST_TIT.TIME_REQ,4,2)))
ENDIF
f1_butt = 1
clr_butt = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
ON KEY LABEL F1 DO Set_Date
DO Prp_Nav_2
DO D_Win_N WITH 5, 38, "Список N "+ALLTRIM(STR(LIST_TIT.LST_NUM))
@ 2, 3 SAY "Собрать к           (F1)   ч   м"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 13 GET d PICTURE "@D"
*@ 2, 24 GET f1_butt PICTURE "@*N F1" VALID Set_Date()
@ 2, 28 GET h PICTURE "@Z 99"
@ 2, 32 GET m PICTURE "@Z 99"
*@ 2, 38 GET clr_butt PICTURE "@*N Не собирать" VALID Clr_Ready()
@ 3,  3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE VALID Tst_Bl()

IF ex = 1
  IF EMPTY(d)
    tm = "        "
  ELSE
    tm = RIGHT(STR(100+h),2)+":"+RIGHT(STR(100+m),2)+":00"
  ENDIF
  REPLACE LIST_TIT.DATE_REQ WITH d, LIST_TIT.TIME_REQ WITH tm
  ex = SELECT()
  SELECT 0
  USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS OT_0110
  SEEK STR(LIST_TIT.LST_NUM,6)
  REPLACE REST WHILE LIST_NUM = STR(LIST_TIT.LST_NUM,6) ;
          REQ_DATE WITH d, REQ_TIME WITH tm
  USE
  SELECT (ex)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clr_Ready    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Очистка даты и времени.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.01.2000 ──┘
PROCEDURE Clr_Ready

d = {}
h = 0
m = 0
SHOW GET d
SHOW GET m
SHOW GET h

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Date     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Установка даты по календарю.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.01.2000 ──┘
PROCEDURE Set_Date

d = Get_Date(d)
SHOW GET d
IF EMPTY(d)
  h = 0
  m = 0
  SHOW GET h
  SHOW GET m
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Bl       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Проверка корректности бланка.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.01.2000 ──┘
PROCEDURE Tst_Bl

PRIVATE tm_w, mss

IF ex = 2
  RETURN .T.
ENDIF

tm_w = RIGHT(STR(100+h),2)+":"+RIGHT(STR(100+m),2)+":00"
IF EMPTY(d)
  RETURN .T.
ENDIF

IF .NOT. BETWEEN(tm_w, b_stock, e_stock) .OR. .NOT. BETWEEN(m, 0, 59)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы некорректно указали время, или склад в это время не работает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF .NOT. EMPTY(d) .AND. hard_sale
  DO Get_Sto WITH LIST_TIT.ORD_QUEUE
  IF Delta_T(d, tm_w, DATE(), TIME()) < t_stock
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Склад не в состоянии собрать заявку к этому времени!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_Nt      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                              Примечание.                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.01.2000 ──┘
PROCEDURE Modi_Nt

IF .NOT. EMPTY(STO_MARK) .OR. .NOT. EMPTY(CHECK_S)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Заявка уже в наборе или собрана!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
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

*┌──────────────────────────────────────────┐
*│ ....:....!....:....!....:....!....:....! │
*│                                          │
*│     < OK Ctrl-W > < Отказаться Esc >     │
*└──────────────────────────────────────────┘

PRIVATE ex, nt
ex = 1
nt = NOTE
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 46, "Примечание"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 3 GET nt
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  REPLACE NOTE WITH nt
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Dlv_S        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Запуск программы доставки.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.03.2000 ──┘
PROCEDURE Dlv_S

PRIVATE s_sav, d_num, d_date, let_modi

let_modi = .T.
s_sav = SELECT()
SELECT 0
USE (base_path+"DELIVERY") ORDER TAG LST_NUM ALIAS DLV_0327 AGAIN
IF SEEK(LIST_TIT.LST_NUM)
  d_num = DOC_NUM
  d_date = DOC_DATE
  let_modi = EMPTY(READ_DATE) .AND. ;
             EMPTY(CALL_DATE) .AND. ;
             EMPTY(GAVE_DATE) .AND. ;
             EMPTY(TERM_DATE)
ELSE
  d_num = LIST_TIT.LST_NUM
  d_date = DATE()
ENDIF
USE
SELECT (s_sav)

IF let_modi
  DO Delivery WITH d_num, d_date, .T.
ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Эту заявку уже нельзя корректировать!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ent_Firm     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Вход в поле предприятия.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.07.2000 ──┘
PROCEDURE Ent_Firm

PRIVATE c_access, s_sav

IF EMPTY(nm_w)
  c_access = 0
ELSE
  c_access = ACCOUNT.CUS_CODE
  IF .NOT. Leg_Firm(ACCOUNT.CUS_CODE, f_c000)
    f_c000 = "   "
    f_n000 = SPACE(LEN(f_n000))
    b_c000 = "   "
    b_n000 = SPACE(LEN(f_n000))
    SHOW GET f_c000
    SHOW GET f_n000
    SHOW GET b_n000
  ENDIF
ENDIF

ON KEY LABEL F1 DO Ch0_Firm

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Exi_Firm     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Выход из поля пердприятия.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.07.2000 ──┘
PROCEDURE Exi_Firm

PRIVATE s_sav, mss

IF READKEY() % 256 = 12
  RETURN .T.
ENDIF

f_c000 = CHRTRAN(f_c000, shab1, shab2)
s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_0707 AGAIN
IF .NOT. SEEK(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Некорректный код предприятия"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN .F.
ENDIF
f_n000 = B_NAME
SHOW GET f_n000
lt_wchk = LET_WCHK
IF .NOT. lt_wchk
  w_check = DEF_WCHK
  SHOW GET w_check
ENDIF
USE
IF .NOT. EMPTY(nm_w)
  IF .NOT. Leg_Firm(ACCOUNT.CUS_CODE, f_c000)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Данное предприятие противопоказано выбранному Вами клиенту!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT (s_sav)
    RETURN .F.
  ENDIF
ENDIF
USE (base_path+"BANKS") ORDER TAG NAME ALIAS B_0707 AGAIN
IF .NOT. SEEK(f_c000+b_c000)
  IF SEEK(f_c000)
    b_c000 = BANK_CODE
    b_n000 = B_NAME
  ELSE
    b_c000 = SPACE(LEN(b_c000))
    b_n000 = SPACE(LEN(b_n000))
  ENDIF
  SHOW GET b_n000
ENDIF
USE
SELECT (s_sav)

ON KEY LABEL F1
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch0_Firm     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Выбор предприятия                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07/07/2000 ──┘
PROCEDURE Ch0_Firm

DO G_Firm WITH f_c000, f_n000, IIF(EMPTY(nm_w), .F., ACCOUNT.CUS_CODE)

SHOW GET f_c000
SHOW GET f_n000

RETURN


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура SetBank      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выбор банка                                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07/07/2000 ──┘
PROCEDURE SetBank

IF .NOT. b_sw000
  RETURN .T.
ENDIF

b_sw000 = .F.
SHOW GET b_sw000
DO C_Bank WITH b_c000, b_n000, f_c000
SHOW GET b_n000

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура ModiNote     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Коррекция примечания                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 29.01.2001 ──┘
PROCEDURE ModiNote
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

*┌──────────────────────────────────────────────────────────────────────────┐
*│┌─ Примечание ───────────────────────────────────────────────────────────┐│
*││....:....!....:....!....:....!....:....!....:....!....:....!....:....!..││
*││....:....!....:....!....:....!....:....!....:....!....:....!....:....!..││
*││....:....!....:....!....:....!....:....!....:....!....:....!....:....!..││
*││....:....!....:....!....:....!....:....!....:....!....:....!....:....!..││
*││....:....!....:....!....:....!....:....!....:....!....:....!....:....!..││
*│└────────────────────────────────────────────────────────────────────────┘│
*│                     < OK Ctrl-W > < Отказаться Esc >                     │
*└──────────────────────────────────────────────────────────────────────────┘

PRIVATE ex, tmpNote
tmpNote = LIST_TIT.NOTE
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 11, 78, "Список № "+ALLTRIM(STR(LIST_TIT.LST_NUM))+" "+ALLTRIM(LIST_TIT.LST_NAME)
DO Sun_Bord WITH  2,  2,  8, 75, " Примечание "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 3 EDIT tmpNote SIZE 5, 72
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  REPLACE LIST_TIT.NOTE WITH tmpNote
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура ModiAddr     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Коррекция адреса                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 29.01.2001 ──┘
PROCEDURE ModiAddr
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

*┌──────────────────────────────────┐
*│         ....:....!....:.         │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, tmpAddr
tmpAddr = LIST_TIT.ADDRESS
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 5, 38, "Адрес"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 11 GET tmpAddr
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  REPLACE LIST_TIT.ADDRESS WITH tmpAddr
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Q        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Установка типа очереди.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.05.2001 ──┘
PROCEDURE Get_Q

queuetype = Queue(queuetype)

DO Put_Own WITH "QUEUE", queuetype

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Dlv     Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Список с доставками.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 19.04.2002 ──┘
PROCEDURE Prep_Dlv
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
DELETE FILE(tmpo_path+"TMP_DLV.DBF")
DELETE FILE(tmpo_path+"TMP_DLV.CDX")
CREATE DBF(tmpo_path+"TMP_DLV");
        (D_NUM  C(10), ;
         D_DATE D,     ;
         L_NUM  N( 6), ;
         L_DATE D,     ;
         L_NAME C(32), ;
         VAL    C( 1), ;
         L_SUM  N(15,4))
INDEX ON L_NUM TAG L_NUM

SElECT 0
USE (base_path+"DELIVERY") ORDER TAG LST_NUM ALIAS D_2411 AGAIN

SElECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_2419 AGAIN
SCAN FOR LST_ARM = arm
  IF SEEK (LST_NUM, "D_2411")
    SELECT TMP_DLV
    APPEND BLANK
    REPLACE D_NUM  WITH D_2411.DOC_NUM,  ;
            D_DATE WITH D_2411.DOC_DATE, ;
            L_NUM  WITH LT_2419.LST_NUM, ;
            L_DATE WITH LT_2419.CR_DAT,  ;
            L_NAME WITH LT_2419.LST_NAME,;
            VAL    WITH IIF(LT_2419.IS_USD,"$","Р") ;
            L_SUM  WITH LT_2419.R_TOTAL
  ENDIF
  SELECT LT_2419
ENDSCAN  
USE        

SELECT D_2411
USE

SELECT 0
DO Use_Dummy

SELECT TMP_DLV
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"По Вашим спискам нет ни одной доставки!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN .F.
ENDIF
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View_Dlv OF (menu_name) PROMPT "Печать доставки" KEY Ctrl-A, "Enter"
ON SELECTION PAD View_Dlv OF (menu_name) DO Sw_Mode WITH "prn001"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 69

DO D_Wins WITH ln, wd, "Списки с доставками", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS L_NUM:H="",;
           L_DATE:H="",;
           L_NAME:H="",;
           VAL:H="",;
           L_SUM:H="",;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "prn001")
    ENDIF

  CASE what_do = "prn001"    && Печать доставки
    DO Delivery WITH D_NUM, D_DATE
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
DELETE FILE(tmpo_path+"TMP_DLV.DBF")
DELETE FILE(tmpo_path+"TMP_DLV.CDX")

SELECT (s_sav)

RETURN