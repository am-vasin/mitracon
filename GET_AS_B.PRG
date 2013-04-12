*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║               Прием заявки на бронирование от АС.                     ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Get_AS_B

PRIVATE get_path, mss, l_num, zak_arm

get_path = Where_F("Где ищем заявку")
IF get_path = CHR(0)
  RETURN
ENDIF
IF .NOT. File_O(get_path+"DEBT_OUT.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ну никак не удается найти там заявки."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

USE (get_path+"DEBT_OUT")
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Файл с заявкой пуст!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF
SELECT 0
USE LIST_TIT ORDER TAG LST_ARM

IF EMPTY(DEBT_OUT.N_SP)  && Нет номера списка - создаем!
  SELECT 0
  USE PARMS
  = FLOCK()
  l_num = LAST_LST+1
  REPLACE LAST_LST WITH l_num
  USE
  SELECT LIST_TIT
  APPEND BLANK
  REPLACE LST_ARM  WITH DEBT_OUT.N_S, ;
          LST_NUM  WITH l_num,  ;
          CR_DAT   WITH DATE(), ;
          LST_NAME WITH "Список отдела заказов", ;
          WHO_CR   WITH user,   ;
          WHERE_CR WITH arm
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Создан список N "+ALLTRIM(STR(l_num,10))
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
ELSE
  IF .NOT. SEEK(DEBT_OUT.N_S+STR(DEBT_OUT.N_SP,6))
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Не найден список с номером "+ALLTRIM(STR(DEBT_OUT.N_SP,10))
    mss(3) = ""
    DO Out_Mess WITH 5, "mss"
    CLOSE DATABASES
    RETURN
  ENDIF
  l_num = DEBT_OUT.N_SP
ENDIF
zak_arm = DEBT_OUT.N_S
SELECT 0
USE ORD_D
DELETE FILE (tmpo_path+"ORD_ZAK.DBF")
COPY STRUCTURE TO (tmpo_path+"ORD_ZAK.DBF")
USE (tmpo_path+"ORD_ZAK.DBF")
SELECT 0
USE STOCK ORDER TAG CODE ALIAS STO_COD
SELECT 0
USE STOCK ORDER TAG NAME ALIAS STO_NAM AGAIN
SELECT 0
USE LIST_DET ORDER TAG L_NAME ALIAS LST_NAM
SELECT 0
USE LIST_DET ORDER TAG L_CODE ALIAS LST_COD AGAIN

SELECT DEBT_OUT
PRIVATE a_stru, n_fields, i, arr_in, arr_log
IF .NOT. File_O(log_path+"AS_LOG.DBF")   && Нет журнала - создадим!
  n_fields = AFIELDS(a_stru)
  n_fields = n_fields+1
  DIMENSION a_stru(n_fields,4)
  FOR i = 1 TO n_fields-1
    a_stru(n_fields-i+1,1) = a_stru(n_fields-i,1)
    a_stru(n_fields-i+1,2) = a_stru(n_fields-i,2)
    a_stru(n_fields-i+1,3) = a_stru(n_fields-i,3)
    a_stru(n_fields-i+1,4) = a_stru(n_fields-i,4)
  ENDFOR
  a_stru(1,1) = "GET_DAT"
  a_stru(1,2) = "D"
  a_stru(1,3) = 8
  a_stru(1,4) = 0
  SELECT 0
  CREATE DBF (log_path+"AS_LOG") FROM ARRAY a_stru
  USE (log_path+"AS_LOG") EXCLUSIVE
  INDEX ON DTOS(GET_DAT) TAG HRON
  FOR i = 1 TO 1000
    APPEND BLANK
  ENDFOR
  USE
ENDIF
SELECT 0
USE AS_LOG ORDER TAG HRON
SCATTER TO arr_log BLANK
arr_log(1) = DATE()

SELECT DEBT_OUT

PRIVATE k_lst, res_cod, lst_pos, q1, q2, q_d, new_b
k_lst = STR(l_num,6)

SCAN
  SCATTER TO arr_in
  FOR i = 2 TO ALEN(arr_log)
    arr_log(i) = arr_in(i-1)
  ENDFOR
  SELECT AS_LOG
  GO TOP
  GATHER FROM arr_log
  SELECT DEBT_OUT
  lst_pos = 0                       && Номер записи в заказе
  res_cod = STO_CODE

  IF res_cod # 0                    && Ищем на складе
    IF .NOT. SEEK(res_cod, "STO_COD")
      res_cod = 0
    ENDIF
  ENDIF

  IF EMPTY(res_cod)                 && Пустой код:
    IF SEEK(NAME+PREFIX,"STO_NAM")  && Поищем на складе, мало ли...
      res_cod = STO_NAM.CODE        &&    нашли!
      GO RECNO("STO_NAM") IN STO_COD
      IF SEEK(k_lst+STR(res_cod,7), ;
              "LST_COD")            &&    ... а в списке?
        lst_pos = RECNO("LST_COD")  &&       тоже нашли!!!
      ENDIF
    ENDIF
    IF lst_pos = 0                  && Если до сих пор не нашли в списке,
      IF SEEK(k_lst+NAME+PREFIX,  ;
              "LST_NAM")            &&    поищем по имени...
        lst_pos = RECNO("LST_NAM")  &&     нашли, и, 
        IF res_cod # 0              &&      если появился код,
          SELECT LST_NAM            &&
          REPLACE CODE WITH res_cod &&              привязываем!
          SELECT DEBT_OUT           &&
        ENDIF
      ENDIF
    ENDIF
  ELSE                              &&     Код есть, и тогда...
    IF SEEK(k_lst+STR(res_cod,7), ;
              "LST_COD")            &&    поищем в списке по коду...
      lst_pos = RECNO("LST_COD")    &&      нашли!
    ENDIF
  ENDIF

  *
  *   Количество, которое было в заказе и долг
  IF lst_pos = 0
    q1  = 0
    q_d = 0
    SELECT LST_NAM
    IF DEBT_OUT.REQUIRED > 0
      APPEND BLANK
      REPLACE LIST_NUM WITH STR(l_num,6), ;
              CODE     WITH res_cod,      ;
              NAME     WITH IIF(res_cod#0, STO_COD.NAME, DEBT_OUT.NAME), ;
              PREFIX   WITH IIF(res_cod#0, STO_COD.PREFIX,  DEBT_OUT.PREFIX), ;
              ADDRESS  WITH IIF(res_cod#0, STO_COD.ADDRESS, " ")
    ENDIF
  ELSE
    GO lst_pos IN LST_NAM
    q1  = LST_NAM.QNT+LST_NAM.QNT_DUE
    q_d = LST_NAM.QNT_DUE
    SELECT LST_NAM
  ENDIF
  IF q1+DEBT_OUT.REQUIRED < 0
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Позиция "+ALLTRIM(DEBT_OUT.PREFIX)+" "+ALLTRIM(DEBT_OUT.NAME)+"."
    mss(3) = CHR(0)+"Клиент пытается вернуть больше, чем за ним числится. Игнорируем!"
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT DEBT_OUT
    LOOP
  ENDIF
  q2 = q1+DEBT_OUT.REQUIRED    && Новое требуемое количество
  new_b = QNT_REQ+IIF(res_cod # 0,  ;
          MAX(STO_COD.QNT, 0), 0)    && Доступное нам количество
  new_b = MIN(q2, new_b)             && Новое значение брони
  IF new_b # QNT_REQ                 && Бронь изменилась - дозаявка!
    SELECT ORD_ZAK
    APPEND BLANK
    REPLACE LIST_NUM WITH k_lst,    ;
            ORD_NUM  WITH LIST_TIT.ORD_NUM+1, ;
            CODE     WITH LST_NAM.CODE,       ;
            QNT      WITH new_b-LST_NAM.QNT_REQ
    SELECT STO_COD
    REPLACE QNT      WITH QNT-new_b+LST_NAM.QNT_REQ, ;
            QNT_SAVE WITH QNT_SAVE+new_b-LST_NAM.QNT_REQ
    SELECT LST_NAM
  ENDIF
  REPLACE QNT     WITH new_b,    ;
          QNT_REQ WITH new_b,    ;
          QNT_DUE WITH q2-new_b

  SELECT DEBT_OUT

ENDSCAN

IF RECCOUNT("ORD_ZAK") # 0
  USE ORD_T
  APPEND BLANK
  REPLACE LIST_NUM  WITH ORD_ZAK.LIST_NUM,  ;
          LIST_NAME WITH LIST_TIT.LST_NAME, ;
          ORD_NUM   WITH ORD_ZAK.ORD_NUM,   ;
          ORD_DATE  WITH DATE(),            ;
          ORD_TIME  WITH TIME(),            ;
          WHO       WITH user,              ;
          WHERE     WITH zak_arm
  USE
  SELECT ORD_ZAK
  USE ORD_D
  APPEND FROM (tmpo_path+"ORD_ZAK")
  USE
  SELECT LIST_TIT
  REPLACE ORD_NUM WITH ORD_NUM+1
  IF new_ord
    DO Prn_OrdN WITH LST_NUM, ORD_NUM
  ELSE
    DO Prn_Ord WITH LST_NUM, ORD_NUM
  ENDIF
  IF ORD_NUM > 900
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Обратите внимание: сформировна заявка N "+ALLTRIM(STR(ORD_NUM,3))+"."
    mss(3) = CHR(0)+"Надеюсь, Вы понимаете к чему мы приближамся?"
    mss(4) = ""
    DO Out_Mess WITH 7,"mss"
  ENDIF
ENDIF

CLOSE DATABASES
DELETE FILE (tmpo_path+"ORD_ZAK.DBF")

RETURN
