*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║               Обработка заявки на продажу из брони АС.                ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Sal_AS_B

PRIVATE get_path, mss

get_path = Where_F("Где ищем заявку")
IF get_path = CHR(0)
  RETURN
ENDIF
IF .NOT. File_O(get_path+"SP_ORD.DBF") .OR. .NOT. File_O(get_path+"SP_POS.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ну никак не удается найти там заявки."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

DO Wt_Mess WITH "Обработка заявки на предмет ее корректности..."
DELETE FILE (tmpo_path+"SP_ORD.DBF")
DELETE FILE (tmpo_path+"SP_POS.DBF")
DELETE FILE (tmpo_path+"SP_ORD.CDX")
DELETE FILE (tmpo_path+"DEF_ORD.DBF")
DELETE FILE (tmpo_path+"DEF_ORD.CDX")

COPY FILE (get_path+"SP_ORD.DBF") TO (tmpo_path+"SP_ORD.DBF")
COPY FILE (get_path+"SP_POS.DBF") TO (tmpo_path+"SP_POS.DBF")

USE (tmpo_path+"SP_POS.DBF") EXCLUSIVE
INDEX ON STO_CODE TAG STO_CODE
INDEX ON CODE     TAG ORDER ADDITIVE

SELECT 0
USE (tmpo_path+"SP_ORD.DBF") EXCLUSIVE

SELECT 0
USE STOCK ORDER TAG CODE

SELECT 0
USE LIST_DET ORDER TAG L_CODE

SELECT 0
CREATE DBF (tmpo_path+"DEF_ORD")  ;
   (  PREFIX C( 4), ;
      NAME   C(35), ;
      REASON C(25)  )
USE (tmpo_path+"DEF_ORD") EXCLUSIVE
INDEX ON NAME+PREFIX TAG NAME

SELECT SP_POS
SET ORDER TO TAG STO_CODE
GO TOP

PRIVATE cd_w, pr_w, nm_w, qnt_w, err_q, err_n, err_s
cd_w  = STO_CODE
pr_w  = PREFIX
nm_w  = NAME
qnt_w = 0
err_q = .F.
err_n = .F.
err_s = .F.

SCAN
  IF cd_w # STO_CODE
    DO Add_Def
    cd_w  = STO_CODE
    pr_w  = PREFIX
    nm_w  = NAME
    qnt_w = QTY
  ELSE
    qnt_w = qnt_w+QTY
    IF pr_w # PREFIX .OR. nm_w # NAME
      err_n = .T.
    ENDIF
  ENDIF
ENDSCAN
GO BOTT
DO Add_Def

DO Wt_Mess
IF RECCOUNT("DEF_ORD") # 0
  PRIVATE ln, wd
  DO Use_Dummy
  SELECT DEF_ORD
  ln = MIN(RECCOUNT(), WROWS("")-3)
  wd = FSIZE("PREFIX")+FSIZE("NAME")+FSIZE("REASON")+3
  DO Def_Win WITH "Def_M", "Def_E", "Def_I", ln, wd, "Список дефектных позиций"
  ACTIVATE WINDOW Def_M
  @ WROWS()-1, 2 SAY PADC(" Esc - выход ", WCOLS()-4, "─")
  ACTIVATE WINDOW Def_E
  GO TOP
  BROWSE FIELDS PREFIX:H="" , ;
                NAME:H="" , ;
                REASON:H="" , ;
                DUMMY.F:H=""   ;
         FREEZE DUMMY.F  ;
         NOMENU NOAPPEND NORGRID NODELETE NOCLEAR ;
         WINDOW Def_I IN WINDOW Def_E
  RELEASE WINDOWS Def_M, Def_E, Def_I
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"SP_ORD.DBF")
  DELETE FILE (tmpo_path+"SP_POS.DBF")
  DELETE FILE (tmpo_path+"SP_ORD.CDX")
  DELETE FILE (tmpo_path+"DEF_ORD.DBF")
  DELETE FILE (tmpo_path+"DEF_ORD.CDX")
  RETURN
ENDIF
SELECT DEF_ORD
USE
DELETE FILE (tmpo_path+"DEF_ORD.DBF")
DELETE FILE (tmpo_path+"DEF_ORD.CDX")

USE LIST_DET ALIAS LIST_ADD AGAIN

SELECT 0
USE LIST_TIT ORDER TAG LST_ARM
IF .NOT. SEEK(SP_ORD.N_S+STR(SP_ORD.N_SP,6))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не наден заголовок списка!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"SP_ORD.DBF")
  DELETE FILE (tmpo_path+"SP_POS.DBF")
  DELETE FILE (tmpo_path+"SP_ORD.CDX")
  RETURN
ENDIF

SELECT 0
USE PARMS

SELECT SP_POS
SET ORDER TO TAG ORDER

PRIVATE lst_n, s, p, spc
SELECT SP_ORD
SCAN
  IF .NOT. SEEK(CODE,"SP_POS")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Клиент - "+ALLTRIM(CLIENT)+". Заявка на продажу пуста. Игнорируем!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF
  SELECT PARMS
  = Lock()
  lst_n = LAST_LST+1
  REPLACE LAST_LST WITH lst_n
  UNLOCK
  DO Wt_Mess WITH "Клиент - "+ALLTRIM(SP_ORD.CLIENT)+". Формируем список N "+ALLTRIM(STR(lst_n,10))
  s = 0
  spc = " "
  SELECT SP_POS
  SCAN REST WHILE CODE = SP_ORD.CODE
    IF .NOT. SEEK(STR(N_SP,6)+STR(STO_CODE,7),"LIST_DET")
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Позиция "+ALLTRIM(SP_POS.PREFIX)+" "+  ;
               ALLTRIM(SP_POS.NAME)+"отсутствует в списке."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      LOOP
    ENDIF
    = SEEK(STO_CODE,"STOCK")
    SELECT LIST_ADD
    APPEND BLANK
    p = IIF(own_val = "$", Clc_Rub(STOCK.PRICE), STOCK.PRICE_R)
    REPLACE LIST_NUM  WITH STR(lst_n,6),    ;
            CODE      WITH SP_POS.STO_CODE, ;
            PREFIX    WITH SP_POS.PREFIX,   ;
            NAME      WITH SP_POS.NAME,     ;
            ADDRESS   WITH STOCK.ADDRESS,   ;
            QNT       WITH SP_POS.QTY,      ;
            QNT_REQ   WITH SP_POS.QTY,      ;
            IS_SPEC   WITH IIF(p = SP_POS.LPRICE, " ", "*"), ;
            DEF_PRICE WITH p,               ;
            SAL_PRICE WITH SP_POS.LPRICE
    IF .NOT. EMPTY(IS_SPEC)
      spc = "*"
    ENDIF
    s = s+QNT*SAL_PRICE
    SELECT LIST_DET
    REPLACE QNT     WITH QNT-SP_POS.QTY, ;
            QNT_REQ WITH QNT_REQ-SP_POS.QTY
    IF QNT = 0 .AND. QNT_REQ = 0 .AND. QNT_DUE = 0
      DELETE
    ENDIF
    SELECT SP_POS
  ENDSCAN
  SELECT LIST_TIT
  APPEND BLANK
  REPLACE LST_NUM  WITH lst_n,    ;
          LST_ARM  WITH CHR(255), ;
          CR_DAT   WITH DATE(),   ;
          LST_NAME WITH SP_ORD.CLIENT, ;
          R_TOTAL  WITH s,        ;
          IS_SPEC  WITH spc,      ;
          WHO_CR   WITH user,     ;
          WHERE_CR WITH SP_ORD.N_S
  @ 3, 0 SAY PADC("Готово! Нажмите любую клавишу.", WCOLS())
  = INKEY(0,"M")
  DO Wt_Mess
  SELECT SP_ORD

ENDSCAN

CLOSE DATABASES
DELETE FILE (tmpo_path+"SP_ORD.DBF")
DELETE FILE (tmpo_path+"SP_POS.DBF")
DELETE FILE (tmpo_path+"SP_ORD.CDX")

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║          Обработка позиции на предмет ее корректности.                ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Add_Def
*
* Смотрим на складе:
*
IF .NOT. SEEK(cd_w, "STOCK")    &&     не нашли
  err_s = .T.
ELSE
  IF STOCK.NAME # nm_w .OR. STOCK.PREFIX # pr_w   && нашли, но не то...
    err_n = .T.
  ENDIF
ENDIF
*
* Смотрим в брони:
*
IF .NOT. SEEK(STR(N_SP,6)+STR(cd_w,7),"LIST_DET")  && не нашли
  err_q = .T.
ELSE
  IF qnt_w > LIST_DET.QNT_REQ                      && нашли, но не хватает...
    err_q = .T.
  ENDIF
ENDIF
*
*  Формируем файл ошибок
*
SELECT DEF_ORD
IF err_s
  APPEND BLANK
  REPLACE PREFIX WITH pr_w, ;
          NAME   WITH nm_w, ;
          REASON WITH "Нескладская"
  err_s = .F.
ENDIF
IF err_n
  APPEND BLANK
  REPLACE PREFIX WITH pr_w, ;
          NAME   WITH nm_w, ;
          REASON WITH "Ошибка в маркировке"
  err_n = .F.
ENDIF
IF err_q
  APPEND BLANK
  REPLACE PREFIX WITH pr_w, ;
          NAME   WITH nm_w, ;
          REASON WITH "Не забронирована"
  err_q = .F.
ENDIF
SELECT SP_POS
RETURN
