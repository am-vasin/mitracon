*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║           Новая проверка баланса по налогу на предприятиях.           ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE New_T_S
PARAMETER mode

PRIVATE d_s, ab, sch

sch = SCHEME(13,1)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,4)+","+ ;
      SCHEME(13,5)+","+ ;
      SCHEME(13,6)+","+ ;
      SCHEME(13,7)+","+ ;
      SCHEME(13,8)+","+ ;
      SCHEME(13,9)
IF mode = "c"
  PRIVATE log_rec, err_cnt

  log_rec = Use_Log("Проверка налога")

  USE PARMS
  d_s = DAT_START

  DELETE FILE (log_path+"N_T_S.DBF")
  DELETE FILE (log_path+"N_T_S.CDX")

  CREATE DBF (log_path+"N_T_S")  ;
     (  FIRM_CODE C( 3),   ;
        B_NAME    C(16),   ;
        S_NALOG   N(12,2), ;
        NALOG     N(12,2)  )
  USE (log_path+"N_T_S") EXCLUSIVE
  INDEX ON FIRM_CODE TAG CODE
  APPEND FIELDS FIRM_CODE, B_NAME, S_NALOG, NALOG FROM FIRMS

  *DO Wt_Mess WITH "Расчет налога..."
  *
  *   Расчет поступления налога
  *
  SELECT 0

  USE INCMONEY
  SET FILTER TO DOC_DATE >= d_s .AND. DOC_TYPE $ " Ч"  && Только для прих. и расх. ордеров! ...и чеков!
  SCAN
    IF .NOT. SEEK(FIRM, "N_T_S")
      LOOP
    ENDIF
    SELECT N_T_S
    REPLACE S_NALOG WITH S_NALOG+INCMONEY.N_OF_SALE
    SELECT INCMONEY
  ENDSCAN

*
*    Андрюха! Не забудь вот здесь вставить учет уплаты налогов!
*
*
  SELECT N_T_S
  COUNT FOR S_NALOG # NALOG TO err_cnt
  DO Term_Log WITH log_rec, err_cnt

ELSE

  DO Use_Dummy
  *DO Wt_Mess

  SELECT 0
  USE (log_path+"N_T_S")

  PRIVATE ln, wd, tt, ft
  DIMENSION tt(3), ft(1)
  tt(1) = ;
  "────────────────┬────────────┬────────────┬───"
  tt(2) = ;
  "  Раб. место    │ База данных│  Расчетн.  │Ош."
  tt(3) = ;
  "────────────────┼────────────┼────────────┼───"
  ft(1) = ;
  "────────────────┴────────────┴────────────┴───"

  ln = WROWS("")-10
  wd = LEN(ft(1))
  DO Def_Win WITH "Tst_K_M", "Tst_K_E", "Tst_K_I", ln, wd, ;
                  "Проверка налога", tt, ft

  @ 1, 1 SAY "┌"
  @ 2, 1 SAY "│"
  @ 3, 1 SAY "├"
  @ WROWS()-1, 1 SAY "└"

  @ 1, WCOLS()-2 SAY "┐"
  @ 2, WCOLS()-2 SAY "│"
  @ 3, WCOLS()-2 SAY "┤"
  @ WROWS()-1, WCOLS()-2 SAY "┘"

  ACTIVATE WINDOW Tst_K_E
  GO TOP
  BROWSE FIELDS B_NAME:H="",   ;
                NALOG:H="":P="@Z 999999999.99",   ;
                S_NALOG:H="":P="@Z 999999999.99", ;
                f001 = IIF(S_NALOG # NALOG, " √ ", "   "):H="",   ;
                DUMMY.F FREEZE DUMMY.F  ;
                NOMENU WINDOW Tst_K_I IN WINDOW Tst_K_E COLOR (sch)
  RELEASE WINDOW Tst_K_I
  RELEASE WINDOW Tst_K_E
  RELEASE WINDOW Tst_K_M
ENDIF

CLOSE DATABASES
RETURN
