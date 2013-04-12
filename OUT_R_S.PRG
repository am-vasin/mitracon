*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Печать и/или запись отчета в DBF - формате.               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 09.10.97 12:35:00 ═╝
PROCEDURE Out_R_S
PARAMETERS rep_n, rep_d

if TYPE("ToAsk")='U' && NEW 7.02.2002 BEGIN
  PRIVATE ToAsk
  ToAsk = .T.
else
  if TYPE("ToAsk")#'L'
    wait window 'Out_R_S: Тип переменной ToAsk не logical!'
  endif
endif                && NEW 7.02.2002 END

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

*┌────────────────────────────────────────────────────────────────────┐
*│   [ ] Печать отчета по продажам     [ ] E-mail odp@compel.ru       │
*│                   [ ] Печать отчета по продажам                    │
*│ ┌─ Отчет в DBF-формате ──────────────────────────────────────────┐ │
*│ │ [ ] Продажи   [ ] Приемные акты   [ ] Количество на реализации │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│                  < OK Ctrl-W > < Отказаться Esc >                  │
*└────────────────────────────────────────────────────────────────────┘

PRIVATE ex, is_prn, is_dbfs, is_dbfa, is_dbfn, sup_n, s_sav, own_c
PRIVATE date_rep, is_email, st_email, n_fnames, u_email
DIMENSION f_names(1)
is_prn  = .F.
is_dbfs = .T.
is_dbfa = .T.
is_dbfn = .T.
*is_email = .F.
is_email = ToAsk && NEW 7.02.2002
st_email = Get_Sys('ST_EMAIL')

ex      = 1
s_sav = SELECT()
SELECT 0
USE (base_path+"REP_TIT") ORDER TAG N_REP ALIAS RT_TMP AGAIN
IF .NOT. SEEK(LEFT(DTOS(rep_d),4)+STR(rep_n,5))
  USE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет отчета N "+ALLTRIM(STR(rep_n))+" от "+DTOC(rep_d)
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (s_sav)
  RETURN
ENDIF
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS AC_TMP AGAIN
SEEK RT_TMP.SUP_CODE

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 72, "N "+ALLTRIM(STR(rep_n))+" от "+DTOC(rep_d)+ ;
                ": "+ALLTRIM(AC_TMP.CUS_NAME)
DO Sun_Bord WITH 3, 3, 5, 68, " Отчет в DBF-формате "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
IF Sale_Mode = 'MEGA'
    @ 2,  5 GET is_prn PICTURE "@*C Печать отчета по продажам"
	@ 2, 39 GET is_email PICTURE "@*C E-mail"
	st_email = PADR(ALLTRIM(st_email),80)
	@ 2, 50 GET st_email SIZE 1,19 WHEN is_email
ELSE
    @ 2, 21 GET is_prn PICTURE "@*C Печать отчета по продажам"
ENDIF
@ 4,  5 GET is_dbfs PICTURE "@*C Продажи"
@ 4, 19 GET is_dbfa PICTURE "@*C Приемные акты" DISABLE
@ 4, 39 GET is_dbfn PICTURE "@*C Количество на реализации"
@ 6, 20 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
st_email = ALLTRIM(st_email)
IF ex = 2
  is_prn  = .F.
  is_dbfs = .F.
  is_dbfa = .F.
  is_dbfn = .F.
  is_email = .F.
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

USE (base_path+"CUSTOMER") ALIAS AC_TMP AGAIN
LOCATE FOR SELF # " "
own_c = CODE
USE
IF is_dbfs
  DO Wt_Mess WITH "Отчет по продажам..."
  DO Rep_Sal
  DO Wt_Mess
ENDIF
IF is_dbfa
  DO Wt_Mess WITH "Приемные акты..."
  DO Rep_Inc
  DO Wt_Mess
ENDIF
IF is_dbfn
  DO Wt_Mess WITH "Отчет по наличию..."
  DO Rep_Nal
  DO Wt_Mess
ENDIF

PRIVATE arr_w, sz_req, f_nam1, f_nam2, mss

sz_req = 0
IF is_dbfs
  IF ADIR(arr_w, tmpo_path+"REP_T_S.DBF") # 0
    sz_req = sz_req+arr_w(1,2)
  ENDIF
  IF ADIR(arr_w, tmpo_path+"REP_D_S.DBF") # 0
    sz_req = sz_req+arr_w(1,2)
  ENDIF
ENDIF
IF is_dbfa
  IF ADIR(arr_w, tmpo_path+"REP_T_I.DBF") # 0
    sz_req = sz_req+arr_w(1,2)
  ENDIF
  IF ADIR(arr_w, tmpo_path+"REP_D_I.DBF") # 0
    sz_req = sz_req+arr_w(1,2)
  ENDIF
ENDIF
IF is_dbfn
  IF ADIR(arr_w, tmpo_path+"REP_N.DBF") # 0
    sz_req = sz_req+arr_w(1,2)
  ENDIF
ENDIF
own_c = ALLTRIM(STR(own_c))
RELEASE arr_w
IF sz_req # 0
  if ToAsk
    arr_w = Int_Dir("", "Куда пишем отчет?", sz_req)
  else
    arr_w = tmpo_path
  endif
  arr_w = ALLTRIM(arr_w)
  IF .NOT. EMPTY(arr_w)
	SELECT 0
    ERASE (tmpo_path+'FNAMLIST.DBF')
	CREATE DBF (tmpo_path+'FNAMLIST.DBF') (Name C(50))
    IF is_dbfs
      f_nam1 = arr_w+"st"+own_c+".dbf"
	  DO add_fname with f_nam1
      f_nam2 = arr_w+"sl"+own_c+".dbf"
	  DO add_fname with f_nam2
      DO WHILE .T.
        IF File_O(f_nam1) .OR. File_O(f_nam2)
          PRIVATE mss
          DIMENSION mss(4)
          mss(1) = ""
          mss(2) = CHR(0)+"Там уже есть отчет по продажам!"
          mss(3) = CHR(0)+"  Что будем делать? [ Заменим ] [ Оставим ]"
          mss(4) = ""
          IF Out_Mess(7, "mss") = 2
            EXIT
          ENDIF
          DELETE FILE (f_nam1)
          DELETE FILE (f_nam2)
        ENDIF
        SELECT 0
        USE (tmpo_path+"REP_T_S.DBF")
        CALCULATE MAX(DOC_DATE) TO date_rep
        USE
        COPY FILE (tmpo_path+"REP_T_S.DBF") TO (f_nam1)
        COPY FILE (tmpo_path+"REP_D_S.DBF") TO (f_nam2)
        EXIT
      ENDDO
      DELETE FILE (tmpo_path+"REP_T_S.DBF")
      DELETE FILE (tmpo_path+"REP_D_S.DBF")
    ENDIF
    IF is_dbfa
      f_nam1 = arr_w+"at"+own_c+".dbf"
	  DO add_fname with f_nam1
      f_nam2 = arr_w+"ac"+own_c+".dbf"
	  DO add_fname with f_nam2
      DO WHILE .T.
        IF File_O(f_nam1) .OR. File_O(f_nam2)
          if ToAsk  && NEW 7.02.2002
            PRIVATE mss
            DIMENSION mss(4)
            mss(1) = ""
            mss(2) = CHR(0)+"Там уже есть отчет по приемным актам!"
            mss(3) = CHR(0)+"  Что будем делать? [ Заменим ] [ Оставим ]"
            mss(4) = ""
            IF Out_Mess(7, "mss") = 2
              EXIT
            ENDIF
          ENDIF
          DELETE FILE (f_nam1)
          DELETE FILE (f_nam2)
        ENDIF
        COPY FILE (tmpo_path+"REP_T_I.DBF") TO (f_nam1)
        COPY FILE (tmpo_path+"REP_D_I.DBF") TO (f_nam2)
        EXIT
      ENDDO
      DELETE FILE (tmpo_path+"REP_T_I.DBF")
      DELETE FILE (tmpo_path+"REP_D_I.DBF")
    ENDIF
    IF is_dbfn
      f_nam1 = arr_w+"nl"+own_c+".dbf"
	  DO add_fname with f_nam1
      DO WHILE .T.
        IF File_O(f_nam1)
          IF ToAsk && NEW 7.02.2002
            PRIVATE mss
            DIMENSION mss(4)
            mss(1) = ""
            mss(2) = CHR(0)+"Там уже есть отчет по наличию!"
            mss(3) = CHR(0)+"  Что будем делать? [ Заменим ] [ Оставим ]"
            mss(4) = ""
            IF Out_Mess(7, "mss") = 2
              EXIT
            ENDIF
          ENDIF
          DELETE FILE (f_nam1)
        ENDIF
        COPY FILE (tmpo_path+"REP_N.DBF") TO (f_nam1)
        EXIT
      ENDDO
      DELETE FILE (tmpo_path+"REP_N.DBF")
    ENDIF
    ERASE (tmpo_path + "FNAMLIST.TXT")
	SELECT FNAMLIST
	COPY ALL TO (tmpo_path + "FNAMLIST.TXT") DELIMITED WITH BLANK
	n_fnames = _TALLY
	USE
    IF (Sale_Mode = 'MEGA') and is_email and is_dbfs and !empty(n_fnames)
      PRIVATE date_rep,FNameArch,DosBat,DOSCom1,DOSCom2,Subj,Handl
      PRIVATE mss, OM7 && NEW 7.02.2002
      IF ToAsk         && NEW 7.02.2002
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"Посылаем отчеты по адресу "+st_email+"?"
        mss(3) = CHR(0)+"[ Послать ] [ Отменить ]"
        mss(4) = ""
        OM7 = Out_Mess(7, "mss")
      ELSE
        OM7 = 1
      ENDIF
      IF (OM7 = 1) or (not ToAsk)  && NEW 07.02.2002
*        if !file('c:\windows\wmailto.ini')
*			report for code=user format comlists.frx to 'c:\windows\wmailto.ini'  ADDITIVE NOCONSOLE
*        endif
		u_email = ""
		IF File(base_path+'EMAIL.DBF')
			use base_path+'email' order code
            sele 0
			use base_path+'persons' order code alias pers4597 again
			set rela to code into email
			seek user
			IF FOUND()
				u_email = TRIM(EMail.EMail)
			ENDIF
			use
			sele email
			use
		ENDIF
        FNameArch = arr_w+"sp"+RIGHT(DTOS(date_rep),6)+".arj"
        DOSCom1 = "arj m "+FNameArch+" !"+tmpo_path+"FNAMLIST.TXT"
        Subj = '"Our report from '+DTOC(date_rep)+'. We clear the marks."'
        DOSCom2 = "wmailto "+st_email+" "+u_email+" -s"+subj+" -i"+FNameArch
        ERASE (tmpo_path+"WMAIL.BAT")
		Handl = FCREATE(tmpo_path+"WMAIL.BAT")
      	= FPUTS(Handl, 'CLS')
      	= FPUTS(Handl, DOSCom1)
      	= FPUTS(Handl, DOSCom2)
	  	= FPUTS(Handl, 'pause')
	  	= FCLOSE(Handl)
      	DosBat=tmpo_path+"WMAIL.BAT"
*	  	CLEAR screen
        IF ToAsk && NEW 7.02.2002
            !&DOSBat
        ENDIF
      ENDIF
  	ENDIF
  ENDIF
ENDIF

IF is_prn
  DO Prn_Rep
ENDIF

SELECT RT_TMP
USE
SELECT (s_sav)

RETURN

PROCEDURE Add_fname
PARAMETER arg
PRIVATE cur_alias
cur_alias = ALIAS()
SELECT FNAMLIST
APPEND BLANK
REPLACE NAME WITH arg
IF !EMPTY(cur_alias)
	SELE (cur_alias)
ENDIF
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║             Формирование DBF - файла с отчетом о продажах.             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 09.10.97 13:59:13 ═╝
PROCEDURE Rep_Sal
PRIVATE last_doc, last_firm, last_dat, d_num, place

*
* Удаляем временные файлы на всякий пожарный...
*
DELETE FILE (tmpo_path+"REP_T_S.DBF")
DELETE FILE (tmpo_path+"REP_D_S.DBF")
DELETE FILE (tmpo_path+"REP_0.DBF")
DELETE FILE (tmpo_path+"REP_0.CDX")

CREATE DBF (tmpo_path+"REP_T_S.DBF") ;
    (  N_REP       N( 5), ;
       D_REP       D,     ;
       FIRM        C( 3), ;
       DOC_NUM     C(10), ;
       DOC_DATE    D,     ;
       CUS_CODE    N( 5), ;
       CUS_NAME    C(16), ;
       ACCOUNT     N(10), ;
       SW_NAL      C( 1), ;
       ORD_NO      N( 6), ;
       INV_NO      N( 6), ;
       TOTAL_R     N(10,2), ;
       DISCOUNT    N( 8,2), ;
       NOTE        C(16), ;
       TRANSMIT_C  N( 6), ;
       SEND_M      N( 5), ;
       REP_SUP     N( 5), ;
       WHO         N( 3), ;
       WHERE       C( 3) )
SELECT 0
CREATE DBF (tmpo_path+"REP_D_S.DBF") ;
    (  FIRM        C( 3), ;
       DOC_NUM     C(10), ;
       DOC_DATE    D,     ;
       CODE        N( 7), ;
       QNT         N( 6), ;
       QNT_REP     N( 6), ;
       PRICE_D_L   N(10,4), ;
       PRICE_D     N(10,4), ;
       PRICE_R     N(8,2), ;
       NOTE        C(16), ;
       WHERE       C( 3) )

SELECT 0
USE (base_path+"REPORT") ORDER TAG N_REP ALIAS RP_0 AGAIN
SEEK LEFT(DTOS(RT_TMP.D_REP),4)+STR(RT_TMP.N_REP,5)
COPY REST WHILE RT_TMP.D_REP = D_REP .AND. RT_TMP.N_REP = N_REP ;
   TO (tmpo_path+"REP_0.DBF")
USE (tmpo_path+"REP_0.DBF") EXCLUSIVE
INDEX ON DTOS(EXP_D)+EXP_N TAG DOC_NUM
GO TOP
last_firm = "   "
last_doc = "          "
last_dat = {}

SCAN
  IF last_doc # EXP_N .OR. last_dat # EXP_D .OR. last_firm # EXP_F
    last_doc = EXP_N
    last_dat = EXP_D
    last_firm = EXP_F
    d_num    = VAL(ALLTRIM(SUBSTR(last_doc,5)))
*    place    = VAL(SUBSTR(last_doc,2,4))
*    place    = CHR(place)
    place    = SUBSTR(last_doc,2,4)
    SELECT REP_T_S
    APPEND BLANK
    REPLACE N_REP       WITH RT_TMP.N_REP, ;
            D_REP       WITH RT_TMP.D_REP, ;
            FIRM        WITH last_firm,;
            DOC_NUM     WITH last_doc, ;
            DOC_DATE    WITH last_dat, ;
            SW_NAL      WITH IIF(REP_0.IS_NAL, " ", "*"), ;
            WHERE       WITH place
  ENDIF
  SELECT REP_D_S
  APPEND BLANK
  REPLACE DOC_NUM     WITH last_doc,      ;
          FIRM        WITH last_firm,     ;
          DOC_DATE    WITH last_dat,      ;
          CODE        WITH REP_0.CODE,    ;
          QNT         WITH REP_0.QNT,     ;
          QNT_REP     WITH REP_0.QNT,     ;
          PRICE_D_L   WITH REP_0.PRICE_D, ;
          PRICE_D     WITH REP_0.PRICE_D, ;
          PRICE_R     WITH REP_0.PRICE_R, ;
          WHERE       WITH place
  SELECT REP_0
ENDSCAN
USE
DELETE FILE (tmpo_path+"REP_0.DBF")
DELETE FILE (tmpo_path+"REP_0.CDX")

SELECT REP_D_S
USE

SELECT REP_T_S
USE

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║          Формирование DBF - файла с отчетом о приемных актах.          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 09.10.97 15:02:29 ═╝
PROCEDURE Rep_Inc
PRIVATE d_num

*
* Удаляем временные файлы на всякий пожарный...
*
DELETE FILE (tmpo_path+"REP_T_I.DBF")
DELETE FILE (tmpo_path+"REP_D_I.DBF")

CREATE DBF (tmpo_path+"REP_T_I.DBF") ;
    (  FIRM        C( 3), ;
       DOC_NUM     C(10), ;
       DOC_DATE    D,     ;
       SUP_CODE    N( 5), ;
       CUS_CODE    N( 5), ;
       FLG_BUY     C( 1), ;
       ORD_NUM     N( 5), ;
       ORD_DATE    D,     ;
       EXP_NUM     N(10), ;
       EXP_DATE    D( 8), ;
       SEND_M      N( 5), ;
       REP_SUP     N( 5), ;
       WHO_ACT     N( 3), ;
       WHERE_ACT   C( 3) )
SELECT 0
CREATE DBF (tmpo_path+"REP_D_I.DBF") ;
    (  CIRM        C( 3), ;
       DOC_NUM     C(10), ;
       DOC_DATE    D,     ;
       CODE        N( 7), ;
       QNT_ORD     N( 6), ;
       QNT_EXP     N( 6), ;
       QNT_INC     N( 6), ;
       QNT_REP     N( 6), ;
       NOTE        C(12), ;
       F_COUNT     C( 1), ;
       F_DIFF      C( 1)  )

SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM ALIAS IN_TMP AGAIN

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG SUP_CODE  ALIAS IN_TMP_T AGAIN
SEEK RT_TMP.SUP_CODE
SCAN REST WHILE RT_TMP.SUP_CODE = SUP_CODE ;
          FOR RT_TMP.N_REP = REP_SUP .AND. FLG_BUY = "R"
  d_num = VAL(ALLTRIM(LEFT(DOC_NUM,8)))*10+VAL(RIGHT(DOC_NUM,1))
  SELECT IN_TMP
  SEEK LEFT(IN_TMP_T.FIRM+DTOS(IN_TMP_T.DOC_DATE),4)+IN_TMP_T.DOC_NUM
  SCAN REST WHILE IN_TMP_T.DOC_DATE = DOC_DATE .AND. ;
                  IN_TMP_T.FIRM     = FIRM .AND.     ;
                  IN_TMP_T.DOC_NUM  = DOC_NUM
    SELECT REP_D_I
    APPEND BLANK
    REPLACE DOC_NUM  WITH IN_TMP.DOC_NUM,  ;
            FIRM     WITH IN_TMP.FIRM,     ;
            DOC_DATE WITH IN_TMP.DOC_DATE, ;
            CODE     WITH IN_TMP.CODE,     ;
            QNT_ORD  WITH IN_TMP.QNT_ORD,  ;
            QNT_EXP  WITH IN_TMP.QNT_EXP,  ;
            QNT_INC  WITH IN_TMP.QNT_INC,  ;
            QNT_REP  WITH IN_TMP.QNT_INC
    SELECT IN_TMP
  ENDSCAN
  SELECT REP_T_I
  APPEND BLANK
  REPLACE DOC_NUM   WITH IN_TMP_T.DOC_NUM,  ;
          FIRM      WITH IN_TMP_T.FIRM,     ;
          DOC_DATE  WITH IN_TMP_T.DOC_DATE, ;
          SUP_CODE  WITH IN_TMP_T.SUP_CODE, ;
          CUS_CODE  WITH IN_TMP_T.CUS_CODE, ;
          FLG_BUY   WITH IN_TMP_T.FLG_BUY,  ;
          ORD_NUM   WITH IN_TMP_T.ORD_NUM,  ;
          ORD_DATE  WITH IN_TMP_T.ORD_DATE, ;
          EXP_NUM   WITH VAL(ALLTRIM(IN_TMP_T.EXP_NUM)), ;
          EXP_DATE  WITH IN_TMP_T.EXP_DATE, ;
          REP_SUP   WITH IN_TMP_T.REP_SUP,  ;
          WHO_ACT   WITH 0,                 ;
          WHERE_ACT WITH "  0"
  SELECT IN_TMP_T
ENDSCAN
USE
SELECT IN_TMP
USE
SELECT REP_T_I
USE
SELECT REP_D_I
USE
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║             Формирование DBF - файла с отчетом по наличию.             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 09.10.97 16:50:24 ═╝
PROCEDURE Rep_Nal

DELETE FILE (tmpo_path+"REP_N.DBF")
DELETE FILE (tmpo_path+"REP_N.CDX")

CREATE DBF (tmpo_path+"REP_N.DBF") ;
    (  CODE    N ( 7), ;
       QNT     N ( 6), ;
       PRICE_D N(8,3), ;
       PRICE_R N(8,2)  )
INDEX ON CODE TAG CODE
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_TMP AGAIN

SELECT 0

USE (base_path+"ACT_S") ORDER TAG DOC_NUM ALIAS IN_TMP AGAIN

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG SUP_CODE  ALIAS IN_TMP_T AGAIN
SEEK RT_TMP.SUP_CODE
*SCAN REST WHILE RT_TMP.SUP_CODE = SUP_CODE FOR FLG_BUY = "R"
SCAN REST WHILE RT_TMP.SUP_CODE = SUP_CODE  ;
          FOR RT_TMP.N_REP = REP_SUP .AND. FLG_BUY = "R"
  SELECT IN_TMP
  SEEK IN_TMP_T.FIRM+LEFT(DTOS(IN_TMP_T.DOC_DATE),4)+IN_TMP_T.DOC_NUM
  SCAN REST WHILE IN_TMP_T.DOC_DATE = DOC_DATE .AND. ;
                  IN_TMP_T.DOC_NUM  = DOC_NUM  .AND. ;
                  IN_TMP_T.FIRM     = FIRM           ;
            FOR QNT_REP # 0
    SELECT REP_N
    IF RT_TMP.PR_TYPE = 1
      = SEEK(IN_TMP.CODE, "STO_TMP")
      IF .NOT. SEEK(IN_TMP.CODE)
        APPEND BLANK
      ENDIF
    ELSE
      APPEND BLANK
    ENDIF
    REPLACE CODE    WITH IN_TMP.CODE, ;
            QNT     WITH QNT+IN_TMP.QNT_REP, ;
            PRICE_D WITH IIF(RT_TMP.PR_TYPE = 1, STO_TMP.PRICE_B, 0), ;
            PRICE_R WITH IIF(RT_TMP.PR_TYPE = 1,    ;
                                  STO_TMP.PRICE_B_R, IN_TMP.PRICE_R)
    SELECT IN_TMP
  ENDSCAN
  SELECT IN_TMP_T
ENDSCAN

USE
SELECT IN_TMP
USE
SELECT REP_N
DELETE TAG CODE
USE
SELECT STO_TMP
USE

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Печать отчета по продажам.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.10.97 12:33:02 ═╝
PROCEDURE Prn_Rep
PRIVATE last_doc, last_dat, d_num, c_name

*
* Удаляем временные файлы на всякий пожарный...
*
DELETE FILE (tmpo_path+"REP_TMP.DBF")

CREATE DBF (tmpo_path+"REP_TMP.DBF") ;
    (  CODE        N( 7), ;
       PREFIX      C( 4), ;
       NAME        C(35), ;
       PRODUCER    C( 8), ;
       IS_NAL      C( 1), ;
       QNT         N( 6), ;
       SUM_D       N(12,3), ;
       SUM_R       N(12,2)  )
INDEX ON IS_NAL+NAME+PREFIX+PRODUCER TAG NAME
INDEX ON IS_NAL+STR(CODE) TAG CODE ADDITIVE

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_TMP AGAIN
SEEK RT_TMP.SUP_CODE
c_name = ALLTRIM(CUS_NAME)

USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST000 AGAIN
SELECT 0
USE (base_path+"REPORT") ORDER TAG N_REP ALIAS RP_0 AGAIN
SEEK LEFT(DTOS(RT_TMP.D_REP),4)+STR(RT_TMP.N_REP,5)
SCAN REST WHILE RT_TMP.D_REP = D_REP .AND. RT_TMP.N_REP = N_REP
  SELECT REP_TMP
  IF SEEK(IIF(RP_0.IS_NAL, "*", " ")+STR(RP_0.CODE))
    REPLACE QNT   WITH QNT+RP_0.QNT, ;
            SUM_R WITH SUM_R+RP_0.QNT*RP_0.PRICE_R, ;
            SUM_D WITH SUM_D+RP_0.QNT*RP_0.PRICE_D
  ELSE
    = SEEK(RP_0.CODE, "ST000")
    APPEND BLANK
    REPLACE CODE     WITH RP_0.CODE,            ;
            PREFIX   WITH ST000.PREFIX,         ;
            NAME     WITH ST000.NAME,           ;
            PRODUCER WITH ST000.PRODUCER,       ;
            QNT      WITH RP_0.QNT,             ;
            IS_NAL   WITH IIF(RP_0.IS_NAL, "*", " "), ;
            SUM_R    WITH RP_0.QNT*RP_0.PRICE_R,;
            SUM_D    WITH RP_0.QNT*RP_0.PRICE_D
  ENDIF
  SELECT RP_0
ENDSCAN

USE

SELECT ST000
USE

SELECT REP_TMP
SET ORDER TO TAG NAME
GO TOP

PRIVATE sb, p_ln, fnt, n_cp, lft, ffeed, n, i, t_p, t_d, f_p, f_d, p_drctry
PRIVATE n_p_det, n_pg, f_pg

DIMENSION sb(6,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Rep_N}"
sb( 2,2) = ALLTRIM(STR(RT_TMP.N_REP))

sb( 3,1) = "{Rep_D}"
sb( 3,2) = DTOC(RT_TMP.D_REP)

sb( 4,1) = "{Sup_Name}"
sb( 4,2) = c_name

sb( 5,1) = "{Total_P   }"
sb( 5,2) = ""

sb( 6,1) = "{Total_D   }"
sb( 6,2) = ""

SELECT 0

USE (base_path+"DOC_FORM")
LOCATE FOR "REP_SUP" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

n = MEMLINES(PAGE_F)
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(PAGE_F,i)
ENDFOR

n = MEMLINES(DOC_H)
DIMENSION t_d(n)
FOR i = 1 TO n
  t_d(i) = MLINE(DOC_H,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

SELECT REP_TMP

DO Ini_Prn WITH "Отчет N "+ALLTRIM(STR(RT_TMP.N_REP)), p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

n_p_det = p_ln-MAX(ALEN(t_p),ALEN(t_d))-MAX(ALEN(f_p),ALEN(f_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = CEILING(RECCOUNT()/n_p_det)
ENDIF

PRIVATE s_d, s_p, is_usd
STORE 0 TO s_d, s_p
is_usd = RT_TMP.VALUTA = "$"

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1
    EJECT
    FOR i = 1 TO ALEN(t_p)
      str_w = t_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(t_d)
      str_w = t_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF

  s_p = 0
  FOR i = 1 TO n_p_det
    str_w = PREFIX+" "+NAME+" "+PRODUCER+" "+TRANSFORM(QNT, "@Z 999999")+ ;
            +" "+IIF(is_usd, TRANSFORM(SUM_D, "@Z 9999999.9999"), ;
                             TRANSFORM(SUM_R, "@Z 999999999.99") )+" "+IS_NAL
    s_p = s_p + IIF(is_usd, SUM_D, SUM_R)
    s_d = s_d + IIF(is_usd, SUM_D, SUM_R)
    @ PROW()+1,0 SAY str_w
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  sb( 5,2) = IIF(is_usd, TRANSFORM(s_p, "@Z 9999999.9999"), ;
                         TRANSFORM(s_p, "@Z 999999999.99") )
  IF f_pg = n_pg
    sb( 6,2) = IIF(is_usd, TRANSFORM(s_d, "@Z 9999999.9999"), ;
                           TRANSFORM(s_d, "@Z 999999999.99") )

    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR
USE

DO Term_Prn WITH "", tmpo_path+"REP_TMP.DBF"

DELETE FILE (tmpo_path+"REP_TMP.DBF")
DELETE FILE (tmpo_path+"REP_TMP.CDX")

RETURN
