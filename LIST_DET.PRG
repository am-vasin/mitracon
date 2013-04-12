*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  เ กฎโ๋ แ แฎคฅเฆจฌ๋ฌ แฏจแช .                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE List_Det
PARAMETER view_only, alt_val
PRIVATE mtk_period

m.mtk_period = "4 คญ๏"

PRIVATE cur_line
cur_line = 1

IF EMPTY(LIST_TIT.LST_NUM)
  RETURN
ENDIF

PRIVATE stopTest
PRIVATE lc, what_do, cl_w, cl_w1, i001, i002, sum_list, sav_val, lst_titl
PRIVATE sw_def, sw_due, sw_ord, sw_dis, sw_spec, cnt_list, sl_mark, st_mark
PRIVATE let_edit, is_anal, is_cor, is_char, ent_rec, prf_sav
PRIVATE sav_auto, ord_sav, pic_w, show_rub, s_due

s_due = 0
ord_sav = "ord_n"
show_rub = LIST_TIT.IS_USD = alt_val
pic_w = IIF(show_rub, "@Z 999999.99999", "@Z 99999.9999")
stopTest = .F.

is_char = SPACE(31)+     REPLICATE("*",126-31 )+ ;
          SPACE(127-126)+REPLICATE("*",175-127)+ ;
          SPACE(223-175)+REPLICATE("*",241-223)+ ;
          SPACE(255-223)

*IF LIST_TIT.IS_CHECK
*  PRIVATE nds_flt
*  nds_flt = 0
*ENDIF

let_edit = .NOT. view_only .AND. .NOT. alt_val
cl_w = SCHEME(13)
i001 = AT(",", cl_w, 6)+1
i002 = AT(",", cl_w, 7)-i001
cl_w1 = STUFF(cl_w, i001, i002, SCHEME(14,1))
cl_w  = STUFF(cl_w, i001, i002, SCHEME(14,2))
lc = STR(LIST_TIT.LST_NUM,6)
lst_titl = "‘ฏจแฎช N "+ALLTRIM(lc)+" ฎโ "+TRANSFORM(LIST_TIT.CR_DAT,"@D")+" "+ALLTRIM(LIST_TIT.LST_NAME)
HIDE MENU List_M
PUSH KEY CLEAR
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

DEFINE MENU Stock IN SCREEN COLOR SCHEME 4

DEFINE PAD Sw_List  OF Stock PROMPT "จค" KEY Ctrl-A, "F2"
ON SELECTION PAD Sw_list  OF Stock Do Sw_Mod WITH "list", "Stock"

DEFINE PAD Report OF Stock PROMPT "‘ฏเ ขช "
ON PAD Report OF Stock ACTIVATE POPUP Report

  DEFINE POPUP Report MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Report PROMPT " ฌฅญ๋" KEY Ctrl-A, "F8"
  ON SELECTION BAR 1 OF Report DO Sw_Mod WITH "cha_seek", "Stock"

  DEFINE BAR  2 OF Report PROMPT "‘ใฌฌ " KEY Ctrl-A, "F1"
  ON SELECTION BAR 2 OF Report DO Sw_Mod WITH "l_sum", "Stock"

  DEFINE BAR  3 OF Report PROMPT "เฎญ์" KEY Ctrl-A, "Ctrl-F1"
  ON SELECTION BAR 3 OF Report DO Sw_Mod WITH "bron", "Stock"

  DEFINE BAR  4 OF Report PROMPT "ขจฆฅญจฅ" KEY Ctrl-A, "F9"
  ON SELECTION BAR 4 OF Report DO Sw_Mod WITH "p_rep", "Stock"

  DEFINE BAR  5 OF Report PROMPT "Product manager" KEY Ctrl-A, "F5"
  ON SELECTION BAR 5 OF Report DO Sw_Mod WITH "pr_manager", "Stock"

  DEFINE BAR  6 OF Report PROMPT "ฎเฏใแ (ฏเจฌฅ็ ญจฅ)" KEY Ctrl-A, "F6"
  ON SELECTION BAR 6 OF Report DO Sw_Mod WITH "cor_note", "Stock"

  DEFINE BAR  7 OF Report PROMPT "‘เฎช ฏฎแโ ขชจ"
  ON SELECTION BAR 7 OF Report DO Sw_Mod WITH "period", "Stock"

  DEFINE BAR  8 OF Report PROMPT " ซจ็จฅ ข ‘" && KEY Ctrl-A, "Ctrl-D"
  ON SELECTION BAR 8 OF Report DO Sw_Mod WITH "Rest_DMS", "Stock"

  IF Is_Acc(1)
    DEFINE BAR  9 OF Report PROMPT " ชใฏฎ็ญ๋ฅ ๆฅญ๋"
    ON SELECTION BAR 9 OF Report DO Sw_Mod WITH "bPrice", "Stock"
  ENDIF

DEFINE PAD Position OF Stock PROMPT "ฎงจๆจ๏"
ON PAD Position OF Stock ACTIVATE POPUP Position

  DEFINE POPUP Position MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Position PROMPT "ฎจแช ฏฎ ็จแซ. ็ แโจ" && KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Position DO Sw_Mod WITH "dig_seek", "Stock"

  DEFINE BAR  2 OF Position PROMPT "ฎจแช" KEY Ctrl-A, "F7"
  ON SELECTION BAR 2 OF Position DO Sw_Mod WITH "sub_seek", "Stock"

  DEFINE BAR 11 OF Position PROMPT "ฎขโฎเ" && KEY Ctrl-A, "F9"
  ON SELECTION BAR 11 OF Position DO Sw_Mod WITH "rep_seek", "Stock"

  DEFINE BAR  3 OF Position PROMPT "๋แฎช ๏ ๆฅญ " KEY Ctrl-A, "F3"
  ON SELECTION BAR 3 OF Position DO Sw_Mod WITH "refuse", "Stock"

  DEFINE BAR  4 OF Position PROMPT "”ใญชๆ. ญ งญ ็ฅญจฅ" KEY Ctrl-A, "F4"
  ON SELECTION BAR 4 OF Position DO Sw_Mod WITH "new_info", "Stock"

  DEFINE BAR  5 OF Position PROMPT " แฏเฎแ" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 5 OF Position DO Sw_Mod WITH "dem_line", "Stock"

  DEFINE BAR  5 OF Position PROMPT "ฎฏ. ญฎฌฅญชซ โใเ " KEY Ctrl-A, ""	&& ชซ แแจไจช ๆจ๏ 29.10.2005
  ON SELECTION BAR 5 OF Position DO Sw_Mod WITH "Add_Sto", "Stock"

  DEFINE BAR  7 OF Position PROMPT "ซ แแ" KEY Ctrl-A, "XXX"	&& ชซ แแจไจช ๆจ๏ 29.10.2005
  ON SELECTION BAR 7 OF Position DO Sw_Mod WITH "GetClass", "Stock"

  DEFINE BAR  8 OF Position PROMPT "แฅ ชซ แแ๋" KEY Ctrl-A, "XXX"
  ON SELECTION BAR 8 OF Position DO Sw_Mod WITH "ClearClass", "Stock"

DEFINE PAD Exit     OF Stock PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit     OF Stock Do Sw_Mod WITH "exit", "Stock"
SELECT 0

sav_val  = 0

DO Use_DMS
SET ORDER TO TAG CODE
SELECT 0
USE (base_path+"DEVELOP") ORDER TAG CODE AGAIN
SELECT 0
USE (base_path+"ACT_S") ORDER TAG SALE AGAIN ALIAS SALE_PARTY
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN
prf_sav = SPACE(FSIZE("PREFIX"))

is_anal = .F.
IF File_O(base_path+"ANALOG.DBF")
  SELECT 0
  USE (base_path+"ANALOG") ORDER TAG CODE_1
  SELECT 0
  USE (base_path+"ANALOG") ORDER TAG CODE ALIAS ANA_C AGAIN
  is_anal = .T.
ENDIF

is_cor = .F.
IF .NOT. EMPTY(path_comm)             && ก้ ๏ คจเฅชโฎเจ๏ แใ้ฅแโขฅโ,   ข ญฅฉ...
  IF File_O(path_comm+"CORPUS.DBF") .AND. ;
     File_O(path_comm+"SUBJECT.DBF")  && ...ฅแโ์ ชฎเฏใแ 
    SELECT 0
    USE (path_comm+"CORPUS") ORDER TAG CODE
    SELECT 0
    USE (path_comm+"SUBJECT") ORDER TAG STO_CODE
    SET RELATION TO COR_CODE INTO CORPUS
    is_cor = .T.
  ENDIF
ENDIF
IF .NOT. is_cor                               && ฎเฏใแฎข ญฅ ญ ่ซจ, ญฎ...
  IF File_O(base_path+"CORPUS.DBF") .AND. ;
     File_O(base_path+"SUBJECT.DBF")          && ...ฎญจ ฅแโ์ ข STORE
    SELECT 0
    USE (base_path+"CORPUS") ORDER TAG CODE
    SELECT 0
    USE (base_path+"SUBJECT") ORDER TAG STO_CODE
    SET RELATION TO COR_CODE INTO CORPUS
    is_cor = .T.
  ENDIF
ENDIF

SELECT 0
USE (base_path+"LINK_ORD") ORDER TAG LIST_CODE ALIAS L_ORD
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_CODE ALIAS LD
SET RELATION TO CODE INTO STOCK, LIST_NUM+STR(CODE) INTO L_ORD

STORE .F. TO sw_def, sw_due, sw_ord, sw_dis, sw_spec, sl_mark, st_mark
sum_list = 0
cnt_list = 0

DO See_Ord WITH lc, sum_list, sw_def, sw_due, sw_ord, sw_dis, sw_spec, ;
                sl_mark, st_mark
IF alt_val
  PRIVATE mss000
  DIMENSION mss000(3)
  mss000(1) = ""
  mss000(2) = CHR(0)+"  ‘ใฌฌ  "+ALLTRIM(TRANSFORM(sum_list, "999 999 999.99"))+ ;
              IIF(show_rub, " เใก", " “ …")+", ‘ฌฎโเจฌ แฏจแฎช? [   ] [ ฅโ ]"
  mss000(3) = ""
  IF Out_Mess(5, "mss000") = 2
    DEACTIVATE MENU Stock
    RELEASE MENU Stock
    SELECT STOCK
    USE
    SELECT LD
    USE
    IF USED("ANALOG")
      SELECT ANALOG
      USE
      SELECT ANA_C
      USE
    ENDIF
    IF USED("CORPUS")
      SELECT CORPUS
      USE
    ENDIF

    IF USED("SUBJECT")
      SELECT SUBJECT
      USE
    ENDIF

    IF USED("L_ORD")
      SELECT L_ORD
      USE
    ENDIF

    SELECT LIST_TIT
    SHOW MENU List_M
    POP KEY
    RETURN
  ENDIF
ENDIF

SELECT LD
SET RELATION TO
IF let_edit
  IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
    PRIVATE mss0
    DIMENSION mss0(3)
    mss0(1) = ""
    mss0(2) = CHR(0)+"เฎค ญฎ!  ช ๏ ชฎเเฅชๆจ๏? ฎ ฌฎฆญฎ ญ  ญฅฃฎ ฏฎแฌฎโเฅโ์."
    mss0(3) = ""
    DO Out_Mess WITH 7, "mss0"
    let_edit = .F.
  ELSE
    IF .NOT. EMPTY(LIST_TIT.IS_DIS)
      PRIVATE mss0
      DIMENSION mss0(3)
      mss0(1) = ""
      mss0(2) = CHR(0)+"…แซจ ๅฎโจโฅ ชฎเเฅชโจเฎข โ์, ฎโฌฅญจโฅ แชจคชใ,   ฏฎช  ฌฎฆญฎ ฏฎแฌฎโเฅโ์..."
      mss0(3) = ""
      DO Out_Mess WITH 7, "mss0"
      let_edit = .F.
    ENDIF
  ENDIF
ENDIF

*IF .NOT. SEEK(lc+"     -1")
*  APPEND BLANK
*  REPLACE LIST_NUM WITH lc, CODE WITH -1
*ENDIF

*
*  เฎขฅเ๏ฅฌ แฎแโฎ๏ญจฅ จ ง ฏฎซญ๏ฅฌ  โเจกใโ๋ แฏจแช 
*

ent_rec = 0
IF .NOT. let_edit     && .OR. cnt_list # 0
  what_do = "list"
ELSE
  what_do = "stock"
ENDIF
DO WHILE .T.
  DO CASE
  CASE what_do = "stock"
    DO Full_Pos
    IF what_do = "stock"
      what_do = "exit"
    ENDIF
  CASE what_do = "list"
    DO Edit_Lst
    IF .NOT. EMPTY(LD.CODE)
      ent_rec = RECNO("STOCK")
    ELSE
      ent_rec = 0
    ENDIF
    IF what_do = "list"
      what_do = "exit"
    ENDIF
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

SELECT STOCK
SET ORDER TO TAG CODE
DEACTIVATE MENU Stock
RELEASE MENU Stock

SELECT LD
SET ORDER TO TAG L_CODE
SET RELATION TO CODE INTO STOCK, LIST_NUM+STR(CODE) INTO L_ORD

DO See_Ord WITH lc, sum_list, sw_def, sw_due, sw_ord, sw_dis, sw_spec, ;
                sl_mark, st_mark

SET RELATION TO

USE IN SALE_PARTY
SELECT STOCK
USE
SELECT DEVELOP
USE
IF USED("DMS")
  USE IN DMS
ENDIF
SELECT L_ORD
USE
SELECT LD
USE
IF USED("ANALOG")
  SELECT ANALOG
  USE
  SELECT ANA_C
  USE
ENDIF

IF USED("CORPUS")
  SELECT CORPUS
  USE
ENDIF

IF USED("SUBJECT")
  SELECT SUBJECT
  USE
ENDIF

SELECT LIST_TIT
REPLACE R_TOTAL WITH IIF(alt_val, R_TOTAL, sum_list),  ;
        IS_DEF  WITH IIF(sw_def, "*", " "), ;
        IS_DUE  WITH IIF(sw_due, "*", " "), ;
        IS_DIS  WITH IIF(.NOT. EMPTY(PRESENT), "*", IIF(sw_dis, "*", " ")), ;
        IS_SPEC WITH IIF(sw_spec,"*", " "), ;
        NOTE_STO WITH IIF(st_mark, "*", " "), ;
        NOTE_SAL WITH IIF(sl_mark, "*", " ")

SHOW MENU List_M
POP KEY
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎฃเ ฌฌ  เ กฎโ๋ แฎ ขแฅฉ ญฎฌฅญชซ โใเฎฉ.                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Full_Pos

*SET NEAR ON
SELECT LD
SET ORDER TO TAG L_CODE

SELECT STOCK
SET ORDER TO TAG NAME

PRIVATE prf_flt, stock_f, prf_show
PRIVATE seek_m, prf_wd, fill_chr, double, rc_flt, jmp, r_must, last_c
prf_wd = FSIZE("NAME","STOCK")
seek_m = "seek"
jmp    = .F.
r_must = 0
fill_chr= "ฑ"
GO TOP

SET CONFIRM OFF

IF mode_pr0
  SET FILTER TO EMPTY(STOP_SALE) && .AND. PRICE_R # 0
ELSE
  SET FILTER TO EMPTY(STOP_SALE) .AND. PRICE_R # 0
ENDIF
          &&  ฃฎเ ฆจข ฅฌ ฏฎงจๆจจ "ญฅ คซ๏ ฏเฎค ฆจ" จ แ ฏใแโ๋ฌจ ๆฅญ ฌจ
DO WHILE seek_m # "exit"
*  SET RELATION TO IIF(SEEK(lc+STR(CODE,7),"LD"), lc+STR(CODE,7),  ;
*                      lc+"     -1") INTO LD
  DO CASE
  CASE seek_m = "seek"
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU Stock
    DEACTIVATE MENU Stock
    seek_m = Sto_Seek()
    ACTIVATE SCREEN
    HIDE MENU Stock
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
  CASE seek_m = "edit"
    seek_m = "seek"
    WAIT " คฅ ๏?  ็ฅฌ ๏ งคฅแ์?!" WINDOW
    LOOP
  CASE seek_m = "list"
    what_do = "list"
    seek_m = "exit"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO
SELECT STOCK
SET FILTER TO
SET RELATION TO
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  ไฎเฌจเฎข ญจ๏ แโเฎชจ ไซ ฆชฎข.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE B_Flags
PRIVATE fl

IF ALIAS() = "STOCK"
  IF .NOT. FOUND("LD") .OR. LD.CODE < 0
    fl = "..."
  ELSE
*    fl = IIF(LD.QNT_REQ = 0, ".", IIF(LD.QNT_REQ=LD.QNT, "Û","ฑ"))+ ;
*         IIF(LD.QNT-LD.QNT_REQ<=STOCK.QNT,".", ;
*             IIF(.NOT. EMPTY(LD.IS_DEF), "ฑ", "Û"))+ ;
*         IIF(.NOT. EMPTY(LD.IS_SPEC), "Û",   ;
*             IIF(LD.DEF_PRICE # LD.SAL_PRICE,"ฑ",".") )
* จช ชฎฃฎ เ งเฅ่ฅญญฎฃฎ คฅไจๆจโ !
    fl = IIF(LD.QNT_REQ = 0, ".", IIF(LD.QNT_REQ=LD.QNT, "Û","ฑ"))+ ;
         IIF(LD.QNT-LD.QNT_REQ<=STOCK.QNT,".", "Û")+ ;
         IIF(.NOT. EMPTY(LD.IS_SPEC), "Û",   ;
         IIF(IIF(LIST_TIT.IS_USD, LD.USD_DEF, Round_6(LD.DEF_PRICE)) #  ;
             IIF(LIST_TIT.IS_USD, LD.USD_PRICE,Round_6(LD.SAL_PRICE)),"ฑ",".") )
  ENDIF
ELSE
  IF CODE > 0
*    fl = IIF(LD.QNT_REQ = 0, ".", IIF(LD.QNT_REQ=LD.QNT, "Û","ฑ"))+ ;
*         IIF(LD.QNT-LD.QNT_REQ<=STOCK.QNT,".", ;
*             IIF(.NOT. EMPTY(LD.IS_DEF), "ฑ", "Û"))+ ;
*         IIF(.NOT. EMPTY(LD.IS_SPEC), "Û",   ;
*             IIF(LD.DEF_PRICE # LD.SAL_PRICE,"ฑ",".") )
    fl = IIF(LD.QNT_REQ = 0, ".", IIF(LD.QNT_REQ=LD.QNT, "Û","ฑ"))+ ;
         IIF(LD.QNT-LD.QNT_REQ<=STOCK.QNT,".", "Û")+ ;
         IIF(.NOT. EMPTY(LD.IS_SPEC), "Û",   ;
         IIF(IIF(LIST_TIT.IS_USD, LD.USD_DEF, Round_6(LD.DEF_PRICE)) #   ;
             IIF(LIST_TIT.IS_USD, LD.USD_PRICE,Round_6(LD.SAL_PRICE)),"ฑ",".") )
* จช ชฎฃฎ เ งเฅ่ฅญญฎฃฎ คฅไจๆจโ !
  ELSE
    fl = ".ÛÛ"
  ENDIF
ENDIF

RETURN fl

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                         ๅฎค ข ฏฎซฅ ๆฅญ๋.                             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE W_Price

sav_val = IIF(LIST_TIT.IS_USD, LD.USD_PRICE, LD.SAL_PRICE)
IF LD.QNT = 0 .AND. LD.QNT_DUE = 0 .OR. .NOT. mode_spec
*  KEYBOARD CHR(9)
  RETURN .F.
ENDIF
RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                ๅฎค ข แ ๆฅซ์๎ แฎๅเ ญฅญจ๏ งญ ็ฅญจ๏.                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rem_Val
PARAMETER vl

sav_val = vl

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ       ใคญ ๏ จ ขฅโขจแโ ๏ ฏเฎฃเ ฌฌ  ข๋ๅฎค  จง ฏฎซ๏ ชฎซจ็ฅแโข .         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Exit_Qnt
PRIVATE v1, pr, delta, i, q_w

IF stopTest
  stopTest = .F.
  RETURN .T.
ENDIF

IF LD.QNT < 0
  IF ALIAS() = "LD" .AND. mode_defD
    IF .NOT. sale_mode = "Mitracon"
      IF QNT+QNT_DUE < 0
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"๋ แโฎซ์ชฎ ญฅ คฎซฆญ๋!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        ACTIVATE WINDOW (ALIAS())
        RETURN .F.
      ENDIF
    ENDIF
    REPLACE QNT_DUE WITH QNT_DUE-QNT, QNT WITH 0
    IF QNT_DUE < 0
       REPLACE QNT WITH QNT_DUE
       REPLACE QNT_DUE WITH 0
     ENDIF
    IF QNT_DUE # 0
      IF mode_ord # "B"
        WAIT WINDOW "€…!  ช ง ญ  เฅ ซจง ๆจ๎!" NOWAIT
      ENDIF

      REPLACE ORD_TYPE WITH mode_ord
      
    ENDIF

*    IF QNT = 0 .AND. QNT_DUE = 0 .AND. QNT_REQ = 0
*      SKIP IN LD
*      KEYBOARD "{DNARROW}"
*    ENDIF
    RETURN .T.
  ENDIF
  IF .NOT. sale_mode = "Mitracon"
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฎงขเ โ ฎแใ้ฅแโขซ๏ฅโแ๏ ็ฅเฅง แฏฅๆจ ซ์ญใ๎ ฏเฎๆฅคใเใ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    ACTIVATE WINDOW (ALIAS())
    RETURN .F.
  ENDIF
ENDIF
SHOW GETS

IF LD.QNT = sav_val
  RETURN .T.
ENDIF
v1 = LD.QNT
*pr = IIF(own_val="$",STOCK.PRICE,STOCK.PRICE_R)
pr = IIF(LIST_TIT.IS_USD, STOCK.PRICE, ROUND_6(STOCK.PRICE_R))
IF LD.CODE < 0
  SELECT LD
  REPLACE QNT WITH 0
  APPEND BLANK
  REPLACE LIST_NUM WITH lc,            ;
          CODE     WITH STOCK.CODE,    ;
          PREFIX   WITH STOCK.PREFIX,  ;
          NAME     WITH STOCK.NAME,    ;
          PRO_CODE WITH STOCK.PRO_CODE,;
          PRODUCER WITH STOCK.PRODUCER,;
          QNT      WITH v1,            ;
          ADDRESS  WITH STOCK.ADDRESS, ;
          USD_DEF  WITH STOCK.PRICE,   ;
          DEF_PRICE WITH STOCK.PRICE_R,;
          USD_PRICE WITH IIF(LIST_TIT.IS_USD, STOCK.PRICE, 0),  ;
          SAL_PRICE WITH IIF(LIST_TIT.IS_USD, 0, STOCK.PRICE_R)
  REPLACE LIST_TIT.WHO_LET WITH 0
  SELECT STOCK
  SHOW WINDOW STOCK REFRESH
ENDIF

i = LD.QNT-MAX(STOCK.QNT,0)-LD.QNT_REQ  && ฅ ๅข โ ฅโ ญ  แชซ คฅ

IF FOUND("STOCK")
  q_w = LD.QNT
*  IF q_w % MAX(1, STOCK.QNT_ROUND) # 0 .AND. q_w > 0
    q_w = Round_P(STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER, q_w, STOCK.QNT_ROUND, STOCK.QNT_PACK, STOCK.PRICE)
    REPLACE LD.QNT WITH q_w
*  ENDIF
ENDIF

IF i > 0 .AND. mode_defA
  WAIT "ฅไจๆจโ!" WINDOW NOWAIT
  IF mode_defD
    REPLACE LD.QNT_DUE WITH LD.QNT_DUE+i, ORD_TYPE WITH mode_ord
  ENDIF
  IF mode_defS
    DO To_Dem0 WITH i  && 0 - คฎก ขซฅญจฅ ข แฏเฎแ จง ไ ฉซ  แฏจแช 
  ENDIF
  REPLACE LD.QNT WITH LD.QNT-i, LD.IS_DEF WITH " "
ELSE
  IF LD.QNT < sav_val  && “ฌฅญ์่จซจ ชฎซจ็ฅแโขฎ: ฏฎ็ฅฌใ?
    i = Reason_D()
    DO CASE
    CASE i = 1   && ๋แฎช ๏ ๆฅญ 
      DO To_Refus WITH sav_val-LD.QNT
    CASE i = 3
*      IF File_O(base_path+"ABSENT.DBF")
*        DO Absent WITH LD.CODE
*      ENDIF
      DO To_Dem0 WITH sav_val-LD.QNT
    ENDCASE
  ENDIF
ENDIF
delta = IIF(LIST_TIT.IS_USD, LD.USD_PRICE, LD.SAL_PRICE)*(LD.QNT-sav_val)
*sum_list = sum_list+delta

ACTIVATE WINDOW L_Det_M BOTTOM
*@ WROWS()-2, 3 SAY STR(sum_list,12,2) COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS())

sav_val = LD.QNT

*IF LD.QNT = 0 .AND. LD.QNT_DUE = 0 .AND. LD.QNT_REQ = 0
*  SKIP IN LD
*  KEYBOARD "{DNARROW}"
*ENDIF

DO Show_D

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ข๋ๅฎค  จง ฏฎซ๏ ๆฅญ๋.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Exit_Prc
PRIVATE delta

IF stopTest
  stopTest  = .F.
  RETURN .T.
ENDIF

IF LIST_TIT.IS_USD
  IF LD.USD_PRICE = sav_val
    RETURN .T.
  ENDIF
  IF LD.USD_PRICE < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"โฎ ็โฎ ง  ๆฅญ  โ ช ๏? คใฌ ฉโฅแ์!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    REPLACE LD.USD_PRICE WITH sav_val
    RETURN .F.
  ENDIF
ELSE
  IF LD.SAL_PRICE < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"โฎ ็โฎ ง  ๆฅญ  โ ช ๏? คใฌ ฉโฅแ์!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    REPLACE LD.SAL_PRICE WITH sav_val
    RETURN .F.
  ENDIF
ENDIF
IF LD.CODE # 0
  IF LIST_TIT.IS_USD
    REPLACE LD.IS_SPEC WITH IIF(LD.USD_PRICE = LD.USD_DEF, " ", "*")
  ELSE
    REPLACE LD.IS_SPEC WITH IIF(LD.SAL_PRICE = ROUND_6(LD.DEF_PRICE), " ", "*")
  ENDIF
ELSE
  REPLACE LD.IS_SPEC WITH "*"
ENDIF
REPLACE LIST_TIT.WHO_LET WITH 0,  ;
        LD.SAL_PRICE     WITH ROUND_6(LD.SAL_PRICE)
delta = LD.QNT*(IIF(LIST_TIT.IS_USD, LD.USD_PRICE, LD.SAL_PRICE)-sav_val)
*sum_list = sum_list+delta

ACTIVATE WINDOW L_Det_M BOTTOM
*@ WROWS()-2, 3 SAY STR(sum_list,12,2) COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เจ็จญ  ใฌฅญ์่ฅญจ๏ ชฎซจ็ฅแโข .                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Reason_D
PRIVATE tit, ch, wd, ln, w_sav, i, s
DIMENSION ch(3)
tit   = "ฎ็ฅฌใ ใฌฅญ์่จซจ ชฎซ-ขฎ?"
ch(2) = "่จกช  ขขฎค         "
ch(3) = "ฅโ ญ  แชซ คฅ       "
ch(1) = "๋แฎช ๏ ๆฅญ         "

PUSH KEY CLEAR
ln = 6
wd = LEN(tit)
w_sav = WOUTPUT()
DO D_W WITH "Reason_D", ln, wd, tit
i = 1
DO WHILE .T.
  i = MIN(i,3)
  i = MAX(i,1)
  @ 1,1 MENU ch, 3
  READ MENU TO i
  s = READKEY() % 256
  IF s = 15
    EXIT
  ENDIF
  IF s = 12
    i = 1
    EXIT
  ENDIF
ENDDO
RELEASE WINDOW Reason_D
IF EMPTY(w_sav)
  ACTIVATE SCREEN
ELSE
  ACTIVATE WINDOW (w_sav)
ENDIF

POP KEY

RETURN i

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ง ญฅแฅญจ๏ ง ฏจแจ ข แฏเฎแ จง แฏจแช .             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE To_Dem0
PARAMETER q    && ฎซจ็ฅแโขฎ
PRIVATE sel_sav

IF .NOT. USED("DEMAND")
  RETURN
ENDIF

sel_sav = SELECT()
SELECT 0
SELECT DEMAND
APPEND BLANK
REPLACE STO_CODE WITH LD.CODE,   ;
        PREFIX   WITH LD.PREFIX, ;
        NAME     WITH LD.NAME,   ;
        PRO_CODE WITH LD.PRO_CODE, ;
        PRODUCER WITH LD.PRODUCER, ;
        DATE     WITH DATE(),    ;
        QTY      WITH q,         ;
        PRICE    WITH IIF(LIST_TIT.IS_USD, 0, LD.SAL_PRICE), ;
        PRICE_D  WITH IIF(LIST_TIT.IS_USD, LD.USD_PRICE, 0), ;
        CLI_CODE WITH LIST_TIT.CLI_CODE, ;
        CLIENT   WITH LIST_TIT.LST_NAME, ;
        FUNCTION WITH LD.FUNCTION,       ;
        AMODEL   WITH LD.AMODEL, ;
        WHO      WITH user,      ;
        WHERE    WITH arm
IF USED("DEM_LINK")
  SELECT DEM_LINK
  GO TOP
  IF .NOT. EMPTY(LST_NUM) .OR. BOF() .OR. EOF()
    APPEND BLANK
  ENDIF
  REPLACE LST_NUM WITH LIST_TIT.LST_NUM, LINK WITH RECNO("DEMAND")
ENDIF
SELECT (sel_sav)
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ง ญฅแฅญจ๏ ฎโช ง  ฏฎ ข๋แฎชฎฉ ๆฅญฅ.               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE To_Refus
PARAMETER q    && ฎซจ็ฅแโขฎ
PRIVATE sel_sav

IF .NOT. USED("REFUSAL")
  RETURN
ENDIF

sel_sav = SELECT()
SELECT REFUSAL
APPEND BLANK
REPLACE STO_CODE WITH LD.CODE,   ;
        PREFIX   WITH LD.PREFIX, ;
        NAME     WITH LD.NAME,   ;
        PRO_CODE WITH LD.PRO_CODE, ;
        PRODUCER WITH LD.PRODUCER, ;
        DATE     WITH DATE(),    ;
        QTY      WITH q,         ;
        PRICE    WITH IIF(LIST_TIT.IS_USD, 0, LD.SAL_PRICE),  ;
        PRICE_D  WITH IIF(LIST_TIT.IS_USD, LD.USD_PRICE, 0),  ;
        CUS_CODE WITH LIST_TIT.CLI_CODE, ;
        CLIENT   WITH LIST_TIT.LST_NAME, ;
        FUNCTION WITH LD.FUNCTION,       ;
        AMODEL   WITH LD.AMODEL, ;
        WHO      WITH user,      ;
        WHERE    WITH arm
SELECT (sel_sav)
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ฏฎจแช  ฏฎงจๆจจ.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Seek_Pos
PARAMETERS seek_type  && "S" - ฏฎ ชฎญโฅชแโใ, "R" - ฏฎขโฎเ, "A" -  ญ ซฎฃฎข.

PRIVATE rc_sav, l, ln, wd, c, st, s, ord, pic, choose, rel_sav, ord, tit
PRIVATE tmp_file, tmp1_file, tmpClass

*
*    …แซจ ฏฎจแช  ญ ซฎฃฎข, โฎ งคฅแ์: แโ เ ๏ ขฅโข์ ฏฎจแช   ญ ซฎฃฎข ญฅ เ กฎโ ฅโ!
*
tmpClass = ""
IF TYPE("keyClass") = "C"
  tmpClass = keyClass
ENDIF

IF seek_type = "S"
  l = Pos_Cont(SET("FILTER"), tmpClass)
  IF BETWEEN(l, 1, RECCOUNT())
    GO l
    RETURN .T.
  ENDIF
  IF m.l < 0
    DO Add_Sto WITH IIF(m.view_only, "", STR(LIST_TIT.LST_NUM, 6)), -m.l
  ENDIF
  RETURN .T.
ENDIF
IF seek_type = "R"
  l = Pos_Cont(SET("FILTER"), tmpClass, .F., .T.)
  IF BETWEEN(l, 1, RECCOUNT())
    GO l
    RETURN .T.
  ENDIF
  RETURN .F.
ENDIF
IF seek_type = "A"
  DO Changes WITH STOCK.CODE
  l = Ch_Chang(ALLTRIM(STOCK.PREFIX)+" "+ALLTRIM(STOCK.NAME)+" "+ALLTRIM(STOCK.PRODUCER), .T.)
  IF l = 0
    RETURN .F.
  ENDIF
  SELECT STOCK
  wd = RECNO()
  ln = TAG()
  SET ORDER TO TAG CODE
  IF .NOT. SEEK(l)
    SET ORDER TO (ln)
    IF BETWEEN(wd, 1, RECCOUNT())
      GO wd
    ENDIF
    RETURN .F.
  ENDIF
  SET ORDER TO (ln)
  RETURN .T.
ENDIF

*
*  แ๏ชจฅ ฌฎคจไจช ๆจจ ฏเฎฃเ ฌฌ๋ ฏเจขฅซจ ช โฎฌใ, ็โฎ ค ซ์่ฅ ํโฎฃฎ ฌฅแโ 
*  ชฎค ญฅ ข๋ฏฎซญ๏ฅโแ๏ ญจชฎฃค !
*

PUSH KEY CLEAR
choose = .F.
IF seek_type = "S"
  tit = "’ฎ, ็โฎ ใค ซฎแ์ ญ ฉโจ"
  st  = SPACE(FSIZE("NAME","STOCK"))
  ln = 8
  wd = 60
  pic = " แช โ์ Ctrl-W ;\? โช ง โ์แ๏ Esc "
  l = CEILING((WROWS("")-ln)/2)
  c = FLOOR((WCOLS("")-wd)/2)
  DEFINE WINDOW Seek_Pos FROM l, c TO l+ln-1, c+wd-1 SHADOW NONE COLOR SCHEME 13
  ACTIVATE WINDOW Seek_Pos
  @ 1, 1 TO WROWS()-1, WCOLS()-2
  @ 0, 0 SAY PADC("ขฅคจโฅ ่ กซฎญ คซ๏ ฏฎจแช ", WCOLS()) COLOR SCHEME 5
  PRIVATE c01, c02, ex
  ex    = 1
  c01   = FLOOR((WCOLS()-LEN(st))/2)
  c02   = FLOOR((WCOLS()-LEN(pic))/2)+2

  DO WHILE .T.
    @ 3, c01 GET st
    @ 5, c02 GET ex PICTURE "@*HT \!"+pic
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
      mss(2) = CHR(0)+"๋ ง ก๋ซจ ใช ง โ์ ่ กซฎญ คซ๏ ฏฎจแช ."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      LOOP
    ENDIF
    s = .T.
    EXIT
  ENDDO
  RELEASE WINDOW Seek_Pos
ELSE
  s = .T.
  IF .NOT. USED("ANALOG")
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฅโ ค ญญ๋ๅ คซ๏ ฏฎจแช ."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    s = .F.
  ENDIF
ENDIF
PRIVATE als
IF s
  als = ALIAS()
  rel_sav = SET("RELATION")
  ord = SET("ORDER")
  rc_sav = RECNO()

  tmp1_file = SYS(3)
  SELECT 0
  CREATE DBF (tmpo_path+tmp1_file) (REC_NO N(10),   ;
                                    IS_STO C(3),    ;
                                    PREFIX C(FSIZE("PREFIX","STOCK")),  ;
                                    NAME   C(FSIZE("NAME",  "STOCK")),  ;
                                    PRODUCER C(FSIZE("PRODUCER",  "STOCK")),  ;
                                    PRICE  N(FSIZE("PRICE_R","STOCK"), 2) )
  USE (tmpo_path+tmp1_file) ALIAS VIEW EXCLUSIVE
  INDEX ON NAME+PREFIX TAG NAME

  DO Wt_Mess WITH "้ฅฌ..."
  IF seek_type = "S" 
    IF als = "STOCK"
      SET RELATION TO
    ELSE
      SEEK lc
    ENDIF
    st = SYS(15,lwr,ALLTRIM(st))
    SELECT (als)
    
    IF als = "STOCK"
      SET ORDER TO 0
      SCAN FOR st $ SYS(15,lwr,NAME) .AND. EMPTY(DATE_OFF) && .AND. QNT > 0
        SELECT VIEW
        APPEND BLANK
        REPLACE REC_NO WITH RECNO("STOCK"),             ;
                IS_STO WITH IIF(STOCK.QNT > 0, " ๛ ", "   "), ;
                PREFIX WITH STOCK.PREFIX,               ;
                NAME   WITH STOCK.NAME,                 ;
                PRODUCER WITH STOCK.PRODUCER,           ;
                PRICE  WITH IIF(LIST_TIT.IS_USD, STOCK.PRICE, STOCK.PRICE_R)
        SELECT STOCK
      ENDSCAN
    ELSE
      SCAN REST WHILE LIST_NUM = lc FOR st $ SYS(15,lwr,NAME)
        SELECT VIEW
        APPEND BLANK
        REPLACE REC_NO WITH RECNO(als),                       ;
                IS_STO WITH IIF(STOCK.QNT > 0, " ๛ ", "   "), ;
                PREFIX WITH LD.PREFIX,                        ;
                NAME   WITH LD.NAME,                          ;
                PRODUCER WITH STOCK.PRODUCER,           ;
                PRICE  WITH IIF(LIST_TIT.IS_USD, LD.USD_PRICE, LD.SAL_PRICE)
        SELECT LD
      ENDSCAN
    ENDIF
  ELSE
    SELECT STOCK
    SET RELATION TO
    SET ORDER TO TAG CODE
    tmp_file = SYS(3)
    SELECT 0
    CREATE DBF (tmpo_path+tmp_file) (SOURCE C(FSIZE("SOURCE","ANALOG")))
    USE (tmpo_path+tmp_file) ALIAS TMP_S
    SELECT ANALOG
    IF is_sto
      tit = ALLTRIM(STOCK.PREFIX+" "+STOCK.NAME)+" -  ญ ซฎฃจ"
      c = .T.
      l = "Û"+STR(STOCK.CODE,FSIZE("NAME_1")-1)
    ELSE
      tit = prf_flt+" -  ญ ซฎฃจ"
      c = .F.
      l = PADR(prf_flt,FSIZE("NAME_1"))
    ENDIF

    SEEK l
    SCAN REST WHILE c .AND. CODE_1 = STOCK.CODE .OR.  ;
                      .NOT. c .AND. l = NAME_1
      SELECT VIEW
      APPEND BLANK
      REPLACE REC_NO WITH ANALOG.CODE_2
      SELECT TMP_S
      APPEND BLANK
      REPLACE SOURCE WITH ANALOG.SOURCE
      SELECT ANALOG
    ENDSCAN
    SELECT VIEW
    SET ORDER TO 0
    l = 0
    SCAN
      IF SEEK(REC_NO, "STOCK")
        l = l+1
        REPLACE REC_NO WITH RECNO("STOCK"),             ;
                IS_STO WITH IIF(STOCK.QNT > 0, " ๛ ", "   "), ;
                PREFIX WITH STOCK.PREFIX,               ;
                NAME   WITH STOCK.NAME,                 ;
                PRODUCER WITH STOCK.PRODUCER,           ;
                PRICE  WITH ROUND_6(STOCK.PRICE_R)
      ELSE
        DELETE
      ENDIF
    ENDSCAN
    SET ORDER TO TAG NAME
    IF l = 0
      ZAP
    ENDIF
    GO TOP
    SELECT STOCK
  ENDIF

  DO Wt_Mess
  IF RECCOUNT("VIEW") = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฅโ โ ชจๅ ฏฎงจๆจฉ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT VIEW
    USE
    DELETE FILE (tmpo_path+tmp1_file+".DBF")
    DELETE FILE (tmpo_path+tmp1_file+".CDX")
    
    IF USED("TMP_S")
      SELECT TMP_S
      USE
      DELETE FILE (tmpo_path+tmp_file+".DBF")
    ENDIF
    SELECT (als)
    SET RELATION TO &rel_sav
    SET ORDER TO &ord
    GO rc_sav
  ELSE
    DEFINE MENU Seek_Pos IN SCREEN COLOR SCHEME 4
    DEFINE PAD Choose OF Seek_Pos PROMPT "๋กเ โ์"    KEY Ctrl-A, "Enter"
    IF seek_type = "A"
      DEFINE PAD Source OF Seek_Pos PROMPT "แโฎ็ญจช" KEY Ctrl-A, "F1"
      ON SELECTION PAD Source  OF Seek_Pos Do Ex_Proc WITH "Seek_Pos","{F1}"
    ENDIF
    DEFINE PAD Abort  OF Seek_Pos PROMPT "โช ง โ์แ๏" KEY Ctrl-A, "Esc"

    ON SELECTION PAD Choose OF Seek_Pos Do Ex_Proc WITH "Seek_Pos",13
    ON SELECTION PAD Abort  OF Seek_Pos Do Ex_Proc WITH "Seek_Pos",27
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU Seek_Pos
    ON KEY LABEL F10 DO F10_Proc WITH Seek_Pos
    PRIVATE wd00, ln00, l00, c00
    ln00 = WROWS("")-5
    wd00 = IIF(LIST_TIT.IS_USD, 57, 60)
    l00  = CEILING((WROWS("")-ln00)/2)
    c00  = FLOOR((WCOLS("")-wd00)/2)
    DEFINE WINDOW W_1 FROM l00, c00 TO l00+ln00-1, c00+wd00-1 NONE SHADOW COLOR SCHEME 13
    ln00 = ln00-3
    wd00 = wd00-4
    l00  = l00+2
    c00  = c00+2
    DEFINE WINDOW W_2 FROM l00, c00 TO l00+ln00-1, c00+wd00-1 NONE COLOR SCHEME 13
    ln00 = ln00+4
    wd00 = wd00+20
    l00  = l00-3
    c00  = c00-2
    DEFINE WINDOW W_3 FROM l00, c00 TO l00+ln00-1, c00+wd00-1 NONE COLOR SCHEME 13
    ACTIVATE WINDOW W_1
    @ 0, 0 SAY PADC(tit,WCOLS()) COLOR SCHEME 5
    @ 1, 1 TO WROWS()-1, WCOLS()-2
    s = " ซฤฤฤ จฌฅญฎข ญจฅ"
    s = PADR(s,WCOLS()-8,"ฤ")+"–ฅญ "
    @ 1, 2 SAY s
    ACTIVATE WINDOW W_2
    SELECT VIEW
    GO TOP
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F10 ACTIVATE MENU Seek_Pos
    ON KEY LABEL F1 DO Sh_Source
    ON KEY LABEL LeftMouse DO M_Browse WITH "Seek_Pos", "W_2", .T.

    BROWSE FIELDS IS_STO:H="", ;
                  PREFIX:H="", ;
                  NAME:H="",   ;
                  PRODUCER:H="",   ;
                  P000=IIF(LIST_TIT.IS_USD,STOCK.PRICE,STOCK.PRICE_R):P=pic_w:H="",   ;
                  P001=IIF(LIST_TIT.IS_USD,"$", "เ"):H="":1:W=.T.,   ;
                  DUMMY.F:H="" ;
             FREEZE DUMMY.F  ;
             NOMENU NORGRID NOAPPEND NODELETE NOMODIFY NOCLEAR  ;
             WINDOW W_3 IN WINDOW W_2

    SELECT (als)
    IF MOD(READKEY(),256) = 12
      GO rc_sav
    ELSE
      GO VIEW.REC_NO
      choose = .T.
    ENDIF
    IF USED("TMP_S")
      SELECT TMP_S
      USE
      DELETE FILE (tmpo_path+tmp_file+".DBF")
    ENDIF
    SELECT VIEW
    USE
    DELETE FILE (tmpo_path+tmp1_file+".DBF")
    DELETE FILE (tmpo_path+tmp1_file+".CDX")
    SELECT (als)
    SET RELATION TO &rel_sav
    SET ORDER TO &ord
    ON KEY LABEL Home
    ON KEY LABEL End
    ON KEY LABEL Enter
    ON KEY LABEL F1
    ON KEY LABEL F10
    ON KEY LABEL LeftMouse
    RELEASE WINDOW W_1
    RELEASE WINDOW W_2
    RELEASE WINDOW W_3
    DEACTIVATE MENU Seek_Pos
    HIDE MENU Seek_Pos
    RELEASE MENU Seek_Pos
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
  ENDIF
ENDIF
POP KEY
RETURN choose

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ , ฏฎช ง๋ข ๎้ ๏ จแโฎ็ญจช.                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sh_Source
PRIVATE wd, s, crsr
PUSH KEY CLEAR
crsr = SET("CURSOR") = "ON"
SET CURSOR OFF
wd = FSIZE("SOURCE","TMP_S")+8
DO D_W WITH "Source", 4, wd, "แโฎ็ญจช", 1
@ 3, 2 SAY PADC("  ฆฌจโฅ ซ๎กใ๎ ชซ ขจ่ใ ", WCOLS()-4, "ฤ")
s = RECNO()
SELECT TMP_S
GO s
s = TMP_S.SOURCE
s = IIF(EMPTY(s), PADR("ฅโ ค ญญ๋ๅ!",LEN(s)),s)
@ 2, 4 SAY s
= INKEY(0,"M")
RELEASE WINDOW Source
SELECT VIEW
IF crsr
  SET CURSOR ON
ENDIF
POP KEY
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  แฌฅญ๋ เฅฆจฌ .                              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sw_Mod
PARAMETERS m, m_nam
PRIVATE w

w = IIF(TYPE("m_nam") = "C", m_nam, "")

seek_m = m
KEYBOARD CHR(27)

IF .NOT. EMPTY(w)
  DEACTIVATE MENU (w)
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  เ กฎโ๋ แฎ แฏจแชฎฌ.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Edit_Lst
PRIVATE tmpOrd

SELECT STOCK
SET ORDER TO TAG CODE
SELECT LD
SET RELATION TO CODE INTO STOCK, LIST_NUM+STR(CODE) INTO L_ORD
DO CASE
CASE ord_sav = "ord_n"
  SET ORDER TO TAG L_NAME
CASE ord_sav = "ord_a"
  SET ORDER TO TAG L_ADDRESS
CASE ord_sav = "ord_u"
  SET ORDER TO TAG LIST
ENDCASE

IF .NOT. WEXIST("Not_Sto")
  DEFINE WINDOW Not_Sto FROM 1,WCOLS("")-18 TO 3,WCOLS("")-2 NONE SHADOW COLOR SCHEME 7
  ACTIVATE WINDOW Not_Sto NOSHOW
  @ 1, 0 SAY PADC("ฅโ ข แฏจแชฅ!",WCOLS())
ENDIF

PRIVATE prf_flt, stock_f, prf_show
PRIVATE md_rem, seek_m, prf_wd, fill_chr, double, rc_flt, jmp, i, r_must
prf_wd = FSIZE("NAME","STOCK")
seek_m = "edit"
md_rem = "edit"
fill_chr= "ฑ"
GO TOP

SET CONFIRM OFF

*
*   ฅญ๎ ฎกเ กฎโชจ ฎแโ โชฎข ( Rest )
*
DEFINE MENU Rest IN SCREEN COLOR SCHEME 4
DEFINE PAD Print   OF Rest PROMPT "ฅ็ โ์" KEY Ctrl-A, "F1"
ON SELECTION PAD Print OF Rest DO Sw_Mod WITH "Print_L", "Rest"
DEFINE PAD Return OF Rest PROMPT "ฅเญใโ์แ๏" KEY Ctrl-A, "Esc"
ON SELECTION PAD Return OF Rest DO Sw_Mod WITH "edit", "Rest"

*
*   ฅญ๎ ฎกเ กฎโชจ คฅไจๆจโ  ( Deficit )
*
DEFINE MENU Deficit IN SCREEN COLOR SCHEME 4
DEFINE PAD Auto   OF Deficit PROMPT "€ขโฎฌ โ" KEY Ctrl-A, "F4"
ON SELECTION PAD Auto OF Deficit DO Auto_Def
*DEFINE PAD Let    OF Deficit PROMPT " งเฅ่จโ์" KEY Ctrl-A, "F3"
*ON SELECTION PAD Let OF Deficit DO Sw_Let
DEFINE PAD Mode   OF Deficit PROMPT "ฅฆจฌ"
ON SELECTION PAD Mode OF Deficit DO Set_Mode1
DEFINE PAD Return OF Deficit PROMPT "ฅเญใโ์แ๏" KEY Ctrl-A, "Esc"
ON SELECTION PAD Return OF Deficit  DO Sw_Mod WITH "edit", "Deficit"

*
*   ฅญ๎ ฎกเ กฎโชจ แฏจแช  ( Edit_Lst )
*
DEFINE MENU Edit_Lst IN SCREEN COLOR SCHEME 4

*IF let_edit
  DEFINE PAD Mark   OF Edit_Lst PROMPT "โฌฅโจโ์"
  ON PAD Mark       OF Edit_Lst ACTIVATE POPUP Mark
  DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Mark PROMPT "ญขฅเโจเฎข โ์ ฏฎฌฅโชใ" KEY Ctrl-A, "F3"
  DEFINE BAR  2 OF Mark PROMPT "ญขฅเโจเฎข โ์ ขแฅ"
  DEFINE BAR  3 OF Mark PROMPT "ฎฌฅโจโ์ ขแฅ"
  DEFINE BAR  4 OF Mark PROMPT "‘ญ๏โ์ ขแฅ ฏฎฌฅโชจ"
  DEFINE BAR  5 OF Mark PROMPT "‘ใฌฌ  ฏฎ ฏฎฌฅ็ฅญญ๋ฌ ฏฎงจๆจ๏ฌ" KEY Ctrl-A, "Ctrl-S"
  ON SELECTION BAR 1 OF Mark DO Sw_Mark
  ON SELECTION BAR 2 OF Mark DO Sw_MarkA WITH 1
  ON SELECTION BAR 3 OF Mark DO Sw_MarkA WITH 2
  ON SELECTION BAR 4 OF Mark DO Sw_MarkA WITH 3
  ON SELECTION BAR 5 OF Mark DO Sum_Mark
*ENDIF

DEFINE PAD Seek   OF Edit_Lst PROMPT "ฎจแช"
ON PAD Seek       OF Edit_Lst ACTIVATE POPUP Seek
DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4
i = 1
IF let_edit
  DEFINE BAR  i OF Seek PROMPT " ญฎฌฅญชซ โใเฅ" KEY Ctrl-A, "F2"
  ON SELECTION BAR i OF Seek  DO Sw_Mod WITH "stock", "Edit_Lst"
  i = i+1
ENDIF

DEFINE BAR  i OF Seek PROMPT "ฎ ชฎญโฅชแโใ" KEY Ctrl-A, "F7"
ON SELECTION BAR i OF Seek  DO Sw_Mod WITH "sub_seek", "Edit_Lst"
i = i+1
*DEFINE BAR  i OF Seek PROMPT " ฌฅญ๋" KEY Ctrl-A, "F8"

IF let_edit

  DEFINE PAD Exec   OF Edit_Lst PROMPT "กเ กฎโช "
  ON PAD Exec       OF Edit_Lst ACTIVATE POPUP Exec
  DEFINE POPUP Exec MARGIN RELATIVE SHADOW COLOR SCHEME 4
  i = 1

  DEFINE BAR  i OF Exec PROMPT "ฅแชซ คแชจฅ ฏฎงจๆจจ"
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "strange", "Edit_Lst"
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "ฅไจๆจโ, ง ฌฅญ๋"
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "def_chang", "Edit_Lst"
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "ฅไจๆจโ"
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "deficit", "Edit_Lst"
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "”ฎเฌจเฎข ญจฅ ง ๏ขฎช ญ  ฏฎแโ."
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "sup_order", "Edit_Lst"
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "ฎฏ. ญฎฌฅญชซ โใเ "
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "Add_Sto", "Edit_Lst"
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "แโ โชจ"
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "rest", "Edit_Lst"
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "ฎซฃ: ญฅ ง ช ง๋ข โ์" KEY Ctrl-A, "F11"
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "stop_ord", "Edit_Lst"
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "‘ขฅเจโ์ ๆฅญ๋ แฎ แ็ฅโฎฌ"
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "cmp_prc", "Edit_Lst"
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "ฎซฃ ข ง ฏเฎแ" KEY Ctrl-A, "F8"
  ON SELECTION BAR i OF Exec DO Due_To WITH 1
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "ฅแ์ คฎซฃ ข ง ฏเฎแ"
  ON SELECTION BAR i OF Exec DO Due_To WITH 2
  i = i+1

  DEFINE BAR  i OF Exec PROMPT " ฏเฎแ ข คฎซฃ"
  ON SELECTION BAR i OF Exec DO To_Due WITH 1
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "ฅแ์ ง ฏเฎแ ข คฎซฃ"
  ON SELECTION BAR i OF Exec DO To_Due WITH 2
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "“ค ซจโ์ คฎซฃ" KEY Ctrl-A, "F6"
  ON SELECTION BAR i OF Exec DO Del_Due WITH 1
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "“ค ซจโ์ ขฅแ์ คฎซฃ"
  ON SELECTION BAR i OF Exec DO Del_Due WITH 2
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "ฅฆจฌ"
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "set_mode", "Edit_Lst"
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "‘ชจคช "
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "set_dis", "Edit_Lst"
  i = i+1
  IF acc_level < 3
    DEFINE BAR  i OF Exec PROMPT "‘ฏฅๆๆฅญ๋"
    ON SELECTION BAR i OF Exec DO Sw_Mod WITH "spec_prc", "Edit_Lst"
    i = i+1
  ENDIF
  DEFINE BAR  i OF Exec PROMPT "ฎแแโ ญฎขจโ์ ๆฅญ๋"
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "rev_price", "Edit_Lst"
  i = i+1

  DEFINE BAR  i OF Exec PROMPT "’จฏ ง ช ง "
  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "set_type", "Edit_Lst"
  i = i+1

  IF EMPTY(LIST_TIT.WHO_LET) .AND. acc_level > 1
    DEFINE BAR  i OF Exec PROMPT "“โขฅเคจโ์ ๆฅญ๋"
    ON SELECTION BAR i OF Exec DO Sw_Mod WITH "let_price", "Edit_Lst"
    i = i+1
  ENDIF
*  DEFINE BAR  i OF Exec PROMPT "เฎค โ์"
*  ON SELECTION BAR i OF Exec DO Sw_Mod WITH "sal_list", "Edit_Lst"
*  i = i+1
ENDIF

DEFINE PAD Docs   OF Edit_Lst PROMPT "ฎชใฌฅญโ๋"
ON PAD Docs       OF Edit_Lst ACTIVATE POPUP Docs_D
DEFINE POPUP Docs_D MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF Docs_D PROMPT "‘ฏจแฎช "
ON SELECTION BAR 1 OF Docs_D DO Sw_Mod WITH "p_list", "Edit_Lst"
DEFINE BAR 2 OF Docs_D PROMPT "‘ฏจแฎช แ ญฎเฌฎฉ ฎโฏใแช "
ON SELECTION BAR 2 OF Docs_D DO Sw_Mod WITH "p1_list", "Edit_Lst"
DEFINE BAR 3 OF Docs_D PROMPT " ๏ขช  ญ  แชซ ค "
ON SELECTION BAR 3 OF Docs_D DO Sw_Mod WITH "sto_ord", "Edit_Lst"
DEFINE BAR 12 OF Docs_D PROMPT " ๏ขชจ ญ  ฏฎแโ ขชใ "
ON SELECTION BAR 12 OF Docs_D DO Sw_Mod WITH "v_ord", "Edit_Lst"
DEFINE BAR 4 OF Docs_D PROMPT "‘็ฅโ "
ON SELECTION BAR 4 OF Docs_D DO Sw_Mod WITH "bill", "Edit_Lst"
DEFINE BAR 5 OF Docs_D PROMPT "‘็ฅโ กฅง แฏจแช "
ON SELECTION BAR 5 OF Docs_D DO Sw_Mod WITH "z_bill", "Edit_Lst"
DEFINE BAR 6 OF Docs_D PROMPT "‘ใฌฌ  ฏฎ แฏจแชใ" KEY Ctrl-A, "F1"
ON SELECTION BAR 6 OF Docs_D DO Sw_Mod WITH "l_sum", "Edit_Lst"
DEFINE BAR 13 OF Docs_D PROMPT "เจฌฅ็ ญจฅ" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION BAR 13 OF Docs_D DO Sw_Mod WITH "Note", "Edit_Lst"
DEFINE BAR 7 OF Docs_D PROMPT "เฎญ์" KEY Ctrl-A, "Ctrl-F1"
ON SELECTION BAR 7 OF Docs_D DO Sw_Mod WITH "bron", "Edit_Lst"
DEFINE BAR 8 OF Docs_D PROMPT "‘ฏเ ขช  ฏฎ ฏฎงจๆจจ" KEY Ctrl-A, "F9"
ON SELECTION BAR 8 OF Docs_D DO Sw_Mod WITH "p_rep", "Edit_Lst"
DEFINE BAR 9 OF Docs_D PROMPT "Product manager" KEY Ctrl-A, "F5"
ON SELECTION BAR 9 OF Docs_D DO Sw_Mod WITH "Product", "Edit_Lst"
DEFINE BAR 10 OF Docs_D PROMPT "”ใญชๆ. ญ งญ ็ฅญจฅ" KEY Ctrl-A, "F4"
ON SELECTION BAR 10 OF Docs_D DO Sw_Mod WITH "Function", "Edit_Lst"
DEFINE BAR 11 OF Docs_D PROMPT "‘เฎช ฏฎแโ ขชจ"
ON SELECTION BAR 11 OF Docs_D DO Sw_Mod WITH "Period", "Edit_Lst"
DEFINE BAR 14 OF Docs_D PROMPT " ซจ็จฅ ข ‘" KEY Ctrl-A, "Ctrl-D"
ON SELECTION BAR 14 OF Docs_D DO Sw_Mod WITH "Rest_DMS", "Edit_Lst"
IF Is_Acc(1)
  DEFINE BAR 15 OF Docs_D PROMPT " ชใฏฎ็ญ๋ฅ ๆฅญ๋"
  ON SELECTION BAR 15 OF Docs_D DO Sw_Mod WITH "bPrice", "Edit_Lst"
ENDIF

IF let_edit
  DEFINE PAD Commun OF Edit_Lst PROMPT "ฎฌฌใญจช ๆจจ"
  ON PAD Commun OF Edit_Lst ACTIVATE POPUP Commun
  DEFINE POPUP Commun MARGIN RELATIVE SHADOW COLOR SCHEME 4
  DEFINE BAR 1 OF Commun PROMPT "ชแฏฎเโ (ฎญ ฆฅ จฌฏฎเโ) ฏฎงจๆจฉ "
  ON SELECTION BAR 1 OF Commun DO Sw_Mod WITH "export", "Edit_Lst"
*  DEFINE BAR 2 OF Commun PROMPT "ฌฏฎเโ ฏฎงจๆจฉ "
*  ON SELECTION BAR 2 OF Commun DO Sw_Mod WITH "import", "Edit_Lst"
  DEFINE BAR 3 OF Commun PROMPT "ขฎค ง ๏ขชจ "
  ON SELECTION BAR 3 OF Commun DO Sw_Mod WITH "get_ord", "Edit_Lst"
  
ENDIF
DEFINE PAD Set_Ord   OF Edit_Lst PROMPT "ฎเ๏คฎช"
ON PAD Set_Ord       OF Edit_Lst ACTIVATE POPUP Ords_D
DEFINE POPUP Ords_D MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR 1 OF Ords_D PROMPT " จฌฅญฎข ญจ๏ "
ON SELECTION BAR 1 OF Ords_D DO Sw_Mod WITH "ord_n", "Edit_Lst"
DEFINE BAR 2 OF Ords_D PROMPT "€คเฅแ  "
ON SELECTION BAR 2 OF Ords_D DO Sw_Mod WITH "ord_a", "Edit_Lst"
DEFINE BAR 3 OF Ords_D PROMPT "ฎเ๏คฎช ขขฎค  "
ON SELECTION BAR 3 OF Ords_D DO Sw_Mod WITH "ord_u", "Edit_Lst"

DEFINE PAD Exit   OF Edit_Lst PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF Edit_Lst Do Sw_Mod WITH "exit", "Edit_Lst"

IF ent_rec # 0
  GO ent_rec
ENDIF
md_rem = seek_m
DO WHILE seek_m # "exit"
  SET RELATION TO CODE INTO STOCK, LIST_NUM+STR(CODE) INTO L_ORD
  DO CASE
  CASE seek_m = "edit"
    md_rem = seek_m
    seek_m = "exit"
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU Edit_Lst
    ON KEY LABEL F10 ACTIVATE MENU Edit_Lst
    DO D_Win WITH lst_titl, 2, show_rub
    ACTIVATE WINDOW L_Det_M BOTTOM
*    @ WROWS()-2, 3 SAY STR(sum_list,12,2) COLOR SCHEME 1
    ON KEY LABEL F7 DO Sw_Mod WITH "sub_seek"
    ON KEY LABEL Ctrl+F DO Sw_Mod WITH "rep_seek"
    ON KEY LABEL F3 DO Sw_Mark
    ON KEY LABEL Ctrl+S DO Sum_Mark
    IF let_edit
      ON KEY LABEL F2 DO Sw_Mod WITH "stock"
      ON KEY LABEL F8 DO Due_To WITH 1
      ON KEY LABEL F6 DO Del_Due WITH 1
      ON KEY LABEL Ctrl+R DO Rem_Dubl
    ENDIF
    ON KEY LABEL Ctrl+D DO Sw_Mod WITH "Rest_DMS"
    ON KEY LABEL LeftMouse DO M_Browse WITH "Edit_Lst", "L_Det_E"
    ON KEY LABEL F1 DO Sw_Mod WITH "l_sum"
    ON KEY LABEL Ctrl+F1 DO Sw_Mod WITH "bron"
    ON KEY LABEL F4 DO Sw_Mod WITH "Function"
    ON KEY LABEL F5 DO Sw_Mod WITH "Product"
    ON KEY LABEL F9 DO Sw_Mod WITH "p_rep"
    ON KEY LABEL F11 DO Sw_Mod WITH "stop_ord"
    ON KEY LABEL Ctrl-Enter DO Sw_Mod WITH "Note"
    ACTIVATE WINDOW L_Det_E
    IF LIST_TIT.IS_USD
      IF alt_val
        BROWSE FIELDS  LD.MARK_TMP:H="":R:W=.F.,  ;
                       LD.PREFIX:H="":R:W=.F., ;
                       Brw_Name=Nm_Part(LD.NAME):25:H="":R:W=.F.,   ;
                       f0001 = Show_Rest():H="":W=.F., ;
                       LD.QNT:H="":P="@Z":W=let_edit.AND.Rem_Val(LD.QNT):V=Exit_Qnt():F,   ;
                       P_TMP = Clc_Rub(LD.USD_PRICE):H="":P="@Z 999999.99999":W=.F.,   ;
                       P001="เ":H="":1:W=.F.,   ;
                       LD.QNT_DUE:H="":P="@Z":W=let_edit.AND.Sav_Due().AND.mode_due:V=Exit_Due(), ;
                       f0002 = B_Flags():R:H="":W=.F., ;
                       f0003 = LD.IS_OPT:R:H="":W=.F., ;
                       f0004 = IIF(FOUND("L_ORD"), "๛", " "):R:H="":W=.F., ;
                       fNote = IIF(EMPTY(LD.NOTE), " ", "N"):R:H="":W=.F., ;
                       DUMMY.F:H="":W=.NOT.let_edit   ;
                KEY lc          ;
                WHEN Show_D()   ;
                NOMENU NORGRID NOAPPEND NOCLEAR WINDOW L_Det_I IN WINDOW L_Det_E
       ELSE
         BROWSE FIELDS LD.MARK_TMP:H="":R:W=.F.,  ;
                       LD.PREFIX:H="":R:W=.F., ;
                       Brw_Name=Nm_Part(LD.NAME):25:H="":R:W=.F.,   ;
                       f0001 = Show_Rest():H="":W=.F., ;
                       LD.QNT:H="":P="@Z":W=let_edit.AND.Rem_Val(LD.QNT):V=Exit_Qnt():F,   ;
                       LD.USD_PRICE:H="":P=pic_w:W=let_edit.AND.W_Price():V=Exit_Prc():F,   ;
                       P001="$":H="":1:W=.F.,   ;
                       LD.QNT_DUE:H="":P="@Z":W=let_edit.AND.Sav_Due().AND.mode_due:V=Exit_Due(), ;
                       f0002 = B_Flags():R:H="":W=.F., ;
                       f0003 = LD.IS_OPT:R:H="":W=.F., ;
                       f0004 = IIF(FOUND("L_ORD"), "๛", " "):R:H="":W=.F., ;
                       fNote = IIF(EMPTY(LD.NOTE), " ", "N"):R:H="":W=.F., ;
                       DUMMY.F:H="":W=.NOT.let_edit   ;
                KEY lc          ;
                WHEN Show_D()   ;
               NOMENU NORGRID NOAPPEND NOCLEAR WINDOW L_Det_I IN WINDOW L_Det_E
       ENDIF
      ACTIVATE WINDOW L_Det_M BOTTOM
    ELSE
      IF alt_val
        BROWSE FIELDS  LD.MARK_TMP:H="":R:W=.F.,  ;
                       LD.PREFIX:H="":R:W=.F., ;
                       Brw_Name=Nm_Part(LD.NAME):25:H="":R:W=.F.,   ;
                       f0001 = Show_Rest():H="":W=.F., ;
                       LD.QNT:H="":P="@Z":W=let_edit.AND.Rem_Val(LD.QNT):V=Exit_Qnt():F,   ;
                       P_TM = ROUND(SAL_PRICE/kurs,4):H="":P="@Z 99999.9999":W=.F.,   ;
                       P001="$":H="":1:W=.F.,   ;
                       LD.QNT_DUE:H="":P="@Z":W=let_edit.AND.Sav_Due().AND.mode_due:V=Exit_Due(), ;
                       f0002 = B_Flags():R:H="":W=.F., ;
                       f0003 = LD.IS_OPT:R:H="":W=.F., ;
                       f0004 = IIF(FOUND("L_ORD"), "๛", " "):R:H="":W=.F., ;
                       fNote = IIF(EMPTY(LD.NOTE), " ", "N"):R:H="":W=.F., ;
                       DUMMY.F:H="":W=.NOT.let_edit   ;
                KEY lc          ;
                WHEN Show_D()   ;
                NOMENU NORGRID NOAPPEND NOCLEAR WINDOW L_Det_I IN WINDOW L_Det_E
      ELSE
        BROWSE FIELDS  LD.MARK_TMP:H="":R:W=.F.,  ;
                       LD.PREFIX:H="":R:W=.F., ;
                       Brw_Name=Nm_Part(LD.NAME):25:H="":R:W=.F.,   ;
                       f0001 = Show_Rest():H="":W=.F., ;
                       LD.QNT:H="":P="@Z":W=let_edit.AND.Rem_Val(LD.QNT):V=Exit_Qnt():F,   ;
                       LD.SAL_PRICE:H="":P=pic_w:W=let_edit.AND.W_Price():V=Exit_Prc():F,   ;
                       P001="เ":H="":1:W=.F.,   ;
                       LD.QNT_DUE:H="":P="@Z":W=let_edit.AND.Sav_Due().AND.mode_due:V=Exit_Due(), ;
                       f0002 = B_Flags():R:H="":W=.F., ;
                       f0003 = LD.IS_OPT:R:H="":W=.F., ;
                       f0004 = IIF(FOUND("L_ORD"), "๛", " "):R:H="":W=.F., ;
                       fNote = IIF(EMPTY(LD.NOTE), " ", "N"):R:H="":W=.F., ;
                       DUMMY.F:H="":W=.NOT.let_edit   ;
                KEY lc          ;
                WHEN Show_D()   ;
                NOMENU NORGRID NOAPPEND NOCLEAR WINDOW L_Det_I IN WINDOW L_Det_E
      ENDIF
    ENDIF
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    DEACTIVATE MENU Edit_Lst
    ON KEY LABEL F10
    ON KEY LABEL F7
    ON KEY LABEL F2
    ON KEY LABEL F4
    ON KEY LABEL F6
    ON KEY LABEL F3
    ON KEY LABEL Ctrl+S
    ON KEY LABEL Ctrl+R
    ON KEY LABEL LeftMouse
    ON KEY
  CASE seek_m = "def_chang"    && ฅไจๆจโ ง ฌฅญ๋
    DO View_Ch WITH LIST_TIT.LST_NUM
    stopTest = .T.
    seek_m = md_rem
  CASE seek_m = "Note"         && เจฌฅ็ ญจฅ
    IF .NOT. EOF() .AND. .NOT. BOF()
      IF EMPTY(LD.CODE)
        DO LD_Note WITH LIST_NUM, NAME+PREFIX+PRODUCER, .NOT. let_Edit
      ELSE
        DO LD_Note WITH LIST_NUM, CODE, .NOT. let_Edit
      ENDIF
    ENDIF
    seek_m = md_rem
  CASE seek_m = "stop_ord"     && ฎซฃ: ญฅ ง ช ง๋ข โ์
    IF LD.QNT_DUE # 0
      DO Stop_Ord WITH LD.LIST_NUM, LD.CODE, LD.QNT_DUE
    ENDIF
    seek_m = md_rem
  CASE seek_m = "Add_Sto"     && ฎฏ. ญฎฌฅญชซ โใเ 
    DO Add_Sto WITH STR(LIST_TIT.LST_NUM, 6)
    seek_m = md_rem
  CASE seek_m = "sup_order"   &&  ๏ขชจ ญ  ฏฎแโ ขชใ
    DO Order_0 WITH LIST_TIT.LST_NUM
    seek_m = md_rem
  CASE seek_m = "cmp_prc"      && ‘ขฅเจโ์ ๆฅญ๋ แฎ แ็ฅโฎฌ
    PRIVATE mssPrc, cmpType
    DIMENSION mssPrc[3]
    mssPrc[1] = ""
    mssPrc[2] = CHR(0)+" ชจฅ ฏฎงจๆจจ ฏเฎขฅเจโ์? [ แฅ ] [ ’ฎซ์ชฎ ฎโฌฅ็ฅญญ๋ฅ ]"
    mssPrc[3] = ""
    m.cmpType = Out_Mess(5, "mssPrc") = 2
    DO Cmp_Prc WITH LD.LIST_NUM, m.cmpType
    seek_m = md_rem
  CASE seek_m = "deficit"      && ฅไจๆจโ
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU Deficit
    ON KEY LABEL F10 ACTIVATE MENU Deficit
    i = "…”–’: "+lst_titl
    DO D_Win WITH i, 2, show_rub
    ACTIVATE WINDOW L_Det_M BOTTOM
*    @ WROWS()-2, 3 SAY STR(sum_list,12,2) COLOR SCHEME 1
    ACTIVATE WINDOW L_Det_E
    ON KEY LABEL F4 DO Auto_Def
*    ON KEY LABEL F3 DO Sw_Let
    ON KEY LABEL LeftMouse DO M_Browse WITH "Deficit", "L_Det_E"
    *
    *  ฅซ ฅฌ ํโฎ ญฅ FOR ข Browse, คซ๏ โฎฃฎ, ็โฎ ก๋ แฎฎโข. ขฅซ แฅก๏ ชฎญโฅชแโญ. ฏฎจแช
    *
    SET FILTER TO EMPTY(CODE) .AND. QNT > 0 .OR. MAX(STOCK.QNT,0)-QNT+QNT_REQ < 0
    SEEK lc
    IF FOUND()
      sav_val = QNT
      BROWSE FIELDS LD.MARK_TMP:H="",  ;
                    LD.PREFIX:H="", ;
                    Brw_Name=Nm_Part(LD.NAME):25:H="",   ;
                    f0001 = Show_Rest():H="", ;
                    LD.QNT:H="":P="@Z":W=Rem_Val(LD.QNT):V=Def_Qnt():F,   ;
                    P000=IIF(LIST_TIT.IS_USD, LD.USD_PRICE, LD.SAL_PRICE):H="":P=pic_w,   ;
                    P001=IIF(LIST_TIT.IS_USD,"$", "เ"):H="":1:W=.F.,   ;
                    LD.QNT_DUE:H="":P="@Z",   ;
                    f0002 = B_Flags():R:H="":3, ;
                    f0003 = LD.IS_OPT:R:H="":W=.F., ;
                    f0004 = IIF(FOUND("L_ORD"), "๛", " "):R:H="":W=.F., ;
                    fNote = IIF(EMPTY(LD.NOTE), " ", "N"):R:H="":W=.F. ;
              FREEZE LD.QNT   ;
              KEY lc          ;
              WHEN Show_D()   ;
              NOMENU NORGRID NOAPPEND NOCLEAR WINDOW L_Det_I IN WINDOW L_Det_E
    ELSE
      PRIVATE no_def
      DIMENSION no_def(3)
      no_def(1) = ""
      no_def(2) = CHR(0)+"ฅโ คฅไจๆจโ !"
      no_def(3) = ""
      DO Out_Mess WITH 7, "no_def"
    ENDIF
    SET FILTER TO
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    DEACTIVATE MENU Deficit
    ON KEY LABEL F10
    ON KEY LABEL F3
    ON KEY LABEL F4
    ON KEY LABEL LeftMouse
    ON KEY
    seek_m = md_rem
  CASE seek_m = "rest"       && แโ โชจ
    IF TYPE("tmpPerc") # "N"
      PRIVATE tmpPerc
      tmpPerc = Rest_P()
      IF tmpPerc <= 0
        seek_m = md_rem
        RELEASE tmpPerc
        LOOP
      ENDIF
    ENDIF
    tmpPerc = (1-tmpPerc/100)
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU Rest
    ON KEY LABEL F10 ACTIVATE MENU Rest
    i = "‘’€’: "+lst_titl
    DO D_Win WITH i, 2, show_rub
    ACTIVATE WINDOW L_Det_M BOTTOM
*    @ WROWS()-2, 3 SAY STR(sum_list,12,2) COLOR SCHEME 1
    ACTIVATE WINDOW L_Det_E
    ON KEY LABEL F1 DO Sw_Mod WITH "Print_L", "Rest"
    ON KEY LABEL LeftMouse DO M_Browse WITH "Rest", "L_Det_E"
    *
    *  ฅซ ฅฌ ํโฎ ญฅ FOR ข Browse, คซ๏ โฎฃฎ, ็โฎ ก๋ แฎฎโข. ขฅซ แฅก๏ ชฎญโฅชแโญ. ฏฎจแช
    *
    SET FILTER TO LD.QNT > STOCK.QNT*tmpPerc
    SEEK lc
    IF FOUND()
      sav_val = QNT
      BROWSE FIELDS LD.MARK_TMP:H="",  ;
                    LD.PREFIX:H="", ;
                    Brw_Name=Nm_Part(LD.NAME):25:H="",   ;
                    f0001 = Show_Rest():H="", ;
                    LD.QNT:H="":P="@Z":W=Rem_Val(LD.QNT):V=Exit_Qnt():F,   ;
                    P000=IIF(LIST_TIT.IS_USD, LD.USD_PRICE, LD.SAL_PRICE):H="":P=pic_w,   ;
                    P001=IIF(LIST_TIT.IS_USD,"$", "เ"):H="":1:W=.F.,   ;
                    LD.QNT_DUE:H="":P="@Z",   ;
                    f0002 = B_Flags():R:H="":3, ;
                    f0003 = LD.IS_OPT:R:H="":W=.F., ;
                    f0004 = IIF(FOUND("L_ORD"), "๛", " "):R:H="":W=.F.,;
                    fNote = IIF(EMPTY(LD.NOTE), " ", "N"):R:H="":W=.F. ;
              FREEZE LD.QNT   ;
              KEY lc          ;
              WHEN Show_D()   ;
              NOMENU NORGRID NOAPPEND NOCLEAR WINDOW L_Det_I IN WINDOW L_Det_E
    ELSE
      PRIVATE no_def
      DIMENSION no_def(3)
      no_def(1) = ""
      no_def(2) = CHR(0)+"ฅโ ฏฎงจๆจฉ, ฎฃฎซ๏๎้จๅ แชซ ค!"
      no_def(3) = ""
      DO Out_Mess WITH 7, "no_def"
    ENDIF
    SET FILTER TO
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    DEACTIVATE MENU Rest
    ON KEY
    IF seek_m # "Print_L"
      RELEASE tmpPerc
      seek_m = md_rem
    ENDIF

  CASE seek_m = "strange"    && —ใฆจฅ ฏฎงจๆจจ
    DO Strange
    seek_m = md_rem
  CASE seek_m = "l_sum"
    DO Lst_Sum WITH LIST_TIT.LST_NUM
    seek_m = "edit"
  CASE seek_m = "Product"
    IF CODE = 0
      PRIVATE no_sto
      DIMENSION no_sto(3)
      no_sto(1) = ""
      no_sto(2) = CHR(0)+"ฅแชซ คแช ๏ ฏฎงจๆจ๏!"
      no_sto(3) = ""
      DO Out_Mess WITH 7, "no_sto"
    ELSE
      DO Autor WITH CODE
    ENDIF
    seek_m = "edit"
  CASE seek_m = "Function"
    IF CODE = 0
      PRIVATE no_sto
      DIMENSION no_sto(3)
      no_sto(1) = ""
      no_sto(2) = CHR(0)+"ฅแชซ คแช ๏ ฏฎงจๆจ๏!"
      no_sto(3) = ""
      DO Out_Mess WITH 7, "no_sto"
    ELSE
      DO Sto_Add WITH CODE, .T.
    ENDIF
    seek_m = "edit"
  CASE seek_m = "Period"
    IF CODE = 0
      PRIVATE no_sto
      DIMENSION no_sto(3)
      no_sto(1) = ""
      no_sto(2) = CHR(0)+"ฅแชซ คแช ๏ ฏฎงจๆจ๏!"
      no_sto(3) = ""
      DO Out_Mess WITH 7, "no_sto"
    ELSE
      DO Time_Inc WITH CODE
    ENDIF
    seek_m = "edit"
  CASE seek_m = "Rest_DMS"
    IF CODE = 0
      PRIVATE no_sto
      DIMENSION no_sto(3)
      no_sto(1) = ""
      no_sto(2) = CHR(0)+"ฅแชซ คแช ๏ ฏฎงจๆจ๏!"
      no_sto(3) = ""
      DO Out_Mess WITH 7, "no_sto"
    ELSE
      DO Rest_DMS WITH CODE
    ENDIF
    seek_m = "edit"
  CASE seek_m = "bPrice"
    IF CODE = 0
      PRIVATE no_sto
      DIMENSION no_sto(3)
      no_sto(1) = ""
      no_sto(2) = CHR(0)+"ฅแชซ คแช ๏ ฏฎงจๆจ๏!"
      no_sto(3) = ""
      DO Out_Mess WITH 7, "no_sto"
    ELSE
      DO V_B_Prc WITH CODE
    ENDIF
    seek_m = "edit"
  CASE seek_m = "sub_seek"
    seek_m = "edit"
  CASE seek_m = "ord_n"
    ord_sav = "ord_n"
    SET ORDER TO TAG L_NAME
    seek_m = "edit"
  CASE seek_m = "ord_a"
    ord_sav = "ord_a"
    SET ORDER TO TAG L_ADDRESS
    seek_m = "edit"
  CASE seek_m = "ord_u"
    ord_sav = "ord_u"
    SET ORDER TO TAG LIST
    seek_m = "edit"
  CASE seek_m = "s_del"
    SEEK lc
    SCAN REST WHILE LIST_NUM = lc
      IF CODE > 0
        LOOP
      ENDIF
      REPLACE QNT WITH 0
    ENDSCAN
    seek_m = "edit"
  CASE seek_m = "p_list"     && ‘ฏจแฎช
    DO P_List WITH LIST_TIT.LST_NUM, alt_val && WITH "List_M"
    seek_m = md_rem
  CASE seek_m = "Print_L"     && ‘ฏจแฎช
    DO P_List WITH LIST_TIT.LST_NUM, alt_val, tmpPerc && WITH "List_M"
    seek_m = "rest"
  CASE seek_m = "p1_list"     && ‘ฏจแฎช แ ญฎเฌฎฉ ฎโฏใแช 
    DO P_List1 WITH LIST_TIT.LST_NUM, alt_val && WITH "List_M"
    seek_m = md_rem
  CASE seek_m = "bill"       && ‘็ฅโ
    DO Bill_0 WITH LIST_TIT.LST_NUM
    seek_m = md_rem
  CASE seek_m = "z_bill"     && ‘็ฅโ กฅง แฏจแช 
    DO Bill
    seek_m = md_rem
  CASE seek_m = "v_ord"      &&  ๏ขชจ ญ  ฏฎแโ ขชใ
    DO V_Ord WITH LIST_NUM, CODE
    seek_m = md_rem
  CASE seek_m = "bron"      && ‘ฏเ ขช  ฏฎ ฏฎงจๆจจ
    IF EMPTY(CODE)
      PRIVATE mss000
      DIMENSION mss000(3)
      mss000(1) = ""
      mss000(2) = CHR(0)+"ฅแชซ คแช ๏ ฏฎงจๆจ๏!"
      mss000(3) = ""
      DO Out_Mess WITH 7, "mss000"
    ELSE
      DO View_Br WITH CODE
    ENDIF
    seek_m = md_rem
  CASE seek_m = "p_rep"      && ‘ฏเ ขช  ฏฎ ฏฎงจๆจจ
    IF EMPTY(CODE)
      PRIVATE mss000
      DIMENSION mss000(3)
      mss000(1) = ""
      mss000(2) = CHR(0)+"ฅแชซ คแช ๏ ฏฎงจๆจ๏!"
      mss000(3) = ""
      DO Out_Mess WITH 7, "mss000"
    ELSE
      DO Move_S WITH CODE
    ENDIF
    seek_m = md_rem
  CASE seek_m = "sto_ord"    &&  ๏ขช  ญ  แชซ ค
*    DO Prep_Ord
    DO Prp_Ord WITH LIST_NUM
*    @ WROWS()-2, 3 SAY STR(sum_list,12,2) COLOR SCHEME 1
    seek_m = md_rem
  CASE seek_m = "set_dis"
    DO Discount WITH "", ""
    seek_m = IIF(.NOT. EMPTY(LIST_TIT.IS_DIS), "exit", md_rem)
  CASE seek_m = "set_mode"
    DO Set_Mode WITH ""
    seek_m = md_rem
  CASE seek_m = "let_price"   && “โขฅเฆคฅญจฅ ๆฅญ
    DO Let_Sale
    seek_m = md_rem
  CASE seek_m = "export"   && ชแฏฎเโ ฏฎงจๆจฉ
    DO Export
    seek_m = md_rem

  CASE seek_m = "set_type"  && “แโ ญฎขช  โจฏ  ง ช ง 
    IF .NOT. BOF() .AND. .NOT. EOF()
      m.tmpOrd = NEW_ORD
      m.tmpOrd = Ch_O_T(m.tmpOrd)
      REPLACE NEW_ORD WITH m.tmpOrd
    ENDIF
    seek_m = md_rem
  CASE seek_m = "get_ord"   && ขฎค ง ๏ขชจ ๏ชฎก๋ แ คจแชฅโ๋
    DO Get_ord WITH "List_M"
    seek_m = md_rem
  CASE seek_m = "spec_prc"    && “แโ ญฎขช  แฏฅๆๆฅญ
    DO Set_Spec
    seek_m = md_rem
  CASE seek_m = "rev_price"    && ฎแแโ ญฎขซฅญจฅ ๆฅญ
    DO Rev_Price WITH "List_M"
    seek_m = md_rem
*  CASE seek_m = "sal_list"     && เฎค ฆ 
*    DO List_Sal WITH "List_M"
*    seek_m = md_rem
*    IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
*      seek_m = "exit"
*    ENDIF
  CASE seek_m = "stock"
    md_rem = seek_m
    what_do = "stock"
    seek_m = "exit"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO
SELECT LD
SET RELATION TO
DEACTIVATE MENU Edit_Lst
RELEASE MENU Edit_Lst EXTENDED
DEACTIVATE MENU Deficit
RELEASE MENU Deficit EXTENDED
RELEASE WINDOW L_Det_M
RELEASE WINDOW L_Det_E
RELEASE WINDOW L_Det_I
RELEASE WINDOW Not_Sto
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ           เฎฃเ ฌฌ  ฎกเ กโ็จช ฌ๋่ชจ ข เฅฆจฌฅ BROWSE.                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_Browse
PARAMETERS menu_n, w_ext, is_enter
PRIVATE ml, mc

IF .NOT. EMPTY(menu_n) .AND. MROW("") = 0

  IF MENU() == UPPER(menu_n)
    RETURN
  ENDIF

  ACTIVATE MENU (menu_n)
  RETURN
ENDIF

IF MROW(w_ext) >=0 .AND. MCOL(w_ext) >= 0  && ‘โเฎช  แฏจแช 
  IF is_enter
    KEYBOARD CHR(13)
  ENDIF
  RETURN
ENDIF

ml = MROW("")
mc = MCOL("")

= INKEY("M")
IF BETWEEN(mc, WLCOL(w_ext), WLCOL(w_ext)+WCOLS(w_ext)-1)
  IF ml = WLROW(w_ext)-1
    KEYBOARD "{PgUp}"
    RETURN
  ENDIF
  IF ml = WLROW(w_ext)+WROWS(w_ext)
    KEYBOARD "{PgDn}"
    RETURN
  ENDIF
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  ใแโ ญฎขชจ/ฎ็จแโชจ ฏฎฌฅโชจ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sw_Mark
PRIVATE rc, sel_sav
*sel_sav = SELECT()
SELECT LD
IF cur_line # RECNO("LD")
*  WAIT WINDOW ALLTRIM(LD.NAME)
  GO cur_line
ENDIF
*IF BETWEEN(rc, 1, RECCOUNT())
*  GO rc
  REPLACE MARK_TMP WITH IIF(EMPTY(MARK_TMP), "๛", " ")
*ENDIF
*SELECT (sel_sav)
DEACTIVATE MENU Edit_Lst

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  ฌ แแฎขฎฉ ใแโ ญฎขชจ/ฎ็จแโชจ ฏฎฌฅโชจ.              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sw_MarkA
PARAMETERS md

PRIVATE als, rc

als = SELECT()
SELECT LD
rc = RECNO()
SEEK lc
SCAN REST WHILE lc = STR(LIST_TIT.LST_NUM,6)
  DO CASE
  CASE md = 1
    REPLACE LD.MARK_TMP WITH IIF(EMPTY(LD.MARK_TMP), "๛", " ")
  CASE md = 2
    REPLACE LD.MARK_TMP WITH "๛"
  CASE md = 3
    REPLACE LD.MARK_TMP WITH " "
  ENDCASE
ENDSCAN
GO rc
SELECT (als)
DEACTIVATE MENU Edit_Lst

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                           กเ กฎโช  คฅไจๆจโ .                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Def_Qnt
PRIVATE v1, pr, delta, i, q_w
IF LD.QNT < 0
  IF ALIAS() = "LD" .AND. CODE = 0 .AND. mode_defD
    IF QNT+QNT_DUE < 0
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"๋ แโฎซ์ชฎ ญฅ คฎซฆญ๋!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      ACTIVATE WINDOW (ALIAS())
      RETURN .F.
    ENDIF
    REPLACE QNT_DUE WITH QNT_DUE+QNT, QNT WITH 0, ORD_TYPE WITH mode_ord
*    IF LD.QNT = 0 .AND. LD.QNT_DUE = 0 .AND. LD.QNT_REQ = 0
*      SKIP IN LD
*      KEYBOARD "{DNARROW}"
*    ENDIF
    RETURN .T.
  ENDIF
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฎงขเ โ ฎแใ้ฅแโขซ๏ฅโแ๏ ็ฅเฅง แฏฅๆจ ซ์ญใ๎ ฏเฎๆฅคใเใ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  ACTIVATE WINDOW (ALIAS())
  RETURN .F.
ENDIF
SHOW GETS

IF LD.QNT = sav_val
  RETURN .T.
ENDIF

IF LD.QNT > sav_val .AND. LD.QNT > 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ใค  ฆฅ กฎซ์่ฅ, ฅแซจ จ โ ช ญฅ ๅข โ ฅโ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  ACTIVATE WINDOW (ALIAS())
  REPLACE QNT WITH sav_val
  RETURN .F.
ENDIF

i = sav_val-LD.QNT
IF mode_defD
  REPLACE LD.QNT_DUE WITH LD.QNT_DUE+i, ORD_TYPE WITH mode_ord
ENDIF
IF mode_defS
  DO To_Dem0 WITH i  && 0 - คฎก ขซฅญจฅ ข แฏเฎแ จง ไ ฉซ  แฏจแช 
ENDIF
delta = IIF(LIST_TIT.IS_USD, LD.USD_PRICE, LD.SAL_PRICE)*(LD.QNT-sav_val)
*sum_list = sum_list+delta

ACTIVATE WINDOW L_Det_M BOTTOM
*@ WROWS()-2, 3 SAY STR(sum_list,12,2) COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS())

sav_val = LD.QNT

*IF LD.QNT = 0 .AND. LD.QNT_DUE = 0 .AND. LD.QNT_REQ = 0
*  SKIP IN LD
*  KEYBOARD "{DNARROW}"
*ENDIF
IF FOUND("STOCK")
  q_w = LD.QNT
*  IF q_w % MAX(1, STOCK.QNT_ROUND) # 0 .AND. q_w > 0
    q_w = Round_P(STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER, q_w, STOCK.QNT_ROUND, STOCK.QNT_PACK, STOCK.PRICE)
    REPLACE LD.QNT WITH q_w
*  ENDIF
ENDIF

DO Show_D

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                €ขโฎฌ โจ็ฅแช ๏ ฎกเ กฎโช  คฅไจๆจโ .                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Auto_Def
PRIVATE rc_sav, i, mss

PUSH KEY CLEAR
rc_sav = RECNO()

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  ๋คฅซ๏ฅฌ แชซ คแชฎฉ คฅไจๆจโ ข ฎโคฅซ์ญ๋ฉ แฏจแฎช? [   ] [ ฅโ ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 1
  DO Def_List WITH LIST_TIT.LST_NUM
ENDIF

SEEK lc
SCAN REST WHILE lc = LIST_NUM
*  IF .NOT. EMPTY(IS_DEF) .AND. CODE # 0
*    LOOP
*  ENDIF
  IF CODE > 0
    i = QNT-MAX(STOCK.QNT,0)-QNT_REQ
    IF i <= 0
      LOOP
    ENDIF
  ELSE
    i = QNT
  ENDIF
*  sum_list = sum_list-i*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
  REPLACE QNT WITH QNT-i
  IF sale_mode = "Mitracon"
    IF QNT < STOCK.QNT_ROUND
      IF .NOT. EMPTY(LD.IS_OPT)
        IF LIST_TIT.IS_USD
          REPLACE LD.IS_OPT    WITH " ",         ;
                  LD.USD_PRICE WITH STOCK.PRICE, ;
                  LD.USD_DEF   WITH STOCK.PRICE, ;
                  LD.DEF_PRICE WITH STOCK.PRICE_R
        ELSE
          REPLACE LD.IS_OPT    WITH " ",           ;
                  LD.SAL_PRICE WITH STOCK.PRICE_R, ;
                  LD.USD_DEF   WITH STOCK.PRICE,   ;
                  LD.DEF_PRICE WITH STOCK.PRICE_R
        ENDIF
      ENDIF
    ELSE
      IF EMPTY(LD.IS_SPEC) .AND.  STOCK.QNT_ROUND > 1
        IF LIST_TIT.IS_USD
          REPLACE LD.IS_OPT    WITH "*",            ;
                  LD.USD_PRICE WITH STOCK.PRICE_O1, ;
                  LD.USD_DEF   WITH STOCK.PRICE_O1, ;
                  LD.DEF_PRICE WITH STOCK.PRICE_O1_R
        ELSE
          REPLACE LD.IS_OPT    WITH " ",             ;
                  LD.SAL_PRICE WITH STOCK.PRICE_O1_R,;
                  LD.USD_DEF   WITH STOCK.PRICE_O1,  ;
                  LD.DEF_PRICE WITH STOCK.PRICE_O1_R
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  IF mode_defD
    REPLACE LD.QNT_DUE WITH LD.QNT_DUE+i, ORD_TYPE WITH mode_ord
  ENDIF
  IF mode_defS
    DO To_Dem0 WITH i  && 0 - คฎก ขซฅญจฅ ข แฏเฎแ จง ไ ฉซ  แฏจแช 
  ENDIF
ENDSCAN

ACTIVATE WINDOW L_Det_M BOTTOM
*@ WROWS()-2, 3 SAY STR(sum_list,12,2) COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS())
GO rc_sav
sav_val = QNT
POP KEY
DEACTIVATE MENU Deficit

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             คแโเฎฉช  ญ ค ฏเฎฃเ ฌฌฎฉ ใแโ ญฎขชจ เฅฆจฌ .                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Mode1

HIDE MENU Deficit
DO Set_Mode
SHOW MENU Deficit
ACTIVATE WINDOW (ALIAS())
DEACTIVATE MENU Deficit

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ           “แโ ญฎขช /ฎ็จแโช  ไซ ฆช  "ขแฅ เ ขญฎ ฏเฎค โ์!".              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sw_Let
PRIVATE i

IF .NOT. EMPTY(IS_DEF)
  REPLACE IS_DEF WITH " "
ELSE
  i = 0
  IF CODE = 0
    i = QNT
  ELSE
    i = QNT-MAX(STOCK.QNT,0)-QNT_REQ
  ENDIF
  IF i > 0
    REPLACE IS_DEF WITH "*"
  ELSE
    PRIVATE enoug
    PRIVATE enoug(3)
    enoug(1) = ""
    enoug(2) = CHR(0)+"€ ็โฎ เ งเฅ่ โ์?  โ ช ๅข โ ฅโ!"
    enoug(3) = ""
    DO Out_Mess WITH 7, "enoug"
  ENDIF
ENDIF
DEACTIVATE MENU Deficit

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                            ฎซฃ ข ง ฏเฎแ.                             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Due_To
PARAMETER alone

PRIVATE sel_sav, rc_sav

PUSH KEY CLEAR
sel_sav = SELECT()
SELECT LD

IF alone = 1
*  sum_list = sum_list+QNT_DUE*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
  REPLACE LD.QNT WITH LD.QNT+LD.QNT_DUE, LD.QNT_DUE WITH 0
ELSE
  rc_sav = RECNO()
  = SEEK(lc)
  SCAN REST WHILE LIST_NUM = lc FOR QNT_DUE # 0
*    sum_list = sum_list+QNT_DUE*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
    REPLACE LD.QNT WITH LD.QNT+LD.QNT_DUE, LD.QNT_DUE WITH 0
  ENDSCAN
  GO rc_sav
ENDIF

SELECT (sel_sav)
ACTIVATE WINDOW L_Det_M BOTTOM
*@ WROWS()-2, 3 SAY STR(sum_list,12,2) COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS())
POP KEY
IF .NOT. EMPTY(MENU())
  KEYBOARD CHR(27)
ENDIF

RETURN


*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                             ฏเฎแ ข คฎซฃ.                             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE To_Due
PARAMETER alone
PRIVATE sel_sav, rc_sav, mv_qnt
sav_val = 0

PUSH KEY CLEAR
sel_sav = SELECT()
SELECT LD

IF alone = 1
  m.mv_qnt = MAX(LD.QNT-LD.QNT_REQ, 0)
  REPLACE LD.QNT_DUE WITH LD.QNT_DUE+m.mv_qnt, ORD_TYPE WITH mode_ord
  REPLACE LD.QNT WITH LD.QNT-m.mv_qnt
ELSE
  rc_sav = RECNO()
  = SEEK(lc)
  SCAN REST WHILE LIST_NUM = lc FOR QNT # 0
    m.mv_qnt = MAX(LD.QNT-LD.QNT_REQ, 0)
    REPLACE LD.QNT_DUE WITH LD.QNT_DUE+m.mv_qnt, ORD_TYPE WITH mode_ord
    REPLACE LD.QNT WITH LD.QNT-m.mv_qnt
  ENDSCAN
  GO rc_sav
ENDIF
SELECT (sel_sav)
ACTIVATE WINDOW L_Det_M BOTTOM
ACTIVATE WINDOW (ALIAS())
POP KEY
IF .NOT. EMPTY(MENU())
  KEYBOARD CHR(27)
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                           ็จแโช  คฎซฃ .                              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Del_Due
PARAMETER alone

PRIVATE sel_sav, rc_sav

PUSH KEY CLEAR
sel_sav = SELECT()
SELECT LD

IF alone = 1
  REPLACE LD.QNT_DUE WITH 0
ELSE
  rc_sav = RECNO()
  = SEEK(lc)
  REPLACE REST WHILE LIST_NUM = lc FOR QNT_DUE # 0 LD.QNT_DUE WITH 0
  GO rc_sav
ENDIF

SELECT (sel_sav)
POP KEY
IF .NOT. EMPTY(MENU())
  KEYBOARD CHR(27)
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                          “แโ ญฎขช  แฏฅๆๆฅญ.                           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Spec
PRIVATE s0, rc, prc, sel_rem, ex, ln, wd

sel_rem = SELECT()
PUSH KEY CLEAR
DEFINE WIND H_Line FROM 0, 0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW H_Line

ln = 8
wd = 42

DO D_W WITH "Set_Spec", ln, wd, "“แโ ญฎขช  แฏฅๆๆฅญ", 1

prc = 0
ex = 1
@ 3, 9 SAY "เฎๆฅญโ แชจคชจ" GET prc PICTURE "@Z 9999.99"
@ 5, 4 GET ex PICTURE "@*HT \  OK Ctrl-W  ;\? โช ง โ์แ๏ Esc "
READ CYCLE VALID R_Dis()

IF ex = 1
  rc = RECNO()
  ln = 0
  s0 = 0
  prc = 1-prc/100
  SELECT 0
  USE path_comm+"SUBJECT" AGAIN ALIAS SUB_2315 ORDER TAG STO_CODE
  SELECT LD
  SEEK lc
  SCAN REST WHILE LIST_NUM = lc
    IF .NOT. EMPTY(MARK_TMP)
      = SEEK(CODE, "SUB_2315")
      IF EMPTY(SUB_2315.NODISC)
        IF LIST_TIT.IS_USD
          REPLACE USD_PRICE WITH USD_PRICE*prc, ;
                  MARK_TMP  WITH " ",           ;
                  IS_SPEC   WITH IIF(USD_PRICE # DEF_PRICE, "*", " ")
        ELSE
          REPLACE SAL_PRICE WITH ROUND_6(DEF_PRICE*prc),   ;
                  MARK_TMP  WITH " ",                      ;
                  IS_SPEC   WITH IIF(SAL_PRICE # ROUND_6(DEF_PRICE), "*", " ")
        ENDIF
        ln = ln+1
      ENDIF
    ENDIF
    s0 = s0+IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)*QNT
  ENDSCAN
  GO rc
  IF ln = 0 .AND. LIST_NUM = lc   && ใ ญจ ไจฃ  แฅกฅ!  ช ํโฎ ฌฎฆฅโ ก๋โ์?!
    s0 = s0-IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)*QNT
    = SEEK(CODE, "SUB_2315")
    IF EMPTY(SUB_2315.NODISC)
      IF LIST_TIT.IS_USD
        REPLACE USD_PRICE WITH USD_PRICE*prc, ;
                IS_SPEC   WITH IIF(USD_PRICE # DEF_PRICE, "*", " ")
      ELSE
        REPLACE SAL_PRICE WITH ROUND_6(DEF_PRICE*prc),   ;
                IS_SPEC   WITH IIF(SAL_PRICE # ROUND_6(DEF_PRICE), "*", " ")
      ENDIF
    ENDIF
    s0 = s0+IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)*QNT
  ENDIF
  SELECT LIST_TIT
  REPLACE R_TOTAL WITH s0, WHO_LET WITH IIF(acc_level < 2, user, 0)
*  ACTIVATE WINDOW L_Det_M BOTTOM
*  @ 1, 49 SAY STR(s0,12,2) COLOR SCHEME 1
*  sum_list = s0
  SELECT (sel_rem)
ENDIF

RELEASE WINDOW Set_Spec
RELEASE WINDOW H_Line
SELECT SUB_2315
USE
SELECT (sel_rem)

POP KEY
RETURN
************************************************************************
PROCEDURE R_Dis
PRIVATE s, mss
IF READKEY() % 256 = 12
  ex = 2
ENDIF
IF ex # 1
  RETURN .T.
ENDIF
IF prc >= 100
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ใ จ แชจคช ! ’ ช ญฅ ก๋ข ฅโ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF File_O(base_path+"DISBOUND.MEM")
  RESTORE FROM (base_path+"DISBOUND.MEM") ADDITIVE
  IF prc > disbound
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ๋ ใแโ ญฎขจซจ แชจคชใ "+ALLTRIM(STR(prc))+ ;
                    ", ํโฎ ฏเ ขจซ์ญฎ? [   ] [ ฅโ ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ขฎแแโ ญฎขซฅญจ๏ ๆฅญ.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rev_Price
PARAMETERS ext_menu

IF .F. &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
PRIVATE start_p, sale_p, ex, ln, wd, rc
DO Entry_P WITH ext_menu, ""

ln = 5
wd = 44
DO D_W WITH "R_Price", ln, wd, "ฎแแโ ญ ขซจข ฅฌ ๆฅญ๋:", 1

start_p = .T.
sale_p = .T.
ex = 1

@ 2, 3 GET start_p PICTURE "@*C  ็ ซ์ญ๋ฅ ๆฅญ๋"
@ 2,25 GET sale_p  PICTURE "@*C –ฅญ๋ ฏเฎค ฆจ"
@ 3, 6 GET ex      PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE

RELEASE WINDOW "R_Price"
IF .NOT. start_p .AND. .NOT. sale_p .OR. ex = 2
  DO Exit_P WITH ext_menu, ""
  RETURN
ENDIF
ENDIF &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

rc = RECNO()
SEEK lc
*sum_list = 0
SCAN REST WHILE LIST_NUM = lc
  IF sale_mode = "Mitracon"
    IF QNT >= STOCK.QNT_ROUND .AND. STOCK.QNT_ROUND > 1
      REPLACE DEF_PRICE WITH STOCK.PRICE_O1_R, ;
              USD_DEF   WITH STOCK.PRICE_O1,   ;
              IS_OPT    WITH "*"
    ELSE
      REPLACE DEF_PRICE WITH STOCK.PRICE_R, ;
              USD_DEF   WITH STOCK.PRICE,   ;
              IS_OPT    WITH " "
    ENDIF
    REPLACE SAL_PRICE WITH IIF(LIST_TIT.IS_USD, 0, DEF_PRICE),   ;
            USD_PRICE WITH IIF(LIST_TIT.IS_USD, USD_DEF, 0),     ;
            IS_SPEC   WITH " "
  ELSE
    REPLACE DEF_PRICE WITH STOCK.PRICE_R, ;
            USD_DEF   WITH STOCK.PRICE,   ;
            SAL_PRICE WITH IIF(LIST_TIT.IS_USD, 0, STOCK.PRICE_R),  ;
            USD_PRICE WITH IIF(LIST_TIT.IS_USD, STOCK.PRICE, 0),    ;
            IS_SPEC   WITH " "
  ENDIF
*  sum_list = sum_list+QNT*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
ENDSCAN
IF BETWEEN(rc, 1, RECCOUNT())
  GO rc
ENDIF

*IF sale_p
  SELECT LIST_TIT
  REPLACE IS_SPEC WITH " ", ;
          IS_DIS  WITH " ", ;
          PRESENT WITH 0,   ;
          R_TOTAL WITH sum_list,  ;
          WHO_LET WITH 0
  SELECT LD
  ACTIVATE WINDOW L_Det_M BOTTOM
*  @ WROWS()-2, 3 SAY STR(sum_list,12,2) COLOR SCHEME 1
*ENDIF

DO Exit_P WITH ext_menu, ""
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    ๋ๅฎค จง ไจชโจขญฎฃฎ ฏฎซ๏.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sk
PRIVATE rk, lk, rc_old, r_w

SELECT STOCK   && ฎ็ฅฌใ-โฎ ฌฅญ๏ฅโแ๏  ซจ แ ?!

IF double # 0
  double = 0
  RETURN .T.     && CHRSAW() .OR. r_must = 0
ENDIF

rk = MOD(READKEY(), 256)

DO CASE
CASE rk = 0    && Back Space ! Left Arrow
  IF LEN(prf_flt) = 0
    IF .NOT. prf_flt == prf_show .AND. .NOT. CHRSAW()
      DO Show_Prf
    ENDIF
    RETURN CHRSAW() .OR. r_must = 0
  ELSE
    prf_flt = LEFT(prf_flt,LEN(prf_flt)-1)
  ENDIF
CASE rk = 15   && ‘จฌขฎซ

  IF LEN(prf_flt) = prf_wd
    IF .NOT. prf_flt == prf_show .AND. .NOT. CHRSAW()
      DO Show_Prf
    ENDIF
    RETURN CHRSAW() .OR. r_must = 0
  ENDIF
  prf_flt = prf_flt+DUMMY.F
OTHERWISE
  RETURN CHRSAW() .OR. r_must = 0
ENDCASE

DO Show_Prf

IF .NOT. stock_f .AND. rk # 0
  RETURN .T.
ENDIF

rc_flt = 0
rc_old = RECNO()
stock_f = SEEK(IIF(ALIAS() = "STOCK", "", lc)+prf_flt)
r_w = IIF(stock_f, RECNO(), RECNO(0))

IF stock_f
  HIDE WINDOW Not_Sto
ELSE
  SHOW WINDOW Not_Sto
  ACTIVATE WINDOW (ALIAS())
ENDIF

r_must = IIF(r_w = rc_old, r_must, r_w)
IF CHRSAW()
  RETURN .T.
ENDIF

RETURN r_must = 0

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ข๋แขฅโชจ ฏเฅไจชแ .                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_Prf

PRIVATE l_s, c_s, wn

l_s = IIF(TYPE("l_prf") = "N", l_prf, 1)
c_s = IIF(TYPE("c_prf") = "N", c_prf, 9)
wn  = IIF(TYPE("w_main") = "C", w_main, "L_Det_M")

ACTIVATE WINDOW (wn) SAME
*@ 1, 9 SAY PADR(prf_flt,prf_wd,fill_chr) COLOR SCHEME 1
@ l_s, c_s SAY PADR(prf_flt,prf_wd) COLOR SCHEME 1
prf_show = prf_flt
*ACTIVATE WINDOW (ALIAS())
ACTIVATE WINDOW Stock

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    ฅเฅฌฅ้ฅญจฅ ฏฎ ก งฅ.                               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Jmp

jmp = .T.
double = 2
GO r_must
r_must = 0

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     ‘ใฌฌ  ฏฎฌฅ็ฅญญ๋ๅ ฏฎงจๆจฉ                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sum_Mark

PRIVATE s, rc

rc = RECNO()
SEEK lc
s = 0
SCAN REST WHILE lc =LIST_NUM
  IF EMPTY(MARK_TMP)
    LOOP
  ENDIF
  s = s+QNT*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
ENDSCAN
GO rc
s = ALLTRIM(STR(s,15,2))
WAIT s WINDOW NOWAIT
DEACTIVATE MENU Edit_Lst
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎฃเ ฌฌ  ข๋ขฎค  จญไฎเฌ ๆจจ ฎ ไจเฌฅ จ ฎ ญ ซจ็จจ.             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_D
PRIVATE q, tmpC, tmpQnt, s_sav

cur_line = RECNO("LD")
IF .NOT. EMPTY(STOCK.NOTE)
  s = ALLTRIM(STOCK.NOTE)
  IF .NOT. EMPTY(s)
    WAIT WINDOW s NOWAIT
  ENDIF
ENDIF
ACTIVATE WINDOW L_Det_M BOTTOM
@ WROWS()-3, WCOLS()-29 SAY IIF(STOCK.QNT_SAVE-LD.QNT_REQ > 0, "๛", " ")
@ WROWS()-2, 16 SAY LD.PRODUCER COLOR SCHEME 1
@ WROWS()-2, 25 SAY STR(STOCK.QNT,6) COLOR SCHEME 1
@ WROWS()-2, 32 SAY STR(STOCK.QNT+STOCK.QNT_SAVE-STOCK.QNT_REP,6) COLOR SCHEME 1
@ WROWS()-2, 39 SAY STR(STOCK.QNT_STORE,6) COLOR SCHEME 1
IF EMPTY(SUBSTR(STOCK.NAME, 26))
  @ WROWS()-4, 2 SAY REPLICATE("ฤ", 60)
ELSE
  @ WROWS()-4, 2 SAY LEFT(STOCK.NAME, WCOLS()-14) COLOR SCHEME 1
*  @ ROW(), COL()+1 SAY STOCK.NAME COLOR SCHEME 1
*  @ ROW(), COL()+1 SAY STOCK.PRODUCER COLOR SCHEME 1
ENDIF
IF is_cor
  @ WROWS()-3, 3 SAY " ฎเฏใแ "
  q = SPACE(12)
  IF SEEK(STOCK.CODE,"SUBJECT")
    IF FOUND("CORPUS")
      q = LEFT(CORPUS.NAME,12)
    ENDIF
  ENDIF
  @ WROWS()-2, 2 SAY q COLOR SCHEME 1
  @ WROWS()-3, WCOLS()-8 SAY PADR(SUBJECT.SEGMENT, 6) COLOR SCHEME 1
ENDIF

DO Show_Opt WITH WROWS()-4
=SEEK(STOCK.PRO_CODE, "DEVELOP")
IF EMPTY(DEVELOP.PERIOD)
  @ WROWS()-2, WCOLS()-5 SAY "    "
ELSE
  @ WROWS()-2, WCOLS()-5 SAY "จเโ" COLOR SCHEME 7
ENDIF

IF USED("DMS")
*  m.tmpC = ALLTRIM(STR(STOCK.CODE))
*  m.tmpC = PADR(m.tmpC, FSIZE("CODE", "DMS"))
  m.tmpQnt = 0
*  IF SEEK(m.tmpC, "DMS")
  IF SEEK(STOCK.CODE, "DMS")
    m.s_sav = SELECT()
    SELECT DMS
    SCAN REST WHILE CODE = STOCK.CODE
      IF .NOT. EMPTY(m.mtk_period)
        IF QNT = STOCK.QNT .AND. ALLTRIM(SYS(15, m.lwr, DMS.SUP_DATE)) = m.mtk_period
          LOOP
        ENDIF
      ENDIF
      m.tmpQnt = m.tmpQnt+DMS.QNT
    ENDSCAN
    SELECT (m.s_sav)
  ENDIF
  @ WROWS()-1, 20 SAY "‘ "+TRANSFORM(MIN(m.tmpQnt, 999999), "@Z 999999") 
ENDIF

@ WROWS()-1, 4 SAY "€เโ. "+Articul(CODE)

ACTIVATE WINDOW (ALIAS())

RETURN .T.

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  Rest_P        งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                            เฎๆฅญโ ฎแโ โช .                            ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 26.04.2001 ฤฤู
PROCEDURE Rest_P
*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*

PRIVATE stat_type     && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
                                        1 - กซ ญช;
                                        2 - BROWSE - โ กซจๆ ;
                                        3 - BROWSE - แฏจแฎช.
PRIVATE what_do       && ฌ๏ เฅฆจฌ .
PRIVATE menu_name     && ฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎.
PRIVATE last_mouse    && เฅฌ๏ ฏฎแซฅคญฅฃฎ ญ ฆ โจ๏ งขฅเจญฎฉ ชญฎฏชจ.
PRIVATE win_name      && ฌ๏ ฎชญ  ( ฎชฎญ คซ๏ BROWSE ).

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*
* แโ โฎช
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ      ฎฏใแโจฌ๋ฉ ฎแโ โฎช 99%      ณ
*ณ                                  ณ
*ณ < OK Ctrl-W > < โช ง โ์แ๏ Esc > ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex, tmpV
ex = 1
tmpV = 0
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 6, 38, "แโ โฎช"

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 2,  8 SAY "ฎฏใแโจฌ๋ฉ ฎแโ โฎช   %"
@ 2, 27 GET tmpV PICTURE "99"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE

IF ex # 1
  *
  * โเ ก โ๋ข ฅฌ กซ ญช
  tmpV = -1
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN tmpV

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  Cor_Note      งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                            เจฌฅ็ ญจฅ ช ชฎเฏใแใ.                       ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 28.12.2002 ฤฤู
PROCEDURE Cor_Note
PRIVATE cor_note

cor_note = ""
IF is_cor
  IF SEEK(STOCK.CODE, "SUBJECT")
    IF FOUND("CORPUS")
      cor_note = ALLTRIM(LEFT(CORPUS.DESCR,70))
    ENDIF
  ENDIF
ENDIF


IF EMPTY(cor_note)
  WAIT WINDOW "เจฌฅ็ ญจฅ ญฅ ฏเฅคใแฌฎโเฅญฎ" NOWAIT
ELSE
  WAIT WINDOW " "+cor_note+" " NOWAIT
ENDIF

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  Sav_Due       งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                           ๅฎค ข ฏฎซฅ คฎซฃ                             ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 10/04/2005 ฤฤู
PROCEDURE Sav_Due

s_due = LD.QNT_DUE

RETURN .T.

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  Exit_Due      งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                          ๋ๅฎค จง ฏฎซ๏ คฎซฃ                            ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 10/04/2005 ฤฤู
PROCEDURE Exit_Due
IF s_due = LD.QNT_DUE
  RETURN .T.
ENDIF
IF mode_ord # "B"
  WAIT WINDOW "€…!  ช ง ญ  เฅ ซจง ๆจ๎!" NOWAIT
ENDIF

REPLACE LD.QNT WITH MAX(LD.QNT-LD.QNT_DUE+s_due, 0), ORD_TYPE WITH mode_ord

RETURN .T.

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  Show_Rest     งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                ‘จฌขฎซ คซ๏ ฎแโ โชฎข ข BROWSE คซ๏ แฏจแช                  ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 10.07.2006 ฤฤู
PROCEDURE Show_Rest
PARAMETER prntPrg

IF TYPE("m.prntPrg") # "N"
  IF .NOT. FOUND("STOCK")
    RETURN "Û"
  ENDIF
ENDIF

IF .NOT. STOCK.QNT > 0
  RETURN "ฑ"
ENDIF

= SEEK(STR(STOCK.CODE, 7), "SALE_PARTY")
IF SALE_PARTY.FLG_BUY = "B"
  RETURN ""
ENDIF

RETURN ""
