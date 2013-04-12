*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ        เฎฃเ ฌฌ  ชฎเเฅชๆจจ ญ ชซ คญ๋ๅ                                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Corr
PRIVATE fltr, sum_ch, sum_bn, sum_or, sum_ot, sum_1, sum_2, nav, k
PRIVATE l1, c1, ln, wd, cl, w, delta, no_nkl
DIMENSION no_nkl(3)
no_nkl(1) = ""
no_nkl(2) = PADC("ฅโ ญ ชซ คญ๋ๅ, ใคฎขซฅโขฎเ๏๎้จๅ ใช ง ญญ๋ฌ ใแซฎขจ๏ฌ!",78)
no_nkl(3) = ""
*
*   —จโ ฅฌ งญ ็ฅญจ๏ ไจซ์โเ 
*
IF .NOT. FILE(tmp_path+"DUMMY.DBF")
  CREATE DBF (tmp_path+"DUMMY") ( FF C(1) )
ELSE
  USE (tmp_path+"DUMMY")
ENDIF
SELECT 0
USE (base_path+"FILTER")
SCATTER TO fltr

*
*   โชเ๋ข ฅฌ ไ ฉซ๋ ญ ชซ คญ๋ๅ จ ข๋็จแซ๏ฅฌ แใฌฌ๋
*
sum_bn = 0
sum_or = 0
sum_ot = 0
sum_ch = 0
sum_1  = 0
sum_2  = 0

USE (base_path+"SALE_TIT")
SCAN
  IF .NOT. BETWEEN(SALE_TIT.DOC_DATE, fltr(3), fltr(4))
    LOOP
  ENDIF
  DO CASE
  CASE SW_NAL = 0
    sum_ot = sum_ot+TOTAL_R
  CASE SW_NAL = 1
    IF ORD_NO = -1
      sum_ch = sum_ch+TOTAL_R
    ELSE
      sum_or = sum_or+TOTAL_R
    ENDIF
  OTHERWISE
    sum_bn = sum_bn+TOTAL_R
  ENDCASE
ENDSCAN
sum_1 = sum_or+sum_bn+sum_ch
sum_2 = sum_1 +sum_ot
SET FILTER TO BETWEEN(SALE_TIT.DOC_DATE, fltr(3), fltr(4)) .AND. ;
    ( fltr(5)=1 .AND. SALE_TIT.SW_NAL=2 .OR.  ;
      fltr(8)=1 .AND. SALE_TIT.SW_NAL=1 .AND. SALE_TIT.ORD_NO = -1 .OR.  ;
      fltr(6)=1 .AND. SALE_TIT.SW_NAL=1 .AND. SALE_TIT.ORD_NO # -1 .OR.  ;
      fltr(7)=1 .AND. SALE_TIT.SW_NAL=0 )
GO TOP
IF BOF() .OR. EOF()
  DO Out_Mess WITH 7, "no_nkl"
  nav = "filter"
ELSE
  nav = "n_list"
ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT SALE_TIT
GO TOP

DO Cr_Wind WITH "W_Main", WROWS("")-3, 74
CLEAR

l1 = WLROW("W_Main")+10
ln = WROWS("")-l1
wd = 79
c1 = FLOOR((WCOLS("")-wd)/2)

cl = SCHEME(13,1)+","+SCHEME(13,2)+","+SCHEME(13,3)+","+  ;
     SCHEME(13,4)+","+SCHEME(13,5)+","+SCHEME(13,6)+","+  ;
     SCHEME(14,2)+","+SCHEME(13,8)
DEFINE WINDOW WM1 FROM l1, c1 TO l1+ln-2, c1+wd-1  ;
                  SHADOW NONE COLOR SCHEME 13
DEFINE WINDOW WE1 FROM l1+2, c1+1 TO l1+ln-3, c1+wd-2  ;
                  NONE COLOR SCHEME 13
DEFINE WINDOW WI1 FROM l1-1, c1-1 TO l1+ln-2, c1+wd+5  ;
                  NONE COLOR (cl)

KEYBOARD CHR(27)
DO Set_Fltr.spr WITH .F.
@ 9, 0 TO WROWS()-1,WCOLS()-1
@ 9, 3 SAY "  โ  "
@ 9,13 SAY " ฎฌฅเ "
@ 9,22 SAY " ’จฏ "
@ 9,31 SAY " ‘ใฌฌ  "
@ 9,40 SAY " ฎชใฏ โฅซ์ "
@ 9,57 SAY " เจฌฅ็ ญจฅ "
DEFINE WINDOW W_Ext FROM WLROW()+10,        WLCOL()+1   ;
                    TO   WLROW()+WROWS()-2, WLCOL()+WCOLS()-2  ;
                    NONE COLOR SCHEME 13
ACTIVATE WINDOW W_Ext
DEFINE WINDOW W_Int FROM WLROW()-3,       WLCOL()-2   ;
                    TO   WLROW()+WROWS(), WLCOL()+WCOLS()+10  ;
                    NONE COLOR SCHEME 13
DO N_List.mpr

DO WHILE .T.
  DO CASE
  CASE nav = "n_list"   && แฏจแฎช ญ ชซ คญ๋ๅ
    SET SYSMENU AUTO
    ACTIVATE WINDOW W_Main
    ACTIVATE WINDOW W_Ext
    ON KEY LABEL Enter DO Interr WITH "n_cont"
    ON KEY LABEL F2    DO Interr WITH "filter"
    ON KEY LABEL F3    DO N_Type
    ON KEY LABEL F4    DO Interr WITH "sav_c_p"
    BROWSE FIELDS DOC_DATE:H="", ;
    F1=PADC(ALLTRIM(WHERE)+"-"+ALLTRIM(STR(DOC_NUM,10)),9), ;
    F2=IIF(SW_NAL=0, SPACE(6), IIF(SW_NAL=2, "ก/ญ ซ.",  ;
           PADC(ALLTRIM(STR(ORD_NO,10)),6) ) ),  ;
    TOTAL_R,   ;
    CUS_NAME,  ;
    NOTE,      ;
    DUMMY.FF  FREEZE DUMMY.FF SAVE NOMENU NORGRID WINDOW W_Int IN WINDOW W_Ext

    ON KEY LABEL Enter
    ON KEY LABEL F2
    ON KEY LABEL F3
    ON KEY LABEL F4
    IF nav = "n_list"
      nav = "exit"
    ENDIF
  CASE nav = "n_cont"    && แฎคฅเฆจฌฎฅ ญ ชซ คญฎฉ
    SET SYSMENU OFF
    w = " ชซ. N "+PADC(ALLTRIM(WHERE)+"-"+ALLTRIM(STR(DOC_NUM,10)),9)+ ;
        " ฎโ "+DTOC(DOC_DATE)+" ‘ใฌฌ  "+STR(TOTAL_R,FSIZE("TOTAL_R"),2)+  ;
        IIF(SW_NAL=0, SPACE(6), IIF(SW_NAL=2, " (ก/ญ ซ.)",  ;
           " ("+PADC(ALLTRIM(STR(ORD_NO,10)),6)+")" ) )
    k = STR(YEAR(DOC_DATE),4)+WHERE+STR(DOC_NUM,6)
    SELECT SALE
    SEEK k
    delta = IIF(FOUND(), PRICE_R, 0)
    ACTIVATE WINDOW WM1
    @ 0, 3 SAY w
    @ 1, 0 TO WROWS()-1, WCOLS()-1
    @ 1, 2 SAY " จฌฅญฎข ญจฅ"
    @ 1,34 SAY "ฎซ-ขฎ"
    @ 1,45 SAY "–ฅญ "
    @ 1,57 SAY "‘ใฌฌ "
    @ 1,65 SAY "เจฌฅ็ ญจฅ"
    ACTIVATE WINDOW WE1
    BROWSE FIELDS STOCK.PREFIX,  ;
                  STOCK.NAME:25, ;
                  QNT,           ;
                  PRICE_R:W=Wh():V=Vl(), ;
                  F0 = STR(QNT*PRICE_R,12,2), ;
                  NOTE FREEZE PRICE_R KEY k ;
                  NOMENU NORGRID WINDOW WI1 IN WINDOW WE1
    DEACTIVATE WINDOW WM1
    DEACTIVATE WINDOW WE1
    nav = "n_list"
    SELECT SALE_TIT
  CASE nav = "filter"    && ไจซ์โเ
    SET SYSMENU OFF
    ACTIVATE WINDOW W_Main SAME
    DO Set_Fltr.spr WITH .F.
    GO TOP
    IF BOF() .OR. EOF()
      DO Out_Mess WITH 7, "no_nkl"
      nav = "filter"
    ELSE
      nav = "n_list"
    ENDIF
    IF MOD(READKEY(),256) = 12
      nav = "exit"
    ENDIF
  CASE nav = "sav_c_p"    && แฎๅเ ญฅญจฅ ชฎญโเฎซ์ญฎฉ โฎ็ชจ
    SET SYSMENU OFF
    DO Wt_Mess WITH "‘ฎๅเ ญ๏ฅฌ แฎแโฎ๏ญจฅ..."
    DELETE FILE (base_path+"C_P.DBF")
    DELETE FILE (base_path+"C_P_T.DBF")
    SELECT SALE
    COPY TO (base_path+"C_P.DBF") ALL
    SELECT SALE_TIT
    w = RECNO()
    SET FILTER TO
    COPY TO (base_path+"C_P_T.DBF") ALL
    SET FILTER TO BETWEEN(SALE_TIT.DOC_DATE, fltr(3), fltr(4)) .AND. ;
        ( fltr(5)=1 .AND. SALE_TIT.SW_NAL=2 .OR.  ;
          fltr(6)=1 .AND. SALE_TIT.SW_NAL=1 .OR.  ;
          fltr(7)=1 .AND. SALE_TIT.SW_NAL=0 )   
    GO w
    nav = "n_list"
    DO Wt_Mess
    SET SYSMENU AUTO
  OTHERWISE
    RELEASE MENU _MSYSMENU
    EXIT
  ENDCASE
ENDDO

CLOSE DATABASES
CLEAR WIND
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ        เฎฃเ ฌฌ -ฎกเ กฎโ็จช ฏเฅเ๋ข ญจ๏.                               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Interr
PARAMETERS vl
nav = vl
KEYBOARD CHR(27)
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ   เฎฃเ ฌฌ๋ ง ฏฎฌจญ ญจ๏ ญ ็ ซ์ญฎฃฎ งญ ็ฅญจ๏ จ ฎโเ กฎโชจ จงฌฅญฅญจฉ.    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Wh
delta = PRICE_R
RETURN .T.

PROCEDURE Vl
PRIVATE sel, w_sav
sel = SELECT()
w_sav = WOUTPUT()
delta = (PRICE_R-delta)*QNT
SELECT SALE_TIT
REPLACE TOTAL_R WITH TOTAL_R+delta
ACTIVATE WINDOW WM1 SAME
@ 0, 41 SAY STR(TOTAL_R,FSIZE("TOTAL_R"),2)
DO CASE
CASE SW_NAL = 0
  sum_ot = sum_ot+delta
CASE SW_NAL = 1
  IF ORD_NO = -1
    sum_ch = sum_ch+delta
  ELSE
    sum_or = sum_or+delta
  ENDIF
CASE SW_NAL = 2
  sum_bn = sum_bn+delta
ENDCASE
sum_1 = sum_or+sum_bn+sum_ch
sum_2 = sum_1 +sum_ot

DO Draw_S

delta = SALE.PRICE_R
SELECT (sel)
ACTIVATE WINDOW (w_sav) SAME
RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                       เฎฃเ ฌฌ  ข๋ขฎค  แใฌฌ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Draw_S
ACTIVATE WINDOW W_Main SAME

@ 4,36 SAY sum_bn ;
	SIZE 1,12 ;
	PICTURE "999999999.99"
@ 5,36 SAY sum_or ;
	SIZE 1,12 ;
	PICTURE "999999999.99"
@ 6,36 SAY sum_ch ;
	SIZE 1,12 ;
	PICTURE "999999999.99"

*@ 6,36 SAY sum_ot ;
*	SIZE 1,12 ;
*	PICTURE "999999999.99"
@ 6,51 SAY sum_1 ;
	SIZE 1,12 ;
	PICTURE "999999999.99"
*@ 6,51 SAY sum_2 ;
*	SIZE 1,12 ;
*	PICTURE "999999999.99"
RETURN
