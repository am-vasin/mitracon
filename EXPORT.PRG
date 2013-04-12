*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ํชแฏฎเโ  ฏฎงจๆจฉ.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Export

PRIVATE l_type, is_new, ex, l_name, ln, wd, to_save, tmpArr
PRIVATE sel_rem, rec_rem, mss, r_1, usd_sw, tmpNote

to_save = .F.
DO Entry_P WITH "Edit_Lst", ""
sel_rem = SELECT()
rec_rem = RECNO("LIST_TIT")
SELECT 0
USE (m.base_path+"LINK_ORD") SHARED AGAIN ALIAS LNK_5A10 ORDER TAG LIST_CODE

SELECT LIST_TIT
usd_sw = IS_USD

l_type = 1
is_new = .F.
ex     = 1
l_name = SPACE(32)
ln     = 12
wd     = 53
DO D_W WITH "Export", ln, wd, "ชแฏฎเโ ฏฎงจๆจฉ", 1

@  2, 3 TO 4, 49
@  6, 3 TO 9, 49
@  2, 5 SAY " ฎงจๆจจ คซ๏ ฏฅเฅค ็จ "
@  6, 5 SAY " ‘ฏจแฎช "
@  3, 6 GET l_type PICTURE "@*RH ฎฌฅ็ฅญญ๋ฅ ;ฎซฃ ;ฅแชซ คแชจฅ"
@  5, 7 GET to_save PICTURE "@*C ‘ฎๅเ ญจโ์ ฏฎงจๆจจ ข จแๅฎคญฎฌ แฏจแชฅ"
@  7, 5 GET is_new PICTURE "@*C ฎข๋ฉ"
@  8, 5 SAY "ฌ๏ แฏจแช " GET l_name WHEN Def_Lst()
@ 10,10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ ฎงจๆจจ คซ๏ ฏฅเฅค ็จ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ ( ) ฎฌฅ็ฅญญ๋ฅ   ( ) ฎซฃ   ( ) ฅแชซ คแชจฅ ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ     [ ] ‘ฎๅเ ญจโ์ ฏฎงจๆจจ ข จแๅฎคญฎฌ แฏจแชฅ     ณ
*ณ ฺฤ ‘ฏจแฎช ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ [ ] ฎข๋ฉ                                   ณ ณ
*ณ ณ ฌ๏ แฏจแช  ____.____!____.____!____.____!__ ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ        < OK Ctrl-W > < โช ง โ์แ๏ Esc >         ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู


RELEASE WINDOW Export

IF ex = 1
  PRIVATE new_lst, w0, w1, i0, nxt_r
  w0 = 1
  w1 = FIELD(w0)
  SELECT LD
  DO WHILE .NOT. EMPTY(w1)
    IF w1 = "QNT_DUE"
      i0 = w0
      EXIT
    ENDIF
    w0 = w0+1
    w1 = FIELD(w0)
  ENDDO
  DIMENSION w0(1)
  new_lst = STR(LIST_TIT.LST_NUM,6)
  GO rec_rem IN LIST_TIT
  SEEK lc
  DO WHILE lc = LIST_NUM
    IF CODE < 0
      SKIP
      LOOP
    ENDIF
    w1 = RECNO()
    SKIP
    nxt_r = RECNO()
    GO w1
    DO CASE
    CASE l_type = 1   && ฎฌฅ็ฅญญ๋ฅ
      IF .NOT. EMPTY(MARK_TMP)
        IF .NOT. EMPTY(QNT_REQ)
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"ฎงจๆจ๏ "+ALLTRIM(PREFIX)+" "+ALLTRIM(NAME)+  ;
                   " /"+ALLTRIM(PRODUCER)+"/ ง กเฎญจเฎข ญ !"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
        ELSE
          IF to_save
            m.tmpNote = NOTE
            SCATTER TO tmpArr
            APPEND FROM ARRAY tmpArr
            REPLACE NOTE WITH tmpNote
            REPLACE LIST_NUM WITH new_lst
            REPLACE QNT WITH QNT+QNT_DUE
            REPLACE QNT_DUE WITH 0
          ELSE
            DO WHILE SEEK(LIST_NUM+STR(CODE), "LNK_5A10")
              REPLACE LNK_5A10.LIST_NUM WITH new_lst
            ENDDO
            REPLACE MARK_TMP WITH " ", ;
                    LIST_NUM WITH new_lst
          ENDIF
        ENDIF
      ENDIF
    CASE l_type = 2  && ฎซฃ
      IF .NOT. EMPTY(QNT_DUE)
        IF to_save
          m.tmpNote = NOTE
          SCATTER TO tmpArr
          APPEND FROM ARRAY tmpArr
          REPLACE NOTE WITH tmpNote
          REPLACE LIST_NUM WITH new_lst
          REPLACE QNT WITH QNT_DUE
          REPLACE QNT_DUE WITH 0
          REPLACE QNT_REQ WITH 0
        ELSE
          DO WHILE SEEK(LIST_NUM+STR(CODE), "LNK_5A10")
            REPLACE LNK_5A10.LIST_NUM WITH new_lst
          ENDDO
          IF QNT_REQ = 0 .AND. QNT = 0
            REPLACE MARK_TMP WITH " ", ;
                    LIST_NUM WITH new_lst
          ELSE
            m.tmpNote = NOTE
            SCATTER TO w0
            w0(i0) = 0
            REPLACE MARK_TMP WITH " ", ;
                    LIST_NUM WITH new_lst,  ;
                    QNT_REQ  WITH 0,   ;
                    QNT      WITH 0,   ;
                    IS_DEF   WITH " "
            APPEND FROM ARRAY w0
            IF .NOT. EMPTY(m.tmpNote)
              REPLACE NOTE WITH m.tmpNote
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    CASE l_type = 3   && ฅแชซ คแชจฅ
      IF EMPTY(CODE)
        IF to_save
          m.tmpNote = NOTE
          SCATTER TO tmpArr
          APPEND FROM ARRAY tmpArr
          REPLACE NOTE WITH tmpNote
          REPLACE LIST_NUM WITH new_lst
        ELSE
          REPLACE MARK_TMP WITH " ", ;
                  LIST_NUM WITH new_lst
        ENDIF
      ENDIF
    ENDCASE
    IF nxt_r > RECCOUNT() .OR. nxt_r = 0
      EXIT
    ENDIF
    GO nxt_r
  ENDDO

  *
  *   ก๊ฅคจญ๏ฅฌ ข ญฎขฎฌ แฏจแชฅ แๅฎคญ๋ฅ ฏฎงจๆจจ
  *
  PRIVATE ord_sav, vl, rc_, _q1, _q2, _q3
  ord_sav = SET("ORDER")
  
  *   ฅแชซ คแชจฅ ฏฎงจๆจจ
  SET ORDER TO TAG L_NAME
  SEEK new_lst
  vl  = SPACE(10)
  STORE 0 TO rc_, _q1, _q2, _q3
  SCAN REST WHILE new_lst = LIST_NUM FOR CODE = 0
    IF vl == NAME+PREFIX
      _q1 = QNT
      _q2 = QNT_REQ
      _q3 = QNT_DUE
      DELETE
      GO rc_
      REPLACE QNT     WITH QNT    +_q1,  ;
              QNT_REQ WITH QNT_REQ+_q2,  ;
              QNT_DUE WITH QNT_DUE+_q3
    ENDIF
    rc_ = RECNO()
    vl  = NAME+PREFIX
  ENDSCAN

  *   ‘ชซ คแชจฅ ฏฎงจๆจจ
  SET ORDER TO TAG L_CODE
  SEEK new_lst
  vl  = 0
  STORE 0 TO rc_, _q1, _q2, _q3
  SCAN REST WHILE new_lst = LIST_NUM FOR CODE > 0
    IF vl = CODE
      _q1 = QNT
      _q2 = QNT_REQ
      _q3 = QNT_DUE
      DELETE
      GO rc_
      REPLACE QNT     WITH QNT    +_q1,  ;
              QNT_REQ WITH QNT_REQ+_q2,  ;
              QNT_DUE WITH QNT_DUE+_q3
    ENDIF
    rc_ = RECNO()
    vl  = CODE
  ENDSCAN

  SET ORDER TO &ord_sav
  *
  *   เฎแโ ขซ๏ฅฌ ไซ ฆชจ ข ญฎขฎฌ แฏจแชฅ
  *
  PRIVATE s_w, sw0, sw1, sw2, sw3
  SEEK new_lst
  s_w = 0
  sw0 = " "
  sw1 = " "
  sw2 = " "
  sw3 = " "
  SCAN REST WHILE new_lst = LIST_NUM FOR CODE >= 0
    IF usd_sw
      s_w = s_w+QNT*USD_PRICE
    ELSE
      s_w = s_w+ROUND(QNT*SAL_PRICE,2)
    ENDIF
    IF FOUND("STOCK")
      IF QNT-QNT_REQ > STOCK.QNT .AND. EMPTY(IS_DEF)
        sw0 = "*"
      ENDIF
    ELSE
      sw0 = "*"
    ENDIF
    IF QNT_DUE # 0
      sw1 = "*"
    ENDIF
    IF IIF(usd_sw, USD_PRICE, SAL_PRICE) # IIF(usd_sw, USD_DEF, DEF_PRICE)
      IF EMPTY(IS_SPEC)
        sw2 = "*"
      ELSE
        sw3 = "*"
      ENDIF
    ENDIF
  ENDSCAN
  SELECT LIST_TIT
  SEEK arm+new_lst
  REPLACE IS_DEF  WITH sw0,  ;
          IS_DUE  WITH sw1,  ;
          IS_DIS  WITH sw2,  ;
          IS_SPEC WITH sw3,  ;
          R_TOTAL WITH s_w
  *
  *   เฎแโ ขซ๏ฅฌ ไซ ฆชจ ข แโ เฎฌ แฏจแชฅ
  *
  SELECT LD
  SEEK lc
  sum_list = 0
  sw0 = " "
  sw1 = " "
  sw2 = " "
  sw3 = " "
  SCAN REST WHILE lc = LIST_NUM FOR CODE >= 0
    sum_list = sum_list+QNT*IIF(usd_sw, USD_PRICE, SAL_PRICE)
    IF FOUND("STOCK")
      IF QNT-QNT_REQ > STOCK.QNT .AND. EMPTY(IS_DEF)
        sw0 = "*"
      ENDIF
    ELSE
      sw0 = "*"
    ENDIF
    IF QNT_DUE # 0
      sw1 = "*"
    ENDIF
    IF IIF(usd_sw, USD_PRICE, SAL_PRICE) # IIF(usd_sw, USD_DEF, DEF_PRICE)
      IF EMPTY(IS_SPEC)
        sw2 = "*"
      ELSE
        sw3 = "*"
      ENDIF
    ENDIF
  ENDSCAN
  SELECT LIST_TIT
  GO rec_rem
  REPLACE IS_DEF  WITH sw0,  ;
          IS_DUE  WITH sw1,  ;
          IS_DIS  WITH sw2,  ;
          IS_SPEC WITH sw3,  ;
          R_TOTAL WITH sum_list
  ACTIVATE WINDOW L_Det_M BOTTOM
  @ WROWS()-2, 16 SAY STR(sum_list,12,2) COLOR SCHEME 1
*  @ 1, 49 SAY STR(sum_list,12,2) COLOR SCHEME 1

ENDIF
DO Exit_P WITH "Edit_Lst", ""

GO rec_rem IN LIST_TIT
USE IN LNK_5A10
SELECT (sel_rem)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ข๋กฎเ /แฎงค ญจ๏ แฏจแช .                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Def_Lst

IF is_new
  DO Corr_Tit WITH "new", ""
  REPLACE IS_USD WITH usd_sw
  is_new = .F.
ELSE
  IF .NOT. What_Lst()
    RETURN .F.
  ENDIF
  IF BOF() .OR. EOF()
    RETURN .F.
  ENDIF
ENDIF
l_name = LIST_TIT.LST_NAME
SHOW GET l_name

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ข๋กฎเ  แฏจแช .                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE What_Lst

PRIVATE what_do, ret_w

*
*    PAD - ฌฅญ๎ ฏฅเขฎฃฎ ใเฎขญ๏
*
DEFINE MENU What_L IN SCREEN COLOR SCHEME 4
DEFINE PAD Choose OF What_L PROMPT "๋กฎเ"  KEY Ctrl-A, "Enter"
*DEFINE PAD Seek   OF What_L PROMPT "ฎจแช " KEY Ctrl-A, "F7"
DEFINE PAD Exit   OF What_L PROMPT "๋ๅฎค"  KEY Ctrl-A, "Esc"

ON SELECTION PAD Choose OF What_L DO Ch_Mode WITH "What_L","choose"
*ON SELECTION PAD Seek   OF What_L DO Ch_Mode WITH "What_L","seek"
ON SELECTION PAD Exit   OF What_L DO Ex_Proc WITH "What_L"

DO Entry_P WITH "", "What_L"

SELECT LIST_TIT

what_do = "list"
ret_w   = .F.
DO W_Lst
DO WHILE .T.
  DO CASE
  CASE what_do = "list"     && ‘ฏจแชจ
*    ON KEY LABEL F7    DO Ch_Mode WITH "What_L","seek"
    ON KEY LABEL Enter DO Ch_Mode WITH "What_L","choose"
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "What_L"
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU What_L
    ACTIVATE WINDOW WLst_E
    BROWSE FOR IS_USD = usd_sw .AND. RECNO() # rec_rem .AND. EMPTY(LIST_TIT.SAL_NUM) ;
           KEY arm ;
           FIELDS LIST_TIT.LST_NUM:H="", LIST_TIT.CR_DAT:H="",  ;
                  LIST_TIT.LST_NAME:H="",  ;
                  DUMMY.F:H="" ;
           FREEZE DUMMY.F ;
           NOMENU NORGRID NOAPPEND NODELETE NOMODIFY NOCLEAR REST ;
           WINDOW WLst_I IN WINDOW WLst_E
*    ON KEY LABEL F7
    ON KEY LABEL Home
    ON KEY LABEL End
    ON KEY LABEL Enter
    ON KEY LABEL LeftMouse
    IF what_do = "list"
      what_do = "abort"
    ENDIF
  CASE what_do = "seek"       && ฎจแช
    DO Seek_Lst WITH "What_L"
    what_do = "list"
  CASE what_do = "choose"     && ๋กฎเ
    ret_w = .T.
    EXIT
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW WLst_I
RELEASE WINDOW WLst_E
RELEASE WINDOW WLst_M

DO Exit_P WITH "", "What_L"

RETURN ret_w

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  แฎงค ญจ๏ ฎชฎญ คซ๏ ฏฅเฅ็ญ๏ แฏจแชฎข.               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE W_Lst

PRIVATE l, c, ln, wd, tit2
DIMENSION tit2(1)
*
*   แญฎขญฎฉ ฏฅเฅ็ฅญ์ ฏฎซฅฉ คซ๏ BROWSE
*
tit2(1) = PADC("ฤฎฌฅเ",FSIZE("LST_NUM" ,"LIST_TIT"),"ฤ")+"ฤ"+ ;
          PADC(" โ ",IIF(SET("CENTURY")="ON",10,8),"ฤ")+"ฤ"+ ;
          PADC("ฌ๏ แฏจแช ฤฤฤฤฤฤฤฤ",FSIZE("LST_NAME" ,"LIST_TIT"),"ฤ")
ln = WROWS("")-10

DO Def_Win WITH "WLst_M", "WLst_E", "WLst_I", ln, LEN(tit2(1)),   ;
                "๋กฅเจโฅ แฏจแฎช", tit2

RETURN
