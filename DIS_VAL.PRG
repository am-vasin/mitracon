*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ข๋กฎเ  โจฏ  แชจคชจ.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Dis_Val
PARAMETERS ord_lst, sum1, sum2
*
*  ord_lst  - แโเฎช  แฎ แฏจแชฎฌ ง ช งฎข ( ฅแซจ ฏใแโฎ - โฅชใ้จฉ );
*  sum1     - แใฌฌ  กฅง แฏฅๆๆฅญ;
*  sum2     - แใฌฌ  ฏฎ แฏฅๆๆฅญ ฌ.
*

PRIVATE sch_lst, sch_recs, ns, ln, ls, pic1, pic2, pic3, wd
PRIVATE wd1, wd2, wd3, wd4, ttl
PRIVATE c1, c2, c3, c4, remember, disbound
disbound = 0
IF File_O(base_path+"DISBOUND.MEM")
  RESTORE FROM (base_path+"DISBOUND.MEM") ADDITIVE
ENDIF

DIMENSION sch_lst(1), sch_recs(1)
ns = 1
sch_lst(1)  = PADR("ใ็ญ ๏ ใแโ ญฎขช ",FSIZE("SCH_NAM","DIS_SCH"))
sch_recs(1) = 0

SELECT DIS_SCH

pic2 = "@*R "+ALLTRIM(sch_lst(1))
pic3 = "@*H \ “แโ ญฎขจโ์ Ctrl-W ;  โช ง โ์แ๏ Esc   "
SCAN
  IF EMPTY(SCH_NAM)
    LOOP
  ENDIF
  ns = ns+1
  DIMENSION sch_lst(ns), sch_recs(ns)
  sch_lst(ns)  = SCH_NAM
  sch_recs(ns) = RECNO()
  pic2 = pic2+";"+ALLTRIM(sch_lst(ns))
ENDSCAN
pic1 = "@*RH ‘ชจคช    ;ฎค เฎช"
wd1  = LEN(pic1)+3
wd2  = IIF(ns = 1, 0, FSIZE("SCH_NAM","DIS_SCH")+8)
wd3  = 31
wd4  = LEN(pic3)-1
wd   = MAX(wd1,wd2,wd3,wd4)+6

c1   = FLOOR((wd-wd1)/2)
c2   = FLOOR((wd-wd2)/2)
c3   = FLOOR((wd-wd3)/2)
c4   = FLOOR((wd-wd4)/2)

ls = ALEN(sch_lst)
ls = IIF(ls = 1, 0, ls+2)
ln = ls+11
IF EMPTY(ord_lst)
  ttl = "‘ชจคช : "+ALLTRIM(LIST_TIT.LST_NAME)
ELSE
  ttl = " แ็ฅโ แชจคชจ"
ENDIF

DO D_W WITH "Discount", ln, wd, ttl, 1

PRIVATE ex, dis_typ, dis_sum, res_sum, dis_sch, dis_prc

ex = 1
res_sum = 0
dis_sum = 0
dis_prc = 0
dis_typ = 1
dis_sch = 1
remember = 0

@ 3, c1 GET dis_typ PICTURE pic1 WHEN dis_sum = 0

IF ls # 0
  @ 4, c2 TO 3+ls, c2+wd2-1
  @ 4, c2+2 SAY " ‘ๅฅฌ  "
  @ 5, c2+2 GET dis_sch PICTURE pic2 VALID Clc_Dis1(Sch_Recs(dis_sch))
ENDIF
res_sum = sum1+sum2
@ 4+ls, c3 SAY "‘ฏฅๆๆฅญ๋    "+STR(sum2,10,2)
@ 5+ls, c3 SAY "ฅง แฏฅๆๆฅญ "+STR(sum1,10,2)
@ 6+ls, c3 SAY "‘ชจคช      " GET dis_sum PICTURE "9999999.99" ;
              WHEN Entry_F(dis_sum) VALID Clc_Dis(dis_sum/sum1,dis_sum)
@ 6+ls, c3+23  GET dis_prc PICTURE "9999.99"   ;
              WHEN Entry_F(dis_prc) VALID Clc_Dis(dis_prc/100, dis_prc)
@ 6+ls, c3+31  SAY "%"
@ 7+ls, c3 SAY "โฎฃฎ       "+STR(res_sum,10,2)
@ 8+ls, c4 GET ex PICTURE pic3
READ CYCLE

IF MOD(READKEY(),256) = 12 .OR. ex = 2
  = Clc_Dis(0)
ENDIF

RELEASE WINDOW Discount

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎฃเ ฌฌ  เ งกฎเชจ แโเฎชจ แๅฅฌ๋ จ เ แ็ฅโ  แชจคชจ.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Clc_Dis1
PARAMETER rc

PRIVATE s1, p1, str_w, s_w, p_w, i

IF READKEY() % 256 = 12
  RETURN
ENDIF

IF rc # 0
  GO rc IN DIS_SCH
  str_w = ALLTRIM(DIS_SCH.SCH_VAL)
ELSE
  str_w = ""
ENDIF
s1 = 0
p1 = 0
DO WHILE .NOT. EMPTY(str_w)
  i = RAT("!",str_w)
  p1 = ALLTRIM(SUBSTR(str_w, i+1))
  p1 = VAL(p1)
  str_w = LEFT(str_w, i-1)
  IF EMPTY(str_w)
    s1 = 0
  ELSE
    i = RAT("!",str_w)
    s1 = ALLTRIM(SUBSTR(str_w, i+1))
    s1 = VAL(s1)
    str_w = LEFT(str_w, i-1)
  ENDIF
  IF s1*kurs_b <= sum1
    EXIT
  ENDIF
ENDDO

= Clc_Dis(p1/100)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ฏฅเฅแ็ฅโ  ๆฅญ.                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Clc_Dis
PARAMETERS coeff, v_new

IF READKEY() = 12
  RETURN .T.
ENDIF

IF TYPE("v_new") = "N"
  IF v_new = remember
    RETURN .T.
  ENDIF
ENDIF

IF disbound # 0
  IF coeff*100 > disbound
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ๋ ใแโ ญฎขจซจ แชจคชใ "+ALLTRIM(STR(coeff*100,10,1))+ ;
                    "%, ํโฎ ฏเ ขจซ์ญฎ? [   ] [ ฅโ ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

PRIVATE c_w, sum_1
IF coeff > 1
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"‘ชจคช  กฎซ์่ฅ แโ  ฏเฎๆฅญโฎข? คใฌ ฉโฅแ์!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dis_typ = 2  && ฎค เฎช
  c_w = 0
ELSE
  c_w = coeff
ENDIF

DO Wt_Mess WITH "‘็จโ ฅฌ..."

PRIVATE i_p, lst_len, n_wk, sum_d, sum_s
SELECT LIST_DIS
i_p = 1
IF EMPTY(ord_lst)
  lst_len = 7
  n_wk = STR(LIST_TIT.LST_NUM,6)
ELSE
  lst_len = LEN(ord_lst)
  n_wk = LEFT(ord_lst,6)
ENDIF

PRIVATE is_dis_, who_let_, dis_sav, prs_sum
IF coeff = 0
  is_dis_ = " "
  who_let_ = 0
ELSE
  is_dis_ = "*"
  who_let_ = IIF(acc_level < 2, user, 0)
ENDIF

dis_sum = 0
prs_sum = 0
DO WHILE i_p < lst_len
  = SEEK(arm+n_wk, "LIST_TIT")
  SEEK n_wk
  present_ = 0
  sum_1 = 0
  SCAN REST WHILE n_wk = LIST_DIS.LIST_NUM FOR CODE >= 0
    IF EMPTY(IS_SPEC) .AND. EMPTY(IS_OPT) .AND. EMPTY(SUB_DIS.NODISC)
      IF LIST_TIT.IS_USD
        REPLACE SAL_PRICE WITH 0, ;
                USD_PRICE WITH USD_DEF*(1-c_w)
      ELSE
        REPLACE SAL_PRICE WITH Round_6(DEF_PRICE*(1-c_w)), ;
                USD_PRICE WITH 0
      ENDIF
      dis_sum = dis_sum+QNT*IIF(LIST_TIT.IS_USD, USD_DEF-USD_PRICE, DEF_PRICE-SAL_PRICE)
      IF dis_typ = 2  && ฎค เฎช
        present_ = present_+QNT*IIF(LIST_TIT.IS_USD, USD_DEF, DEF_PRICE)*coeff
      ENDIF
    ENDIF
    sum_1 = sum_1+QNT*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
  ENDSCAN
  IF dis_typ = 2  && ฎค เฎช
    prs_sum = prs_sum+present_
  ENDIF
  SELECT LIST_TIT
  REPLACE LIST_TIT.R_TOTAL WITH sum_1,    ;
          LIST_TIT.IS_DIS  WITH is_dis_,  ;
          LIST_TIT.WHO_LET WITH who_let_, ;
          LIST_TIT.PRESENT WITH present_
  SELECT LIST_DIS
  IF EMPTY(ord_lst)
    EXIT
  ELSE
    i_p = i_p+7
    n_wk = SUBSTR(ord_lst,i_p,6)
  ENDIF
ENDDO

DO Wt_Mess

IF dis_typ = 2  && ฎค เฎช
  dis_sum = prs_sum
ENDIF
res_sum = sum1+sum2-IIF(dis_typ = 2, 0, dis_sum)
dis_prc = ROUND(dis_sum/sum1*100,2)
@ 6+ls, c3+12 SAY dis_sum PICTURE "9999999.99" COLOR SCHEME 1
@ 6+ls, c3+23 SAY dis_prc PICTURE "9999" COLOR SCHEME 1
@ 7+ls, c3+12 SAY STR(res_sum,10,2)

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ขๅฎค  ข ฏฎซฅ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Entry_F
PARAMETER v

IF dis_sch # 1
  RETURN .F.
ENDIF
remember = v
RETURN
