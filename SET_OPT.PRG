*ЪДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДї
*і   Џа®жҐ¤га  Set_Opt      ђ §а Ў®взЁЄ Ђ­¤аҐ© ‚ бЁ­                      і
*ГДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДґ
*і                                                                        і
*і                     “бв ­®ўЄ /б­пвЁҐ ®Їв®ў®© жҐ­л.                     і
*і                                                                        і
*АДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДДД 25.04.2001 ДДЩ
PROCEDURE Set_Opt

IF sale_mode # "Mitracon"
  RETURN
ENDIF

IF LD.CODE # STOCK.CODE
  RETURN
ENDIF

IF STOCK.QNT_OPT <= 1 .OR. .NOT. EMPTY(LD.IS_SPEC)
  RETURN
ENDIF

IF LD.QNT >= STOCK.QNT_OPT
  IF EMPTY(LD.IS_OPT)
    IF LIST_TIT.IS_USD
      REPLACE LD.IS_OPT    WITH "*",            ;
              LD.USD_PRICE WITH STOCK.PRICE_O1, ;
              LD.USD_DEF   WITH STOCK.PRICE_O1, ;
              LD.DEF_PRICE WITH STOCK.PRICE_O1_R
    ELSE
      REPLACE LD.IS_OPT    WITH "*",              ;
              LD.SAL_PRICE WITH STOCK.PRICE_O1_R, ;
              LD.USD_DEF   WITH STOCK.PRICE_O1,   ;
              LD.DEF_PRICE WITH STOCK.PRICE_O1_R
    ENDIF
  ENDIF
ELSE
  IF .NOT. EMPTY(LD.QNT)
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
  ENDIF
ENDIF

RETURN
