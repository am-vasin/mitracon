*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 蹍������ □��� ������ ├ �����.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Get_Ord

PARAMETERS ext_menu

PRIVATE pth, pth_w, ex, ln, wd, sel, mss, rc_sv, prc_sv_r, prc_sv_d, code_sv

IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ��錢��!? ���甌� 礒� �牀���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF .NOT. EMPTY(LIST_TIT.PRESENT) .OR. .NOT. EMPTY(LIST_TIT.IS_DIS)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ������ 竅����←キ� 瓷┐��, ゥ 甄イ礇� 甅����� �皙キ�碎."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

DO Entry_P WITH ext_menu

pth = SPACE(40)
pth_w = ""

sel = SELECT()
ln = 5
wd = LEN(pth)+6
DO D_W WITH "Path", ln, wd, "���Θ皀, �ぅ ��絎え矚� ���� � ����М�", 1
ln = FLOOR(wd/2)-16
ex = 1
@ 2, 3  GET pth
@ 3, ln GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID Opn_File()

RELEASE WINDOW "Path"

IF .NOT. USED("Ord")
  SELECT (sel)
  DO Exit_P WITH ext_menu
  RETURN
ENDIF
SELECT 0
USE LIST_DET AGAIN ALIAS LDW
SELECT 0
USE STOCK ORDER TAG NAME AGAIN ALIAS STO_O
SELECT ORD
ln = STR(LIST_TIT.LST_NUM,6)
wd = " "
SCAN FOR .NOT. EMPTY(NAME) .AND. .NOT. EMPTY(QNT)
  SELECT LDW
  APPEND BLANK
  IF .NOT. SEEK(ORD.NAME+ORD.PREFIX,"STO_O")
    wd = "*"
    REPLACE LIST_NUM WITH ln,         ;
            PREFIX   WITH ORD.PREFIX, ;
            NAME     WITH ORD.NAME,   ;
            AMODEL   WITH ORD.MODEL,  ;
            FUNCTION WITH ORD.FUNC,   ;
            QNT      WITH ORD.QNT
  ELSE
    prc_sv_r = STO_O.PRICE_R
    prc_sv_d = STO_O.PRICE
    code_sv = STO_O.CODE
    rc_sv   = RECNO("STO_O")
    *
    *  ���薀� ｀�ゥ ��ゅ�わ蜒� ��Ж罔� 痼イ� �牀腮� �牀├〓え皀�ォ.
    *
    DO WHILE .T.
      SKIP IN STO_O
      IF EOF("STO_O") .OR. BOF("STO_O")
        EXIT
      ENDIF
      IF ORD.NAME # STO_O.NAME .OR. ORD.PREFIX # STO_O.PREFIX
        EXIT
      ENDIF
      IF STO_O.PRICE_R = 0 .OR. STO_O.QNT = 0
        LOOP
      ENDIF
      IF IIF(LIST_TIT.IS_USD, STO_O.PRICE_R < prc_sv_r, STO_O.PRICE < prc_sv_d)
        prc_sv_r = STO_O.PRICE_R
        prc_sv_d = STO_O.PRICE
        code_sv = STO_O.CODE
        rc_sv   = RECNO("STO_O")
      ENDIF
    ENDDO
    GO rc_sv IN STO_O
    IF prc_sv_r = 0 .OR. prc_sv_d = 0
      WAIT WINDOW ALLTRIM(STO_O.PREFIX)+" "+ALLTRIM(STO_O.NAME)+" "+ ;
                  ALLTRIM(STO_O.PRODUCER)+" - �竅��� 罐��!"
    ENDIF
    pth = IIF(QNT > STO_O.QNT, "*", " ")
    wd = IIF(EMPTY(pth), wd, pth)
    REPLACE LIST_NUM WITH ln,         ;
            CODE     WITH STO_O.CODE, ;
            ADDRESS  WITH STO_O.ADDRESS, ;
            PREFIX   WITH STO_O.PREFIX,;
            PRODUCER WITH STO_O.PRODUCER,;
            NAME     WITH STO_O.NAME, ;
            AMODEL   WITH ORD.MODEL,  ;
            FUNCTION WITH ORD.FUNC,   ;
            QNT      WITH ORD.QNT,    ;
            IS_DEF   WITH pth,        ;
            USD_DEF  WITH prc_sv_d,   ;
            DEF_PRICE WITH prc_sv_r,  ;
            USD_PRICE WITH IIF(LIST_TIT.IS_USD, prc_sv_d, 0),  ;
            SAL_PRICE WITH IIF(LIST_TIT.IS_USD, 0, prc_sv_r)
    IF .NOT. EMPTY(STO_O.NOTE)
      DO View_Nt WITH "STO_O"
    ENDIF
  ENDIF
  SELECT ORD
ENDSCAN
USE
SELECT STO_O
USE
SELECT LDW
USE
SELECT LIST_TIT
REPLACE IS_DEF WITH wd
DO U_Double WITH ln
SELECT (sel)
DO Exit_P WITH ext_menu
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                 蹍������ �皖琺皋� ����� ������.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Opn_File
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

pth_w = ALLTRIM(pth)
IF LEN(pth_w) > 0 .AND. RIGHT(pth_w,1) # "\"
  pth_w = pth_w+"\"
ENDIF
IF .NOT. FILE(pth_w+"ORD.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ ��� ������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

SELECT 0
USE (pth_w+"ORD.DBF")
RETURN .T.
