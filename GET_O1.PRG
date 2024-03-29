*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          蹍������ □��� ������ ├ �牀├〓�讚��� �����.               �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Get_O1

PARAMETERS ext_menu

PRIVATE pth, pth_w, ex, ln, wd, sel, mss, rc_sv, prc_sv_r, prc_sv_d, code_sv
PRIVATE s00r, s00d, q_w, q_sav, b_mode, is_rus

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

DO OrdTr
IF .NOT. File_O(tmpo_path+"T_ORD_.DBF")
  RETURN
ENDIF

sel = SELECT()
SELECT 0
USE (tmpo_path+"T_ORD_.DBF") ALIAS ORD
SELECT 0
USE (base_path+"LIST_DET") AGAIN ALIAS LDW
SELECT 0
USE (base_path+"RUSBREND") ORDER TAG CODE AGAIN ALIAS BREND
SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME_P AGAIN ALIAS STO_O
SET RELATION TO PRO_CODE INTO BREND
SET FILTER TO EMPTY(STOP_SALE)
SELECT ORD
ln = STR(LIST_TIT.LST_NUM,6)
wd = " "
STORE 0 TO s00r, s00d
b_mode = B_Mode()
SCAN FOR .NOT. EMPTY(NAME) .AND. .NOT. EMPTY(QNT)
  SELECT LDW
  APPEND BLANK
  IF .NOT. SEEK(ORD.NAME+ORD.PREFIX+IIF(EMPTY(ORD.PRODUCER), "", ORD.PRODUCER),"STO_O")
    wd = "*"
    REPLACE LIST_NUM WITH ln,         ;
            PREFIX   WITH ORD.PREFIX, ;
            NAME     WITH ORD.NAME,   ;
            PRODUCER WITH ORD.PRODUCER, ;
            AMODEL   WITH ORD.MODEL,  ;
            FUNCTION WITH ORD.FUNC,   ;
            QNT      WITH ORD.QNT
  ELSE
    prc_sv_r = STO_O.PRICE_R
    prc_sv_d = STO_O.PRICE
    code_sv = STO_O.CODE
    q_sav   = STO_O.QNT
    rc_sv   = RECNO("STO_O")
    is_rus = FOUND("BREND")
    *
    *  ���薀� ｀�ゥ ��ゅ�わ蜒� ��Ж罔� 痼イ� �牀腮� �牀├〓え皀�ォ.
    *
    DO WHILE .T. .AND. EMPTY(ORD.PRODUCER)
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
      IF Better()
        is_rus = FOUND("BREND")
        prc_sv_r = STO_O.PRICE_R
        prc_sv_d = STO_O.PRICE
        code_sv = STO_O.CODE
        rc_sv   = RECNO("STO_O")
        q_sav   = STO_O.QNT
      ENDIF
    ENDDO
    GO rc_sv IN STO_O
    IF is_rus .AND. b_mode = 3
      DELETE
      SELECT ORD
      LOOP
    ENDIF
    IF ORD.PRICE > 0
      IF LIST_TIT.IS_USD
        prc_sv_d = ORD.PRICE
      ELSE
        prc_sv_r = ORD.PRICE
      ENDIF
    ENDIF
    IF IIF(LIST_TIT.IS_USD, prc_sv_d, prc_sv_r) = 0
      WAIT WINDOW ALLTRIM(STO_O.PREFIX)+" "+ALLTRIM(STO_O.NAME)+" "+ ;
                  ALLTRIM(STO_O.PRODUCER)+" - �竅��� 罐��!"
    ENDIF
    pth = IIF(QNT > STO_O.QNT, "*", " ")
    wd = IIF(EMPTY(pth), wd, pth)
    q_w = ORD.QNT
*    IF q_w % MAX(STO_O.QNT_ROUND,1) # 0 .AND. q_w > 0
      q_w = Round_P(STO_O.PREFIX+" "+STO_O.NAME+STO_O.PRODUCER, q_w, STO_O.QNT_ROUND, STO_O.QNT_PACK, STO_O.PRICE)
*    ENDIF
    IF EMPTY(STO_O.CODE)   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      IF EOF("STO_O") .OR. BOF("STO_O")
        WAIT WINDOW "���ユ �����!"
        SUSPEND
      ELSE
        WAIT WINDOW "鉑皰� ���!"
        SUSPEND
      ENDIF
    ENDIF   &&&&&&&&&&&&&&&&&&&&&&&&&
    REPLACE LIST_NUM WITH ln,         ;
            CODE     WITH STO_O.CODE, ;
            ADDRESS  WITH STO_O.ADDRESS, ;
            PREFIX   WITH STO_O.PREFIX,;
            PRODUCER WITH STO_O.PRODUCER,;
            NAME     WITH STO_O.NAME, ;
            AMODEL   WITH ORD.MODEL,  ;
            FUNCTION WITH ORD.FUNC,   ;
            QNT      WITH q_w,        ;
            IS_DEF   WITH pth,        ;
            USD_DEF  WITH prc_sv_d,   ;
            DEF_PRICE WITH prc_sv_r,  ;
            USD_PRICE WITH IIF(LIST_TIT.IS_USD, prc_sv_d, 0),  ;
            SAL_PRICE WITH IIF(LIST_TIT.IS_USD, 0, prc_sv_r)
    s00r = s00r+SAL_PRICE*QNT
    s00d = s00d+USD_PRICE*QNT
    IF .NOT. EMPTY(STO_O.NOTE)
      DO View_Nt WITH "STO_O"
    ENDIF
  ENDIF
  IF LIST_TIT.IS_USD
    REPLACE IS_SPEC WITH IIF(USD_DEF = USD_PRICE, " ", "*")
  ELSE
    REPLACE IS_SPEC WITH IIF(DEF_PRICE = SAL_PRICE, " ", "*")
  ENDIF
  SELECT ORD
ENDSCAN
USE
DELETE FILE (tmpo_path+"T_ORD_.DBF")
SELECT BREND
USE
SELECT STO_O
USE
SELECT LDW
USE
SELECT LIST_TIT
REPLACE IS_DEF WITH wd, R_TOTAL WITH IIF(IS_USD, s00d, s00r)
DO U_Double WITH ln
SELECT (sel)
DO Exit_P WITH ext_menu
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� B_Mode       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                 �ウ━ ��ぁ��� 筥爼 �牀├〓え皀�ォ ��.                  �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 17.07.2000 陳�
PROCEDURE B_Mode

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�  ( ) �瓮 ��↓�罐���                  �
*�  ( ) ��瘁┤瓷┘ � ��甄イ�銕 �腑爛れ  �
*�  ( ) ��瘁┤瓷┘ �� �爛か���碎        �
*�                                      �
*�               < OK >                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, vl
ex = 1
vl = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 42, "�ウ━ �襦��� ÷キぎ�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2, 4 GET vl PICTURE "@*RV �瓮 ��↓�罐���; ��瘁┤瓷┘ � ��甄イ�銕 �腑爛れ; ��瘁┤瓷┘ �� �爛か���碎"
@ WROWS()-2, FLOOR(WCOLS()/2-3) GET ex PICTURE "@*HT \ OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN vl

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Better       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                              �竍┤ ÷キ�?                              �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 17.07.2000 陳�
PROCEDURE Better
PRIVATE res
*SUSP &&&&&&&&

res = IIF(LIST_TIT.IS_USD, STO_O.PRICE   < prc_sv_d, ;
                      STO_O.PRICE_R < prc_sv_r) .OR. ;
      q_sav = 0 .AND. STO_O.QNT # 0

DO CASE
CASE b_mode = 1
  RETURN res
CASE b_mode = 2
  IF FOUND("BREND")
    RETURN res .AND. (is_rus .OR. q_sav = 0 .AND. STO_O.QNT # 0)
  ELSE
    RETURN res
  ENDIF
OTHERWISE
  IF FOUND("BREND")
    RETURN .F.
  ENDIF
ENDCASE
