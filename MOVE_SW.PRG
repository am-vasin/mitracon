*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Move_S       ��о�｀砒┴ ��むォ ��瓱�           16.06.97 14:58:12 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                          �※Ε�┘ �� 瓷��ゃ.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Move_SW
PARAMETER cd_pos
PRIVATE sel_sav, is_dummy, doc_wd, t_col, c_w

t_col = LEFT(SCHEME(1,8), AT("/",SCHEME(1,8))-1)
IF RIGHT(t_col,1) = "+"
  t_col = LEFT(t_col,LEN(t_col)-1)+"*"
ENDIF
t_col = LEFT(SCHEME(1,1), AT("/",SCHEME(1,1))-1)+"/"+t_col
PUSH KEY CLEAR
sel_sav = SELECT()
IF USED("DUMMY")
  is_dummy = .T.
ELSE
  is_dummy = .F.
  DO Use_Dummy
ENDIF
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN ALIAS STO_0
SEEK cd_pos
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD P_Doc OF (menu_name) PROMPT "�ョ�碎 ぎ�祠キ��" KEY Ctrl-A, "Enter"
ON SELECTION PAD P_Doc OF (menu_name) DO Sw_Mode WITH "P_Doc"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
IF .NOT. Prep_M()
  SELECT STO_0
  USE
  SELECT (sel_sav)
  POP KEY
  RETURN
ENDIF
*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(WROWS("")-14, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("CENTURY") = "ON", 70, 68)

DO D_Wins WITH ln, wd, ;
               ALLTRIM(STO_0.PREFIX+" "+STO_0.NAME+" "+STO_0.PRODUCER), 1, 2

@ 2, 2 SAY PADC("����",IIF(SET("CENTURY") = "ON", 10, 8),"�")+"�"+ ;
     "�┓ ぎ�.陳陳��キ�����-〓陳����.陳��┼.����-〓陳����.陳��┼."
doc_wd = IIF(SET("CENTURY") = "ON", 19, 17)

@ 1, doc_wd+3 SAY PADC("�※Ε�┘",29) COLOR (t_col)
c_w = COL()+1
@ 1, c_w      SAY PADC("�痰�皖�",20) COLOR (t_col)
@ WROWS()-4,  3 SAY " ���. �メ皰 "
@ WROWS()-4, 20 SAY " 蹍��▲� "
@ WROWS()-4, c_w SAY PADC(" ���� ����諷 ", 20, "�")
@ WROWS()-3, c_w SAY STR(STO_0.QNT+STO_0.QNT_SAVE+STO_0.QNT_REZ,6) COLOR SCHEME 1
@ WROWS()-3, COL()+1 SAY STR(STO_0.QNT_REP,6) COLOR SCHEME 1
@ WROWS()-3, COL()+1 SAY STR(STO_0.QNT_REAL,6) COLOR SCHEME 1

what_do = "List"
GO BOTTOM


DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS DOC_DATE:H="",  ;
                  TYPE_VIS:H="",  ;
                  PRICE:H="":P="@Z 99999999", ;
                  QNT:H="":P="@Z 999999", ; 
                  QNT_REP:H="":P="@Z 999999", ; 
                  QNT_FIL:H="":P="@Z 999999", ; 
                  QNT_REST:H="":P="@Z 999999", ; 
                  REST_REP:H="":P="@Z 999999", ; 
                  REST_FIL:H="":P="@Z 999999", ; 
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN ShowWho()              ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "P_Docs")
    ENDIF

  CASE what_do = "P_Docs"    && �ョ�碎 ぎ�祠キ��

    DO P_Docs
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
USE
DELETE FILE (tmpo_path+"MOVE.DBF")
DELETE FILE (tmpo_path+"MOVE.CDX")
RELEASE MENU (menu_name) EXTENDED
SELECT STO_0
USE
IF .NOT. is_dummy
  SELECT DUMMY
  USE
ENDIF
SELECT (sel_sav)
POP KEY

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 16.06.97 15:01:08 夕
PROCEDURE Prep_M

PRIVATE qnt_s, dat_s, n_vis, w_who, w_where, w_cus
PRIVATE w_fil, c_sav, p_sav, d_sav, dt_sav
PRIVATE q_s, q_s_r, q_s_f

DELETE FILE (tmpo_path+"MOVE.DBF")
DELETE FILE (tmpo_path+"MOVE.CDX")

SELECT 0
USE (base_path+"PARMS") ALIAS PARMS_0
dat_s = DAT_START
USE

CREATE DBF (tmpo_path+"MOVE.DBF")  ;
   ( DOC_DATE  D,     ;
     DOC_TIME  C( 5), ;
     DOC_NUM   C(10), ;
     DOC_VIS   C(11), ;
     DOC_TYPE  C( 1), ;
     TYPE_VIS  C( 8), ;
     PRICE     N( 8), ;
     QNT       N( 8), ;
     QNT_REP   N( 8), ;
     QNT_FIL   N( 8), ;
     IS_FIL    L,     ;
     QNT_REST  N( 8), ;
     REST_FIL  N( 8), ;
     REST_REP  N( 8), ;
     CUSTOMER  C(50), ;
     WHO       C(29), ;
     WHERE     C(16)  )
INDEX ON DTOS(DOC_DATE)+DOC_TIME TAG HRON

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS WHERE_0 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO_0 AGAIN

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUST_0 AGAIN

*
*    踸絎�
*
SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG DOC_NUM ALIAS ACT_T_0 AGAIN
SET RELATION TO SUP_CODE  INTO CUST_0, ;
                WHO_ACT   INTO WHO_0, ;
                WHERE_ACT INTO WHERE_0

SELECT 0
USE (base_path+"ACT_S") ORDER CODE ALIAS ACT_0 AGAIN
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ACT_T_0

SEEK STO_0.CODE
SCAN REST WHILE CODE = STO_0.CODE FOR DOC_DATE >= dat_s
  n_vis = ALLTRIM(RIGHT(DOC_NUM,2))
  n_vis = ALLTRIM(LEFT(DOC_NUM,8))+IIF(EMPTY(n_vis), "", "/"+n_vis)
  IF FOUND("WHO_0")
    w_who = ALLTRIM(WHO_0.FAMILY)+" "+  ;
            LEFT(WHO_0.NAME,1)+". "+    ;
            LEFT(WHO_0.S_NAME,1)+"."
  ELSE
    w_who = ""
  ENDIF
  IF FOUND("WHERE_0")
    w_where = WHERE_0.ARM_N
  ELSE
    w_where = ""
  ENDIF
  w_cus = CUST_0.CUS_NAME
  SELECT MOVE
  APPEND BLANK
  REPLACE DOC_DATE WITH ACT_T_0.DOC_DATE,;
          DOC_NUM  WITH ACT_T_0.DOC_NUM, ;
          DOC_VIS  WITH n_vis,           ;
          DOC_TYPE WITH "I",             ;
          TYPE_VIS WITH IIF(ACT_T_0.SUP_CODE < 0, "��玻.", "��. ���"), ;
          PRICE    WITH ACT_0.PRICE_R,   ;
          QNT      WITH ACT_0.QNT_INC,   ;
          QNT_REP  WITH ACT_0.QNT_REP,   ;
          CUSTOMER WITH CUST_0.CUS_NAME, ;
          WHO      WITH w_who,           ;
          WHERE    WITH w_where
  SELECT ACT_0
ENDSCAN

*
*     �����き襯 �ムイ�腮 �� 爛��├�罔�
*
USE (base_path+"RYN_M_T") ORDER TAG DOC_NUM ALIAS RYN_T_0 AGAIN
SET RELATION TO CUS_CODE INTO CUST_0, ;
                WHO      INTO WHO_0, ;
                WHERE    INTO WHERE_0

SELECT ACT_T_0
USE (base_path+"RYN_M") ORDER TAG CODE ALIAS RYN_0 AGAIN
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+STR(DOC_NUM,5) INTO RYN_T_0

SEEK STO_0.CODE
SCAN REST WHILE CODE = STO_0.CODE FOR DOC_DATE >= dat_s
  n_vis = ALLTRIM(STR(DOC_NUM))
  IF FOUND("WHO_0")
    w_who = ALLTRIM(WHO_0.FAMILY)+" "+  ;
            LEFT(WHO_0.NAME,1)+". "+    ;
            LEFT(WHO_0.S_NAME,1)+"."
  ELSE
    w_who = ""
  ENDIF
  IF FOUND("WHERE_0")
    w_where = WHERE_0.ARM_N
  ELSE
    w_where = ""
  ENDIF
  w_cus = CUST_0.CUS_NAME
  SELECT MOVE
  APPEND BLANK
  REPLACE DOC_DATE WITH RYN_T_0.DOC_DATE,;
          DOC_NUM  WITH n_vis,           ;
          DOC_VIS  WITH n_vis,           ;
          DOC_TYPE WITH "F",             ;
          TYPE_VIS WITH "�� 爛��.",      ;
          QNT      WITH -RYN_0.QNT,      ;
          QNT_FIL  WITH RYN_0.QNT,       ;
          IS_FIL   WITH .T.,             ;
          CUSTOMER WITH CUST_0.CUS_NAME, ;
          WHO      WITH w_who,           ;
          WHERE    WITH w_where
  SELECT RYN_0
ENDSCAN

*
*     �����き襯 �牀��Θ
*
USE (base_path+"CUSTOMER") ORDER TAG CODE ALIAS CUST_1 AGAIN

SELECT RYN_T_0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS SALE_T_0 AGAIN
SET RELATION TO CUS_CODE INTO CUST_0, ;
                CUS_CODE INTO CUST_1,;
                WHO      INTO WHO_0, ;
                WHERE    INTO WHERE_0

SELECT 0
USE (base_path+"SALE") ORDER TAG CODE ALIAS SALE_0 AGAIN
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO SALE_T_0

SEEK STO_0.CODE
c_sav = 0
p_sav = 0
d_sav = "          "
dt_sav = {}
SCAN REST WHILE CODE = STO_0.CODE FOR DOC_DATE >= dat_s .AND. LEFT(DOC_NUM,1) = sys_char
  w_fil = .F.
  IF FOUND("CUST_1")
    IF .NOT. EMPTY(CUST_1.RYN) .AND. EMPTY(SALE_T_0.LIST_NUM)
      w_fil = .T.
    ENDIF
  ENDIF
      
  n_vis = LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))
  IF FOUND("WHO_0")
    w_who = ALLTRIM(WHO_0.FAMILY)+" "+  ;
            LEFT(WHO_0.NAME,1)+". "+    ;
            LEFT(WHO_0.S_NAME,1)+"."
  ELSE
    w_who = ""
  ENDIF
  IF FOUND("WHERE_0")
    w_where = WHERE_0.ARM_N
  ELSE
    w_where = ""
  ENDIF
  IF FOUND("CUST_0")
    w_cus = CUST_0.CUS_NAME
  ELSE
    w_cus = SALE_T_0.CUS_NAME
  ENDIF
  SELECT MOVE
  IF c_sav = SALE_0.CODE .AND. p_sav = SALE_0.PRICE_R .AND. ;
     d_sav = SALE_0.DOC_NUM .AND. dt_sav = SALE_0.DOC_DATE
    REPLACE QNT     WITH QNT-IIF(w_fil, 0, SALE_0.QNT), ;
            QNT_FIL WITH QNT-IIF(w_fil, SALE_0.QNT, 0), ;
            QNT_REP WITH QNT_REP-SALE_0.QNT_REP
  ELSE
    APPEND BLANK
    REPLACE DOC_DATE WITH SALE_T_0.DOC_DATE,;
            DOC_TIME WITH STR(SALE_T_0.DOC_TIME,5),;
            DOC_NUM  WITH SALE_0.DOC_NUM,  ;
            DOC_VIS  WITH n_vis,           ;
            DOC_TYPE WITH "S",             ;
            TYPE_VIS WITH IIF(SALE_T_0.CUS_CODE < 0, "��玻.", "蹍����"), ;
            IS_FIL   WITH w_fil,           ;
            QNT      WITH IIF(w_fil, 0, -SALE_0.QNT), ;
            QNT_FIL  WITH IIF(w_fil, -SALE_0.QNT, 0), ;
            QNT_REP  WITH -SALE_0.QNT_REP, ;
            PRICE    WITH SALE_0.PRICE_R,  ;
            CUSTOMER WITH w_cus,           ;
            WHO      WITH w_who,           ;
            WHERE    WITH w_where
  ENDIF
  c_sav = SALE_0.CODE
  p_sav = SALE_0.PRICE_R
  d_sav = SALE_0.DOC_NUM
  dt_sav = SALE_0.DOC_DATE
  SELECT SALE_0
ENDSCAN

USE
SELECT SALE_T_0
USE
SELECT CUST_1
USE
SELECT CUST_0
USE
SELECT WHO_0
USE
SELECT WHERE_0
USE

SELECT MOVE
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��Ж罔� あ└キ�� �� °��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"MOVE.DBF")
  DELETE FILE (tmpo_path+"MOVE.CDX")
  RETURN .F.
ENDIF
q_s   = STO_0.QNT_START
q_s_r = STO_0.QNT_S_REP
q_s_f = STO_0.QNT_S_RYN

SCAN
  q_s   = q_s+QNT
  q_s_r = q_s_r+QNT_REP
  q_s_f = q_s_f+QNT_FIL
  REPLACE QNT_REST WITH q_s,   ;
          REST_FIL WITH q_s_f, ;
          REST_REP WITH q_s_r
ENDSCAN
GO TOP

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        ��筮爼�罔����� 痰牀��.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 16.06.97 12:35:10 夕
PROCEDURE ShowWho

ACTIVATE WINDOW (win_name+"_M") SAME

@ 1,  2 SAY PADC("���-� "+ALLTRIM(DOC_VIS), doc_wd) COLOR (t_col)

@ WROWS()-3,  3 SAY WHERE  COLOR SCHEME 1
@ WROWS()-3, COL()+1 SAY WHO COLOR SCHEME 1
@ WROWS()-2,  3 SAY "��┘�� "+CUSTOMER && COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      蹍������ �ョ�皋 ぎ�祠キ皰�.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 17.06.97 14:43:58 夕
PROCEDURE P_Docs

DO CASE
CASE DOC_TYPE = "S"      && �����き�� �牀��Θ
  DO Prn_Inv WITH DOC_NUM, DOC_DATE
CASE DOC_TYPE = "I"      && 踸ガ�覃 ���
  DO P_Act WITH DOC_NUM, DOC_DATE
CASE DOC_TYPE = "F"      && �����き�� �ムイ�腮 � 筥����
  DO P_Inv_F WITH VAL(ALLTRIM(DOC_NUM)), DOC_DATE
ENDCASE

RETURN
