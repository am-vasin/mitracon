*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�               蹍������ �襪�腮 甎��→� �� ��瘁�.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Mon_Hist
PARAMETERS arm0, v_w

PRIVATE arm_w, k00, i, s_dat, e_dat, mn_w, a_name, rest_bd, mss, pic


HIDE MENU Main_M
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

USE (base_path+"PARMS")
IF TYPE("arm0") # "C"
*  s_dat = DATE()-7
  s_dat = DAT_START
  arm_w = arm
ELSE
  s_dat = DAT_START
  arm_w = arm0
ENDIF

e_dat = DATE()

USE (base_path+"ARM_LST") ORDER TAG ARM_C
SEEK arm_w
mn_w = IIF(v_w = " �", S_KASSA, S_KASSA_U)
rest_bd = IIF(v_w = " �", KASSA, KASSA_USD)
pic = IIF(v_w = " �", "@Z 999 999 999.99", "@Z 9 999 999.9999")
a_name = ALLTRIM(ARM_N)
a_sys  = ARM_S

DO Use_Dummy

IF FILE(tmpo_path+"TMPREP1.DBF") .OR. FILE(tmpo_path+"TMPREP1.CDX")
  DELETE FILE (tmpo_path+"TMPREP1.DBF")
  DELETE FILE (tmpo_path+"TMPREP1.CDX")
ENDIF

CREATE DBF (tmpo_path+"TMPREP1")  ;
      ( DAT   D,     ;
        TIM   N( 5), ;
        TYP   C( 1), ;
        D_TP  C( 1), ;
        NUM   C(11), ;
        FIRM  C( 3), ;
        S_K   N(13,4), ;
        D_NAM C(10), ;
        INV   C(11), ;
        BIL   C(10), ;
        B_T   C( 1), ;
        LST   C( 6), ;
        S_I   N(13,4), ;
        RST   N(13,4)  )
USE (tmpo_path+"TMPREP1") EXCLUSIVE
INDEX ON DTOS(DAT)+STR(TIM,5) TAG HRON

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG HRON ALIAS INC

*
*   踸絎き襯 �爐ム� ���� 皋��� � 腑��
*
SCAN FOR DOC_TYPE $ IIF(v_w = " �", " �", "3") .AND. WHERE = arm_w
  IF DOC_DATE < s_dat
    LOOP
  ENDIF
  SELECT TMPREP1
  APPEND BLANK
  REPLACE DAT   WITH INC.DOC_DATE,   ;
          TIM   WITH INC.DOC_TIME,   ;
          D_TP  WITH INC.DOC_TYPE,   ;
          FIRM  WITH INC.FIRM,       ;
          D_NAM WITH IIF(INC.MONEY_R > 0, "踸�.", "��痳.")+" �爐.", ;
          NUM   WITH PADL(LEFT(INC.DOC_NUM,1)+ALLTRIM(SUBSTR(INC.DOC_NUM,2)),11), ;
          S_K   WITH IIF(v_w = " �",INC.MONEY_R+INC.N_OF_SALE,INC.MONEY_D),  ;
          INV   WITH INC.SALE_NUM
  SELECT INC
ENDSCAN

USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS INC
SELECT TMPREP1
*
*   �����き襯
*
SCAN
  IF EMPTY(INV)
    LOOP
  ENDIF
  IF .NOT. SEEK(FIRM+LEFT(DTOS(DAT),4)+LEFT(INV,10), "INC")
    REPLACE INV WITH PADL(LEFT(INV,4)+"-"+ALLTRIM(SUBSTR(INV,5)),11)
    LOOP
  ENDIF
  REPLACE INV WITH PADL(LEFT(INV,4)+"-"+ALLTRIM(SUBSTR(INV,5)),11),  ;
          LST WITH INC.LIST_NUM, ;
          BIL WITH INC.NEW_DOC,  ;
          B_T WITH INC.NEW_TYPE, ;
          D_NAM WITH IIF(SUBSTR(INC.DOC_NUM,2,3) = "***", "���. 腑�","�����き��"), ;
          S_I WITH IIF(v_w = "$", INC.TOTAL_D, INC.TOTAL_R)
ENDSCAN

SELECT INC
USE (base_path+"KASSA") ALIAS INC
*
*   ����� ぅ�ィ
*
SCAN FOR WHERE = arm_w .AND. VALUTA $ v_w
  IF DOC_DATE < s_dat
    LOOP
  ENDIF
  SELECT TMPREP1
  APPEND BLANK
  REPLACE DAT WITH INC.DOC_DATE,   ;
          TIM WITH INC.DOC_TIME,   ;
          TYP WITH IIF(INC.WHO_GET = 0, "-", "+"), ;
          D_NAM WITH "����� ぅ�.", ;
          NUM WITH PADL(LEFT(INC.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(INC.DOC_NUM,5)),11), ;
          S_K WITH -INC.TOTAL
  SELECT INC
ENDSCAN
USE

SELECT TMPREP1
SCAN
  REPLACE RST WITH mn_w
  IF TYP # "-"    && �� 竍�硅��ガ �ャ癶. �砒モ�
    mn_w = mn_w+S_K
  ENDIF
ENDSCAN
SELECT 0

DELETE FILE (tmpo_path+"TMPREP2.DBF")
DELETE FILE (tmpo_path+"TMPREP2.CDX")
CREATE DBF (tmpo_path+"TMPREP2")  ;
      ( DT   D,     ;
        MN   N(10,2), ;
        SM_I N(10,2), ;
        SM_E N(10,2), ;
        SM_D N(10,2)  )
USE TMPREP2 EXCLUSIVE
INDEX ON DTOS(DT) TAG HRON

*IF TYPE("arm0") # "C"
*  s_dat = DATE()-7
*ENDIF

PRIVATE dt0, si, se, sd, sd_s
dt0  = {}
si   = 0
se   = 0
sd   = 0
sd_s = 0
SELECT TMPREP1
SCAN
  IF dt0 # DAT
    SELECT TMPREP2
    IF .NOT. EMPTY(dt0)
      REPLACE SM_I WITH si,  ;
              SM_E WITH se,  ;
              SM_D WITH sd
      si = 0
      se = 0
      sd = 0
    ENDIF
    APPEND BLANK
    REPLACE DT WITH TMPREP1.DAT, ;
            MN WITH TMPREP1.RST
    SELECT TMPREP1
    dt0 = DAT
  ENDIF
  IF TYP = "-"
    sd = sd-S_K
    sd_s = sd_s-S_K
  ENDIF
  IF S_K > 0
    si = si+S_K
  ELSE
    se = se-S_K
  ENDIF
ENDSCAN
SELECT TMPREP2
IF RECCOUNT() # 0
  REPLACE SM_I WITH si,  ;
          SM_E WITH se,  ;
          SM_D WITH sd
ELSE
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"TMPREP2.DBF")
  DELETE FILE (tmpo_path+"TMPREP1.DBF")
  PRIVATE mss00
  DIMENSION mss00(3)
  mss00(1) = ""
  mss00(2) = CHR(0)+"�※Ε��� ぅ�ィ �モ!"
  mss00(3) = ""
  DO Out_Mess WITH 7, "mss00"
  RETURN
ENDIF
GO TOP

PRIVATE wd1, ln1, ln2, wd2, t1, t2, f1, what_do, t_docs

DEFINE MENU Mon1 IN SCREEN && COLOR SCHEME 4
DEFINE PAD Choose OF Mon1 PROMPT "���祠キ硅" KEY Ctrl-A, "Enter"
DEFINE PAD Exit   OF Mon1 PROMPT "�諷��" KEY Ctrl-A, "Esc"

ON SELECTION PAD Choose OF Mon1 DO Sw_Mod WITH "mon2", "Mon1"
ON SELECTION PAD Exit   OF Mon1 DO Sw_Mod WITH "exit", "Mon1"

DEFINE MENU Mon2 IN SCREEN && COLOR SCHEME 4
DEFINE PAD Docs OF Mon2 PROMPT "���祠キ硅"
DEFINE PAD Exit   OF Mon2 PROMPT "��Б���" KEY Ctrl-A, "Esc"

ON PAD Docs OF Mon2 ACTIVATE POPUP Docs
  
  DEFINE POPUP Docs MARGIN RELATIVE SHADOW && COLOR SCHEME 4

  DEFINE BAR  1 OF Docs PROMPT "���祠キ�" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 1 OF Docs DO Sw_Mod WITH "Doc_1", "Mon2"

  DEFINE BAR  2 OF Docs PROMPT "�����き��" KEY Ctrl-A, "Ctrl-Enter"
  ON SELECTION BAR 2 OF Docs DO Sw_Mod WITH "Doc_2", "Mon2"

  DEFINE BAR  3 OF Docs PROMPT "�腑�-���矣��"
  ON SELECTION BAR 3 OF Docs DO Sw_Mod WITH "Doc_3", "Mon2"

ON SELECTION PAD Exit   OF Mon2 DO Sw_Mod WITH "mon1", "Mon2"

ln2 = WROWS("")-8
DIMENSION t2(1), f2(2)
t2(1) = "陳陳��痰�皰�陳��爼陳N ぎ�祠キ����┓ ぎ�-��陳陳陳陳�祠��陳陳陳�ャ癶ムΔ.�"
f2(1) = ""
f2(2) = " �����き�� N NNNNNNNNNNN �� ��.��.���� �祠�� 9 999 999.9999  "
wd2 = LEN(t2(1))
*敖陳陳�痰�皰�陳��爼陳N ぎ�祠キ����┓ ぎ�-��陳陳陳陳�祠��陳陳陳�ャ癶ムΔ.朕
*�9 999 999.9999 ��� NNNNNNNNNNNN NNNNNNNNNN 9 999 999.9999 9 999 999.9999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� �����き�� N NNNNNNNNNNN �� ��.��.���� �祠�� 9 999 999.9999             �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

DO Def_Win WITH "W2_M", "W2_E", "W2_I", ln2, wd2, "", t2, f2
HIDE WINDOW W2_M

ln1 = WROWS("")-9
ln1 = MIN(ln1,RECCOUNT("TMPREP2"))
DIMENSION t1(1), f1(3)
*敖�����陳陳陳陳陳���瘁�陳陳陳陳�踸絎つ陳陳陳陳��痳�つ陳陳陳�ャ癶ムΔ.朕
*���.��.���� 9 999 999.9999 9 999 999.9999 9 999 999.9999 9 999 999.9999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� �痰�皰� 9 999 999.9999   �ャ癶. �砒モ� 9 999 999.9999                �
*� �� ��Д 9 999 999.9999                                               �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
t1(1) = "陳����陳陳陳陳陳���瘁�陳陳陳陳�踸絎つ陳陳陳陳��痳�つ陳陳陳�ャ癶ムΔ.�"
wd1 = LEN(t1(1))
f1(1) = ""
f1(2) = " �痰�皰� "+TRANSFORM(mn_w, pic)+"   �ャ癶. �砒モ� "+TRANSFORM(sd_s, pic)
f1(2) = PADR(f1(2),wd1)
f1(3) = " �� ��Д "+TRANSFORM(rest_bd, pic)
f1(3) = PADR(f1(3),wd1)

DO Def_Win WITH "W1_M", "W1_E", "W1_I", ln1, wd1, a_name, t1, f1

what_do = "mon1"
SELECT TMPREP2
GO BOTTOM
DO WHILE .T.
  DO CASE
  CASE what_do = "mon1"
    SELECT TMPREP2
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("�キ� F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU Mon1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F10 ACTIVATE MENU Mon1
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "Mon1", "W1_M", "W1_E"
    ACTIVATE WINDOW W1_E
    BROWSE FIELDS DT:P="@D":H="", ;
                  MN:P=pic:H= "", ;
                  SM_I:P=pic:H= "", ;
                  SM_E:P=pic:H= "", ;
                  SM_D:P=pic:H= "", ;
                  DUMMY.F:H=""   ;
                  KEY DTOS(s_dat), DTOS(e_dat)  ;
                  FREEZE DUMMY.F NOAPPEND NODELETE NOMODIFY NOCLEAR NOMENU ;
                  WINDOW W1_I IN W1_E
    ON KEY LABEL Enter
    ON KEY LABEL F10
    ON KEY LABEL LeftMouse
    IF READKEY() % 256 = 12 .OR. what_do = "exit"
      EXIT
    ELSE
      = SEEK(DTOS(DT),"TMPREP1")
      what_do = "mon2"
    ENDIF
  CASE what_do = "mon2"
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("�キ� F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU Mon2
    ACTIVATE WINDOW W2_M
    @ 0, 0 SAY PADC("�※Ε�┘ ぅ�ィ �� "+TRANSFORM(TMPREP2.DT,"@D"),WCOLS()) COLOR SCHEME 5
    SELECT TMPREP1
    ACTIVATE WINDOW W2_E
    ON KEY LABEL F10 ACTIVATE MENU Mon2
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mod WITH "Doc_2", "Mon2"
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "Mon2", "W2_M", "W2_E"
    BROWSE FIELDS RST:P=pic:H= "", ;
                  FIRM:H="",  ;
                  f000 = D_Char(TYP)+NUM:12:H= "", ;
                  D_NAM:H="", ;
                  S_K:H="":P=pic, ; 
                  f001 = IIF(TYP="-", -S_K, 0):H="":P=pic, ; 
                  DUMMY.F:H=""   ;
                  KEY DTOS(TMPREP2.DT) ;
                  WHEN Show_Inv() ;
                  FREEZE DUMMY.F NOAPPEND NODELETE NOMODIFY NOCLEAR NOMENU ;
                  WINDOW W2_I IN W2_E
    ON KEY
    IF READKEY() % 256 = 12 .AND. what_do = "mon2"
      HIDE WINDOW W2_E, W2_M
      what_do = "mon1"
    ELSE
      IF what_do = "mon2"
        what_do = "Doc_1"
      ENDIF
    ENDIF
  CASE what_do = "Doc_1"
    ACTIVATE SCREEN
    HIDE MENU Mon2
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    DO P_Doc
    what_do = "mon2"
  CASE what_do = "Doc_2"
    IF EMPTY(INV)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"� �����き�� �� °��..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      ACTIVATE SCREEN
      HIDE MENU Mon2
      @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
      DO Prn_Inv WITH FIRM, LEFT(ALLTRIM(INV), 4)+PADL(SUBSTR(ALLTRIM(INV),6),6), DAT
    ENDIF
    what_do = "mon2"
  CASE what_do = "Doc_3"
    IF EMPTY(INV)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"� �����き�� �� °��..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      ACTIVATE SCREEN
      HIDE MENU Mon2
      @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
      DO P_Bill_F WITH FIRM, BIL, DAT, B_T
    ENDIF
    what_do = "mon2"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO
RELEASE MENU Mon1
RELEASE MENU Mon2
RELEASE WINDOWS W1_M, W2_M, W1_E, W2_E, W1_I, W2_I

CLOSE DATABASES
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                                Footer.                                �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Show_Inv

ACTIVATE WINDOW W2_M
@ WROWS()-1, 20 SAY INV
@ WROWS()-1, 35 SAY DAT
@ WROWS()-1, 52 SAY S_I PICTURE pic
ACTIVATE WINDOW (ALIAS())
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ 甃キ� 爛Θ��.                              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sw_Mod
PARAMETERS m, m_nam
PRIVATE w

w = IIF(TYPE("m_nam") = "C", m_nam, "")

what_do = m
KEYBOARD CHR(27)

IF .NOT. EMPTY(w)
  DEACTIVATE MENU (w)
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�        蹍������ - �÷�｀砒┴ �謌�� か� 甎�瓷��.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE M_Proc_1
PARAMETER menu_nm, wm, we
PRIVATE mc, mr

*
*  �瓱�縲����� �キ�
*
IF MENU() == UPPER(menu_nm)
  RETURN
ENDIF

IF MROW("") = 0
  IF .NOT. MENU() == UPPER(menu_nm)
    ACTIVATE MENU (menu_nm)
  ENDIF
  RETURN
ENDIF

*
*  ｪ�� BROWSE
*
mc = MCOL(we)
mr = MROW(we)
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  �甅�↓�� ����
*
mc = MCOL(wm)
mr = MROW(wm)
IF mc >= 0 .AND. mr >= 0
  IF mr = 1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WROWS(we)+2
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                     蹍������ �ョ�皋 ぎ�祠キ皰�.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE P_Doc

PRIVATE d_n

ACTIVATE SCREEN
HIDE MENU Mon2
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

IF INLIST(TYP, "-", "+")  && ����� ぅ�ィ
  d_n = ALLTRIM(NUM)
  d_n = LEFT(d_n,4)+PADL(SUBSTR(d_n,6),6)
  DO P_Rep WITH d_n, DAT
ELSE
  d_n = ALLTRIM(NUM)
  d_n = LEFT(d_n,1)+PADL(SUBSTR(d_n,2),9)
  DO P_Money WITH FIRM, D_TP, d_n, DAT
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    �━〓� - ��爲ム 皋�� ぎ�祠キ��.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE D_Char
PARAMETER c

IF INLIST(c, "-", "+", " ")
  RETURN c
ENDIF

IF c = "2"
  RETURN "!"
ENDIF

RETURN "$"
