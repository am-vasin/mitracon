*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ �ョ�皋 ��痳�Δキ┤ �爬ガ��.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Diff_AS

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
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
s_sav = SELECT()

IF .NOT. File_O(path_comm+"INVOICET.DBF") .OR. .NOT. File_O(path_comm+"INVOICED.DBF") .OR. ;
   .NOT. File_O(path_comm+"INVOICET.CDX") .OR. .NOT. File_O(path_comm+"INVOICED.CDX")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 磴�モ瘴 ���皋 ����� � �����き覓�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
SELECT 0
USE (path_comm+"INVOICET")
SELECT 0
DO Use_Dummy
SELECT INVOICET
GO BOTTOM

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "�皙モ�碎" KEY Ctrl-A, "F3"
ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Work OF (menu_name) PROMPT "��｀��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Work OF (menu_name) DO Sw_Mode WITH "Work"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, m_list, prnt_sw, dt0
prnt_sw = .F.
m_list = ""
dt0 = DATE()
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 22

DO D_Wins WITH ln, wd, "�ムョキ� �����き諷", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F3    DO Mark_

*
*    BROWSE - �キ�
*
    BROWSE FIELDS M000 = IIF(STR(RECNO())+"!" $ m_list, "�", " "):1:H="", ;
                  EXP_DATE:H="", ;
                  EXP_NUM:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Work")
    ENDIF

  CASE what_do = "Work"    && 蹍甃�矗 甎�瓷�

    IF EMPTY(m_list)
      m_list = STR(RECNO())+"!"
    ENDIF
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  [ ���讓� ��痳�Δキ�� ] [ �瓮 ��Ж罔� ]"
    mss(3) = ""
    prnt_sw = Out_Mess(5,"mss") = 1
    DO Fill_Dbf
    DO P_Diff
    EXIT

  CASE what_do = "Mark"    && �皙モ��: 竅����→�/甅閧┘

    DO Mark_
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
CLOSE DATABASES
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      �痰���→�/甅閧┘ �皙モ��.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Mark_
PRIVATE p

p = AT(STR(RECNO())+"!",m_list)
IF p = 0
  m_list = m_list+STR(RECNO())+"!"
ELSE
  m_list = STUFF(m_list, p, 11, "")
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ �牀▲爲� ��玻オ皚�痰� ���ォ ゛����.             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tst_Blank
PRIVATE s, mss, f_w

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

f_w = ALLTRIM(f_name)
IF .NOT. "." $ f_w
  f_w = f_w+".DBF"
ENDIF

IF .NOT. File_O(ALLTRIM(f_w))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ ����� � �����き覓�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

f_name = f_w

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�           蹍������ ������キ�� ∇ガキ��� ��щ ��痳�Δキ┤.            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Fill_Dbf
PRIVATE pr_nams, pr_cods, i, n, n_1, n_w

DELETE FILE (tmpo_path+"Diff_Inc.dbf")
DELETE FILE (tmpo_path+"Diff_Inc.cdx")

SELECT 0
USE (base_path+"DEVELOP")
i = 0
SCAN
  i = i+1
  DIMENSION pr_nams(i), pr_cods(i)
  pr_nams(i) = B_NAME
  pr_cods(i) = CODE
ENDSCAN

SET EXCLUSIVE ON
CREATE DBF (tmpo_path+"Diff_Inc")  ;
  ( BOX_NO   C(10), ;
    ACT_NO   C(10), ;
    ACT_DT   D,     ;
    CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    PRO_CODE N( 5), ;
    MRK      C( 1), ;
    QNT_INV  N( 6), ;
    QNT_ACT  N( 6)  )

USE (tmpo_path+"Diff_Inc")
INDEX ON BOX_NO+STR(CODE,7) TAG CODE
INDEX ON BOX_NO+NAME+PREFIX+STR(PRO_CODE,5) TAG NAME ADDITIVE

SET EXCLUSIVE OFF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (path_comm+"INVOICED") ORDER TAG EXP_NUM ALIAS SRS
SET RELATION TO STO_CODE INTO STOCK

SELECT INVOICET

DO WHILE .NOT. EMPTY(m_list)
  n_w = VAL(ALLTRIM(LEFT(m_list,10)))
  GO n_w
  m_list = SUBSTR(m_list,12)
  IF dt0 > EXP_DATE
    dt0 = EXP_DATE
  ENDIF
  SELECT SRS
  SEEK LEFT(DTOS(INVOICET.EXP_DATE),4)+INVOICET.EXP_NUM
  SCAN REST WHILE INVOICET.EXP_DATE = EXP_DATE .AND. INVOICET.EXP_NUM = EXP_NUM
    n = 1
    FOR i = 1 TO ALEN(pr_nams)
      IF ALLTRIM(PRODUCER) == ALLTRIM(pr_nams(i))
        n = pr_cods(i)
        EXIT
      ENDIF
    ENDFOR
    SELECT DIFF_INC
    APPEND BLANK
    REPLACE BOX_NO   WITH SRS.EXP_NUM,  ;
            CODE     WITH SRS.STO_CODE, ;
            PREFIX   WITH IIF(FOUND("STOCK"), STOCK.PREFIX, SRS.PREFIX), ;
            NAME     WITH IIF(FOUND("STOCK"), STOCK.NAME, SRS.NAME), ;
            PRODUCER WITH IIF(FOUND("STOCK"), STOCK.PRODUCER, SRS.PRODUCER), ;
            PRO_CODE WITH IIF(FOUND("STOCK"), STOCK.PRO_CODE, n), ;
            QNT_INV  WITH SRS.QTY
    SELECT SRS
  ENDSCAN
  SELECT INVOICET

ENDDO
USE
SELECT SRS
USE (base_path+"ACT_S") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT 0
USE (base_path+"ACT_S_T")

n = "**********"
SELECT DIFF_INC
SET ORDER TO TAG CODE
PRIVATE a_r_sav, key_w
SCAN
  IF .NOT. ALLTRIM(n) == ALLTRIM(BOX_NO)   && ��ュ��� �� ��≪� ��牀！�!
    n_1 = ALLTRIM(BOX_NO)
    SELECT ACT_S_T
    LOCATE FOR ALLTRIM(EXP_NUM) == n_1 .AND.  DOC_DATE >= dt0
    n = DIFF_INC.BOX_NO
    DO WHILE FOUND()
      DO Add_Act
      SELECT ACT_S_T
      a_r_sav = RECNO()
      key_w = FIRM+DTOS(DOC_DATE)+DOC_NUM
      SCAN REST FOR "!" $ DOC_NUM .AND. key_w = FIRM+DTOS(EXP_DATE)+EXP_NUM  && ��玻オ皋珮鉗┘ ��硅
        DO Add_Act
        SELECT ACT_S_T
      ENDSCAN
      SELECT ACT_S_T
      GO a_r_sav
      SKIP
      LOCATE REST FOR ALLTRIM(EXP_NUM) == n_1 .AND.   ;
               DOC_DATE >= dt0
    ENDDO
    SELECT DIFF_INC
  ENDIF
ENDSCAN

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�               蹍������ ぎ��←キ�� �爬ガ���� ����.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Add_Act
PRIVATE rc, i

rc = RECNO("DIFF_INC")
SELECT ACT_S
SEEK ACT_S_T.FIRM+LEFT(DTOS(ACT_S_T.DOC_DATE),4)+ACT_S_T.DOC_NUM
SCAN REST WHILE FIRM = ACT_S_T.FIRM .AND. DOC_NUM = ACT_S_T.DOC_NUM .AND. DOC_DATE = ACT_S_T.DOC_DATE
  *
  *   �薀� �� ��ゃ...
  *
  SELECT DIFF_INC
  SET ORDER TO TAG CODE
  i = " "
  IF .NOT. SEEK(n+STR(ACT_S.CODE,7))
    SET ORDER TO TAG NAME
    IF SEEK(n+STOCK.NAME+STOCK.PREFIX+STR(STOCK.PRO_CODE,5))
      i = "!"
    ENDIF
  ENDIF
  IF .NOT. FOUND()
    APPEND BLANK
    REPLACE BOX_NO WITH n
  ENDIF
  REPLACE ACT_NO   WITH ACT_S_T.DOC_NUM,  ;
          ACT_DT   WITH ACT_S_T.DOC_DATE, ;
          CODE     WITH ACT_S.CODE,       ;
          PREFIX   WITH STOCK.PREFIX,     ;
          NAME     WITH STOCK.NAME,       ;
          PRODUCER WITH STOCK.PRODUCER,   ;
          PRO_CODE WITH STOCK.PRO_CODE,   ;
          MRK      WITH IIF(ACT_S.DOC_DATE = STOCK.DATE_ON, "�", i), ;
          QNT_ACT  WITH QNT_ACT+ACT_S.QNT_INC
  SELECT ACT_S
ENDSCAN

SELECT DIFF_INC
GO rc

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ �ョ�皋 ぎ�祠キ��.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE P_Diff

PRIVATE p_ln, fnt, n_cp, lft, sb, n, t_p, f_p, f_d, i, pic, v_sw, p_drctry
PRIVATE n_p_det, n_pg, f_pg, sp1, sp2, sp3, sp4, sd1, sd2, sd3, sd4
PRIVATE mss, ffeed

DIMENSION sb(1,2)
sb( 1,1) = "{Page}"
sb( 1,2) = ""

n = ALEN(sb,1)
FOR i = 1 TO n
  sb(i,2) =PADR(sb(i,2), LEN(sb(i,1)))
ENDFOR

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "DIFF_INC" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

USE

SELECT DIFF_INC
SET ORDER TO TAG NAME

DO Ini_Prn WITH "�イ���痰� ��痳�Δキ┤", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
PRIVATE pg_wk, doc_wk, f_lin
f_pg  = 1
doc_wk = "**********"
pg_wk  = p_ln-ALEN(t_p)-4
f_lin  = pg_wk+10
DO WHILE .NOT. EOF()
  IF f_lin >= pg_wk
    IF f_pg # 1
      EJECT
    ENDIF
    sb( 1,2) = ALLTRIM(STR(f_pg))
    sb( 1,2) = PADL(sb(1,2), LEN(sb(1,1)))
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
    f_lin = 0
    f_pg = f_pg+1
  ENDIF
  IF doc_wk # BOX_NO
    doc_wk = BOX_NO
    str_w = "********** ��牀！� N "+ALLTRIM(BOX_NO)+" *****************************"
    @ PROW()+1, 0 SAY str_w
    f_lin = f_lin+1
    LOOP
  ENDIF
  IF prnt_sw .AND. QNT_INV = QNT_ACT
    SKIP
    LOOP
  ENDIF
  str_w = PREFIX+" "+NAME+" "+PRODUCER+" "+MRK+" "+ ;
          TRANSFORM(QNT_INV, "@Z 999999")+" "+TRANSFORM(QNT_ACT, "@Z 999999")+  ;
          IIF(QNT_INV = QNT_ACT, "  ", " �")+IIF(EMPTY(ACT_NO), "    �モ",ACT_NO)
  @ PROW()+1, 0 SAY str_w
  f_lin = f_lin+1
  SKIP

ENDDO
USE

DO Term_Prn WITH "", tmpo_path+"Diff_Inc.dbf"

RETURN
