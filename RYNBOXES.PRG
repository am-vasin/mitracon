*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� RynBoxes     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�        ��瓷��お� �����き�� �ムイ�腮 �� 爛��├�罔� �� ��牀！��.         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 12.09.2000 様�
PROCEDURE RynBoxes
PARAMETERS d_num, d_date

PRIVATE s_sav, mss, path_as, cus_sav
s_sav = SELECT()
path_as = ALLTRIM(Get_Sys("PATH_AS"))
IF EMPTY(path_as)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ ぎ痰祚� � 甎�瓷� ��牀｀�"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

SELECT 0
USE (base_path+"RYN_M_T") AGAIN ALIAS SOURCE_T ORDER TAG DOC_NUM
= SEEK(STR(YEAR(d_date),4)+STR(d_num,5))
cus_sav = CUS_CODE
IF EMPTY(TECHNID)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  �� 磚ムキ�, 艪� 轤� �� �����き��? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
ELSE
  SELECT 0
  USE (path_as+"BOXNO") ORDER TAG TECHNID
  IF SEEK(SOURCE_T.TECHNID)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��牀！� か� 轤�� �����き�� 礒� 痲�爼�牀����!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT SOURCE_T
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
  USE
  SELECT SOURCE_T
ENDIF
DO BoxesTmp
SELECT BOXESTMP
IF RECCOUNT() = 0
  USE
  DELETE FILE (tmpo_path+"BOXESTMP.DBF")
  SELECT MATERS
  USE
  SELECT BOXNO
  USE
  SELECT SOURCE_T
  USE
  SELECT (s_sav)
  RETURN
ENDIF

DO Prp_List

DO F_Box

SELECT SOURCE_T
USE
SELECT MATERS
USE
SELECT BOXNO
USE
SELECT BOXESTMP
USE
SELECT BOXCONT
USE
SELECT BOXDISTR
USE
DELETE FILE (tmpo_path+"BOXESTMP.DBF")
DELETE FILE (tmpo_path+"BOXESTMP.CDX")
DELETE FILE (tmpo_path+"BOXCONT.DBF")
DELETE FILE (tmpo_path+"BOXCONT.CDX")
DELETE FILE (tmpo_path+"BOXDISTR.DBF")
DELETE FILE (tmpo_path+"BOXDISTR.CDX")

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_List     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ��ぃ�皰→� ��｀腑�� �����.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.09.2000 陳�
PROCEDURE Prp_List

SELECT 0
DELETE FILE (tmpo_path+"BOXCONT.DBF")
DELETE FILE (tmpo_path+"BOXCONT.CDX")
CREATE DBF (tmpo_path+"BOXCONT") ;
  ( BOX_REF   N(10), ;
    CODE      N( 7), ;
    PURSYS    C( 1), ;
    IDL       N(10), ;
    QNT       N( 6) )
INDEX ON STR(BOX_REF)+STR(CODE) TAG CODE

SELECT 0
DELETE FILE (tmpo_path+"BOXDISTR.DBF")
DELETE FILE (tmpo_path+"BOXDISTR.CDX")
CREATE DBF (tmpo_path+"BOXDISTR") ;
  ( CODE      N( 7), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    PURSYS    C( 1), ;
    IDL       N(10), ;
    QNT       N( 6), ;
    QNT_READY N( 6), ;
    QNT_BOX   N( 6)  )
INDEX ON CODE TAG CODE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE
SET ORDER TO TAG CODE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_0912 AGAIN
SELECT 0
USE (base_path+"RYN_M") ORDER TAG DOC_NUM ALIAS SOURCE_D AGAIN
= SEEK(STR(YEAR(SOURCE_T.DOC_DATE),4)+STR(SOURCE_T.DOC_NUM,5))
SCAN REST WHILE SOURCE_T.DOC_DATE = DOC_DATE .AND. SOURCE_T.DOC_NUM = DOC_NUM
  SELECT BOXDISTR
  IF .NOT. SEEK(SOURCE_D.CODE)
    = SEEK(SOURCE_D.CODE, "ST_0912")
    APPEND BLANK
    REPLACE CODE      WITH SOURCE_D.CODE,  ;
            PURSYS    WITH SOURCE_D.PURSYS,;
            IDL       WITH SOURCE_D.IDL,   ;
            PREFIX    WITH ST_0912.PREFIX, ;
            NAME      WITH ST_0912.NAME,   ;
            PRODUCER  WITH ST_0912.PRODUCER
  ENDIF
  REPLACE QNT  WITH QNT+SOURCE_D.QNT
  SELECT SOURCE_D
ENDSCAN

USE
SELECT ST_0912
USE
SELECT BOXDISTR

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fil_Boxes    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          ������キ┘ ��牀｀�.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.09.2000 陳�
PROCEDURE F_Box

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

SELECT 0
DO Use_Dummy
SELECT 0

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Box OF (menu_name) PROMPT "��牀！�" KEY Ctrl-A, "Tab"
ON SELECTION PAD Box OF (menu_name) DO Sw_Mode WITH "Box"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 67

DO D_Wins WITH ln, wd, "��瓷��お� �� ��牀！��: ���-� N "+  ;
               ALLTRIM(STR(SOURCE_T.DOC_NUM))+" �� "+DTOC(SOURCE_T.DOC_DATE), 2, 0
what_do = "List"
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC0912 AGAIN
= SEEK(SOURCE_T.CUS_CODE)
@ 1, 1 SAY CUS_NAME
USE
@ 2,  1 SAY "��牀！�"
@ 2,  9 SAY ALLTRIM(BOXNO.OFFICE)+ALLTRIM(STR(BOXNO.BOXNO))+" "+BOXNO.MASK+SPACE(20)
@ 3,  2 SAY "陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳��瓮��陳��Й.陳�痰��.�� ���."

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 2,  9 SAY ALLTRIM(BOXNO.OFFICE)+ALLTRIM(STR(BOXNO.BOXNO))+" "+BOXNO.MASK+SPACE(20)
    DO Prp_Nav_1
    SELECT BOXDISTR
    SET ORDER TO TAG NAME
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Box"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="",  ;
                  NAME:H="",    ;
                  PRODUCER:H="",;
                  QNT:H="":P="999999", ;
                  QNT001 = QNT_READY+QNT_BOX:H="":P="999999", ;
                  QNT002 = QNT-QNT_READY-QNT_BOX:H="":P="999999", ;
                  QNT_BOX:H="":P="999999":V=Tst_Qnt()    ;
           FREEZE QNT_BOX ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "List")
    ENDIF

  CASE what_do = "Box"    && ��キ� ��牀！�

    DO Ch_Box    
    what_do = "List"

  OTHERWISE

    IF .NOT. SaveCont()
      EXIT
    ENDIF
    what_do = "List"

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Qnt      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          蹍▲爲� ����腑痰��.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.09.2000 陳�
PROCEDURE Tst_Qnt
PRIVATE mss

IF QNT-QNT_READY-QNT_BOX < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������ ����腑痰�� �モ � �����き��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF QNT_BOX < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����腑痰〓 �� ��Ε� °碎 �矗���皀�讚覓!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Box       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             ��キ� ��牀！�.                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.09.2000 陳�
PROCEDURE Ch_Box

DO To_Box
DO BoxesTmp
DO From_Box

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� To_Box       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                               � ��牀！�.                               �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.09.2000 陳�
PROCEDURE To_Box

SELECT BOXDISTR
SCAN FOR QNT_BOX # 0
  SELECT BOXCONT
  IF .NOT. SEEK("         0")
    APPEND BLANK
  ENDIF
  REPLACE BOX_REF WITH BOXESTMP.REF_NO,;
          CODE    WITH BOXDISTR.CODE,  ;
          PURSYS  WITH BOXDISTR.PURSYS,;
          IDL     WITH BOXDISTR.IDL,   ;
          QNT     WITH BOXDISTR.QNT_BOX
  SELECT BOXDISTR
  REPLACE QNT_READY WITH QNT_READY+QNT_BOX
  REPLACE QNT_BOX   WITH 0
ENDSCAN

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� From_Box       ��о�｀砒┴ ��むォ ��瓱�                    �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                               �� ��牀！�.                              �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.09.2000 陳�
PROCEDURE From_Box

SELECT BOXDISTR
SET ORDER TO CODE
SELECT BOXCONT
DO WHILE SEEK(STR(BOXESTMP.REF_NO))
  SELECT BOXDISTR
  SEEK BOXCONT.CODE
  REPLACE QNT_READY WITH QNT_READY-BOXCONT.QNT, ;
          QNT_BOX   WITH BOXCONT.QNT
  SELECT BOXCONT
  REPLACE BOX_REF WITH 0
ENDDO
SELECT BOXDISTR
SET ORDER TO NAME

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� SaveCont     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    ��縲���碎 ��瓷��お� �� ��牀！��.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.09.2000 陳�
PROCEDURE SaveCont

PRIVATE mss, s_sav, r_sav, to_work, cont_work

to_work   = .T.
cont_work = .F.
*
*  蹍▲瑙ガ ≡� �� ��Й�Ε��?
*
s_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN ALIAS ST_0913
SELECT BOXDISTR
r_sav = RECNO()
SCAN
  IF QNT_READY+QNT_BOX # QNT
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"�� ≡� ��Ж罔� �����痰跪 ��Й�Ε��. �皰 ＜ぅ� ぅ��碎?"
    mss(3) = CHR(0)+"  [ 蹍ぎ�Θ� ] [ ��▲琥━ ]"
    mss(4) = ""
    IF Out_Mess(7,"mss") = 1
      to_work   = .F.
      cont_work = .T.
    ELSE
      to_work   = .F.
      cont_work = .F.
    ENDIF
    EXIT
  ENDIF
ENDSCAN
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SELECT (s_sav)

IF to_work
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ��縲���碎 ��瓷��お� �� ��牀！��? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(5,"mss") = 1
    PRIVATE tmpIdl, tmpLgS
    to_work = .T.
    cont_work = .F.
    DO To_Box
    SELECT 0
    USE (path_as+"LOGSTOCK")
    SELECT BOXESTMP
    SET RELATION TO
    SELECT BOXCONT
    SCAN FOR BOX_REF # 0
      IF BOX_REF # RECNO("BOXNO")
        GO BOXCONT.BOX_REF IN BOXNO
      ENDIF
      SELECT ST_0913
      = SEEK(BOXCONT.CODE)
      SELECT MATERS
*      tmpIdl = IDL_MT
      tmpLgS = LGSTCODE
      REPLACE LGSTCODE WITH LGSTCODE+1   && IDL_MT WITH IDL_MT+1,
      SELECT LOGSTOCK
      APPEND BLANK
      REPLACE PURSYS    WITH BOXCONT.PURSYS,  ;
              SUBSYS    WITH MATERS.SUBSYS,   ;
              IDL       WITH BOXCONT.IDL,     ;
              LGCODE    WITH tmpLgS,          ;
              STO_CODE  WITH BOXCONT.CODE,    ;
              PREFIX    WITH ST_0913.PREFIX,  ;
              NAME      WITH ST_0913.NAME,    ;
              PRODUCER  WITH ST_0913.PRODUCER,;
              DATE_ON   WITH DATE(),          ;
              DATE_MOVE WITH DATE(),          ;
              QTY       WITH BOXCONT.QNT,     ;
              BOX_YEAR  WITH BOXNO.BOX_YEAR,  ;
              CUS_CODE  WITH cus_sav,         ;
              OFFICE    WITH BOXNO.OFFICE,    ;
              BOXNO     WITH BOXNO.BOXNO
      SELECT BOXCONT
    ENDSCAN
    SELECT LOGSTOCK
    USE
  ELSE
    cont_work = .F.
    to_work = .F.
  ENDIF
ENDIF

IF .NOT. cont_work .AND. .NOT. to_work
  SELECT BOXESTMP
  SCAN
    SELECT BOXNO
    REPLACE TECHNID WITH ""
    SELECT BOXESTMP
  ENDSCAN
ENDIF

SELECT ST_0913
USE
SELECT (s_sav)

RETURN cont_work
