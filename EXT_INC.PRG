*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ext_Inc      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ｡��｀皖� ����� �爬絎�� ��痰�∫┴�                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 11.12.2005 様�
PROCEDURE Ext_Inc
PARAMETERS inc_ID	&& ���ム ����皰→�

PRIVATE mss, s_sav, fld_list, mode_tch, tmpSCode, tmpNDS, st_rec, st_tag
m.mode_tch = .T.
PRIVATE FName
m.FName = ""
m.st_rec = 0
m.st_tag = ""
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"INC_LIST") SHARED AGAIN ORDER TAG L_CODE ALIAS IL_5C11
SEEK STR(m.inc_ID, 5)
LOCATE REST FOR CODE # 0 .AND. LIST_NO = STR(m.inc_ID, 5)
IF FOUND()
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"����皰→� �� �竅��, □�� ����諷 ���爛薀�."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF

DIMENSION fld_list[6]
m.fld_list = ""
SELECT 0
IF .NOT. FilStru()
  USE IN IL_5C11
  SELECT (m.s_sav)
  RETURN
ENDIF

IF .NOT. Fil_Tmp()
  USE IN IL_5C11
  IF USED("LD")
    m.FName = DBF("LD")
    USE IN LD
    DELETE FILE (m.FName)
    DELETE FILE (LEFT(m.FName, LEN(m.FName)-3)+".cdx")
  ENDIF
  SELECT (m.s_sav)
  RETURN
ENDIF

USE (m.base_path+"INC_TIT") SHARED AGAIN ALIAS INC_T_5C12 IN 0
GO m.inc_ID
m.tmpSCode = INC_T_5C12.SUP_CODE
m.tmpNDS   = INC_T_5C12.NDS_
USE IN INC_T_5C12
SELECT STOCK
IF USED("STOCK")
  m.st_rec = RECNO("STOCK")
  m.st_tag = TAG()
ELSE
  USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE IN 0
ENDIF
DO WHILE .T.
  SELECT LD
  DO Strng WITH m.tmpSCode
  SELECT LD
  LOCATE FOR EMPTY(CODE)
  IF FOUND()
    DIMENSION mss[4]
    mss[1] = ""
    mss[2] = CHR(0)+"��������! �� ≡� ��Ж罔� ��甎�Л���."
    mss[3] = CHR(0)+"  [ 蹍ぎ�Θ碎 ��甎�Л����┘ ] [ ����爬牀��碎 �ム�甎�Л���襯 ��Ж罔� ]"
    mss[4] = ""
    IF Out_Mess(7, "mss") = 1
      LOOP
    ENDIF
    DELETE ALL FOR EMPTY(CODE)
  ENDIF
  EXIT
ENDDO

SELECT STOCK
SET ORDER TO TAG CODE
SELECT LD
SET RELATION TO CODE INTO STOCK
SCAN
  SELECT IL_5C11
  APPEND BLANK
  REPLACE LIST_NO	WITH STR(m.inc_ID, 5), ;
          CODE		WITH LD.CODE,          ;
          PREFIX	WITH LD.PREFIX,        ;
          NAME		WITH LD.NAME,          ;
          PRODUCER	WITH LD.PRODUCER,      ;
          QNT_INV	WITH LD.QNT,           ;
          QNT		WITH LD.QNT,           ;
          PRICE_0	WITH LD.PRICE,         ;
          PRICE		WITH LD.PRICE*(1+m.tmpNDS/100)
  SELECT LD
ENDSCAN

IF m.st_rec = 0
  USE IN STOCK
ELSE
  SELECT STOCK
  IF BETWEEN(m.st_rec, 1, RECCOUNT("STOCK"))
    GO m.st_rec
  ENDIF
  SET ORDER TO TAG (m.st_tag)
ENDIF
USE IN IL_5C11

IF USED("LD")
  m.FName = DBF("LD")
  USE IN LD
  DELETE FILE (m.FName)
  DELETE FILE (LEFT(m.FName, LEN(m.FName)-3)+".cdx")
ENDIF

SELECT (m.s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� FilStru      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    �襦�� ����� � ������┘ 痰珮�矣琺                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.12.2005 陳�
PROCEDURE FilStru

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*楽� [ ] ��� ����� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖....:....!....:....!....:....!....:....!....:....!....:....!...崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*楽� ｯ����┘ ���ォ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖[ ] ��� ��Ж罔�  ....:....!.... [ ] ��爼�        ....:....!....崖
*崖[ ] 踳筥��      ....:....!.... [ ] ����腑痰〓   ....:....!....崖
*崖[ ] ��━キ����┘ ....:....!.... [ ] �キ�         ....:....!....崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�                 < OK Ctrl-W > < �皖���碎瘴 Esc >                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, sw_fil, sw_code, sw_prf, sw_name, sw_prd, sw_qnt, sw_prc, let_fl
PRIVATE tmpFld
DIMENSION tmpFld[1]
tmpFld[1] = ""
m.let_fl = .F.
m.FName = ""
m.ex = 1
STORE .F. TO sw_fil, m.sw_code, m.sw_prf, m.sw_name, m.sw_prd, m.sw_qnt, m.sw_prc
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 69, "ｯ�荐皀 √�き�� ����"
DO Sun_Bord WITH  2,  2,  4, 66, " [ ] ��� ����� "
DO Sun_Bord WITH  5,  2,  9, 66, " ｯ����┘ ���ォ "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  2,  5 GET m.sw_fil PICTURE "@*C ��� ����� " VALID G_File()
@  3,  3 EDIT m.FName SIZE 1, 63 WHEN .F.
@  6,  3 GET sw_code PICTURE "@*C ��� ��Ж罔�" VALID G_Field(1, 6, 20, "N") WHEN m.let_fl
@  7,  3 GET sw_prf  PICTURE "@*C 踳筥��" VALID G_Field(2, 7, 20, "C") WHEN m.let_fl
@  8,  3 GET sw_name PICTURE "@*C ��━キ����┘" VALID G_Field(3, 8, 20, "C") WHEN m.let_fl
@  6, 35 GET sw_prd  PICTURE "@*C ��爼�" VALID G_Field(4, 6, 52, "C") WHEN m.let_fl
@  7, 35 GET sw_qnt  PICTURE "@*C ����腑痰〓" VALID G_Field(5, 7, 52, "N") WHEN m.let_fl
@  8, 35 GET sw_prc  PICTURE "@*C �キ�" VALID G_Field(6, 8, 52, "N") WHEN m.let_fl
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Fld()

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  POP KEY
  RELEASE WINDOW (win_name)
  RETURN .T.
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN .F.


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_File       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �襦�� �����                                 �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.12.2005 陳�
PROCEDURE G_File
PRIVATE w_sav, tmp_v, tmp_p, new, i, err_sav, err_no, s_sav, mss, ret_v

ret_v = .T.

m.w_sav = WOUTPUT()

m.tmp_p = RAT("\", m.FName)
IF m.tmp_p = 0
  m.tmp_p = ""
ELSE
  m.tmp_p = LEFT(m.FName, m.tmp_p-1)
ENDIF

m.tmp_v = Int_File(m.tmp_P, "dbf", "���Θ皀 ���� ��痰�∫┴�")

m.new = m.FName # m.tmp_v
IF .NOT. EMPTY(m.tmp_v)
  IF m.FName # m.tmp_v
    m.new = .T.
    m.FName = m.tmp_v
  ENDIF
ENDIF

ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 2
ACTIVATE WINDOW (m.w_sav)

IF .NOT. EMPTY(m.FName) .OR. m.new
  DIMENSION tmpFld[1, 2]
  tmpFld[1, 1] = ""
  tmpFld[1, 2] = "CN"
  m.s_sav = SELECT()
  SELECT 0
  m.err_sav = ON("ERROR")
  m.err_no = 0
  ON ERROR m.err_no = ERROR()
  USE (m.FName) ALIAS TMP_00000
  ON ERROR &err_sav
  IF m.err_no # 0
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"�� 磴�モ瘴 �皖琺碎 ��゛�聶. �荐！� "+ALLTRIM(STR(m.err_no))+"."
    mss[3] = ""
    DO Out_mess WITH 7, "mss"
    m.FName = ""
    m.ret_v = .F.
  ELSE
    m.i = 2
    DO WHILE .T.
      IF .NOT. EMPTY(FIELD(m.i-1))
        DIMENSION tmpFld[m.i, 2]
        tmpFld[m.i, 1] = FIELD(m.i-1)
        tmpFld[m.i, 2] = TYPE(FIELD(m.i-1))
      ELSE
        EXIT
      ENDIF
      m.i = m.i+1
    ENDDO
  ENDIF
  USE
  SELECT (m.s_sav)
ENDIF

IF EMPTY(m.FName) .OR. m.new
  m.fld_list = ""
  m.let_fl = .F.
  @ 6, 20 SAY SPACE(14)
  @ 7, 20 SAY SPACE(14)
  @ 8, 20 SAY SPACE(14)
  @ 6, 52 SAY SPACE(14)
  @ 7, 52 SAY SPACE(14)
  @ 8, 52 SAY SPACE(14)
ENDIF

SHOW GET m.FName
m.sw_fil = .F.
SHOW GET m.sw_fil
m.let_fl = .NOT. EMPTY(m.FName)

RETURN m.ret_v

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_Field      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                               �襦�� ����                               �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.12.2005 陳�
PROCEDURE G_Field
PARAMETERS tmpIndex, tmpLine, tmpCol, tmpType

PRIVATE arr_c, arr_n, i, n_c, n_n
STORE 0 TO m.n_c, m.n_n
FOR m.i = 1 TO ALEN(tmpFld, 1)
  IF m.i = 1 .OR. tmpFld[m.i, 2] $ "CN"
    m.n_n = m.n_n+1
    DIMENSION arr_n[m.n_n]
    arr_n[m.n_n] = tmpFld[m.i, 1]
  ENDIF
  IF m.i = 1 .OR. tmpFld[m.i, 2] $ "C"
    m.n_c = m.n_c+1
    DIMENSION arr_c[m.n_c]
    arr_c[m.n_c] = tmpFld[m.i, 1]
  ENDIF
ENDFOR

fld_list[m.tmpIndex] = Ch_Fld(IIF(m.tmpType = "N", "arr_n", "arr_c"), fld_list[m.tmpIndex])
@ m.tmpLine, m.tmpCol SAY PADR(fld_list[m.tmpIndex], 14)
STORE .F. TO sw_fil, m.sw_code, m.sw_prf, m.sw_name, m.sw_prd, m.sw_qnt, m.sw_prc
SHOW GETS
*SHOW GET m.sw_code

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Fld       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                               �襦�� ����                               �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.12.2005 陳�
PROCEDURE Ch_Fld
PARAMETERS arr_f, s_val

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                �
*�< OK Ctrl-W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, tmpVal
m.ex = 1
m.tmpVal = m.s_Val
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH MIN(20, ALEN(&arr_f)+6), 36, "���Θ皀 ����"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 2 GET m.tmpVal FROM &arr_f SIZE WROWS()-4, WCOLS()-4

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF m.ex # 1
  m.tmpVal = m.s_Val
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.tmpVal

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Fld      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                蹍▲爲� ��玻オ皚�痰� ������キ�� ゛����                 �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.12.2005 陳�
PROCEDURE Tst_Fld

PRIVATE mss

IF m.ex # 1
  RETURN .T.
ENDIF

IF EMPTY(fld_list[3])
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"���� ��━キ������ ぎ�Ν� °碎 礫�����..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(fld_list[5])
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"���� ����腑痰�� ぎ�Ν� °碎 礫�����..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fil_Tmp      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                 ��Г��┘ � ������キ┘ ∇ガキ���� �����                 �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.12.2005 陳�
PROCEDURE Fil_Tmp

PRIVATE tmpName, tmpFld, mss

m.tmpName = SYS(3)
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	CODE		N( 7), ;
	    PREFIX		C( 4), ;
	    NAME		C(35), ;
	    PRODUCER	C( 8), ;
	    QNT			N(10), ;
	    PRICE		n(10,2) )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS LD

SELECT 0
USE (m.FName) SHARED AGAIN ALIAS TMP_SOURCE
SCAN
  SELECT LD
  APPEND BLANK

  IF .NOT. EMPTY(fld_list[1])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[1])
    m.tmpFld = &tmpFld
    IF TYPE("m.tmpFld") = "C"
      m.tmpFld = VAL(m.tmpFld)
    ENDIF
    REPLACE CODE WITH m.tmpFld
  ENDIF

  IF .NOT. EMPTY(fld_list[2])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[2])
    m.tmpFld = &tmpFld
    m.tmpFld = PADL(ALLTRIM(m.tmpFld), 4)
    REPLACE PREFIX WITH m.tmpFld
  ENDIF

  IF .NOT. EMPTY(fld_list[3])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[3])
    m.tmpFld = &tmpFld
    REPLACE NAME WITH m.tmpFld
  ENDIF

  IF .NOT. EMPTY(fld_list[4])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[4])
    m.tmpFld = &tmpFld
    REPLACE PRODUCER WITH m.tmpFld
  ENDIF

  IF .NOT. EMPTY(fld_list[5])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[5])
    m.tmpFld = &tmpFld
    IF TYPE("m.tmpFld") = "C"
      m.tmpFld = VAL(m.tmpFld)
    ENDIF
    REPLACE QNT WITH m.tmpFld
  ENDIF

  IF .NOT. EMPTY(fld_list[6])
    m.tmpFld = "TMP_SOURCE."+ALLTRIM(fld_list[6])
    m.tmpFld = &tmpFld
    IF TYPE("m.tmpFld") = "C"
      m.tmpFld = VAL(m.tmpFld)
    ENDIF
    REPLACE PRICE WITH m.tmpFld
  ENDIF

  SELECT TMP_SOURCE

ENDSCAN

USE IN TMP_SOURCE

USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS ST_5C12 IN 0
SELECT LD
SCAN FOR .NOT. EMPTY(CODE)
  IF QNT =< 0
    DELETE
  ELSE
    IF .NOT. SEEK(CODE, "ST_5C12")
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"���甌� 甌ぅ爨�� ��Ж罔� � �オ�玻オ皚覓� ������."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE IN ST_5C12
      RETURN .F.
    ENDIF
  ENDIF
ENDSCAN

SELECT ST_5C12
SET ORDER TO TAG NAME_P
SELECT LD
SCAN FOR EMPTY(CODE)
  IF EMPTY(NAME) .OR. QNT <= 0
    DELETE
  ELSE
    IF SEEK(NAME+PREFIX+PRODUCER, "ST_5C12")
      REPLACE CODE WITH ST_5C12.CODE
    ENDIF
  ENDIF
ENDSCAN

USE IN ST_5C12

RETURN .T.
