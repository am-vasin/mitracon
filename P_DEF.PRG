*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�   蹍罐ゃ�� P_Def        �о�｀砒┴ ���┃� ���矗┘���                 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                   蹍������ �ョ�皋 甎�瓷� "�ヤ�罔�".                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 09.07.2002 夕
PROCEDURE P_Def
PARAMETER doc_n

PRIVATE s_sav   && ���ム ��｀腑� �゛�痰� か� 〓Б����
PRIVATE d_n     && ���ム ぎ�祠キ�� � 皀�痰�〓� ※ぅ
PRIVATE d_d     && ���� ぎ�祠キ�� � 皀�痰�〓� ※ぅ
PRIVATE l_name  && ��━キ����┘ 甎�瓷�
PRIVATE sb      && ��瘁│ ������┘ �瓮△����ォ
PRIVATE t_d     && ��瘁│ 甌 痰牀���� ��゛��� "������〓� ぎ�祠モ�"
PRIVATE t_p     && ��瘁│ 甌 痰牀���� ��゛��� "������〓� 痰����肓"
PRIVATE f_d     && ��瘁│ 甌 痰牀���� ��゛��� "��あ�� ぎ�祠モ�"
PRIVATE f_p     && ��瘁│ 甌 痰牀���� ��゛��� "��あ�� 痰����肓"
PRIVATE n, j    && �腑砒┴�, ┃ぅ�瘠, �����モ琺 罔���
PRIVATE p_ln    && ��┃� 痰����肓 � 痰牀���
PRIVATE fnt     && �爬籵: " " - �°膈覃, "1" - 瓲�硅� (condenced)
PRIVATE n_cp    && ��甄� ���┤
PRIVATE lft     && ���� 甄ア� � 瓱�〓���
PRIVATE ffeed   && ��▲琥キ┘ ぎ�祠キ��: �� �矗�罐皀�讚�� - �牀��� 痰牀�,
                &&      �矗���皀�讚�� - �牀��� 筮爼���
PRIVATE p_drcty && �����←キ┘ �ョ�皋 (�爬�皀�)

s_sav = SELECT()

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_2708 AGAIN
IF .NOT. SEEK(doc_n)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ 甎�瓷� � ���ム�� "+ALLTRIM(STR(doc_n))
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ELSE
  IF "ぅ筥罔�" $ LST_NAME
    d_n   = ALLTRIM(STR(doc_n))
    l_name  = LST_NAME
    d_d     = CR_DAT
    DO Prep_Def
  ELSE
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"���甌� � "+ALLTRIM(STR(doc_n))+' �� 甌ぅ爨�� "ぅ筥罔�!"'
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
ENDIF

*
*   ��ぃ�皰→� ∇ガキ���� �����.
*
DO Wt_Mess WITH "��ぃ�皰→� 甎�瓷�"
DO Wt_Mess

*
*  ｯ�瘠��ガ �瓮△�����
DIMENSION sb(7,2)
sb(1,1) = "{�矗.   }"
sb(1,2) = ""

sb(2,1) = "{���甌�}"
sb(2,2) = d_n

sb(3,1) = "{����      }"
sb(3,2) = DTOC(d_d)

sb(4,1) = "{��� 甎�瓷�                      }"
sb(4,2) = l_name

sb(5,1) = "{�皰�� �� 痰�}"
sb(5,2) = ""

sb(6,1) = "{�皰�� �� ぎ�}"
sb(6,2) = ""

sb(7,1) = "{���鈞�   }"
sb(7,2) = IIF(LT_2708.IS_USD,"  �.�.   ","  珮�.   ")

*   ���� ぅ���讚�� 痰牀��
*
DIMENSION sb_det(12,2)
*
sb_det(01,1) = "{踳�}"						&& 踳筥��
sb_det(02,1) = "{��━キ����┘           }"	&& ��━キ����┘
sb_det(03,1) = "{��爼� }"					&& ��爼� �牀├〓え皀��
sb_det(04,1) = "{�-〓  }"		    		&& ���-〓 (ぅ筥罔�)
sb_det(05,1) = "{�キ�      }"
sb_det(06,1) = "{�祠��     }"
sb_det(07,1) = "{ProductMan   }"
sb_det(08,1) = "{X}"                        && �� ���祚�碎: � - �┴����, � - 瓮����, _ - �モ ���爛��
sb_det(09,1) = "{Obs}"
sb_det(10,1) = "{���� ��痰.}"
sb_det(11,1) = "{�┼���}"
sb_det(12,1) = "{�ェム�}"

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "LST_DEF" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN   && ��┃� 痰����肓 � 痰牀���
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT  && �爬籵: " " - �°膈覃, "1" - 瓲�硅� (condenced)
n_cp = DOC_FORM.N_COPIES   && ��甄� ���┤
lft  = DOC_FORM.LEFT_FIELD && ���� 甄ア� � 瓱�〓���
ffeed =DOC_FORM.F_FEED     && ��▲琥キ┘ ぎ�祠キ��: 
                           &&      �� �矗�罐皀�讚�� - �牀��� 痰牀�,
                           &&      �矗���皀�讚�� - �牀��� 筮爼���
p_drctry = DOC_FORM.P_DIR  && �����←キ┘ �ョ�皋 (�爬�皀�)

n = MEMLINES(PAGE_H)  &&     ������〓� 痰����肓 (�� �ム〓�)
IF n > 0
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ELSE
  DIMENSION t_p(1)
  t_p(1) = ""
ENDIF

n = MEMLINES(DOC_H)  &&     ������〓� �ム〓� 痰����肓
IF n > 0
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ELSE
  DIMENSION t_d(ALEN(t_p))
  FOR j = 1 TO ALEN(t_p)
    t_d(j) = t_p(j)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)  &&     ��あ�� 痰����肓 (�� ��甄イ�ォ!)
IF n > 0
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ELSE
  DIMENSION f_p(1)
  f_p(1) = ""
ENDIF

n = MEMLINES(DOC_F)  &&     ��あ�� ��甄イ�ォ 痰����肓
IF n > 0
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ELSE
  DIMENSION f_d(ALEN(f_p))
  FOR j = 1 TO ALEN(f_p)
    f_d(j) = f_p(j)
  ENDFOR
ENDIF

*		��゛�� ぅ���讚�� 痰牀��
*
n = MEMLINES(DETAIL)
IF n = 0
  DIMENSION det(1)
  det(1) = ""
ELSE
  DIMENSION det(n)
  FOR i = 1 TO n
    det(i) = MLINE(DETAIL,i)
  ENDFOR
ENDIF

USE

DO Ini_Prn WITH "�皖�щ", p_ln, lft, n_cp, fnt, ffeed, p_drctry

PRIVATE sum_p, sum_d, nn
sum_d = 0

SELECT TMP_DEF
GO TOP

nn = RECCOUNT()		&& �瓮�� ぅ���讚諷 �����
n_p_det  = p_ln-ALEN(t_d)-MAX(ALEN(f_p),ALEN(f_d))
							&& ���-〓 痰牀� ��� ぅ���讚竡 ��痰� �� �ム〓� 痰����罐
n_p_det1 = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
							&& ���-〓 痰牀� ��� ぅ���讚竡 ��痰� �� ���ム〓� 痰����罐
n_p_det  = FLOOR(n_p_det/ALEN(det))		&& ���-〓 ぅ���讚諷 ����� �� �ム〓� 痰����罐
n_p_det1 = FLOOR(n_p_det1/ALEN(det))	&& ���-〓 ぅ���讚諷 ����� �� ���ム〓� 痰����罐
n_pg = 1
nn = nn-n_p_det		&& ����讓� ぅ���讚諷 �珮�� �� ≡ュ 痰������� ．� �ム〓�

IF n_p_det <= 0		&& ��；�� ≡� 痰����聶 ��� ������→�... �� ����� 痰�����!
  n_p_det = RECCOUNT()
ELSE
  n_pg = 1+CEILING(MAX(nn, 0)/n_p_det1)
ENDIF

FOR f_pg = 1 TO n_pg		&& �� 痰�������...
  sb(1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))	&& �矗�����/�矗����
  sb(1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1   && ������〓� �� �ム〓� 痰����肓
    EJECT
    n_p_det = n_p_det1		&& �ムメ��←錺� か┃� 痰����肓...
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
  ELSE        && ������〓� �ム〓� 痰����肓
    FOR i = 1 TO ALEN(t_d)
      str_w = t_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF
  *
  *   �モ��讚�� ��痰� 痰����肓
  * 
  sum_p = 0

  i = 0		&& �腑砒┴ ぅ���讚諷 �珮��
  DO WHILE .T.
    sb_det(01,2) = TMP_DEF.PREFIX
    sb_det(02,2) = TMP_DEF.NAME
    sb_det(03,2) = TMP_DEF.PRODUCER
    sb_det(04,2) = STR(TMP_DEF.QNT,6)
    sb_det(05,2) = STR(TMP_DEF.PRICE*TMP_DEF.QNT,10,3)
    sb_det(06,2) = STR(TMP_DEF.PRICE*TMP_DEF.QNT,10,3)
    sb_det(07,2) = TMP_DEF.P_MEN
    sb_det(08,2) = TMP_DEF.X
    sb_det(09,2) = IIF(TMP_DEF.OBSOLETE, "O"," ")
    sb_det(10,2) = DTOC(TMP_DEF.W_DATE)
    sb_det(11,2) = STR(TMP_DEF.QNT_FIL,6)
    sb_det(12,2) = STR(TMP_DEF.QNT_REZ,6)

    FOR k = 1 TO ALEN(det)		&& ��め����→� ���ォ � �ョ�碎 ぅ���讚�� �珮���
      str_w = det(k)
      FOR j = 1 TO ALEN(sb_det,1)
        str_w = STRTRAN(str_w, sb_det(j,1), sb_det(j,2) )
      ENDFOR
      @ PROW(), 0 SAY str_w
    ENDFOR
    sum_p = sum_p + VAL(sb_det(6,2))
    sum_d = sum_d + VAL(sb_det(6,2))
    i = i+1
    SKIP
    IF EOF("TMP_DEF")
      EXIT
    ENDIF
    IF  i >= n_p_det
      EXIT
    ENDIF
  ENDDO

  *
  *    ｪ�����┘ 痰����肓
  *
  sb(5,2) = STR(sum_p,10,3)
  sb(6,2) = STR(sum_d,10,3)
  IF f_pg = n_pg		&& ��甄イ�闖 痰������?
    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR

USE
SELECT LT_2708
USE

DO Term_Prn WITH "", (tmpo_path+"TMP_DEF")

DELETE FILE(tmpo_path+"TMP_DEF.DBF")

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Def                                                   �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    ��ぃ�皰→� ∇ガキ���� �����.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 17.06.2002 陳�
PROCEDURE Prep_Def
PRIVATE dat_wait

SELECT 0
DELETE FILE(tmpo_path+"TMP_DEF.DBF")
CREATE DBF(tmpo_path+"TMP_DEF");
         ( PREFIX   C( 4),  ;
           NAME     C(35),  ;
           PRODUCER C( 8),  ;
           QNT      N( 6),  ;
           PRICE    N(10,3),;
           SUMM     N(10,3),;
           P_MEN    C(13),  ;
           X        C( 1),  ;
           OBSOLETE L,      ;     
           W_DATE   D,      ;
           QNT_FIL  N( 6),  ;
           QNT_REZ  N( 6) )
INDEX ON NAME TAG NAME

SELECT 0
IF File_O(path_comm+"PURCHASE.DBF")
  USE (path_comm+"PURCHASE") ALIAS P_2708 ORDER TAG NAME_DATE AGAIN
  SET FILTER TO PURFLAG = "K"
ENDIF

SELECT 0
USE (path_comm+"SUBJECT") ORDER TAG STO_CODE ALIAS SJ_2708 AGAIN

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_2708 AGAIN
SET RELATION TO CODE INTO SJ_2708

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS LD_2708 AGAIN
SET RELATION TO CODE INTO S_2708
IF SEEK(STR(doc_n,6))
  SCAN REST WHILE LIST_NUM = STR(doc_n,6) FOR .NOT.EMPTY(QNT)
    IF SEEK(CODE, "S_2708")
      dat_wait = {}
      IF USED("P_2708")
        SELECT P_2708
        IF SEEK(S_2708.NAME+S_2708.PREFIX+S_2708.PRODUCER, "P_2708")
          dat_wait = P_2708.DATE
        ENDIF
      ENDIF
      SELECT TMP_DEF
      APPEND BLANK
      REPLACE PREFIX   WITH S_2708.PREFIX,   ;
              NAME     WITH S_2708.NAME,     ;
              PRODUCER WITH S_2708.PRODUCER, ;
              QNT      WITH LD_2708.QNT,     ;
              PRICE    WITH IIF(LT_2708.IS_USD,LD_2708.USD_PRICE,LD_2708.SAL_PRICE),;
              SUMM     WITH IIF(LT_2708.IS_USD,LD_2708.USD_PRICE*LD_2708.QNT,LD_2708.SAL_PRICE*LD_2708.QNT),;
              X        WITH SJ_2708.NOPUR,   ;
              OBSOLETE WITH SJ_2708.OBSOLETE,;     
              P_MEN    WITH SJ_2708.AUTHOR,  ;
              W_DATE   WITH dat_wait,        ;
              QNT_FIL  WITH S_2708.QNT_REAL, ;
              QNT_REZ  WITH S_2708.QNT_REZ
      SELECT LD_2708
    ENDIF
  ENDSCAN
ENDIF

IF USED("P_2708")
  SELECT P_2708
  USE
ENDIF

SELECT SJ_2708
USE
SELECT S_2708
USE
SELECT LD_2708
USE

SELECT TMP_DEF
RETURN