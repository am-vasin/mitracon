*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� P_Bill       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             �ョ�碎 瘍モ�.                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 01/19/1999 様�
PROCEDURE P_Bill
PARAMETERS firm_c, doc_n, doc_d  &&, flgs
*
*  firm_c - ��� �爛く爬閧��;
*  doc_n  - ���ム ぎ�祠キ��;
*  doc_d  - �─� ���� ぎ�祠キ��, �─� ��� � 瓱�〓�讚�� ┼� 腮甄�〓� 筮爼�皀;
*  flg    - 筰�Κ�.
*
*IF .NOT. List()
*  RETURN
*ENDIF

PRIVATE s_sav   && ���ム ��｀腑� �゛�痰� か� 〓Б����
PRIVATE d_year  && ��� ぎ�祠キ��
PRIVATE d_n     && ���ム ぎ�祠キ�� � 皀�痰�〓� ※ぅ
PRIVATE tmpList && ���ム 甎�瓷�
PRIVATE d_d     && ���� ぎ�祠キ�� � 皀�痰�〓� ※ぅ
PRIVATE a_code  && Ko� ��｀皚┴�, ��爬絎ぎ��∵ィ� ぅ�譯� 
PRIVATE c_name  && ��� ��┘��� (メ�� ����〓� ━ゥ� �メ皰)
PRIVATE c_code  && ��� ��┘��� (メ�� ����〓� ━ゥ� �メ皰)
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
PRIVATE k00     && ��鈑 か� ���瓷� ぎ�祠キ��
PRIVATE is_r    && �┓ 瘍モ� - 珮゛ア覃
PRIVATE tmpType && �┓ ぎ�祠キ��
PRIVATE tmpDCode && ��� ぎ�祠キ��

PRIVATE wDoc, wFirm, wDate

s_sav = SELECT()

*
* ��爼�珮ガ ��� ぎ�祠キ��
IF TYPE("doc_d") = "D"
  d_year = LEFT(DTOS(doc_d),4)
ENDIF
IF TYPE("doc_d") = "N"
  d_year = STR(doc_d,4)
ENDIF
IF TYPE("doc_d") = "C"
  d_year = doc_d
ENDIF

*
* �━〓�讚覃 ���ム ぎ�祠キ��
d_n = LEFT(doc_n,4)+"-"+ALLTRIM(RIGHT(doc_n,6))

*
*  ������→� ぎ�祠キ皰�...
SELECT 0
USE (base_path+"BILLS") ORDER TAG DOC_NUM ALIAS TIT_PRINT AGAIN
k00 = firm_c+d_year+doc_n
*
*  �薀�...
IF .NOT. SEEK(k00)  && �� ��茫�...
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ ぎ�祠キ�� � ���ム�� "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
tmpList = TRANSFORM(NUM_LIST, "@Z 999999")
tmpType = 0
IF FSIZE("DOC_TYPE") # 0
  tmpType = IIF(.NOT. EMPTY(DOC_TYPE), 1, 0)
ENDIF
wFirm = FIRM
wDoc  = DOC_NUM
wDate = DOC_DATE
is_r = VALUTA = "�"
a_code = WHO

*
*  �薀� ��┘���...
IF .NOT. EMPTY(CUS_CODE)
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_PRINT AGAIN
  SEEK TIT_PRINT.CUS_CODE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_CODE
  USE
ELSE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_NAME
ENDIF

SELECT TIT_PRINT
d_d = DTOC(DOC_DATE)

*
*   ��ぃ�皰→� ∇ガキ���� �����.
*
*  ��������! 踸 �諷�ぅ ├ ����� ≡� �皖琺硅� � �牀������ �ョ�皋 �����
*            �牀�� ∇ガキ���� � ������→� ぎ�Ν� °碎 ���琺硅!
*            �爛�キ�覃 ���� �� ��Ε� ━モ� ┃ぅ�甌�!
*

DO Wt_Mess WITH "��ぃ�皰→� 甎�瓷�"
DO Prep_Tmp
DO Wt_Mess
SELECT TIT_PRINT

*
*  ｯ�瘠��ガ �瓮△�����
DIMENSION sb(37,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Doc_Num  }"
sb( 2,2) = d_n

sb( 3,1) = "{Doc_Date}"
sb( 3,2) = d_d

sb(36,1) = "{Lst }"
sb(36,2) = TRANSFORM(TIT_PRINT.NUM_LIST, "@Z 999999")

sb( 4,1) = "{Firm             }"
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS TMP_PRINT AGAIN
SEEK TIT_PRINT.FIRM
sb(28, 1) = "{�礫�〓え皀��          }"
sb(28, 2) = ALLTRIM(BOSS)
sb(29, 1) = "{��. ＜紕��皀�         }"
sb(29, 2) = ALLTRIM(ABAK)
sb(30, 1) = "{��瘁��                }"
sb(30, 2) = ALLTRIM(KASSIR)

sb(27,1) = "{%NSP}"
sb(27,2) = P_NALOG

sb( 4,2) = ALLTRIM(L_NAME)

sb( 5,1) = "{INN     }"
sb( 5,2) = INN

sb( 6,1) = "{Bank             }"
USE (base_path+"BANKS") ORDER TAG BANK ALIAS TMP_PRINT AGAIN
SEEK TIT_PRINT.FIRM+TIT_PRINT.BANK
sb( 6,2) = ALLTRIM(L_NAME)

sb( 7,1) = "{Acc_Bill}"
sb( 7,2) = ALLTRIM(ACC_NO)

sb( 8,1) = "{Korr_Bill}"
sb( 8,2) = ALLTRIM(CORR_NO)

sb( 9,1) = "{BIK    }"
sb( 9,2) = ALLTRIM(BIK)

sb(10,1) = "{OKONH}"
sb(10,2) = ""

sb(11,1) = "{OKPO }"
sb(11,2) = ""
USE

sb(12,1) = "{Customer         }"
sb(12,2) = c_name

sb(13,1) = "{Sum_0        }"
sb(13,2) = ""

sb(14,1) = "{Sum_H        }"
sb(14,2) = ""

sb(15,1) = "{Sum_0_Pag    }"
sb(15,2) = ""

sb(16,1) = "{Sum_H_Pag    }"
sb(16,2) = ""

sb(17,1) = "{Sum_NDS      }"
sb(17,2) = ALLTRIM(TRANSFORM(TIT_PRINT.BILL_SUM-TIT_PRINT.SUM_0,"999 999 999 999.99"))

sb(18,1) = "{Prc_NDS}"
sb(18,2) = ALLTRIM(STR(TIT_PRINT.NDS_,5,1))

sb(19,1) = "{Sum_Note1                                                         }"

sb(20,1) = "{Sum_Note2                                                         }"

sb(21,1) = "{Sum_Note3                                                         }"

PRIVATE s_note
DIMENSION s_note(3)
s_note(1) = LEN(sb(19,1))
s_note(2) = LEN(sb(21,1))
s_note(3) = LEN(sb(21,1))
DO Nt_Lines WITH TIT_PRINT.BILL_SUM, s_note, TIT_PRINT.VALUTA
sb(19,2) = s_note(1)
sb(20,2) = s_note(2)
sb(21,2) = s_note(3)

sb(23,1) = "{Sum_H_NSP}"
sb(23,2) = ROUND(TIT_PRINT.BILL_SUM*(100+sb(27,2))/100,2)

sb(24,1) = "{Sum_Note1+NSP                                                     }"

sb(25,1) = "{Sum_Note2+NSP                                                     }"

sb(26,1) = "{Sum_Note3+NSP                                                     }"

DIMENSION s_note(3)
s_note(1) = LEN(sb(19,1))
s_note(2) = LEN(sb(21,1))
s_note(3) = LEN(sb(21,1))
DO Nt_Lines WITH sb(23,2), s_note, TIT_PRINT.VALUTA
sb(24,2) = s_note(1)
sb(25,2) = s_note(2)
sb(26,2) = s_note(3)
sb(27,2) = STR(sb(27,2), 6, 1)
sb(23,2) = STR(sb(23,2), 11, 2)

sb(22,1) = "{Autor                       }"
sb(33,2) = SPACE(16)
sb(34,2) = SPACE(6)
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
sb(35,1) = "{E-mail �キイΕ��}"
sb(33,2) = ""
sb(34,2) = ""
sb(35,2) = ""
IF a_code = 0
  sb(22,2) = "���!"
ELSE
  IF SEEK (a_code)
    sb(22,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
    sb(33,2) = W_PHONE1
    sb(34,2) = W_PHONE2
  ELSE
    sb(22,2) = "?!!"
  ENDIF
  sb(35,2) = ALLTRIM(EML)
ENDIF         
sb(22,2) = PADR(sb(22,2),LEN(sb(22,1)))

sb(31,1) = "{��� ��┘��� }"
sb(37,1) = "{��� ��┘���   }"
sb(32,1) = "{�むメ ��┘���                                               }"
sb(33,1) = "{Phone         }"
sb(34,1) = "{Phn }"

USE (base_path+"CUS_BIL") ORDER TAG CUS_CODE ALIAS CB9903 SHARED AGAIN
IF SEEK(c_code, "CB9903")
  IF .NOT. EMPTY(CUS_NAME)
    sb(12,2) = CUS_NAME
  ENDIF
  sb(31,2) = INN
  sb(32,2) = ADDRESS
  sb(37,2) = KPP
ELSE
  sb(31,2) = ""
  sb(32,2) = ""
  sb(37,2) = ""
ENDIF
USE

* �с��ガ ������┘ ぎ�祠キ��
SELECT TIT_PRINT
USE (base_path+"DOC_FORM")
IF tmpType = 0
  tmpDCode = IIF(is_r, "BILL", "BILL$")
ELSE
  tmpDCode = IIF(is_r, "LBILL", "LBILL$")
ENDIF

  LOCATE FOR tmpDCode == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN-1 && ��┃� 痰����肓 � 痰牀���
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT  && �爬籵: " " - �°膈覃, "1" - 瓲�硅� (condenced)
n_cp = DOC_FORM.N_COPIES   && ��甄� ���┤
lft  = DOC_FORM.LEFT_FIELD && ���� 甄ア� � 瓱�〓���
ffeed =DOC_FORM.F_FEED     && ��▲琥キ┘ ぎ�祠キ��: 
                           &&      �� �矗�罐皀�讚�� - �牀��� 痰牀�,
                           &&      �矗���皀�讚�� - �牀��� 筮爼���
p_drctry = DOC_FORM.P_DIR  && �����←キ┘ �ョ�皋 (�爬�皀�)

*
*  ��爼�珮ガ ��゛��� か� ������→�� � ��あ����
*
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

USE

SELECT TMP_LST

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

PRIVATE n_p_det  && ��甄� ぅ���讚諷 痰牀� �� 痰����罐
PRIVATE n_pg     && ��甄� 痰����� � ぎ�祠キ皀
PRIVATE f_pg     && �オ竕�� 痰������ ぎ�祠キ�� �爬 �ョ�皋
PRIVATE str_w    && ｡��� �ョ���ガ�� 痰牀��
PRIVATE sum_p0, sum_p1 && ...�祠�� �� 痰�������
PRIVATE sum_d0, sum_d1 && ...�祠�� �� ぎ�祠キ矣
PRIVATE nn       && ���ム 痰牀�� �� ��瑙お�...
PRIVATE tmpLen

n_p_det = p_ln-MAX(ALEN(f_p),ALEN(f_d))-MAX(ALEN(t_p),ALEN(t_d))
n_p_det = FLOOR(n_p_det/2)
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
*  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
  DO CASE
  CASE RECCOUNT() < FLOOR((p_ln-ALEN(f_d)-ALEN(t_d))/2)	&& �瓮�� �き� 痰������
    n_pg = 1
  CASE RECCOUNT() < FLOOR((p_ln-ALEN(f_d)-ALEN(t_d))/2)+ ;
                    FLOOR((p_ln-ALEN(f_p)-ALEN(t_p))/2)		&& �瓮�� あ� 痰����肓
    n_pg = 2
  OTHERWISE
    m.tmpLen = RECCOUNT()-FLOOR((p_ln-ALEN(t_d)-ALEN(f_p))/2)
    n_pg = 1+FLOOR(m.tmpLen/FLOOR((p_ln-ALEN(t_p)-ALEN(f_p))/2))
    m.tmpLen = m.tmpLen % FLOOR((p_ln-ALEN(t_p)-ALEN(f_p))/2)
    IF m.tmpLen = 0
      n_pg = n_pg-1
      m.tmpLen = FLOOR((p_ln-ALEN(t_p)-ALEN(f_p))/2)
    ENDIF
    IF m.tmpLen > FLOOR((p_ln-ALEN(t_p)-ALEN(f_d))/2)
      n_pg = n_pg+2
    ELSE
      n_pg = n_pg+1
    ENDIF
  ENDCASE
ENDIF
*
*  ������〓� ぎ�祠キ��
sb( 1,2) = "1/"+ALLTRIM(STR(n_pg))
sb( 1,2) = PADL(sb(1,2), LEN(sb(1,1)))
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

STORE 0 TO sum_d0, sum_d1, nn
FOR f_pg = 1 TO n_pg    && �� 痰�������...
  * ��┃� �腑爛き�� 痰����肓
  DO CASE
  CASE n_pg = 1
    n_p_det = RECCOUNT()
  CASE n_pg = 2
    IF f_pg = 1
      n_p_det = FLOOR((p_ln-ALEN(f_p)-ALEN(t_d))/2)
      IF n_p_det = RECCOUNT()
        n_p_det = n_p_det-1
      ENDIF
    ELSE
      n_p_det = RECCOUNT()-nn
    ENDIF
  OTHERWISE
    DO CASE
    CASE f_pg = 1
      n_p_det = FLOOR((p_ln-ALEN(f_p)-ALEN(t_d))/2)
    CASE f_pg = n_pg
      n_p_det = RECCOUNT()-nn
    CASE f_pg = n_pg-1
      n_p_det = MIN(FLOOR((p_ln-ALEN(f_p)-ALEN(t_p))/2), RECCOUNT()-nn-1)
    OTHERWISE
      n_p_det = FLOOR((p_ln-ALEN(f_p)-ALEN(t_p))/2)
    ENDCASE
  ENDCASE
  IF f_pg # 1  && ������〓� 痰����肓 (�� �ム〓�!)
    *  �甄� ━ゥ� �メ皰 �祠ム�罔� 痰�����...
    sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
    sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
    EJECT
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
  ENDIF
  STORE 0 TO sum_p0, sum_p1
  IF RECCOUNT() # 0
    *
    *  �モ��讚襯 痰牀�� �腑爛き�� 痰����肓
    FOR i = 1 TO n_p_det
      nn = nn+1
      @ PROW()+1,0 SAY NAME+TRANSFORM(QNT,"@Z 99999")+" "+  ;
                            TRANSFORM(PRC, "@Z 999999999.99")+ ;
                            TRANSFORM(SUM_0, "@Z 9999999999.99")+ ;
               IIF(is_r,TRANSFORM(SUM_T, "@Z 9999999999.99"),"")
      @ PROW()+1,0 SAY CLASS
      sum_p0 = sum_p0+SUM_0
      sum_p1 = sum_p1+SUM_T
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sb(15,2) = TRANSFORM(sum_p0, "9999999999.99")
  sb(15,2) = PADL(sb(15,2), LEN(sb(15,1)))
  sb(16,2) = TRANSFORM(sum_p1, "9999999999.99")
  sb(16,2) = PADL(sb(16,2), LEN(sb(16,1)))
  sum_d0 = sum_d0+sum_p0
  sum_d1 = sum_d1+sum_p1  
  *
  *  ｪ�����┘ �腑爛き�� 痰����肓 (�� ��甄イ�ォ!)
  *
  IF f_pg # n_pg
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR
sb(13,2) = TRANSFORM(sum_d0, "9999999999.99")
sb(13,2) = PADL(sb(13,2), LEN(sb(13,1)))
sb(14,2) = TRANSFORM(sum_d1, "9999999999.99")
sb(14,2) = PADL(sb(14,2), LEN(sb(14,1)))
*
*   ｪ�����┘ ��甄イ�ォ 痰����肓
*
FOR i = 1 TO ALEN(f_d)
  str_w = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR
str_w = DBF("TMP_LST")
USE

DO Term_Prn WITH "", str_w, IIF(TYPE("c_code") = "N", c_code, .F.), "BILL0", wFirm, wDoc, wDate

DELETE FILE (str_w)
SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    ����皰→� 甌ぅ爨━��� ぎ�祠キ��.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 01/19/1999 陳�
PROCEDURE Prep_Tmp

PRIVATE tag_n, tag_exp, cName
SELECT 0
CREATE DBF (tmpo_path+"PRINT.TMP") ;
   (NAME     C(49), ;
    CLASS    C(64), ;
    QNT      N(10), ;
    PRC      N(13,2), ;
    SUM_0    N(15,2), ;
    SUM_T    N(15,2))
USE (tmpo_path+"PRINT.TMP") ALIAS TMP_LST EXCLUSIVE

*
*   �モ��讚覃 ����
SELECT 0
USE (base_path+"CLASSES") ORDER TAG CODE ALIAS TMP_CLASS AGAIN
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS TMP_STO AGAIN
*SET RELATION TO LEFT(CLASS, 2) INTO TMP_CLASS
*SET RELATION TO CLASS INTO TMP_CLASS
SELECT 0
USE (base_path+"BILLS_D") ORDER TAG DOC_NUM ALIAS DET_PRINT AGAIN
SET RELATION TO CODE INTO TMP_STO

*
*   ｯ爛ぅ�錺� ┃ぅ�甅�� �諤�Ε�┘
*
tag_n = 1
DO WHILE .NOT. TAG() == TAG(tag_n)
  tag_n = tag_n+1
ENDDO
tag_exp = SYS(14,tag_n)

= SEEK(k00)

SCAN REST WHILE EVALUATE(tag_exp) = k00   && �� 痰牀��� ぎ�祠キ��
  m.cName = Cls_Name(TMP_STO.CLASS, "TMP_CLASS")
  SELECT TMP_LST
  APPEND BLANK
  REPLACE NAME     WITH DET_PRINT.NAME,   ;
          CLASS    WITH m.cName,   		  ;
          QNT      WITH DET_PRINT.QNT,    ;
          PRC      WITH DET_PRINT.PRICE_0,;
          SUM_0    WITH DET_PRINT.SUM_0,  ;
          SUM_T    WITH DET_PRINT.SUM_T
  SELECT DET_PRINT
ENDSCAN
USE
USE IN TMP_CLASS
USE IN TMP_STO

SELECT TMP_LST

RETURN


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� List         ��о�｀砒┴ ���┃� ���矗┘���                 �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�             �襦�� �ョ�皋 瘍モ� 甌 甎�瓷�� ┼� ．� 甎�瓷�.              �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 02.04.2001 陳�
PROCEDURE List

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                               �
*� < �ェ 甎�瓷� > < �� 甎�瓷�� > �
*�                               �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, p_flg, v_sel
v_sel = SELECT()
ex = 1
p_flg = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 35, "�ョ�碎 瘍モ�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3, 3 GET ex PICTURE "@*HT \ �ェ 甎�瓷� ;\ �� 甎�瓷�� "
READ CYCLE

IF ex = 1
  p_flg = .T.
  DO P_Bill_0 WITH firm_c, doc_n, doc_d
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
SELECT (v_sel)

RETURN ex = 2
