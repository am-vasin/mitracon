*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� P_Check      맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                         룯�졻� 獸쥯惜�． 曄첓                          �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 04.04.2006 컴�
PROCEDURE P_Check
PARAMETERS tmpFirm, tmpDoc, tmpDate

PRIVATE s_sav				&& 눃α� 貰魚젺畑� ��Д� �젩�曄� �∥졹殊
PRIVATE d_year				&& 꺇� ㄾゃД���
PRIVATE d_nc				&& 뜮Д� ㄾ�-�� ㄻ� �η졻� (獸�. 曄�)
PRIVATE d_nb				&& 뜮Д� ㄾ�-�� ㄻ� �η졻� (淞β-�젶栒��)
PRIVATE d_ni				&& 뜮Д� ㄾ�-�� ㄻ� �η졻� (췅か젮췅�)
PRIVATE k00					&& 뒲蹟 ㄻ� ��ⓤ첓 ㄾゃД���
PRIVATE sb					&& 뤳ⅱㄾ��ワ ㄾゃД���
PRIVATE sb_det				&& 뤳ⅱㄾ��ワ ㄵ�젷彛�� 飡昔え
PRIVATE p_ln				&& 꽠Þ� 飡�젺ⓩ� � 飡昔첓�
PRIVATE fnt					&& 섁ⓧ�+롞����졿⑨ 飡�젺ⓩ�
PRIVATE n_cp				&& 뒶エ曄飡¡ ぎ�Ł
PRIVATE lft					&& 떏¡� ��ゥ
PRIVATE ffeed				&& 뇿´殲���� ㄾ�-��
PRIVATE p_drctry			&& 뜝��젪ゥ��� �η졻�
PRIVATE t_d					&& 뇿．ギ¡� ㄾ�-��
PRIVATE f_d					&& 룼ㄲ젷 ㄾ�-��
PRIVATE t_p					&& 뇿．ギ¡� 飡�젺ⓩ�
PRIVATE f_p					&& 룼ㄲ젷 飡�젺ⓩ�
PRIVATE det					&& 렊�젳 ㄵ�젷彛�� 飡昔え
PRIVATE n_p_det1			&& 뒶�-¡ ㄵ�젷彛音 飡昔� 췅 飡�젺ⓩ�
PRIVATE n_p_det				&& 뒶�-¡ ㄵ�젷彛音 飡昔� 췅 飡�젺ⓩ�
PRIVATE n_pg				&& 뒶�-¡ 飡�젺ⓩ � ㄾゃД�收
PRIVATE f_pg				&& 뮙ゃ�좑 飡�젺ⓩ�
PRIVATE c_code				&& 뒶� か�����

PRIVATE n, i, j, k, nn, str_w, line_1, line_2
PRIVATE wDoc, wFirm, wDate

IF TYPE("tmpDate") = "D"
  d_year = LEFT(DTOS(tmpDate),4)
ENDIF
IF TYPE("tmpDate") = "N"
  d_year = STR(tmpDate,4)
ENDIF
IF TYPE("tmpDate") = "C"
  d_year = tmpDate
ENDIF

s_sav = SELECT()

*
*   롡む猶젰� �젩エ劣
*
k00 = tmpFirm+d_year+tmpDoc
SELECT 0
USE (base_path+"SALE_TIT") ALIAS TIT_20220 AGAIN ORDER TAG DOC_NUM
IF .NOT. SEEK(k00)
  USE
  SELECT (s_sav)
  RETURN
ENDIF
wFirm = FIRM
wDoc  = DOC_NUM
wDate = DOC_DATE

IF EMPTY(T_CHECK)
  PRIVATE tmpNum
  tmpNum = Doc_Num("G_CHECK", FIRM, WHERE, DOC_DATE)
  REPLACE T_CHECK WITH STR(tmpNum)
ENDIF
c_code = CUS_CODE

SELECT 0
USE (base_path+"ACCOUNT") ALIAS ACC_20220 AGAIN ORDER TAG CUS_CODE
SEEK TIT_20220.CUS_CODE

SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_20220 AGAIN ORDER TAG CODE
SEEK TIT_20220.WHO

SELECT 0
USE (base_path+"FIRMS") ALIAS FRM_20220 AGAIN ORDER TAG FIRM_CODE
SEEK TIT_20220.FIRM

f_name = SYS(3)
DO Wt_Mess WITH "룼ㄳ�獸˚� ㄾゃД���"
DO Print_Tmp WITH f_name
DO Wt_Mess

***************************************************
*
*   룼ワ ㄾゃД���
*
DIMENSION sb(16,2)
*
sb(01,1) = "{묅�.   }"		&& 묅�젺ⓩ�

*
*		닩��殊十첓獸� ㄾゃД���
*
sb(02,1) = "{뜝か. �  }"	&& 뜮Д� 췅か젮���
sb(02,2) = LEFT(TIT_20220.DOC_NUM, 4)+"-"+ALLTRIM(SUBSTR(TIT_20220.DOC_NUM, 5))
sb(02,2) = PADR(sb(02,2), LEN(sb(02,1)))

sb(03,1) = "{묊-�. �  }"	&& 뜮Д� 淞β�-�젶栒贍
sb(03,2) = LEFT(TIT_20220.NEW_DOC, 1)+"-"+ALLTRIM(SUBSTR(TIT_20220.NEW_DOC, 2))
sb(03,2) = PADR(sb(03,2), LEN(sb(03,1)))

sb(04,1) = "{뮢�.�. � }"	&& 뜮Д� 獸쥯惜�． 曄첓
sb(04,2) = ALLTRIM(TIT_20220.T_CHECK)
sb(04,2) = PADR(sb(04,2), LEN(sb(04,1)))

sb(05,1) = "{꽑��    }"		&& 꽑�� ㄾ�-��
sb(05,2) = DTOC(TIT_20220.DOC_DATE)

sb(06,1) = "{뙠�ⅳ┘�                    }"		&& �™�� ㄾ�-��
sb(06,2) = ALLTRIM(PRS_20220.FAMILY)+" "+LEFT(PRS_20220.NAME, 1)+" "+LEFT(PRS_20220.S_NAME, 1)
sb(06,2) = PADR(sb(06,2), LEN(sb(06,1)))
*
*		뒲����
*
sb(07,1) = "{뒲����                                          }"
sb(07,2) = ACC_20220.CUS_NAME
sb(07,2) = PADR(sb(05,2), LEN(sb(05,1)))

sb(08,1) = "{뤲ⅳ�黍汀��                                     }"
sb(08,2) = FRM_20220.L_NAME
sb(08,2) = PADR(sb(05,2), LEN(sb(05,1)))

sb(09,1) = "{묆К� 꽡� 獸�}"	&& 렊�좑 率К� �� ㄾゃД�栒 (獸嶺�)
sb(09,2) = "               "

sb(10,1) = "{묆К� 꽡�    }"	&& 렊�좑 率К� �� ㄾゃД�栒
sb(10,2) = "               "

sb(11,1) = "{묆К� 묅� 獸�}"	&& 렊�좑 率К� �� 飡�젺ⓩ� (獸嶺�)
sb(11,2) = "               "

sb(12,1) = "{댿�． 묅�    }"	&& 렊�좑 率К� �� 飡�젺ⓩ�
sb(12,2) = "               "

sb(13,1) = "{꽑�� �昔�ⓤ溢     }"
sb(13,2) = Date_Str(TIT_20220.DOC_DATE)

sb(14,1) = "{묆К� �昔�ⓤ溢                                  }"
sb(14,2) = "                                                  "

sb(15,1) = "{묆К� �昔�ⓤ溢 1                                }"
sb(15,2) = "                                                  "

sb(16,1) = "{묆К� �昔�ⓤ溢 2                                }"
sb(16,2) = "                                                  "

***************************************************
*
*   룼ワ ㄵ�젷彛�� 飡昔え
*
DIMENSION sb_det( 8,2)
*
sb_det(01,1) = "{뤲}"						&& 뤲δØ�
sb_det(02,1) = "{뜝º���쥯���           }"	&& 뜝º���쥯���
sb_det(03,1) = "{뵪席� }"					&& 뵪席� �昔�㎖�ㄸ收レ
sb_det(04,1) = "{뼢췅 獸嶺�}"				&& 뼢췅 獸嶺�
sb_det(05,1) = "{뼢췅    }"					&& 뼢췅 �む膝�.
sb_det(06,1) = "{�-¡}"						&& 뒶�-¡
sb_det(07,1) = "{묆К� 獸嶺�  }"			&& 묆К� 獸嶺�
sb_det(08,1) = "{묆К�        }"			&& 묆К� �む膝�

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "T_CHECK" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

*
*		삝∥�� 쭬．ギ˚� ㄾ�-��
*
n = MEMLINES(DOC_H)
IF n = 0
  DIMENSION t_d(1)
  t_d(1) = ""
ELSE
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

*
*		삝∥�� 쭬．ギ˚� 飡�젺ⓩ�
*
n = MEMLINES(PAGE_H)
IF n = 0
  DIMENSION t_p(1)
  t_p(1) = ""
ELSE
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

*
*		삝∥�� ��ㄲ젷� 飡�젺ⓩ�
*
n = MEMLINES(PAGE_F)
IF n = 0
  DIMENSION f_p(1)
  f_p(1) = ""
ELSE
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

*
*		삝∥�� ��ㄲ젷� ㄾ�-��
*
n = MEMLINES(DOC_F)
IF n = 0
  DIMENSION f_d(1)
  f_d(1) = ""
ELSE
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ENDIF

*
*		삝∥�� ㄵ�젷彛�� 飡昔え
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

DO Ini_Prn WITH "뮢쥯惜硫 曄�", p_ln, lft, n_cp, fnt, ffeed, p_drctry

SELECT PRINT_TMP
GO TOP

nn = RECCOUNT()		&& 귗ⅲ� ㄵ�젷彛音 깘뱩�
n_p_det  = p_ln-ALEN(t_d)-MAX(ALEN(f_p),ALEN(f_d))
							&& 뒶�-¡ 飡昔� ��� ㄵ�젷彛莘 �졹筍 췅 ��舒�� 飡�젺ⓩ�
n_p_det1 = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
							&& 뒶�-¡ 飡昔� ��� ㄵ�젷彛莘 �졹筍 췅 뛿��舒�� 飡�젺ⓩ�
n_p_det  = FLOOR(n_p_det/ALEN(det))		&& 뒶�-¡ ㄵ�젷彛音 깘뱩� 췅 ��舒�� 飡�젺ⓩ�
n_p_det1 = FLOOR(n_p_det1/ALEN(det))	&& 뒶�-¡ ㄵ�젷彛音 깘뱩� 췅 뛿��舒�� 飡�젺ⓩ�
n_pg = 1
nn = nn-n_p_det		&& 뫇�レぎ ㄵ�젷彛音 ｀承� 췅 ㏇ε 飡�젺ⓩ졾 ‥� ��舒��

IF n_p_det <= 0		&& 뇿〃エ ㏇� 飡�젺ⓩ� ��� 쭬．ギ˚�... �� 첓え� 飡�젺ⓩ!
  n_p_det = RECCOUNT()
ELSE
  n_pg = 1+CEILING(MAX(nn, 0)/n_p_det1)
ENDIF
PRIVATE sum_d, sum_0_d, sum_p, sum_0_p

STORE 0 TO sum_d, sum_0_d
FOR f_pg = 1 TO n_pg		&& 룼 飡�젺ⓩ젹...
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))	&& 묅�젺ⓩ�/묅�젺ⓩ
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1   && 뇿．ギ¡� 뛿 ��舒�� 飡�젺ⓩ�
    EJECT
    n_p_det = n_p_det1		&& 룯誓飡젪ワ�� ㄻÞ� 飡�젺ⓩ�...
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
  ELSE        && 뇿．ギ¡� ��舒�� 飡�젺ⓩ�
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
  *   꽖�젷彛좑 �졹筍 飡�젺ⓩ�
  * 
  i = 0		&& 묊β葉� ㄵ�젷彛音 ｀承�
  STORE 0 TO sum_p, sum_0_p
  DO WHILE .T.

    ****************************************************
    *
    *     뇿��キ畑� ��ワ ㄵ�젷彛�� ｀承��
    *

    * sb_det(01,1) = "{Pr}"			&& 뤲δØ�
    sb_det(01,2) = LEFT(PRINT_TMP.PREFIX, LEN(sb_det(01,1)))

    * sb_det(02,1) = "{Name                   }"	&& 뜝º���쥯���
    sb_det(02,2) = LEFT(PRINT_TMP.NAME, LEN(sb_det(02,1)))


    * sb_det(03,1) = "{Prod  }"		&& 뵪席� �昔�㎖�ㄸ收レ
    sb_det(03,2) = LEFT(PRINT_TMP.PRODUCER, LEN(sb_det(03,1)))



    *sb_det(04,1) = "{뼢췅 獸嶺�}"				&& 뼢췅 獸嶺�
    sb_det(04,2) = STR(PRINT_TMP.PRICE, 12, 5)
    
    *sb_det(05,1) = "{뼢췅    }"					&& 뼢췅 �む膝�.
    sb_det(05,2) = STR(ROUND(PRINT_TMP.PRICE, 2), 10, 2)

    *sb_det(06,1) = "{�-¡}"						&& 뒶�-¡
    sb_det(06,2) = STR(QNT, 6)

    sb_det(07,1) = "{묆К� 獸嶺�  }"			&& 묆К� 獸嶺�
    sb_det(07,2) = STR(ROUND(QNT*PRICE, 2), 15, 2)

    sb_det(08,1) = "{묆К�        }"			&& 묆К� �む膝�
    sb_det(08,2) = STR(QNT*ROUND(PRICE, 2), 15, 2)


    sum_0_p = sum_0_p + ROUND(QNT*PRICE, 2)
    sum_0_d = sum_0_d + ROUND(QNT*PRICE, 2)
    sum_p   = sum_p   + QNT*ROUND(PRICE, 2)
    sum_d   = sum_d   + QNT*ROUND(PRICE, 2)

    FOR k = 1 TO ALEN(det)		&& 룼ㅱ�젺�˚� ��ゥ� � �η졻� ㄵ�젷彛�� ｀承��
      str_w = det(k)
      FOR j = 1 TO ALEN(sb_det,1)
        str_w = STRTRAN(str_w, sb_det(j,1), sb_det(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR

    i = i+1
    SKIP
    IF EOF("PRINT_TMP")
      EXIT
    ENDIF
    IF  i >= n_p_det
      EXIT
    ENDIF
  ENDDO

  *
  *    렕���젺�� 飡�젺ⓩ�
  *
  sb(11,2) = STR(sum_0_p, 15, 2)
  sb(12,2) = STR(sum_p, 15, 2)

  IF f_pg = n_pg		&& 룼笹ⅳ�程 飡�젺ⓩ�?

    sb( 9,2) = STR(sum_0_d, 15, 2)
    sb(10,2) = STR(sum_d, 15, 2)
    str_w = Capital(Num2RStr(FLOOR(ABS(sum_0_d))))+" 說�. "+ ;
            STR((ABS(sum_0_d)-FLOOR(ABS(sum_0_d)))*100,2)+" ぎ�."
    sb(14,2) = PADR(str_w, LEN(sb(14,1)))

    line_1 = ""
    line_2 = ""
    DO Break_Str WITH str_w, line_1, line_2, LEN(sb(15,1))
    sb(15,2) = line_1
    str_w    = line_2

    line_1 = ""
    DO Break_Str WITH str_w, line_1, line_2, LEN(sb(16,1))
    sb(16,2) = line_1

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

DO Term_Prn WITH "", (tmpo_path+f_name+".DBF"), c_code, "CHECK", wFirm, wDoc, wDate

DELETE FILE (tmpo_path+f_name+".DBF")
DELETE FILE (tmpo_path+f_name+".CDX")

SELECT TIT_20220
USE
SELECT ACC_20220
USE
SELECT PRS_20220
USE
SELECT FRM_20220
USE

SELECT (s_sav)

RETURN

*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� Print_Tmp    맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                      룼ㄳ�獸˚� №�Д���． �젵쳽                       �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 21.02.2002 컴�
PROCEDURE Print_Tmp
PARAMETER tmpName

PRIVATE s_sav, tmpPrice, s0, s1

s_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ALIAS STO_20220 ORDER TAG CODE
SELECT 0
USE (base_path+"SALE") SHARED AGAIN ALIAS DET_20220 ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STO_20220
SELECT 0
CREATE DBF (tmpo_path+tmpName) ;
  ( CODE		N( 7), ;
    PREFIX		C( 4), ;
    NAME		C(35), ;
    PRODUCER	C( 8), ;
    QNT			N( 6), ;
    PRICE		N(12, 5) )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS PRINT_TMP
INDEX ON CODE TAG CODE

SELECT DET_20220
SEEK k00
SCAN REST WHILE DOC_NUM = TIT_20220.DOC_NUM .AND. ;
                FIRM    = TIT_20220.FIRM    .AND. ;
                DOC_DATE= TIT_20220.DOC_DATE
  SELECT PRINT_TMP
*  IF .NOT. SEEK(DET_20220.CODE)
    APPEND BLANK
*  ENDIF
  REPLACE	CODE		WITH DET_20220.CODE,     ;
			PREFIX		WITH STO_20220.PREFIX,   ;
			NAME		WITH STO_20220.NAME,     ;
			PRODUCER	WITH STO_20220.PRODUCER, ;
			QNT			WITH QNT+DET_20220.QNT,  ;
			PRICE		WITH DET_20220.PRICE_R
  SELECT DET_20220
ENDSCAN

USE
SELECT STO_20220
USE
SELECT PRINT_TMP
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
SELECT (s_sav)

RETURN
