*旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
*�   뤲�璵ㅳ�� P_???        맆㏅젩�洵Ø ��ㅰⅸ 궇歲�                      �
*쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴캑
*�                                                                        �
*�                            룯�졻� ㄾゃД���                            �
*�                                                                        �
*읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴� 19.09.2001 컴�
PROCEDURE P_???
PARAMETERS tmpFirm, tmpDoc, tmpDate

PRIVATE d_year

IF TYPE("tmpDate") = "D"
  d_year = LEFT(DTOS(tmpDate),4)
ENDIF
IF TYPE("tmpDate") = "N"
  d_year = STR(tmpDate,4)
ENDIF
IF TYPE("tmpDate") = "C"
  d_year = tmpDate
ENDIF

PRIVATE s_sav				&& 눃α� 貰魚젺畑� ��Д� �젩�曄� �∥졹殊
PRIVATE d_year				&& 꺇� ㄾゃД���
PRIVATE d_n					&& 뜮Д� ㄾ�-�� ㄻ� �η졻�
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
PRIVATE dep_cod				&& 뒶� か�����

PRIVATE n, i, j, k, nn, str_w

s_sav = SELECT()

*
*   롡む猶젰� �젩エ劣
*
k00 = tmpFirm+d_year+tmpDoc
SELECT 0
USE (base_path+"?????") ALIAS ???????? AGAIN ORDER TAG ??????????
SEEK ?????????

SELECT 0
USE (base_path+"????????") ALIAS ??????? AGAIN ORDER TAG ?????????
SEEK ??????????

f_name = SYS(3)
DO Wt_Mess WITH "룼ㄳ�獸˚� ㄾゃД���"
DO Print_Tmp WITH f_name
DO Wt_Mess

***************************************************
*
*   룼ワ ㄾゃД���
*
DIMENSION sb(?????,2)
*
sb(01,1) = "{Page   }"		&& 묅�젺ⓩ�

*
*		닩��殊十첓獸� ㄾゃД���
*
sb(02,1) = "{Doc_Num  }"	&& 뜮Д� ㄾゃД���
sb(02,2) = ??????

sb(03,1) = "{Date    }"		&& 꽑�� ㄾ�-��
sb(03,2) = ???????

sb(04,1) = "{Manager                     }"		&& �™�� ㄾ�-��
sb(04,2) = ALLTRIM(?????.FAMILY)+" "+LEFT(?????.NAME, 1)+" "+LEFT(??????.S_NAME, 1)
sb(04,2) = PADR(sb(04,2), LEN(sb(04,1)))
*
*		뒲����
*
sb(05,1) = "{Customer                                        }"
							&& 닾� か�����
sb(05,2) = ????????.CUS_NAME
sb(05,2) = PADR(sb(05,2), LEN(sb(05,1)))

***************************************************
*
*   룼ワ ㄵ�젷彛�� 飡昔え
*
DIMENSION sb_det(????,2)
*
sb_det(01,1) = "{Pr}"						&& 뤲δØ�
sb_det(02,1) = "{Name                   }"	&& 뜝º���쥯���
sb_det(03,1) = "{Prod  }"					&& 뵪席� �昔�㎖�ㄸ收レ
sb_det(04,1) = "{Price     }"				&& 뼢췅
sb_det(05,1) = "{Qnt }"						&& 뒶�-¡

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "????????" == ALLTRIM(UPPER(DOC_NAME))

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

DO Ini_Prn WITH "뇿Д��", p_ln, lft, n_cp, fnt, ffeed, p_drctry

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
  DO WHILE .T.

    ****************************************************
    *
    *     뇿��キ畑� ��ワ ㄵ�젷彛�� ｀承��
    *

    * sb_det(01,1) = "{Pr}"			&& 뤲δØ�
    sb_det(01,2) = PRINT_TMP.PREFIX

    * sb_det(02,1) = "{Name                   }"	&& 뜝º���쥯���
    sb_det(02,2) = PRINT_TMP.NAME

    * sb_det(03,1) = "{Prod  }"		&& 뵪席� �昔�㎖�ㄸ收レ
    sb_det(03,2) = PRINT_TMP.PRODUCER

    * sb_det(04,1) = "{Price   }"	&& 뼢췅
    IF ???????.IS_USD
      sb_det(04,2) = STR(PRINT_TMP.PRICE, 10, 4)
    ELSE
      sb_det(04,2) = STR(PRINT_TMP.PRICE, 10, 2)
    ENDIF

    * sb_det(05,1) = "{Qnt }"		&& 뒶�-¡
    sb_det(05,2) = STR(PRINT_TMP.QNT, 6)

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
  IF f_pg = n_pg		&& 룼笹ⅳ�程 飡�젺ⓩ�?
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

DO Term_Prn WITH "", (tmpo_path+f_name+".DBF"), dep_cod

DELETE FILE (tmpo_path+f_name+".DBF")
DELETE FILE (tmpo_path+f_name+".CDX")

SELECT ????????????
USE
SELECT ????????????
USE

SELECT (s_sav)

RETURN
