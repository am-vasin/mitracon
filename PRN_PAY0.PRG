*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    �ョ�碎 ����諷 � ���皀Κ�.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prn_Pay0
PARAMETERS d_n, d_d

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, stw, t_p, ffeed, p_drctry
PRIVATE sel_rem, sb
DIMENSION sb(6,2)

sel_rem = SELECT()

SELECT 0

USE INCMONEY ORDER TAG DOC_NUM AGAIN ALIAS TMP000
SEEK "5"+LEFT(DTOS(d_d),4)+d_n
IF .NOT. FOUND()
  USE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���祠キ� �� ���ぅ� "+LEFT(d_n,4)+"-"+ALLTRIM(SUBSTR(d_n,5))+  ;
                  " "+TRANSFORM(d_d,"@D")
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_rem)
  RETURN
ENDIF
sb(1,1) = "{Doc1    }"
sb(1,2) = LEFT(d_n,4)+"-"+ALLTRIM(RIGHT(d_n,6))

sb(2,1) = "{Date1   }"
sb(2,2) = TRANSFORM(ENTER_DAT,"@D")

sb(3,1) = "{Client}"
sb(3,2) = CUS_CODE

sb(4,1) = "{Sum_R   }"
sb(4,2) = STR(MONEY_R,10,2)

sb(5,1) = "{Sum_D   }"
sb(5,2) = STR(MONEY_D,10,4)

sb(6,1) = "{Who}"
sb(6,2) = WHO

USE ACCOUNT ORDER TAG CUS_CODE AGAIN ALIAS TMP000
SEEK sb(3,2)
IF FOUND()
  sb(3,2) = CUS_NAME
ELSE
  sb(3,2) = ""
ENDIF

IF sb(6,2) = 0
  sb(6,2) = "��� □カ!"
ELSE
  USE USERS ORDER TAG USER_CODE AGAIN ALIAS TMP000
  SEEK sb(6,2)
  IF FOUND()
    sb(6,2) = ALLTRIM(USER_NAME)
  ELSE
    sb(6,2) = ""
  ENDIF
ENDIF

USE DOC_FORM
LOCATE FOR "PAY0" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(DOC_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(DOC_H,i)
ENDFOR

USE

DO Ini_Prn WITH "踸絎� �� ぅ��Ж� 0-�� 皋�� N "+sb(1,2), p_ln, lft, n_cp, fnt, ffeed, p_drctry

FOR i = 1 TO ALEN(t_p)
  stw = t_p(i)
  FOR j = 1 TO 6
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  @ PROW()+1, 0 SAY stw
ENDFOR

DO Term_Prn WITH ""

SELECT (sel_rem)
RETURN
