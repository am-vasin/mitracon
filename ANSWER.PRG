*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Answer       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      �癶モ ���祚�皀�� �� ���牀�.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 27.09.1999 様�
PROCEDURE Answer
PARAMETERS l_num

PRIVATE s_sav, mss, c_code
s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS TIT_9927 AGAIN
IF .NOT. SEEK(l_num)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���瓷� � ���ム�� "+ALLTRIM(STR(l_num))+" �� 痺薀痰≪モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
IF .NOT. EMPTY(SAL_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���甌� N "+ALLTRIM(STR(l_num))+" 礒� �牀���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

IF CLI_CODE # 0
  c_code = CLI_CODE
ENDIF

DO Prep_Tmp

DO P_Doc

IF USED("ANSWER")
  SELECT ANSWER
  USE
ENDIF
DELETE FILE (tmpo_path+"ANSWER.DBF")
DELETE FILE (tmpo_path+"CHANGES.DBF")
SELECT TIT_9927
USE
SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.09.1999 陳�
PROCEDURE Prep_Tmp

SELECT 0
DELETE FILE (tmpo_path+"ANSWER.DBF")
CREATE DBF (tmpo_path+"ANSWER") ;
  ( MARK      C(  7), ;
    CODE      N(  7), ;
    PREFIX    C(  4), ;
    NAME      C( 78), ;
    PRODUCER  C(  8), ;
    PRICE     N(8,2), ;
    QNT_REQ   N(  6), ;
    QNT_ROUND N(  6), ;
    QNT_ANS   N(  6)  )
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_9927 AGAIN
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_NAME ALIAS LST_9927 AGAIN
SET RELATION TO CODE INTO STO_9927
SEEK STR(l_num,6)
SCAN REST WHILE LIST_NUM = STR(l_num,6)
  SELECT ANSWER
  APPEND BLANK
*  REPLACE MARK      WITH IIF(LST_9927.QNT-LST_9927.QNT_REQ > MAX(STO_9927.QNT,0), "0", "")
  REPLACE CODE      WITH LST_9927.CODE,     ;
          PREFIX    WITH LST_9927.PREFIX,   ;
          NAME      WITH LST_9927.NAME,     ;
          PRODUCER  WITH LST_9927.PRODUCER, ;
          PRICE     WITH IIF(TIT_9927.IS_USD, STO_9927.PRICE, STO_9927.PRICE_R), ;
          QNT_REQ   WITH LST_9927.QNT,      ;
          QNT_ROUND WITH STO_9927.QNT_ROUND,;
          QNT_ANS   WITH MIN(LST_9927.QNT, MAX(STO_9927.QNT,0)+LST_9927.QNT_REQ)
  IF LST_9927.QNT-LST_9927.QNT_REQ > MAX(STO_9927.QNT,0)
    DO Add_Chang
  ENDIF
  SELECT LST_9927
ENDSCAN
USE
SELECT STO_9927
USE
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Add_Chang    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          踳か���ガ ���キ�...                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.09.1999 陳�
PROCEDURE Add_Chang

IF LST_9927.CODE = 0
  RETURN
ENDIF
DO Changes WITH LST_9927.CODE
SELECT 0
USE (tmpo_path+"CHANGES.DBF")
SCAN FOR .NOT. EMPTY(CODE)
  = SEEK(CODE, "STO_9927")
  IF STO_9927.QNT > 0
    SELECT ANSWER
    APPEND BLANK
    REPLACE MARK      WITH "���キ� ",         ;
            CODE      WITH STO_9927.CODE,     ;
            PREFIX    WITH STO_9927.PREFIX,   ;
            NAME      WITH STO_9927.NAME,     ;
            PRODUCER  WITH STO_9927.PRODUCER, ;
            PRICE     WITH IIF(TIT_9927.IS_USD, STO_9927.PRICE, STO_9927.PRICE_R), ;
            QNT_ROUND WITH STO_9927.QNT_ROUND,;
            QNT_ANS   WITH MIN(LST_9927.QNT, STO_9927.QNT)
    SELECT CHANGES
  ENDIF
ENDSCAN
USE

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� P_Doc        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��＝癶キ�� �ョ�碎 ぎ�祠キ��.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.09.1999 陳�
PROCEDURE P_Doc

PRIVATE s_sav   && ���ム ��｀腑� �゛�痰� か� 〓Б����
PRIVATE c_name  && ��� ��┘��� (メ�� ����〓� ━ゥ� �メ皰)
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

IF TIT_9927.CLI_CODE # 0
*
*  �薀� ��┘���...
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_9927 AGAIN
  SEEK TIT_9927.CLI_CODE
  c_name = ALLTRIM(CUS_NAME)
  USE
ELSE
  c_name = ALLTRIM(TIT_9927.LST_NAME)
ENDIF

*
*  ｯ�瘠��ガ �瓮△�����
DIMENSION sb( 8,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{L_N }"
sb( 2,2) = STR(TIT_9927.LST_NUM,6)

sb( 3,1) = "{L_Date  }"
sb( 3,2) = DTOC(TIT_9927.CR_DAT)

sb( 4,1) = "{Customer}"
sb( 4,2) = c_name

sb( 5,1) = "{V}"
sb( 5,2) = IIF(TIT_9927.IS_USD, "� �", "�磧")

sb( 6,1) = "{Sum P   }"
sb( 6,2) = ""

sb( 7,1) = "{Sum D   }"
sb( 7,2) = ""

sb( 8,1) = "{Autor                       }"
SELECT 0
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF user = 0
  sb(8,2) = "���!"
ELSE
  IF SEEK (user)
    sb(8,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
  ELSE
    sb(8,2) = "?!!"
  ENDIF
ENDIF         
sb(8,2) = PADR(sb(8,2),LEN(sb(8,1)))
USE

*
* �с��ガ ������┘ ぎ�祠キ��
SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "ANSWER" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN   && ��┃� 痰����肓 � 痰牀���
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT       && �爬籵: " " - �°膈覃, "1" - 瓲�硅� (condenced)
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

SELECT ANSWER

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

PRIVATE n_p_det  && ��甄� ぅ���讚諷 痰牀� �� 痰����罐
PRIVATE n_pg     && ��甄� 痰����� � ぎ�祠キ皀
PRIVATE f_pg     && �オ竕�� 痰������ ぎ�祠キ�� �爬 �ョ�皋
PRIVATE str_w    && ｡��� �ョ���ガ�� 痰牀��

n_p_det = p_ln-MAX(ALEN(f_p),ALEN(f_d))-MAX(ALEN(t_p),ALEN(t_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
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
PRIVATE s000p, s000d
STORE 0 TO s000p, s000d

FOR f_pg = 1 TO n_pg    && �� 痰�������...

  s000p = 0
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
  IF RECCOUNT() # 0
    *
    *  �モ��讚襯 痰牀�� �腑爛き�� 痰����肓
    FOR i = 1 TO n_p_det
      IF EMPTY(MARK)
        @ PROW()+1,0 SAY PREFIX+NAME+PRODUCER+ ;
                         TRANSFORM(QNT_ROUND,"@Z 999999")+" "+;
                         TRANSFORM(QNT_REQ,"@Z 999999")+" "+  ;
                         TRANSFORM(QNT_ANS,"@Z 999999")+" "+  ;
                         TRANSFORM(PRICE, "@Z 99999.99")+" "+ ;
                         TRANSFORM(PRICE*QNT_ANS, "@Z 9999999.99")
        s000p = s000p+PRICE*QNT_ANS
      ELSE
        @ PROW()+1,0 SAY PREFIX+NAME+PRODUCER+ ;
                         TRANSFORM(QNT_ROUND,"@Z 999999")+" "+;
                         MARK+  ;
                         TRANSFORM(QNT_ANS,"@Z 999999")+" "+  ;
                         TRANSFORM(PRICE, "@Z 99999.99")+" "+ ;
                         TRANSFORM(PRICE*QNT_ANS, "@Z 9999999.99")
      ENDIF
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  s000d = s000d+s000p
  sb( 6,2) = STR(s000p,10,2)
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
*
*   ｪ�����┘ ��甄イ�ォ 痰����肓
*
sb( 7,2) = STR(s000d,10,2)
FOR i = 1 TO ALEN(f_d)
  str_w = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR

SELECT ANSWER
USE
DO Term_Prn WITH "", tmpo_path+"ANSWER.DBF", IIF(TYPE("c_code") = "N", c_code, .F.)

SELECT (s_sav)
RETURN
