*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Conv_Dep     ��о�｀砒┴ ��むォ ��瓱�           23.09.98 12:27:58 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�              ���▲珥�罔� (┃ぅ���罔�) ぅ�ィ �� ぅ��Ж皀.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Conv_Dep
PARAMETERS c_code, mute

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*
PRIVATE is_c, tmpNalog

tmpNalog = 0
is_c = .F.

PRIVATE max_ord
*max_ord = 0
*IF File_O(base_path+"MAX_ORD.MEM")
*  RESTORE FROM (base_path+"MAX_ORD.MEM") ADDITIVE
*ENDIF

PRIVATE what_do       && ��� 爛Θ��.
PRIVATE dep_list, n_f
DIMENSION dep_list(1, 6)
*
* ��ぃ�皰→� 甎�瓷� 筥爼
SELECT 0
USE (base_path+"FIRMS") ORDER TAG NAME ALIAS F_9211 AGAIN
n_f = 0
SCAN
  n_f = n_f+1
  DIMENSION dep_list(n_f, 6)
  dep_list(n_f, 1) = FIRM_CODE
  dep_list(n_f, 2) = B_NAME
  dep_list(n_f, 3) = 0
  dep_list(n_f, 4) = 0
  dep_list(n_f, 5) = 0
  dep_list(n_f, 6) = 0
ENDSCAN
USE
IF n_f = 0
  PRIVATE mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 甌Г��� �� �き��� �爛く爬閧��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

DO Use_Link
DO Use_Dummy
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME AGAIN ALIAS ACC_1215
what_do = "List"
IF TYPE("c_code") = "N"
  SET ORDER TO TAG CUS_CODE
  what_do = "Conv"
ENDIF

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷� ��┘�皰�

    SELECT 0
    USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME AGAIN
    GO RECNO("ACC_1215")
    
    what_do = IIF(Custs(""), "Work", "Exit")
    SELECT ACC_1215
    IF BETWEEN(RECNO("ACCOUNT"), 1, RECCOUNT())
      GO RECNO("ACCOUNT")
    ENDIF
    SELECT ACCOUNT
    USE
    SELECT ACC_1215
    

  CASE what_do = "Work"    && ���▲珥�罔�

    DO Prep_F
    DO Conv0
    SELECT ACC_1215
    what_do = "List"

  CASE what_do = "Conv"    && ���▲珥�罔�

    SEEK c_code
    IF Prep_F()
      DO Conv0 WITH "Fill"
    ENDIF
    SELECT ACC_1215
    what_do = "Exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

USE

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_F       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                ��ぃ�皰→� 甎�瓷� ぅ��Ж皰� か� ��┘���.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.02.1999 陳�
PROCEDURE Prep_F

PRIVATE s_sav, i, usd_dep
s_sav = SELECT()

IF TYPE("c_code") = "N"
  is_c = BUHG_REQ.DEP_TYPE = 4
ELSE
  IF TYPE("c_code") = "C"
    is_c = "CHECK" $ c_code
  ENDIF
ENDIF

usd_dep = 0
SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CR_9211 AGAIN
FOR i = 1 TO n_f
  IF SEEK(STR(ACC_1215.CUS_CODE)+dep_list(i, 1))
    dep_list(i, 3) = ACC_SUM
    dep_list(i, 4) = CR_SUM
    dep_list(i, 5) = ACC_SUMD
    dep_list(i, 6) = CRD_SUM
  ELSE
    dep_list(i, 3) = 0
    dep_list(i, 4) = 0
    dep_list(i, 5) = 0
    dep_list(i, 6) = 0
  ENDIF
  IF TYPE("c_code") = "N"
    IF BUHG_REQ.FIRM = dep_list(i,1)
      usd_dep = dep_list(i, 5)
    ENDIF
  ENDIF
ENDFOR
IF TYPE("c_code") = "N"
  PRIVATE   r_fr_d,  d_to_rn, d_to_rt, d_to_rk, d_to_r
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(BUHG_REQ.FIRM))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(BUHG_REQ.FIRM))
  r_fr_d  = BUHG_REQ.IND_INC    && �皰�� �� 珮゛ア覃 ぅ��Ж� (珮�)
  d_to_rn = BUHG_REQ.IND_INC*tmpNalog/100 && ǜ� �����
  d_to_rt = r_fr_d+d_to_rn   && �皰�� 甅閧� � ┃ぅ�瓱珮ガ��� ぅ��Ж�� (珮�)
  d_to_rk = BUHG_REQ.IND_INC/BUHG_REQ.IND_EXP  && �ムメ腮��碎 �� �窶痺...
  d_to_r  = d_to_rt/d_to_rk   && ��閧� � ┃ぅ�瓱珮ガ��� ぅ��Ж�� (USD)
  IF usd_dep < d_to_r .AND. .NOT. mute
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�キィ �� ぅ��Ж皀 �� 紜���モ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
ENDIF
USE
SELECT (s_sav)
RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Conv0        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ������キ┘ ゛���� か� ���▲珥�罔�.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.02.1999 陳�
PROCEDURE Conv0
PARAMETER mode
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �    ��� ....:....!....:....!....:....!....:....!....:....!     � �
*� �         踸�ョ��┘ 1 ....:....!....:....!....:....!..         � �
*� �         踸�ョ��┘ 2 ....:....!....:....!....:....!..         � �
*� �                       ���Κ� 1 2 3 4 5                        � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 [ ] 踳く爬閧┘ ... ....:....!....:. 陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �       �ク�Ж�  999 999 999.99 珮�    9 999 999.9999 USD       � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 �磧�� � � � 陳陳陳陳陳陳陳陳� 敖 � � � 珮゛� 陳陳陳陳陳陳陳陳� �
*� � � ぅ��Ж��   999 999 999.99� � � � ぅ��Ж��   9 999 999.9999$ � �
*� � ����� 999.9% 999 999 999.99� � � �� �窶痺            999.999  � �
*� � �皰��        999 999 999.99� � � �皰��        999 999 999.99� � �
*� � �� �窶痺            999.999  � � ����� 999.9% 999 999 999.99� � �
*� � �� ぅ��Ж�   9 999 999.9999$ � � �� ぅ��Ж�   9 999 999.9999$ � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�                  ( ) 踸絎き覃 �爐ム    ( ) �オ                   �
*�                 踸�ョ��┘ ....:....!....:....!..                 �
*�                 < OK Ctrl-W > < �皖���碎瘴 Esc >                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PRIVATE ex, dep_sw, v_sav
v_sav = 0
dep_sw = .F.
PRIVATE p_dep       && �����皀�� �� �爛く爬閧┘ (��瘁│ dep_list)
PRIVATE dep_r       && �磧�ア覃 ぅ��Ж� (�痰�皰�)
PRIVATE dep_d       && ��ぅ�瓱珮ガ覃 ぅ��Ж� (�痰�皰�)
PRIVATE r_to_d      && ��閧� � 珮゛ア��� ぅ��Ж��
PRIVATE r_to_dn     && ǜ� �����
PRIVATE r_to_dt     && �皰�� �ムアメ皋 �� ┃ぅ�瓱珮ガ覃 ぅ��Ж� (珮�)
PRIVATE r_to_dk     && �ムメ腮��碎 �� �窶痺...
PRIVATE d_fr_r      && � �ムアメ皋 �� ┃ぅ�瓱珮ガ覃 ぅ��Ж� (USD)

PRIVATE d_to_r      && ��閧� � ┃ぅ�瓱珮ガ��� ぅ��Ж�� (USD)
PRIVATE d_to_rk     && �ムメ腮��碎 �� �窶痺...
PRIVATE d_to_rt     && �皰�� 甅閧� � ┃ぅ�瓱珮ガ��� ぅ��Ж�� (珮�)
PRIVATE d_to_rn     && �┃竅 �����
PRIVATE r_fr_d      && �皰�� �� 珮゛ア覃 ぅ��Ж� (珮�)
PRIVATE nt          && 踸�ョ��┘
PRIVATE tmpCheck    && 
tmpCheck = IIF(is_c, 2, 1)
STORE 0 TO p_dep, dep_r, dep_d
STORE 0 TO r_to_d, r_to_dn, r_to_dt, d_fr_r
STORE 0 TO d_to_r, d_to_rt, d_to_rn, r_fr_d
r_to_dk = kurs
d_to_rk = kurs
*
*   踳く爬閧┘
*
p_dep = 1
FOR ex = 1 TO ALEN(dep_list,1)
  IF def_firm = dep_list(ex,1)
    p_dep = ex
    EXIT
  ENDIF
ENDFOR
IF TYPE("c_code") = "N"
  p_dep = 1
  FOR ex = 1 TO ALEN(dep_list,1)
    IF BUHG_REQ.FIRM = dep_list(ex,1)
      p_dep = ex
      EXIT
    ENDIF
  ENDFOR
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  r_fr_d  = BUHG_REQ.IND_INC    && �皰�� �� 珮゛ア覃 ぅ��Ж� (珮�)
  d_to_rn = BUHG_REQ.IND_INC*tmpNalog/100 && ǜ� �����
  d_to_rt = r_fr_d+d_to_rn   && �皰�� 甅閧� � ┃ぅ�瓱珮ガ��� ぅ��Ж�� (珮�)
  d_to_rk = BUHG_REQ.IND_INC/BUHG_REQ.IND_EXP  && �ムメ腮��碎 �� �窶痺...
  d_to_r  = d_to_rt/d_to_rk   && ��閧� � ┃ぅ�瓱珮ガ��� ぅ��Ж�� (USD)
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
IF .NOT. mute
  DO Prp_Nav_2
  DO D_Win_N WITH 22, 71, "��ぅ���罔�, ���▲珥�罔�..."

  *
  *  ��筮爼�罔� � ��┘�皀
  *
  DO Sun_Bord WITH  2,  3,  7, 67, " ��┘�� "
  @  3,  8 SAY "��� "+ACC_1215.CUS_NAME
  @  4, 13 SAY "踸�ョ��┘ 1 "+ACC_1215.ACC_NOTE
  @  5, 13 SAY "踸�ョ��┘ 2 "+ACC_1215.ACC_NOTED
  @  6, 27 SAY "���Κ�"
  @ ROW(), COL()+1 SAY ACC_1215.MARK1 COLOR (SCHEME(1,1))
  @ ROW(), COL()+1 SAY ACC_1215.MARK2 COLOR (SCHEME(1,1))
  @ ROW(), COL()+1 SAY ACC_1215.MARK3 COLOR (SCHEME(1,1))
  @ ROW(), COL()+1 SAY ACC_1215.MARK4 COLOR (SCHEME(1,1))
  @ ROW(), COL()+1 SAY ACC_1215.MARK5 COLOR (SCHEME(1,1))
ENDIF
nt = LEFT(ACC_1215.CUS_NAME,22)
ex = 1
IF .NOT. mute
  DO Sun_Bord WITH  8,  3, 10, 67, "                                      "
  @ 8, 22 SAY dep_list(p_dep,1)+" "+dep_list(p_dep,2)
ENDIF
dep_r = dep_list(p_dep, 3)
dep_d = dep_list(p_dep, 5)
IF .NOT. mute
  @ 9, 11 SAY "�ク�Ж�                 珮�                   USD"
  *
  *   �※Ε�┘ ぅ�ィ
  *
  DO Sun_Bord WITH 11,  3, 17, 34, " �磧�� � � � "
  @ 12, 32 SAY "�"
  @ 13, 32 SAY "�"
  @ 14, 32 SAY "�"
  @ 16, 32 SAY "$"
  DO Sun_Bord WITH 11, 36, 17, 67, " � � � 珮゛� "
  @ 12, 65 SAY "$"
  @ 14, 65 SAY "�"
  @ 15, 65 SAY "�"
  @ 16, 65 SAY "�"

  *------------------------------------------------------------------------
  *      �〓� ���ォ ゛����
  *
  @  8,  6 GET dep_sw PICTURE "@*C 踳く爬閧┘" VALID Ch_Dep()
  @  9, 20 GET dep_r PICTUR "@Z 999 999 999.99" WHEN .F.
  @  9, 42 GET dep_d PICTUR "@Z 9 999 999.9999" WHEN .F.
  @ 12,  5 SAY "� ぅ��Ж��  " GET r_to_d PICTURE "@Z 999 999 999.99"  ;
                     WHEN Save_Val(r_to_d) VALID Clc_RD(1)
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  @ 13,  5 SAY "����� "+STR(tmpNalog,5,1)+"%"    ;
           GET r_to_dn PICTURE "@Z 999 999 999.99" WHEN .F.
  @ 14,  5 SAY "�皰��       " GET r_to_dt PICTURE "@Z 999 999 999.99" WHEN .F.
  @ 15,  5 SAY "�� �窶痺           " GET r_to_dk PICTURE "@Z 999.999" ;
                     WHEN Save_Val(r_to_dk) VALID Clc_RD(2)
  @ 16,  5 SAY "�� ぅ��Ж�  " GET d_fr_r PICTURE "@Z 9 999 999.9999" ;
                     WHEN Save_Val(d_fr_r) VALID Clc_RD(3)
  @ 12, 38 SAY "� ぅ��Ж��  " GET d_to_r PICTURE "@Z 9 999 999.9999" ;
                     WHEN Save_Val(d_to_r) VALID Clc_DR(1)
  @ 13, 38 SAY "�� �窶痺           " GET d_to_rk PICTURE "@Z 999.999" ;
                     WHEN Save_Val(d_to_rk) VALID Clc_DR(2)
  @ 14, 38 SAY "�皰��       " GET d_to_rt PICTURE "@Z 999 999 999.99" WHEN .F.
  @ 15, 38 SAY "����� "+STR(tmpNalog,5,1)+"%"  ;
           GET d_to_rn PICTURE "@Z 999 999 999.99" WHEN .F.
  @ 16, 38 SAY "�� ぅ��Ж�  " GET r_fr_d PICTURE "@Z 999 999 999.99" ;
                     WHEN Save_Val(r_fr_d) VALID Clc_DR(3)

  @ WROWS()-4, 20 GET tmpCheck PICTURE "@*RH 踸絎き覃 �爐ム   ;�オ" WHEN .F.
  @ WROWS()-3, 19 SAY "踸�ョ��┘" GET nt
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

  READ CYCLE
ENDIF
is_c = tmpCheck = 2
IF ex = 1
  *
  * �矗���硅��ガ ゛���
  IF r_to_d # 0 .OR. d_to_r # 0
    IF Prp_Doc(is_c)
      IF TYPE("c_code") = "N"
        IF BUHG_REQ.IND_INC # r_fr_d
          REPLACE BUHG_REQ.R_INC  WITH BUHG_REQ.R_INC+BUHG_REQ.IND_INC-r_fr_d, ;
                  BUHG_REQ.IS_INC WITH .T., ;
                  BUHG_REQ.IND_EXP WITH r_fr_d/d_to_rk,  BUHG_REQ.IND_INC WITH r_fr_d
          PRIVATE mss
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"��������! �爛｀���┘ � 珮゛ア�� ��痰� 磚カ�腮��瘡!"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
        ENDIF
        REPLACE BUHG_REQ.IS_CONV   WITH .F.,  ;
                BUHG_REQ.WHO_INP   WITH user, ;
                BUHG_REQ.WHERE_INP WITH arm,  ;
                BUHG_REQ.DATE_INP  WITH DATE()
        PRIVATE s_sav
        s_sav = SELECT()
        SELECT 0
        USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LLL_000 AGAIN
        IF SEEK(BUHG_REQ.LST_NUM)
          REPLACE KASSA WITH KASSA+r_fr_d
        ENDIF
        USE
        SELECT (s_sav)
      ENDIF
      DO P_Docs
    ENDIF
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Dep       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��キ� �爛く爬閧��.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.02.1999 陳�
PROCEDURE Ch_Dep

IF .NOT. dep_sw
  RETURN .T.
ENDIF
dep_sw = .F.

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   踳く爬閧┘           �ク�Ж�(珮�)   �ク�Ж�(USD)  �
*� 敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� ���� NNNNNNNNNNNNNNNN 999 999 999.99 9 999 999.9999� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�            < OK Ctrl-W > < �皖���碎瘴 Esc >          �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, w_point, w_arr, ln

FOR ex = 1 TO ALEN(dep_list,1)
  DIMENSION w_arr(ex)
  w_arr(ex) = dep_list(ex, 1)+" "+dep_list(ex, 2)+ ;
              TRANSFORM(dep_list(ex, 3), "999 999 999.99")+" "+ ;
              TRANSFORM(dep_list(ex, 5), "9 999 999.9999")
ENDFOR
ln = MIN(ALEN(dep_list,1), 10)
w_point = p_dep
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7+ln, 58, "�襦�� �爛く爬閧��"
@ 2, 5 SAY "踳く爬閧┘           �ク�Ж�(珮�)   �ク�Ж�(USD)"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3, 3 GET w_point FROM w_arr FUNCTION "&" SIZE ln+2, 52
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

RELEASE WINDOW (win_name)
IF ex = 1
  *
  * �矗���硅��ガ ゛���
  p_dep = w_point
  dep_r = dep_list(p_dep, 3)
  dep_d = dep_list(p_dep, 5)
  STORE 0 TO r_to_d, r_to_dn, r_to_dt, d_fr_r
  STORE 0 TO d_to_r, d_to_rt, d_to_rn, r_fr_d
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  @ 13,  5 SAY "����� "+STR(tmpNalog,5,1)
  @ 15, 38 SAY "����� "+STR(tmpNalog,5,1)
ENDIF
*--------------------------------------------------------------------------
SHOW GETS
POP KEY
@ 8, 22 SAY dep_list(p_dep,1)+" "+dep_list(p_dep,2)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Save_Val     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                 ��縲��キ┘ Л�腑��� �爬 √�ぅ � ����.                  �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.02.1999 陳�
PROCEDURE Save_Val
PARAMETERS v
v_sav = v
RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clc_RD       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�           ��爬��硅 �諷��� ├ ���ォ ���▲珥�罔� ����� -> USD.           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.02.1999 陳�
PROCEDURE Clc_RD
PARAMETERS mode
IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

DO CASE
CASE mode = 1      && �磧�ア�� 痺���, 甅━�ガ�� � ぅ��Ж��
  IF v_sav = r_to_d
    RETURN .T.
  ENDIF
  IF r_to_d < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� �÷�皚�� ���▲珥�罔� む磽�� ����※�� ゛����!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  r_to_dn = ROUND(r_to_d*tmpNalog/100,2)
  r_to_dt = r_to_d+r_to_dn
  d_fr_r  = IIF(r_to_dk = 0, 0, ROUND(r_to_dt/r_to_dk,4))
CASE mode = 2      && �窶� か� �ムメ腑�� 珮゛ォ � ぎ���琺
  IF v_sav = r_to_dk
    RETURN .T.
  ENDIF
  IF r_to_dk <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"����� �窶甌� ぎ���牀� �� °��モ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  d_fr_r  = IIF(r_to_dk = 0, 0, ROUND(r_to_dt/r_to_dk,4))
CASE mode = 3      && �����牀��� 痺���, ��痰祚�鉗�� �� ぅ��Ж�
  IF v_sav = d_fr_r
    RETURN .T.
  ENDIF
  IF d_fr_r < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� �÷�皚�� ���▲珥�罔� む磽�� ����※�� ゛����!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF d_fr_r = 0
    r_to_dt = 0
    r_to_dn = 0
    r_to_d  = 0
  ELSE
    IF r_to_d = 0
      r_to_dt = ROUND(d_fr_r*r_to_dk,2)
*      tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                      0, GetNalog(dep_list(p_dep,1)))
      tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                     0, GetNalog(dep_list(p_dep,1)))
      r_to_dn = ROUND(tmpNalog*r_to_dt/(100+tmpNalog),2)
      r_to_d  = r_to_dt-r_to_dn
    ELSE
      r_to_dk = ROUND(r_to_d/d_fr_r, 3)
    ENDIF
  ENDIF
ENDCASE    
DO Clc_Dep
SHOW GETS

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clc_dep      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     ��瘍モ �痰�皖�� �� ぅ��Ж���.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.02.1999 陳�
PROCEDURE Clc_dep

dep_r = dep_list(p_dep, 3) - r_to_d + r_fr_d
dep_d = dep_list(p_dep, 5) - d_to_r + d_fr_r

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clc_DR       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�           ��爬��硅 �諷��� ├ ���ォ ���▲珥�罔� USD -> �����.           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 11.02.1999 陳�
PROCEDURE Clc_DR
PARAMETERS mode

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

DO CASE
CASE mode = 1      && �����牀��� 痺���, 甅━�ガ�� � ぅ��Ж��
  IF v_sav = d_to_r
    RETURN .T.
  ENDIF
  IF d_to_r < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� �÷�皚�� ���▲珥�罔� む磽�� ����※�� ゛����!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  d_to_rt = ROUND(d_to_r*d_to_rk,2)
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  d_to_rn = ROUND(d_to_rt*tmpNalog/(100+tmpNalog),2)
  r_fr_d  = d_to_rt-d_to_rn
CASE mode = 2      && �窶� か� ぎ���牀� � 珮゛�
  IF v_sav = d_to_rk
    RETURN .T.
  ENDIF
  IF d_to_rk <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"����� �窶甌� ぎ���牀� �� °��モ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  d_to_rt = ROUND(d_to_r*d_to_rk,2)
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  d_to_rn = ROUND(d_to_rt*tmpNalog/(100+tmpNalog),2)
  r_fr_d  = d_to_rt-d_to_rn
CASE mode = 3      && �磧�ア� 痺���, ��痰祚�鉗�� �� ぅ��Ж�
  IF v_sav = r_fr_d
    RETURN .T.
  ENDIF
  IF r_fr_d < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� �÷�皚�� ���▲珥�罔� む磽�� ����※�� ゛����!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF r_fr_d = 0
    d_to_rt = 0
    d_to_rn = 0
    d_to_r  = 0
  ELSE
*    tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                    0, GetNalog(dep_list(p_dep,1)))
    tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                   0, GetNalog(dep_list(p_dep,1)))
    IF d_to_r = 0
      d_to_rt = ROUND(r_fr_d*(100+tmpNalog)/100,2)
      d_to_rn = d_to_rt-r_fr_d
      d_to_r  = ROUND(d_to_rt/d_to_rk, 4)
    ELSE
      d_to_rt = ROUND(r_fr_d*(100+tmpNalog)/100,2)
      d_to_rn = d_to_rt-r_fr_d
      d_to_rk = ROUND(d_to_rt/d_to_r, 3)
    ENDIF
  ENDIF
ENDCASE    
DO Clc_Dep
SHOW GETS

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Doc      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        ��爼�牀���┘ ぎ�祠キ皰�.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.02.1999 陳�
PROCEDURE Prp_Doc
PARAMETER is_c

PRIVATE s_sav, doc_n, doc_d, w, doc1, doc2
PRIVATE rst, rst_0, rst_n, rst_d, ord_cnt, numbs, sums, k_w00, j00
PRIVATE is_kkm
is_kkm = .F.

IF is_c
  is_kkm = FirmKKM(dep_list(p_dep,1))
  IF EMPTY(is_kkm)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� ������ �爛く爬閧┬ �モ ��瘁�〓�� ��������!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  is_kkm = VAL(ALLTRIM(is_kkm))
ENDIF

IF .NOT. LetMoney(dep_list(p_dep,1), "3")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������� �爛く爬閧�� �カ讌� �爬�━�碎 ぎ���琺!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF is_c
  IF .NOT. LetMoney(dep_list(p_dep,1), "�")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"������� �爛く爬閧�� �カ讌� �牀；��碎 腑��!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ELSE
  IF .NOT. LetMoney(dep_list(p_dep,1), " ")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"������� �爛く爬閧�� �カ讌� ��｀��碎 � ����膈覓� ぅ�譯���!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"D_LIST.DBF")
CREATE DBF (tmpo_path+"D_LIST") ;
  ( FIRM     C( 3), ;
    DOC_NUM  C(10), ;
    DOC_NUMV C(11), ;
    DOC_DATE D,     ;
    DOC_TYPE C( 1), ;
    DOC_NAME C(16)  )
SELECT 0
*
*  ���祠キ硅, 瓣錻���襯 � あ└キ┘� ぅ�ィ
*
PRIVATE chk_num, chk_date, chk_time, chk_err
chk_num = ""
chk_err = 0
USE (base_path+"INCMONEY") ALIAS MNY_9212 AGAIN
doc_d = DATE()
chk_date = DATE()
chk_time = SECONDS()

IF r_to_d # 0                              

  rst = SELECT()
  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9B23 AGAIN
  SEEK dep_list(p_dep,1)
  max_ord = IIF(FSIZE("M_SUM")=0 .OR. is_c, 0, M_SUM)
  USE 
  SELECT (rst)
  
  rst   = r_to_d        
  rst_0 = ROUND(r_to_d/(100+nds_flt)*100,2)
  rst_n = r_to_dn
  rst_d = ROUND(r_to_d/kurs,4)
  k_w00 = rst/rst_d
  ord_cnt  = 0
  
  IF rst < max_ord .OR. max_ord = 0
    DIMENSION numbs(1), sums(1,4)
    ord_cnt  = 1
    numbs(1) = 0
    sums(1,1) = rst
    sums(1,2) = rst_0
    sums(1,3) = rst_n
    sums(1,4) = rst_d
  ELSE
    DO WHILE rst > 0
      IF rst < max_ord
        sum_w = rst
      ELSE
        sum_w = max_ord-ROUND(0.1*RAND()*max_ord, 0)
      ENDIF
      ord_cnt = ord_cnt+1
      rst = rst-sum_w
      DIMENSION numbs(ord_cnt), sums(ord_cnt,4)
      numbs(ord_cnt) = 0
      sums(ord_cnt,1)  = sum_w
      IF rst # 0
*        tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                        0, GetNalog(dep_list(p_dep,1)))
        tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                       0, GetNalog(dep_list(p_dep,1)))
        sums(ord_cnt,2)  = ROUND(sum_w/(100+nds_flt)*100, 2)
        rst_0 = rst_0-sums(ord_cnt,2)
        sums(ord_cnt,3)  = ROUND(sum_w*tmpNalog/100, 2)
        rst_n = rst_n-sums(ord_cnt,3)
        sums(ord_cnt,4)  = ROUND(sum_w/k_w00,4)
        rst_d = rst_d-sums(ord_cnt,4)
      ELSE
        sums(ord_cnt,2)  = rst_0
        sums(ord_cnt,3)  = rst_n
        sums(ord_cnt,4)  = rst_d
      ENDIF
    ENDDO
  ENDIF

  FOR j00 = 1 TO ord_cnt

    SELECT MNY_9212

    IF is_c              && �オ
      IF File_O(base_path+"TO_KKM.DBF") .AND. is_kkm # 0
        DO Do_Check WITH -sums(j00,2), -sums(j00,1)+sums(j00, 2), -sums(j00, 3), chk_num, chk_date, chk_time, chk_err
        IF .NOT. EMPTY(chk_err)
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"�荐！� "+ALLTRIM(STR(chk_err))+" �爬 ���謔�� �牀；碎 腑�"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
          SELECT MNY_9212
          USE
          SELECT D_LIST
          USE
          DELETE FILE (tmpo_path+"D_LIST.DBF")
          SELECT (s_sav)
          RETURN .F.
        ENDIF
      ENDIF
    ELSE
      chk_num = Doc_Num("EXP_ORDER", dep_list(p_dep,1), arm, doc_d)
      chk_num = sys_char+STR(-chk_num,9)
    ENDIF
    numbs(j00) = chk_num
    APPEND BLANK
*    tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                    0, GetNalog(dep_list(p_dep,1)))
    tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                   0, GetNalog(dep_list(p_dep,1)))
    REPLACE FIRM       WITH dep_list(p_dep,1),  ;
            DOC_TYPE   WITH IIF(is_c, "�", " "),;
            DOC_NUM    WITH chk_num,       ;
            DOC_DATE   WITH chk_date+j00-1,;
            DOC_TIME   WITH chk_time,      ;
            ENTER_DAT  WITH chk_date+j00-1,;
            CUS_CODE   WITH ACC_1215.CUS_CODE,  ;
            CUS_NAME   WITH ACC_1215.CUS_NAME,  ;
            MONEY_0    WITH sums(j00,2), ;
            NDS_       WITH nds_flt, ;
            MONEY_R    WITH -sums(j00,1), ;
            MONEY_D    WITH -sums(j00,4), ;
            NL_PRC     WITH tmpNalog,     ;
            N_OF_SALE  WITH -sums(j00,3), ;
            WHO        WITH user,    ;
            WHERE      WITH arm
    SELECT D_LIST
    APPEND BLANK
    REPLACE FIRM     WITH dep_list(p_dep,1), ;
            DOC_NUM  WITH chk_num , ;
            DOC_NUMV WITH chk_num,  ; 
            DOC_DATE WITH chk_date, ;
            DOC_TYPE WITH IIF(is_c, "�", " "), ;
            DOC_NAME WITH IIF(is_c, "�オ", "��痳�き覃 �爐ム")
  ENDFOR

  SELECT MNY_9212
  doc_n = Doc_Num("MONEY_$", dep_list(p_dep,1), arm, doc_d)
  APPEND BLANK
  REPLACE FIRM       WITH dep_list(p_dep,1),  ;
          DOC_TYPE   WITH "3",                ;
          DOC_NUM    WITH sys_char+STR(doc_n,9), ;
          DOC_DATE   WITH doc_d,   ;
          DOC_TIME   WITH SECONDS(), ;
          ENTER_DAT  WITH doc_d,   ;
          CUS_CODE   WITH ACC_1215.CUS_CODE,  ;
          CUS_NAME   WITH ACC_1215.CUS_NAME,  ;
          MONEY_0    WITH d_fr_r,   ;
          NDS_       WITH 0,        ;
          MONEY_R    WITH ROUND(d_fr_r*kurs,2),  ;
          MONEY_D    WITH d_fr_r,   ;
          NL_PRC     WITH 0,        ;
          N_OF_SALE  WITH 0,        ;
          WHO        WITH user,     ;
          WHERE      WITH arm
  SELECT D_LIST
  APPEND BLANK
  REPLACE FIRM     WITH dep_list(p_dep,1), ;
          DOC_NUM  WITH sys_char+STR(doc_n,9), ;
          DOC_NUMV WITH sys_char+ALLTRIM(STR(doc_n)), ;
          DOC_DATE WITH doc_d, ;
          DOC_TYPE WITH "3",   ;
          DOC_NAME WITH "踸絎� USD"
  SELECT MNY_9212
ENDIF

IF d_to_r # 0

  rst = SELECT()
  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9B23 AGAIN
  SEEK dep_list(p_dep,1)
  max_ord = IIF(FSIZE("M_SUM")=0 .OR. is_c, 0, M_SUM)
  USE 
  SELECT (rst)

  rst   = r_fr_d
  rst_0 = ROUND(r_fr_d/(100+nds_flt)*100,2)
  rst_n = d_to_rn
  rst_d = ROUND(r_fr_d/kurs,4)
  k_w00 = rst/rst_d
  ord_cnt  = 0
  IF rst < max_ord .OR. max_ord = 0
    DIMENSION numbs(1), sums(1,4)
    ord_cnt  = 1
    numbs(1) = 0
    sums(1,1) = rst
    sums(1,2) = rst_0
    sums(1,3) = rst_n
    sums(1,4) = rst_d
  ELSE
    DO WHILE rst > 0
      IF rst < max_ord
        sum_w = rst
      ELSE
        sum_w = max_ord-ROUND(0.1*RAND()*max_ord, 0)
      ENDIF
      ord_cnt = ord_cnt+1
      rst = rst-sum_w
      DIMENSION numbs(ord_cnt), sums(ord_cnt,4)
      numbs(ord_cnt) = 0
      sums(ord_cnt,1)  = sum_w
      IF rst # 0
*        tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                        0, GetNalog(dep_list(p_dep,1)))
        tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                       0, GetNalog(dep_list(p_dep,1)))
        sums(ord_cnt,2)  = ROUND(sum_w/(100+nds_flt)*100, 2)
        rst_0 = rst_0-sums(ord_cnt,2)
        sums(ord_cnt,3)  = ROUND(sum_w*tmpNalog/100, 2)
        rst_n = rst_n-sums(ord_cnt,3)
        sums(ord_cnt,4)  = ROUND(sum_w/k_w00,4)
        rst_d = rst_d-sums(ord_cnt,4)
      ELSE
        sums(ord_cnt,2)  = rst_0
        sums(ord_cnt,3)  = rst_n
        sums(ord_cnt,4)  = rst_d
      ENDIF
    ENDDO
  ENDIF

  FOR j00 = 1 TO ord_cnt

    SELECT MNY_9212

    IF is_c              && �オ
      IF File_O(base_path+"TO_KKM.DBF") .AND. is_kkm # 0
        DO Do_Check WITH sums(j00,2), sums(j00,1)-sums(j00,2), sums(j00,3), chk_num, chk_date, chk_time, chk_err
        IF .NOT. EMPTY(chk_err)
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"�荐！� "+ALLTRIM(STR(chk_err))+" �爬 ���謔�� �牀；碎 腑�"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
          SELECT MNY_9212
          USE
          SELECT D_LIST
          USE
          DELETE FILE (tmpo_path+"D_LIST.DBF")
          SELECT (s_sav)
          RETURN .F.
        ENDIF
      ENDIF
    ELSE
      chk_num = Doc_Num("INC_ORDER", dep_list(p_dep,1), arm, doc_d)
      chk_num = sys_char+STR(chk_num,9)
    ENDIF
    numbs(j00) = chk_num
*    tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                    0, GetNalog(dep_list(p_dep,1)))
    tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                   0, GetNalog(dep_list(p_dep,1)))
    APPEND BLANK
    REPLACE FIRM       WITH dep_list(p_dep,1),  ;
            DOC_TYPE   WITH IIF(is_c, "�", " "),;
            DOC_NUM    WITH chk_num,     ;
            DOC_DATE   WITH chk_date+j00-1,  ;
            DOC_TIME   WITH chk_time,        ;
            ENTER_DAT  WITH chk_date+j00-1,  ;
            CUS_CODE   WITH ACC_1215.CUS_CODE,  ;
            CUS_NAME   WITH ACC_1215.CUS_NAME,  ;
            MONEY_0    WITH sums(j00,2), ;
            NDS_       WITH nds_flt, ;
            MONEY_R    WITH sums(j00,1), ;
            MONEY_D    WITH sums(j00,4), ;
            NL_PRC     WITH tmpNalog,    ;
            N_OF_SALE  WITH sums(j00,3), ;
            WHO        WITH user,    ;
            WHERE      WITH arm
    SELECT D_LIST
    APPEND BLANK
    REPLACE FIRM     WITH dep_list(p_dep,1), ;
            DOC_NUM  WITH chk_num, ;
            DOC_NUMV WITH chk_num, ;
            DOC_DATE WITH chk_date,;
            DOC_TYPE WITH IIF(is_c, "�", " "), ;
            DOC_NAME WITH IIF(is_c, "�オ", "踸絎き覃 �爐ム")
  ENDFOR

  SELECT MNY_9212
  doc_n = Doc_Num("MONEY_$", dep_list(p_dep,1), arm, doc_d)
  APPEND BLANK
  REPLACE FIRM       WITH dep_list(p_dep,1),  ;
          DOC_TYPE   WITH "3",                ;
          DOC_NUM    WITH sys_char+STR(doc_n,9), ;
          DOC_DATE   WITH doc_d,   ;
          DOC_TIME   WITH SECONDS(), ;
          ENTER_DAT  WITH doc_d,   ;
          CUS_CODE   WITH ACC_1215.CUS_CODE,  ;
          CUS_NAME   WITH ACC_1215.CUS_NAME,  ;
          MONEY_0    WITH -d_to_r,   ;
          NDS_       WITH 0,        ;
          MONEY_R    WITH -ROUND(d_to_r*kurs,2),  ;
          MONEY_D    WITH -d_to_r,   ;
          NL_PRC     WITH 0,        ;
          N_OF_SALE  WITH 0,        ;
          WHO        WITH user,     ;
          WHERE      WITH arm
  SELECT D_LIST
  APPEND BLANK
  REPLACE FIRM     WITH dep_list(p_dep,1), ;
          DOC_NUM  WITH sys_char+STR(doc_n,9), ;
          DOC_NUMV WITH sys_char+ALLTRIM(STR(doc_n)), ;
          DOC_DATE WITH doc_d, ;
          DOC_TYPE WITH "3",   ;
          DOC_NAME WITH "��痳�� USD"
  SELECT MNY_9212
ENDIF

*
*   ��玻オ罔� ぅ��Ж��
*
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS DEP_9212 AGAIN
IF .NOT. SEEK(STR(ACC_1215.CUS_CODE)+dep_list(p_dep,1))
  APPEND BLANK
  REPLACE CUS_CODE WITH ACC_1215.CUS_CODE, ;
          FIRM     WITH dep_list(p_dep,1)
ENDIF
REPLACE ACC_SUM  WITH ACC_SUM+r_fr_d-r_to_d, ;
        ACC_SUMD WITH ACC_SUMD+d_fr_r-d_to_r
*
*  �����
*
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FR_9212 AGAIN
SEEK dep_list(p_dep,1)
REPLACE NALOG WITH NALOG+d_to_rn-r_to_dn

USE (base_path+"ARM_LST") ALIAS A1702 AGAIN
PRIVATE kass_usd, kass_r

GO arm_rec
kass_usd = KASSA_USD
kass_r = KASSA
*
*  ��瘁� ��｀腑�� �メ��
*
USE (base_path+"KASSA") ALIAS KSS_9212 AGAIN
IF d_to_rt-r_to_dt # 0
  doc1 = Doc_Num("KASSA", dep_list(p_dep,1), arm, doc_d)
  doc1 = sys_char+CHRTRAN(arm," ","0")+STR(doc1,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc1,      ;
          DOC_DATE   WITH doc_d,     ;
          DOC_TIME   WITH SECONDS(), ;
          VALUTA     WITH "�",       ;
          MONEY_REST WITH 0, ;
          KASSA_REST WITH kass_r-(d_to_rt-r_to_dt), ;
          TOTAL      WITH d_to_rt-r_to_dt, ;
          WHO_REP    WITH user,      ;
          WHO_GET    WITH user,      ;
          WHERE      WITH arm
  SELECT D_LIST
  APPEND BLANK
  REPLACE DOC_NUM  WITH doc1, ;
          DOC_NUMV WITH LEFT(doc1,4)+"-"+ALLTRIM(RIGHT(doc1,5)), ;
          DOC_DATE WITH doc_d, ;
          DOC_TYPE WITH "!", ;
          DOC_NAME WITH "����� ぅ�ィ (珮�)"

  SELECT KSS_9212
ENDIF

IF d_fr_r-d_to_r # 0
  doc2 = Doc_Num("KASSA", dep_list(p_dep,1), arm, doc_d)
  doc2 = sys_char+CHRTRAN(arm," ","0")+STR(doc2,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc2,      ;
          DOC_DATE   WITH doc_d,     ;
          DOC_TIME   WITH SECONDS(), ;
          VALUTA     WITH "$",       ;
          MONEY_REST WITH 0, ;
          KASSA_REST WITH kass_usd-(d_fr_r-d_to_r), ;
          TOTAL      WITH d_fr_r-d_to_r, ;
          WHO_REP    WITH user,      ;
          WHO_GET    WITH user,      ;
          WHERE      WITH arm
  SELECT D_LIST
  APPEND BLANK
  REPLACE DOC_NUM  WITH doc2, ;
          DOC_NUMV WITH LEFT(doc2,4)+"-"+ALLTRIM(RIGHT(doc2,5)), ;
          DOC_DATE WITH doc_d, ;
          DOC_TYPE WITH "!", ;
          DOC_NAME WITH "����� ぅ�ィ (USD)"

  SELECT KSS_9212
ENDIF
*
*   �甅�↓�� ��瘁�
*
USE (base_path+"KASSA_M") ALIAS KSS_9212 AGAIN
IF d_to_rt-r_to_dt # 0
  doc_n = Doc_Num("KASSA_M", dep_list(p_dep,1), arm, doc_d)
  doc_n = sys_char+STR(doc_n,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc_n,  ;
          DOC_DATE   WITH doc_d,  ;
          DOC_TIME   WITH SECONDS(),  ;
          CODE       WITH 1,      ;
          REF_NUM    WITH doc1,   ;
          REF_DATE   WITH doc_d,  ;
          SUM_R      WITH d_to_rt-r_to_dt, ;
          NOTE       WITH nt,     ;
          WHO        WITH user,   ;
          WHERE      WITH arm
ENDIF

IF d_fr_r-d_to_r # 0
  doc_n = Doc_Num("KASSA_M", dep_list(p_dep,1), arm, doc_d)
  doc_n = sys_char+STR(doc_n,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc_n,  ;
          DOC_DATE   WITH doc_d,  ;
          DOC_TIME   WITH SECONDS(),  ;
          CODE       WITH 1,      ;
          REF_NUM    WITH doc2,   ;
          REF_DATE   WITH doc_d,  ;
          SUM_D      WITH d_fr_r-d_to_r,;
          NOTE       WITH nt,     ;
          WHO        WITH user,   ;
          WHERE      WITH arm
ENDIF
IF d_to_r # 0
  doc_n = Doc_Num("KASSA_M", dep_list(p_dep,1), arm, doc_d)
  doc_n = sys_char+STR(doc_n,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc_n,  ;
          DOC_DATE   WITH doc_d,  ;
          DOC_TIME   WITH SECONDS(),  ;
          CODE       WITH 2,      ;
          SUM_R      WITH -d_to_rt,;
          SUM_D      WITH d_to_r, ;
          KURS_FL    WITH d_to_rk,;
          NOTE       WITH nt,     ;
          WHO        WITH user,   ;
          WHERE      WITH arm
ENDIF

IF r_to_d # 0
  doc_n = Doc_Num("KASSA_M", dep_list(p_dep,1), arm, doc_d)
  doc_n = sys_char+STR(doc_n,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc_n,  ;
          DOC_DATE   WITH doc_d,  ;
          DOC_TIME   WITH SECONDS(),  ;
          CODE       WITH 2,      ;
          SUM_R      WITH r_to_dt,;
          SUM_D      WITH -d_fr_r,;
          KURS_FL    WITH d_to_rk,;
          NOTE       WITH nt,     ;
          WHO        WITH user,   ;
          WHERE      WITH arm
ENDIF
USE
SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� P_Docs       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �ョ�碎 ぎ�祠キ皰�.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.02.1999 陳�
PROCEDURE P_Docs

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
SELECT D_LIST

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*���� NNNNNNNNNNN NNNNNNNNNNNNNNN�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 31

DO D_Wins WITH ln, wd, "�筮爼�牀���� ぎ�祠キ硅", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS FIRM:H="", DOC_NUMV:H="", DOC_NAME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && �ョ�碎

    DO CASE
    CASE DOC_TYPE $ " 3�"
      DO P_Money WITH FIRM, DOC_TYPE, DOC_NUM, DOC_DATE
    CASE DOC_TYPE = "!"
      DO P_Rep WITH DOC_NUM, DOC_DATE
    ENDCASE
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
USE
SELECT (s_sav)

RETURN
