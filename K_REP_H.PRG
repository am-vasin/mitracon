*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� K_Rep_H      ��о�｀砒┴ ��むォ ��瓱�           07.09.98 16:59:19 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  ����覃 �砒モ �� ��瘁��覓 ��ム�罔鍖.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE K_Rep_H

PRIVATE d0, d1, kurs_w, d0_, d1_

USE (base_path+"PARMS")
d0 = DAT_START
USE
d1 = DATE()
kurs_w = kurs_mmvb
d0_ = d0
d1_ = d1

IF .NOT. Set_P()
  RETURN
ENDIF

DO Wt_Mess WITH "��ぁ�爲� ����諷..."
DO Prep_Tmp
DO Wt_Mess

PRIVATE p_hole, p_emp
p_hole = .T.
p_emp  = .T.
IF Del_Empty()
  DO P_Doc
ENDIF

CLOSE DATABASES
DELETE FILE (tmpo_path+"K_H_TMP2.DBF")
DELETE FILE (tmpo_path+"K_H_TMP2.CDX")
DELETE FILE (tmpo_path+"K_TMP.DBF")
DELETE FILE (tmpo_path+"K_TMP.CDX")

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           �����モ琺 �砒モ�.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.09.98 17:02:17 夕
PROCEDURE Set_P

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��皀燿�� ��� 陳陳陳陳陳陳朕 �
*� � � ��.��.���� �� ��.��.���� � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�    ��瘍モ �� �窶痺 999.999     �
*�                                �
*�< OK Ctrl-W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 36, "����覃 �砒モ �� ��瘁�"
DO Sun_Bord WITH 2, 3, 4, 32, " ��皀燿�� ��� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  5 SAY "�" GET d0 PICTURE "@D"
@ 3, 18 SAY "��" GET d1 PICTURE "@D"
@ 5,  6 SAY "��瘍モ �� �窶痺" GET kurs_w PICTURE "999.999"
@ 7,  2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                    ��玻オ皚�痰� ������キ�� ゛����.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.09.98 17:10:04 夕
PROCEDURE Tst_Blank

IF ex = 2
  RETURN .T.
ENDIF

IF d0 < d0_
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�����讚�� ���� �� ��Ε� °碎 �キ跏� "+DTOC(d0_)+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF d1 > d1_
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� 瓮��き� 皰�讓� "+DTOC(d1_)+"..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF d1 < d0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�����讚�� ���� ぎ�Ν� °碎 �キ跏� ���ョ���..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF kurs_w <= 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�窶� ぎ����� ぎ�Ε� °碎 ｀�跏� �祀�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            ��ぁ�爲� ����諷.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.09.98 17:16:08 夕
PROCEDURE Prep_Tmp
PRIVATE n, n_c, str_w, pr

DELETE FILE (tmpo_path+"K_H_TMP2.DBF")
DELETE FILE (tmpo_path+"K_H_TMP2.CDX")
DELETE FILE (tmpo_path+"K_TMP.DBF")
DELETE FILE (tmpo_path+"K_TMP.CDX")

SELECT 0
CREATE DBF (tmpo_path+"K_H_TMP2")  ;
  ( G_CODE  N( 3), ;
    O_CODE  N( 3), ;
    L_TYPE  C( 1), ;
    DOC_N   C( 7), ;
    DOC_D   D,     ;
    SUM_P   N(12,4), ;
    SUM_N   N(12,4), ;
    NOTE    C(32)  )

SELECT 0
USE (base_path+"KASSA_M") ORDER TAG HRON
SEEK DTOS(d0)
IF .NOT. FOUND()
  n = RECNO(0)
  IF BETWEEN(n, 1, RECCOUNT())
    GO n
  ENDIF
ENDIF
COPY REST WHILE DOC_DATE <= d1 TO (tmpo_path+"K_TMP")
USE (tmpo_path+"K_TMP") EXCLUSIVE
INDEX ON STR(CODE,3)+DTOS(DOC_DATE) TAG K_TMP

SELECT 0
USE (base_path+"MM_TYPES") ORDER TAG CODE

SELECT 0
USE (base_path+"MM_GROUP")

PRIVATE sum_p0, sum_n0, sum_p1, sum_n1
SCAN     && �� �珮����
  sum_p0 = 0
  sum_n0 = 0
  str_w = OPER_LST
  SELECT K_H_TMP2
  APPEND BLANK
  REPLACE G_CODE  WITH RECNO("MM_GROUP"), ;
          O_CODE  WITH 0, ;
          L_TYPE  WITH "0"

  DO WHILE .NOT. EMPTY(str_w)   && �� 皋��� ��ム�罔� � �珮���
    sum_p1 = 0
    sum_n1 = 0
    n_c = LEFT(str_w,3)
    n   = VAL(ALLTRIM(n_c))
    str_w = SUBSTR(str_w, 5)
    SELECT K_H_TMP2
    APPEND BLANK
    REPLACE G_CODE  WITH RECNO("MM_GROUP"), ;
            O_CODE  WITH n, ;
            L_TYPE  WITH "1"
    SELECT K_TMP
    SEEK n_c
    SCAN REST WHILE CODE = n   && �� ��ム�罔鍖 甌�癶. 皋��
      SELECT K_H_TMP2
      APPEND BLANK
      pr = K_TMP.SUM_D+K_TMP.SUM_R/kurs_w
      REPLACE G_CODE  WITH RECNO("MM_GROUP"), ;
              O_CODE  WITH n, ;
              L_TYPE  WITH "2", ;
              DOC_N   WITH LEFT(K_TMP.DOC_NUM,1)+ALLTRIM(RIGHT(K_TMP.DOC_NUM,6)),  ;
              DOC_D   WITH K_TMP.DOC_DATE, ;
              SUM_P   WITH IIF(pr > 0, pr, 0), ;
              SUM_N   WITH IIF(pr < 0, -pr, 0),;
              NOTE    WITH K_TMP.NOTE
      sum_p1 = sum_p1+SUM_P
      sum_n1 = sum_n1+SUM_N
      SELECT K_TMP
    ENDSCAN
    SELECT K_H_TMP2
    APPEND BLANK
    REPLACE G_CODE  WITH RECNO("MM_GROUP"), ;
            O_CODE  WITH n, ;
            L_TYPE  WITH "3", ;
            SUM_P   WITH sum_p1, ;
            SUM_N   WITH sum_n1
    sum_p0 = sum_p0+sum_p1
    sum_n0 = sum_n0+sum_n1
  ENDDO
  SELECT K_H_TMP2
  APPEND BLANK
  REPLACE G_CODE  WITH RECNO("MM_GROUP"), ;
          O_CODE  WITH n, ;
          L_TYPE  WITH "4", ;
          SUM_P   WITH sum_p0, ;
          SUM_N   WITH sum_n0
ENDSCAN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                   �痰���→� 爛Θ�� �ョ�皋 ぎ�祠キ��.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 08.09.98 12:20:07 夕
PROCEDURE Del_Empty

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                  �
*�    [ ] �ョ�碎 �竅硅� ��ム�罔�    �
*�                                  �
*�    [ ] �ョ�碎 ������� �砒モ�     �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 38, "�┐ �砒モ�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 6 GET p_emp PICTURE "@*C �ョ�碎 �竅硅� ��ム�罔�"
@ 5, 6 GET p_hole PICTURE "@*C �ョ�碎 ������� �砒モ�"
@ 7, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE
PRIVATE n_sav
IF ex = 1
  SELECT K_H_TMP2
  IF .NOT. p_emp
    PRIVATE n_sav
    SET FILTER TO L_TYPE $ "13"
    SCAN
      IF L_TYPE = "1"
        n_sav = RECNO()
      ENDIF
      IF L_TYPE = "3"
        IF n_sav = RECNO()-1
          GO n_sav
          DELETE
          SKIP
          DELETE
        ENDIF
      ENDIF
    ENDSCAN
    SET FILTER TO
  ENDIF
  IF .NOT. p_hole
    DELETE ALL FOR L_TYPE = "2"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      ��＝癶キ�� �ョ�碎 ぎ�祠キ��.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 08.09.98 12:40:13 夕
PROCEDURE P_Doc
PRIVATE p_ln, fnt, n_cp, lft, ffeed, p_drctry
PRIVATE cnt

p_ln = 60
fnt  = " "
n_cp = 1
lft  = 0
ffeed = -1
p_drctry = 0

DO Ini_Prn WITH "��瘁�", p_ln, lft, n_cp, fnt, ffeed, p_drctry

cnt = 0

SELECT K_H_TMP2
SCAN
  cnt = cnt+1
  DO CASE
  CASE L_TYPE = "0"        && ��� �珮��� ��ム�罔�
    GO K_H_TMP2.G_CODE IN MM_GROUP
    @ PROW()+1, 0 SAY PADC(" "+ALLTRIM(MM_GROUP.NAME)+" ", 60, "=")
  CASE L_TYPE = "1"        && ��� ��ム�罔�
    = SEEK(O_CODE, "MM_TYPES")
    @ PROW()+1, 0 SAY "----------- "+ALLTRIM(MM_TYPES.NAME)
  CASE L_TYPE = "2"        && ｯム�罔�
    @ PROW()+1, 0 SAY "   "+ ;
      DTOC(DOC_D)+" "+DOC_N+STR(SUM_P,13,2)+STR(SUM_N,13,2)+" "+NOTE
  CASE L_TYPE = "3"        && �皰�� �� ��ム�罔�
    @ PROW()+1, 0 SAY  ;
    PADR("�皰�� �� ��ム�罔�",IIF(SET("DATE") = "ON", 21, 19))+ ;
      +STR(SUM_P,13,2)+STR(SUM_N,13,2)+STR(SUM_P-SUM_N,13,2)
  CASE L_TYPE = "4"        && �皰�� �� �珮��� ��ム�罔�
    @ PROW()+1, 0 SAY  ;
    PADR("�皰�� �� �珮���",IIF(SET("DATE") = "ON", 21, 19))+ ;
      +STR(SUM_P,13,2)+STR(SUM_N,13,2)+STR(SUM_P-SUM_N,13,2)
  ENDCASE
  IF cnt >= p_ln-10
    EJECT
    cnt = 0
  ENDIF

ENDSCAN

DO Term_Prn WITH ""

RETURN
