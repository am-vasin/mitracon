*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Rep_Slr      ��о�｀砒┴ ��むォ ��瓱�           09.09.98 13:41:08 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       �砒モ �� 皋��� ��腮甄キ┤.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Rep_Slr
PRIVATE d0, d0_, d1, t_names, t_codes, t_numb, p_hole, p_emp

p_hole = .T.
p_emp  = .T.

USE (base_path+"PARMS")
d0  = DAT_START
d0_ = DAT_START
d1  = DATE()
USE (base_path+"SLR_TYPE") ORDER TAG RANG
t_numb = 0
SCAN
  t_numb = t_numb+1
  DIMENSION t_names(t_numb), t_codes(t_numb)
  t_names(t_numb) = "�"+NAME
  t_codes(t_numb) = CODE
ENDSCAN
USE

IF .NOT. Set_P()
  RETURN
ENDIF

DO Prep_Tmp

IF Del_Empty()
  DO P_Doc
ENDIF

CLOSE DATABASES
*DELETE FILE (tmpo_path+"TMP002.DBF")


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
*�  [ ] ���甌� 皋��� ��腮甄キ┤   �
*�                                �
*�< OK Ctrl-W > < �皖���碎瘴 Esc >�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, sw
ex = 1
sw = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 36, "�砒モ �� ��腮甄キ�鍖"
DO Sun_Bord WITH 2, 3, 4, 32, " ��皀燿�� ��� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  5 SAY "�" GET d0 PICTURE "@D"
@ 3, 18 SAY "��" GET d1 PICTURE "@D"
@ 5,  4 GET sw PICTURE "@*C ���甌� 皋��� ��腮甄キ┤" VALID C_List()
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

IF d1 > DATE()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� 瓮��き� 皰�讓� "+DATE()+"..."
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

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                       ��玻オ罔� 甎�瓷� ��ム�罔�.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 07.09.98 14:46:07 夕
PROCEDURE C_List
PRIVATE i
IF .NOT. sw
  RETURN
ENDIF

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

*  �┓� ��腮甄キ┤
*敖陳陳陳陳陳陳陳陳�
*�                 �
*�                 �
*�                 �
*�                 �
*�                 �
*�                 �
*�                 �
*青陳陳陳陳陳陳陳陳�
* Enter - �皙モ�碎
*   Esc - �諷��

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, LEN(t_names(1))+4, "�┓� ��腮甄キ┤"
@ 1, 1 CLEAR TO WROWS()-1, WCOLS()-1
@ WROWS()-2, 0 SAY PADC("Enter - �皙モ�碎", WCOLS())
@ WROWS()-1, 0 SAY PADC("Esc - �諷��", WCOLS())

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
i = 1
DO WHILE .T.
  i = MAX(i,1)
  i = MIN(i, t_numb)
  @ 1, 1 MENU t_names, t_numb, 7
  READ MENU TO i
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF LEFT(t_names(i), 1) = " "
    t_names(i) = "�"+SUBSTR(t_names(i),2)
  ELSE
    t_names(i) = " "+SUBSTR(t_names(i),2)
  ENDIF
  i = i+1
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
sw = .F.
SHOW GET sw
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     ��爼�牀���┘ ∇ガキ���� �����.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 09.09.98 16:11:53 夕
PROCEDURE Prep_Tmp
PRIVATE i, s0, s1

DO Wt_Mess WITH "��ぁ�爲� ����諷..."

USE (base_path+"SLR_MOVE")
DELETE FILE (tmpo_path+"TMP001.DBF")
DELETE FILE (tmpo_path+"TMP001.CDX")

COPY FOR BETWEEN(DOC_DATE, d0, d1) TO (tmpo_path+"TMP001")

USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (tmpo_path+"TMP001") EXCLUSIVE
INDEX ON STR(CODE)+DTOS(DOC_DATE) TAG ORD
SET RELATION TO PS_CODE INTO PERSONS

SELECT 0
DELETE FILE (tmpo_path+"TMP002.DBF")
CREATE DBF (tmpo_path+"TMP002")  ;
  ( DOC_N  C( 7), ;
    DOC_D  D,     ;
    L_TYPE C( 1), ;
    MAN    C(30), ;
    SUM_P  N(10,2), ;
    NOTE   C(24)  )

SELECT TMP001
s0 = 0
FOR i = 1 TO t_numb
  IF LEFT(t_names(i),1) = " "
    LOOP
  ENDIF
  s1 = 0
  SEEK STR(t_codes(i))
  SELECT TMP002
  APPEND BLANK
  REPLACE MAN WITH SUBSTR(t_names(i),2), L_TYPE WITH "0"
  SELECT TMP001
  SCAN REST WHILE CODE = t_codes(i)
    SELECT TMP002
    APPEND BLANK
    REPLACE DOC_N  WITH LEFT(TMP001.DOC_NUM,1)+ALLTRIM(SUBSTR(TMP001.DOC_NUM,2)), ;
            DOC_D  WITH TMP001.DOC_DATE, ;
            L_TYPE WITH "1", ;
            MAN    WITH ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+". "+LEFT(PERSONS.S_NAME,1)+".", ;
            SUM_P  WITH Crpt_2(TMP001.TOTAL), ;
            NOTE   WITH TMP001.NOTE
    s1 = s1+SUM_P
    SELECT TMP001
  ENDSCAN
  SELECT TMP002
  APPEND BLANK
  REPLACE L_TYPE WITH "2", SUM_P WITH s1
  s0 = s0+s1
  SELECT TMP001
ENDFOR

SELECT TMP002
APPEND BLANK
REPLACE L_TYPE WITH "3", SUM_P WITH s0
SELECT PERSONS
USE
SELECT TMP001
USE
DELETE FILE (tmpo_path+"TMP001.DBF")
DELETE FILE (tmpo_path+"TMP001.CDX")
SELECT TMP002
DO Wt_Mess
RETURN

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
*�[ ] �ョ�碎 �竅硅� 皋��� ��腮甄キ┤�
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
@ 3, 2 GET p_emp PICTURE "@*C �ョ�碎 �竅硅� 皋��� ��腮甄キ┤"
@ 5, 6 GET p_hole PICTURE "@*C �ョ�碎 ������� �砒モ�"
@ 7, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE
PRIVATE n_sav
IF ex = 1
  SELECT TMP002
  IF .NOT. p_emp
    PRIVATE n_sav
    SET FILTER TO L_TYPE $ "02"
    SCAN
      IF L_TYPE = "0"
        n_sav = RECNO()
      ENDIF
      IF L_TYPE = "2"
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
    DELETE ALL FOR L_TYPE = "1"
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
PRIVATE cnt, s000

s000 = 0

p_ln = 60
fnt  = "1"
n_cp = 1
lft  = 0
ffeed = -1
p_drctry = 0

DO Ini_Prn WITH "��腮甄キ��", p_ln, lft, n_cp, fnt, ffeed, p_drctry

cnt = 0

SELECT TMP002
SCAN
  cnt = cnt+1
  DO CASE
  CASE L_TYPE = "0"
    @ PROW()+1, 0 SAY "----------- "+ALLTRIM(MAN)
  CASE L_TYPE = "1"
    @ PROW()+1, 0 SAY "   "+ ;
      DTOC(DOC_D)+" "+DOC_N+" "+MAN+STR(SUM_P,11,2)+" "+NOTE
  CASE L_TYPE = "2"
    @ PROW()+1, 0 SAY  ;
    PADR("�皰�� �� 皋�� ��腮甄キ┤",IIF(SET("DATE") = "ON", 53, 51))+STR(SUM_P,10,2)
    s000 = s000+SUM_P
  CASE L_TYPE = "3"
    @ PROW()+1, 0 SAY  ;
    PADR("�皰�� �� ぎ�祠キ矣",IIF(SET("DATE") = "ON", 53, 51))+STR(s000,10,2)
  ENDCASE
  IF cnt >= p_ln-10
    EJECT
    cnt = 0
  ENDIF

ENDSCAN

DO Term_Prn WITH ""

RETURN
