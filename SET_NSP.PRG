*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Set_Nsp      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�         �甅���→� 痺��� ���. ぅ�ィ か� �����き�� (��瘍モ ���).         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 10.04.2000 様�
PROCEDURE Set_Nsp
PARAMETERS pFirm, pNum, pDate

PRIVATE d_year, s_sav

IF TYPE("pDate") = "D"
  d_year = LEFT(DTOS(pDate),4)
ENDIF
IF TYPE("pDate") = "N"
  d_year = STR(pDate,4)
ENDIF
IF TYPE("inv_d") = "C"
  d_year = pDate
ENDIF

s_sav = SELECT()

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS S_0410
IF .NOT. SEEK(pFirm+d_year+pNum)
  USE
  SELECT (s_sav)
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �....:....!....:....!....:....!....:....!....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� �祠�� �� �����き�� 999999999.99   蹍罐�� ��� 999.99 �
*�                                                      �
*� ����膈�� ��痰� 999999999.99   �祠�� ��� 999999999.99 �
*�                                                      �
*�           < OK Ctrl-W > < �皖���碎瘴 Esc >           �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, n_part, s_nsp
t_sum  = TOTAL_R
p_nsp  = NSP_
n_part = KASSA
s_nsp  = ROUND(n_part*NSP_/100, 2)
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 11, 58, "�����き�� N "+LEFT(DOC_NUM,4)+"-"+  ;
                        ALLTRIM(SUBSTR(DOC_NUM, 5))+"/"+FIRM
DO Sun_Bord WITH  2,  3,  4, 54, " ��┘�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  4 GET CUS_NAME WHEN .F.
@ 5,  3 SAY "�祠�� �� �����き��" GET TOTAL_R WHEN .F. PICTURE "999999999.99"
@ 5, 37 SAY "蹍罐�� ���" GET NSP_ WHEN .F. PICTURE "999.99"
@ 7,  3 SAY "����膈�� ��痰�" GET n_part VALID T_Val() PICTURE "999999999.99"
@ 7, 33 SAY "�祠�� ���" GET s_nsp WHEN .F. PICTURE "999999999.99"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  REPLACE KASSA WITH n_part
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
USE
SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� T_Val        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     蹍▲爲� □イキ���� Л�腑���.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10.04.2000 陳�
PROCEDURE T_Val

PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF ABS(n_part) > ABS(TOTAL_R)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����膈�� ��痰� �� ��Ε� �爛�謌�碎 �♂ォ 痺���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF SIGN(n_part) # SIGN(TOTAL_0) .AND. n_part # 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����膈�� ��痰� ぎ�Ν� ━モ� 皰� Ε Л��, 艪� � �♂�� 痺���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

s_nsp  = ROUND(n_part*NSP_/100, 2)
SHOW GET s_nsp

RETURN .T.
