*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Fil_Pric     ��о�｀砒┴ ��むォ ��瓱�           18.06.98 10:20:08 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        �痰���→� 罐� か� 筥�����.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Fil_Pric

PARAMETER f_code, p_type0, vlt0, mute
*
* f_code - ��� 筥�����, メ�� �矚竄痰≪モ - �襦��碎 ├ 甎�瓷�.
* p_type0- �┓ 罐��: 1 - �� �キ閧�, 2 - ��皰���, 3 - 牀Л�膈��
* vlt0   - ���鈞�: 1 - 珮゛�, 2 - USD
* mute   - ����※碎 ≡鍼┤ �襤�� �� 蹣���
*
PRIVATE f_point, f_names, f_list, s_sav, n_fil, mss
PRIVATE tmpName1, tmpName2

s_sav = SELECT()

*
*      ��爼�珮ガ 甎�甌� 筥������.
*
SELECT 0
USE (base_path+"CUSTOMER") ALIAS CS18698 AGAIN
n_fil = 0
SCAN FOR RYN # " "
  n_fil = n_fil+1
  DIMENSION f_names(n_fil), f_list(n_fil)
  f_names(n_fil) = B_NAME
  f_list(n_fil)  = CODE
ENDSCAN
USE
IF n_fil = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� 筥�����..."
  mss(3) = ""
  IF .NOT. mute
    DO Out_Mess WITH 7, "mss"
  ENDIF
  SELECT (s_sav)
  RETURN
ENDIF

*
*    �����讚�� Л�腑�┘ か� 筥�����
*
f_point = 1
IF TYPE("f_code") = "N"
  f_point = ASCAN(f_list,f_code)
  f_point = MAX(1, f_point)
ENDIF

*
*   ���腑��� か� 筮爼�
*
PRIVATE vlt, p_type, is_dis, dis_p, dis_prc, manual, ex, dis_1, dis_n, dis_q
vlt = 1
p_type = 2
IF TYPE("p_type0") = "N"
  p_type = p_type0
ENDIF

is_dis = .F.
dis_p = 1
manual = .F.
dis_prc = 0
ex = 1
*
*   �ムョキ� 甎�瓷��-����皰〓�.
*
DIMENSION dis_1(1), dis_n(1)
dis_1(1) = 0
dis_n(1) = ""
dis_q = 0
IF File_O(base_path+"WK_TIT.DBF")
  USE (base_path+"WK_TIT") ALIAS WK18698 AGAIN
  SCAN
    dis_q = dis_q+1
    DIMENSION dis_1(dis_q), dis_n(dis_q)
    dis_1(dis_q) = RECNO()
    dis_n(dis_q) = L_NAME
  ENDSCAN
  USE
ENDIF
USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE ALIAS RT18698 AGAIN
IF SEEK(f_list(f_point))
  vlt = IIF(VALUTA="$", 2, 1)
ENDIF
IF TYPE("vlt0") = "N"
  vlt = vlt0
ENDIF
USE

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                   敖陳陳陳陳陳陳陳朕            �
*�            �┼��� �....:....!....:.�            �
*�                   青陳陳陳陳陳陳陳潰            �
*� 敖 �キ� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ( ) �� �キ閧�   ( ) ｯ皰���   ( ) ��Л�膈�� � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 ���鈞� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �           ( ) �磧��   ( ) �����琺           � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� 敖 [ ] ��┐�� �� 甎�瓷� 陳陳陳陳陳陳陳陳陳陳陳� �
*� � 敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� � �
*� � �....:....!....:....!....:....!....:....! � � �
*� � 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� � �
*� �            蹍罐�� 瓷┐�� 999.99            � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�              [ ] �竍��� ��玻オ罔�               �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

IF .NOT. mute
  PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
  DO Prp_Nav_2
  DO D_Win_N WITH 20, 53, "�痰���→� 罐� か� 筥�����"
  DO Sun_Bord WITH  5, 3,  7, 49, " �キ� "
  DO Sun_Bord WITH  8, 3, 10, 49, " ���鈞� "
  DO Sun_Bord WITH 11, 3, 16, 49, "                      "
  @ 3, 14 SAY "�┼���"
*------------------------------------------------------------------------
  *      �〓� ���ォ ゛����
  *
  @  2, 21 GET f_point PICTURE "@^" FROM f_names SIZE 1, 18 WHEN TYPE("f_code") # "N"
  @  6,  5 GET p_type PICTURE "@*RH �� �キ閧�  ;ｯ皰���  ;��Л�膈��"
  @  9, 15 GET vlt PICTURE "@*RH �磧��  ;�����琺"
  @ 11,  6 GET is_dis PICTURE "@*C ��┐�� �� 甎�瓷�" WHEN dis_q # 0
  @ 12,  5 GET dis_p PICTURE "@^" FROM dis_n SIZE 1, 42 WHEN is_dis
  @ 15, 16 SAY "蹍罐�� 瓷┐��" GET dis_prc PICTURE "@Z 999.99" WHEN is_dis
  @ 17, 16 GET manual PICTURE "@*C �竍��� ��玻オ罔�"

  @ 18, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

  READ CYCLE
ENDIF
*--------------------------------------------------------------------------
IF ex = 1
  tmpName1 = SYS(3)
  PRIVATE p_d, p_r, tmp_p
  USE (base_path+"RYN_L") ALIAS RL18698 AGAIN ORDER TAG C_CODE
  SEEK STR(f_list(f_point),5)
  COPY REST WHILE CUS_CODE = f_list(f_point) TO (tmpo_path+tmpName1)
  IF p_type # 1
    IF .NOT. mute
      DO Wt_Mess WITH "�痰���→� 罐�"
    ENDIF
    SELECT 0
    USE (base_path+"RYN_L_T") ALIAS RT18698 AGAIN ORDER TAG CUS_CODE
    IF SEEK(f_list(f_point))
      REPLACE DATE_P WITH DATE(), VALUTA WITH IIF(vlt = 1, "�", "$")
    ENDIF
    USE (base_path+"STOCK") ALIAS ST18698 AGAIN ORDER TAG CODE
    SELECT RL18698
    SET RELATION TO CODE INTO ST18698
    SEEK STR(f_list(f_point),5)
    SCAN REST WHILE CUS_CODE = f_list(f_point)
      p_d = ST18698.PRICE
      p_r = IIF(own_val = "$", Clc_Rub(ST18698.PRICE), ST18698.PRICE_R)
      IF p_type = 3
        IF own_val = "$"
          p_d = RetPrice(ST18698.NAME, p_d, EMPTY(ST18698.STOP_PUR), IIF(EMPTY(ST18698.CLASS), ST18698.DIV_CODE, ST18698.CLASS))
          p_r = Clc_Rub(p_d)
        ELSE
          p_d = RetPrice(ST18698.NAME, ST18698.PRICE_R/kurs, EMPTY(ST18698.STOP_PUR), IIF(EMPTY(ST18698.CLASS), ST18698.DIV_CODE, ST18698.CLASS))
          p_r = Clc_Rub(p_d)
        ENDIF
      ENDIF
      REPLACE PRICE_R WITH p_r, PRICE_D WITH p_d
    ENDSCAN
    SELECT ST18698
    USE
    IF .NOT. mute
      DO Wt_Mess
    ENDIF
  ENDIF
  IF is_dis
    IF .NOT. mute
      DO Wt_Mess WITH "�痰���→� 瓷┐��"
    ENDIF
    dis_prc = (100-dis_prc)/100
    SELECT 0
    USE (base_path+"WK_LIST") ALIAS WK18698 AGAIN ORDER TAG LIST_NO
    SEEK dis_1(dis_p)
    SCAN REST WHILE LIST_NO = dis_1(dis_p)
      IF SEEK(STR(f_list(f_point),5)+STR(CODE,7), "RL18698")
        REPLACE RL18698.PRICE_R WITH RL18698.PRICE_R*dis_prc, ;
                RL18698.PRICE_D WITH RL18698.PRICE_D*dis_prc
      ENDIF
    ENDSCAN
    USE
    IF .NOT. mute
      DO Wt_Mess
    ENDIF
  ENDIF
  IF manual
    DO Mnl_Prc WITH f_list(f_point)
  ENDIF
  SELECT 0
  USE (tmpo_path+tmpName1) ALIAS NEW_P
  tmp_p = "C:\"
  tmp_p = New_File(tmp_p, "DBF", "��Ж罔� � ���覓� 罐����")
  IF .NOT. EMPTY(tmp_p)
    DO New_P WITH tmp_p
  ENDIF
  SELECT RL18698
  USE
  SELECT NEW_P
  tmpName1 = DBF()
  USE
  DELETE FILE (tmpName1)
ENDIF

SELECT (s_sav)
IF .NOT. mute
  POP KEY
  RELEASE WINDOW (win_name)
ENDIF
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� New_P        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         ���� � ���覓� 罐����.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 19.02.2002 陳�
PROCEDURE New_P
PARAMETERS f_name

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_2219
SELECT 0
IF vlt = 1
  CREATE DBF(f_name) ;
    ( CODE       N( 7), ;
      PREFIX     C(FSIZE("PREFIX", "STO_2219")),   ;
      NAME       C(FSIZE("NAME", "STO_2219")),     ;
      PRODUCER   C(FSIZE("PRODUCER", "STO_2219")), ;
      OLD_PRICE  N(FSIZE("PRICE_R", "NEW_P"), 2),  ;
      NEW_PRICE  N(FSIZE("PRICE_R", "NEW_P"), 2)   )
ELSE
  CREATE DBF(f_name) ;
    ( CODE       N( 7), ;
      PREFIX     C(FSIZE("PREFIX", "STO_2219")),   ;
      NAME       C(FSIZE("NAME", "STO_2219")),     ;
      PRODUCER   C(FSIZE("PRODUCER", "STO_2219")), ;
      OLD_PRICE  N(FSIZE("PRICE_D", "NEW_P"), 4),  ;
      NEW_PRICE  N(FSIZE("PRICE_D", "NEW_P"), 4)   )
ENDIF
USE (f_name) ALIAS TMP_001
SELECT RL18698
SET ORDER TO C_CODE
SELECT NEW_P

SCAN
  IF .NOT. SEEK(STR(f_list(f_point),5)+STR(CODE,7), "RL18698")
    LOOP
  ENDIF
  IF vlt = 1
    IF RL18698.PRICE_R = PRICE_R
      LOOP
    ENDIF
  ELSE
    IF RL18698.PRICE_D = PRICE_D
      LOOP
    ENDIF
  ENDIF
  = SEEK(CODE, "STO_2219")
  SELECT TMP_001
  APPEND BLANK
  REPLACE CODE       WITH STO_2219.CODE,     ;
          PREFIX     WITH STO_2219.PREFIX,   ;
          NAME       WITH STO_2219.NAME,     ;
          PRODUCER   WITH STO_2219.PRODUCER, ;
          OLD_PRICE  WITH IIF(vlt = 1, NEW_P.PRICE_R, NEW_P.PRICE_D), ;
          NEW_PRICE  WITH IIF(vlt = 1, RL18698.PRICE_R, RL18698.PRICE_D)
  SELECT NEW_P
ENDSCAN

SELECT TMP_001
USE
SELECT STO_2219
USE
RETURN
