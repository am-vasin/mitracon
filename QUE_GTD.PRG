*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Que_Gtd      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�             ��痰���→� ��� �� �甅����┬ ↓ヨ�ィ� ���牀��.              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 06.04.2000 様�
PROCEDURE Que_Gtd

PRIVATE ext_name, sw_emp, sw_GTD, s_sav

STORE .F. TO sw_emp, sw_GTD
ext_name = ""
s_sav = SELECT()
SELECT 0

IF .NOT. Fill_Bl()		&& �痰�膈┴ � 甌痰�� �÷���硅��ガ諷 ��Ж罔�
  SELECT (s_sav)
  RETURN
ENDIF

IF .NOT. Prp_Link()		&& ��ぃ�皰→� ∇ガキ���� ����� か� �÷�｀皖�
  SELECT (s_sav)
  RETURN
ENDIF

DO Gtd_Link WITH .T.

DO Set_Gtd_0

SELECT GTD_LINK
USE
DELETE FILE (tmpo_path+"GTD_LINK.DBF")
DELETE FILE (tmpo_path+"GTD_LINK.CDX")

SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fill_Bl      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ������キ┘ ゛����.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 06.04.2000 陳�
PROCEDURE Fill_Bl

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 [ ] ���� � ������ ��Ж罔� 陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� 敖 �瓷�鈑�碎 ��Ж罔� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �  [ ] � ��� 〓 ≡ュ ��珥�閻  [ ] � �竅硅� ����腮ガ  � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�            < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, swTmp
ex = 1
swTmp = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 60, "�������皀 ゛���"
DO Sun_Bord WITH  2,  3,  4, 56, " [ ] ���� � ������ ��Ж罔� "
DO Sun_Bord WITH  5,  3,  7, 56, " �瓷�鈑�碎 ��Ж罔� "
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2,  6 GET swTmp PICTURE "@*C ���� � ������ ��Ж罔�" VALID G_File()
@ 3,  5 EDIT ext_name SIZE 1, 50 WHEN .F.
@ 6,  6 GET sw_GTD PICTURE "@*C � ��� 〓 ≡ュ ��珥�閻"
@ 6, 33 GET sw_emp PICTURE "@*C � �竅硅� ����腮ガ"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_File()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_File       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �襦�� ����� � ��ぁ�爲��.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 06.04.2000 陳�
PROCEDURE G_File

IF swTmp
  ext_name = Int_File(ext_name, "dbf", "���� � ������ ��Ж罔�")
  SHOW GET ext_name
  swTmp = .F.
  SHOW GET swTmp
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_File     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   蹍▲爲� ��玻オ皚�痰� ━キ� �����.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 06.04.2000 陳�
PROCEDURE Tst_File

IF ex = 2
  RETURN .T.
ENDIF

IF .NOT. File_O(ext_name)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �荐゛�瘡 � 礫����┘� �����..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Link     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�      ��ぃ�皰→� ∇ガキ���� ����� ��Ж罔� か� �÷�｀皖� �÷�｀皖�.      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 07.04.2000 陳�
PROCEDURE Prp_Link

PRIVATE f_code, swTmp
f_code = .T.

USE (ext_name) ALIAS SOURCE AGAIN	&& �痰�膈┴ ����諷

*
*  ｯ爛ぅ�錺� ━� ���� � ��ぎ� ��Ж罔�
IF TYPE("SOURCE.STO_CODE") = "N"
  f_code = "STO_CODE"
ENDIF
IF TYPE("f_code") # "C"
  IF TYPE("SOURCE.CODE") = "N"
    f_code = "CODE"
  ENDIF
ENDIF
IF TYPE("f_code") # "C"
  f_code = Get_Field()
ENDIF
IF TYPE("f_code") # "C"
  USE
  RETURN .F.
ENDIF
f_code = ALLTRIM(f_code)
DELETE FILE (tmpo_path+"GTD_LINK.DBF")
DELETE FILE (tmpo_path+"GTD_LINK.CDX")
SELECT 0
CREATE DBF (tmpo_path+"GTD_LINK") ;
  ( LINK     N(10), ;
    CODE     N( 7), ;
    MARK     L,     ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    GTD_NAME C(29), ;
    GTD_NUM  C(26), ;
    GTD_LINE C( 4), ;
    COUNTRY  C(16), ;
    B_GTD    L  )
SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE DESCENDING ALIAS P_0407 AGAIN
*  DESCENDING - か� 竅��爛��� �ムァ��� - �竅硅� ��� 瓷�爛� ≡ィ� � ��甄イ��� ��珥�閻
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_0407 AGAIN
SELECT SOURCE
SET RELATION TO &f_code INTO S_0407
IF sw_GTD
  SET RELATION TO &f_code INTO P_0407 ADDITIVE
ENDIF sw_GTD
SCAN FOR FOUND("S_0407")
  swTmp = .T.
  IF sw_emp .AND. S_0407.QNT+S_0407.QNT_SAVE+S_0407.QNT_REZ+S_0407.QNT_REAL = 0
    LOOP		&& 蹍�竅��ガ ��Ж罔� � �竅硅� ����腮ガ
  ENDIF
  IF sw_GTD		&& 蹍�竅��ガ ��Ж罔� � ������キ覓 ���
    SELECT P_0407
    swTmp = .F.
    SCAN REST WHILE CODE = S_0407.CODE
      IF EMPTY(GTD_NUM)
        swTmp = .T.
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
  IF swTmp
    SELECT GTD_LINK
    APPEND BLANK
    REPLACE LINK     WITH S_0407.CODE,   ;
            CODE     WITH S_0407.CODE,   ;
            MARK     WITH .T.,           ;
            PREFIX   WITH S_0407.PREFIX, ;
            NAME     WITH S_0407.NAME,   ;
            PRODUCER WITH S_0407.PRODUCER
  ENDIF
  SELECT SOURCE
ENDSCAN
USE
SELECT S_0407
USE
SELECT P_0407
USE
SELECT GTD_LINK
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �痰���瘡 �� �き�� ��Ж罔�..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"GTD_LINK.DBF")
  DELETE FILE (tmpo_path+"GTD_LINK.CDX")
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_Field    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                 ��皀���皋↓覃 �襦�� ���� ���� ��Ж罔�.                 �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 07.04.2000 陳�
PROCEDURE Get_Field

PRIVATE f_list, f_count, i, tmpVal

f_count = 0
i = 0
DO WHILE .T.
  i = i+1
  tmpVal = FIELD(i)
  IF EMPTY(tmpVal)
    EXIT
  ENDIF
  IF TYPE("SOURCE."+tmpVal) = "N"
    f_count = f_count+1
    DIMENSION f_list(f_count)
    f_list(f_count) = PADR(tmpVal, 40)
  ENDIF
ENDDO

IF f_count = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�������覃 ���� ���� �� 甌ぅ爨�� �� �き��� 腮甄�〓�� ����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
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
*���Θ皀 ━� ���� � ��ぎ� ��Ж罔�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!....:....!....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�       < OK Ctrl-W > < �皖���碎瘴 Esc >      �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, ln
ex = 1
ln = MIN(WROWS("")-10, f_count+6)
tmpVal = f_list(1)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ln, 49, "���Θ皀 ━� ���� � ��ぎ� ��Ж罔�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2, 3 GET tmpVal FROM f_list PICTURE "@&T" SIZE ln-4, 43
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex # 1
  mtpVal = .F.
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN tmpVal

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Gtd_0    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��＝癶キ�� ��瘁����→� ���.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 07.04.2000 陳�
PROCEDURE Set_Gtd_0

PRIVATE empSw, gtdSw
empSw = .T.
gtdSw = .F.

IF .NOT. F_Blank()
  RETURN
ENDIF

SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE ALIAS P_0407 AGAIN
SELECT GTD_LINK
SCAN FOR MARK
  IF SEEK(LINK, "P_0407")
    SELECT P_0407
    SCAN REST WHILE CODE = GTD_LINK.LINK
      IF .NOT. empSw .AND. QNT_REST = 0
        LOOP
      ENDIF
      IF .NOT. gtdSw .AND. .NOT. EMPTY(GTD_NUM)
        LOOP
      ENDIF
      REPLACE GTD_NUM  WITH GTD_LINK.GTD_NUM, ;
              GTD_LINE WITH GTD_LINK.GTD_LINE,;
              COUNTRY  WITH GTD_LINK.COUNTRY, ;
              B_GTD    WITH GTD_LINK.B_GTD
    ENDSCAN
    SELECT GTD_LINK
  ENDIF
ENDSCAN
SELECT P_0407
USE
SELECT GTD_LINK
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� F_Blank      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ������キ┘ ゛����.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 07.04.2000 陳�
PROCEDURE F_Blank
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
* 
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                               �
*� [ ] � �竅硅� ����腮ガ  [ ] � ������キ�覓 ���  �
*�                                               �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 51, "�痰���※碎 ��� か� ��珥┤"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  3 GET empSw PICTURE "@*C � �竅硅� ����腮ガ"
@ 3, 26 GET gtdSw PICTURE "@*C � ������キ�覓 ���"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
