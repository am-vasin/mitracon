*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� New_Ver      ��о�｀砒┴ ��むォ ��瓱�           06.02.98 14:13:53 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               �痰���→� ��〓� ▲珀┬ �牀������ �牀��Θ.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE New_Ver

PRIVATE wk_dir, src_dir, n_old, n_new, ex, max_sw, f_name

max_sw = 5

wk_dir = PADR(SET("DEFAULT")+CURDIR(),60)
n_old = Flt_Sw(ALLTRIM(wk_dir))

src_dir = PADR("D:\VASIN\SALE\SALE\",60)
n_new  = IIF(n_old > 4, 1, n_old+1)

ex = 1

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 �ぅ ��絎え矚� ����� ▲珀�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � ....:....!....:....!....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� 敖 �磴� 竅����←│�ガ ��≪� ▲珀�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�               �オ竕ゥ ����Ε�┘ �ムオ�鈑�皀�� 99                 �
*�                                                                  �
*�               ��〓� ����Ε�┘ �ムオ�鈑�皀��   99                 �
*�                                                                  �
*�                 < OK Ctrl-W > < �皖���碎瘴 Esc >                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 14, 70, "�痰���→� ��〓� ▲珀┬ �牀������ �������"
DO Sun_Bord WITH 2, 3, 4, 66, " �ぅ ��絎え矚� ����� ▲珀�� "
DO Sun_Bord WITH 5, 3, 7, 66, " �磴� 竅����←│�ガ ��≪� ▲珀�� "
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  5 GET src_dir VALID Tst_Dir1()
@  6,  5 GET wk_dir  VALID Tst_Dir2()
@  8, 17 SAY "�オ竕ゥ ����Ε�┘ �ムオ�鈑�皀��" GET n_old ; 
              PICTURE "@Z 99" WHEN .F.
@ 10, 17 SAY "��〓� ����Ε�┘ �ムオ�鈑�皀��  " GET n_new ;
              PICTURE "@Z 99" VALID Tst_Sw()

@ 12, 19 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Form()

IF ex = 1
  f_name = "SALE"+ALLTRIM(STR(n_new))+".APP"
  DELETE FILE (ALLTRIM(wk_dir)+f_name)
  COPY FILE (ALLTRIM(src_dir)+"SALE.APP") TO (ALLTRIM(wk_dir)+f_name)
  FOR i = 1 TO max_sw
    DELETE FILE (ALLTRIM(wk_dir)+ALLTRIM(STR(i))+".SW")
  ENDFOR
  CREATE DBF (ALLTRIM(wk_dir)+ALLTRIM(STR(n_new))+".SW") (FFF C(1))
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      蹍▲爲� �竄� � APP-������.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.02.98 14:39:33 夕
PROCEDURE Tst_Dir1
PRIVATE mss

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF .NOT. File_O(ALLTRIM(src_dir)+"SALE.APP")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� 礫������� え爛�皰爬� �モ ����� �爬��Ε���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        蹍▲爲� ��｀腑�� �竄�.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.02.98 14:39:33 夕
PROCEDURE Tst_Dir2
PRIVATE mss

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF .NOT. Is_Dir(ALLTRIM(wk_dir))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� え爛�皰爬� 〓≡� �� 痺薀痰≪モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

n_old = Flt_Sw(ALLTRIM(wk_dir))
n_new  = IIF(n_old > 4, 1, n_old+1)

SHOW GET n_old

SHOW GET n_new

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     蹍▲爲� ���ム� ��〓� ▲珀┬.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.02.98 14:39:33 夕
PROCEDURE Tst_Sw
PRIVATE mss

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF .NOT. BETWEEN(n_new, 1, max_sw)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���ム ▲珀┬ ぎ�Ε� °碎 �� 1 ぎ "+  ;
           ALLTRIM(STR(max_sw))+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF n_new = n_old
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���ム ▲珀┬ ぎ�Ε� �皓���碎瘴 �� 皀�竕ィ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                ｯ爛ぅ�錺� 皀�竕ゥ ����Ε�┘ 礫���皀��.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.02.98 14:23:08 夕
PROCEDURE Flt_Sw
PARAMETER pth
PRIVATE i, v

v = 0
FOR i = 1 TO max_sw
  IF File_O(ALLTRIM(pth)+ALLTRIM(STR(i))+".SW")
    v = i
    EXIT
  ENDIF
ENDFOR

RETURN v

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        蹍▲爲� 筮爼� � 罐���.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.02.98 14:39:33 夕
PROCEDURE Tst_Form
PRIVATE mss

IF .NOT. Tst_Sw()
  RETURN .F.
ENDIF

IF .NOT. Tst_Dir1()
  RETURN .F.
ENDIF

RETURN .T.
