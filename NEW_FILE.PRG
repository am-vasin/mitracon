*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� New_File     ��о�｀砒┴ ��むォ ��瓱�           16.02.97 15:16:27 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               �襦�� �竄� � ━キ� か� 甌Г���ガ��� �����.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE New_File

PARAMETER st_path, ext, titl, sz, fileName
&& �����讚�� Л�腑�┘ か� �竄�, ������〓� ���� � 矗ァ礇��� �牀痰���痰〓.

PRIVATE drv, wk_path, mss, drv_list, i, titl_w, sz_w, ext_w
PRIVATE sp_f, sp_h, f_name

sp_f = 0
sp_h = 0
f_name = SPACE(14)
IF TYPE("fileName") = "C"
  f_name = PADR(fileName,14)
ENDIF

*
*    ���甌� 竅矗��痰�
*
drv = Drv_Lst()
i = 0
DO WHILE LEN(drv) > 0
  i = i+1
  DIMENSION drv_list(i)
  drv_list(i) = LEFT(drv,2)
  drv = SUBSTR(drv,3)
ENDDO

*
*    ��爼�珮ガ ━� え瓷� � �竄�
*
IF TYPE("titl") = "C"
  titl_w = ALLTRIM(titl)
ELSE
  titl_w = "�襦���ガ え爛�皰爬�..."
ENDIF

IF TYPE("ext") = "C"
  ext_w = "."+ALLTRIM(ext)
ELSE
  ext_w = "."
ENDIF

IF TYPE("sz") = "N"
  sz_w = sz
ELSE
  sz_w = 0
ENDIF

IF TYPE("st_path") = "C"
  wk_path = ALLTRIM(st_path)
ELSE
  wk_path = "\"
ENDIF

IF EMPTY(wk_path)
  wk_path = "\"
ELSE
  wk_path = IIF(RIGHT(wk_path,1) = "\", wk_path, wk_path+"\")
ENDIF

IF .NOT. Disk_OK(wk_path)     && �� � ��瑙お� え瓷モ�...
  wk_path = "\"
ENDIF

DO WHILE .T.
  wk_path = FULLPATH(wk_path)   && ���竍�ガ ����覃 �竄� �...
  drv = LEFT(wk_path, 2)        &&    �襪カ錺� む��� 
  wk_path = SUBSTR(wk_path,3)   &&      � 甌＝癶キ�� �竄�

  IF LEN(wk_path) > 1            && ����� 皰 �竄� 礫����...
    IF .NOT. Is_Dir(drv+wk_path) &&       �牀▲瑙ガ ィ� ����腮�
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"��������� ���� え爛�皰爬� �� 痺薀痰≪モ!"
      mss(3) = CHR(0)+"�ムガ ��爿ア竡."
      mss(4) = ""
      DO Out_Mess WITH 7, "mss"
      wk_path = "\"
      LOOP
    ENDIF
  ENDIF
  IF ASCAN(drv_list, drv) = 0
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"�� 痺薀痰≪モ 竅矗��痰�� "+drv+"!"
    mss(3) = CHR(0)+"�ムガ ��爿ア竡 え爛�皰爬� 皀�竕ィ� 竅矗��痰��."
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
    wk_path = "\"
    LOOP
  ENDIF
  EXIT
ENDDO

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
DEFINE WINDOW No_Space FROM 0, WCOLS("")-50 TO 1, WCOLS("")-5 NONE SHADOW COLOR SCHEME 7
ACTIVATE WINDOW No_Space NOSHOW

@ 0, 0 SAY PADC("��������! �爛＜モ瘴 "+  ;
             ALLTRIM(TRANSFORM(sz_w, "99 999 999 999"))+" ����.", WCOLS())
@ 1, 0 SAY PADC("�� 轤�� 竅矗��痰▲ 痰��讓� �モ!", WCOLS())

ACTIVATE SCREEN

*         �襦���ガ え爛�皰爬�...
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 �オ竕�� え爛�皰爬� 陳陳陳陳陳陳陳陳陳陳朕 �
*� � ....:....1....:....2....:....3....:....4 � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�     ��爛�皰爬�      敖 �オ竕┤ え瓷 陳陳陳朕 �
*� 敖陳陳陳陳陳陳陳朕  �     敖陳陳陳陳朕     � �
*� � 12345678 12345 �  �     �          �     � �
*� � 12345678 12345 �  �     青陳陳陳陳潰     � �
*� � 12345678 12345 �  � �瓮�� 99 999 999 999 � �
*� � 12345678 12345 �  � �〓�. 99 999 999 999 � �
*� � 12345678 12345 �  青陳陳陳陳陳陳陳陳陳陳潰 �
*� � 12345678 12345 �   ��� ����� 12345678.123  �
*� � 12345678 12345 �                           �
*� � 12345678 12345 �     <   OK Ctrl-W    >    �
*� � 12345678 12345 �                           �
*� � 12345678 12345 �     < �皖���碎瘴 Esc >    �
*� 青陳陳陳陳陳陳陳潰                           �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

&&   ｡褊←錺� � ������錺� ���� ゛����
PRIVATE wk_list, nm_w, list_p, drv_p, ex

DIMENSION wk_list(1)

drv_p = ASCAN(drv_list, drv)
list_p = 1
nm_w = drv_list(drv_p)+wk_path
DO Dir_List WITH nm_w
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 19, 50, titl_w
DO Sun_Bord WITH 2, 3, 4, 46
@ 2, 5 SAY " �オ竕�� え爛�皰爬� "
DO Sun_Bord WITH 5, 23, 11, 46
@ 5, 25 SAY " �オ竕┤ え瓷 "
@  9,25 SAY "�瓮��"
@ 10,25 SAY "�〓�."
@ 5,  7 SAY "��爛�皰爬�"
DO Show_Space

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  3,  5 EDIT nm_w SIZE 1, 40 WHEN .F. COLOR (SCHEME(13,2))
@  6, 30 GET  drv_p  FROM drv_list PICTURE "@^" SIZE 1,  10 VALID Exit_D()
@  6,  4 GET  list_p FROM wk_list  PICTURE "@&" SIZE 12, 18 VALID Exit_P()  ;
              WHEN ALEN(wk_list) > 1 .OR. .NOT. EMPTY(wk_list(1))
@ 12, 23 SAY "��� �����" GET f_name VALID Tst_F_Name()
@ 14, 25 GET  ex PICTURE "@*VT \!   OK Ctrl-W    ;\? �皖���碎瘴 Esc " SIZE 1, 16, 1

READ CYCLE VALID Enough_Sp()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RELEASE WINDOW No_Space

RETURN IIF(ex = 1, drv+wk_path+UPPER(ALLTRIM(f_name)), "")

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                         蹍▲爲� ━キ� �����.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 16.02.97 15:33:08 夕
PROCEDURE Tst_F_Name

PRIVATE mss

IF ex # 1 .OR. READKEY() % 256 = 12 .OR. EMPTY(f_name)
  RETURN .T.
  nm_w = (drv_list(drv_p)+wk_path+UPPER(ALLTRIM(f_name)))
  SHOW GET nm_w
ENDIF

IF .NOT. "." $ f_name
  f_name = PADR(ALLTRIM(f_name)+ext_w, LEN(f_name))
ENDIF
nm_w = (drv_list(drv_p)+wk_path+UPPER(ALLTRIM(f_name)))
SHOW GET nm_w

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 蹍▲爲� ����腮� �牀痰���痰�� �� え瓷�.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 13.02.97 16:38:28 夕
PROCEDURE Enough_Sp

PRIVATE mss
IF ex # 1 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF sp_f < sz_w
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� え瓷� �� ぎ痰�皰膈� �メ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(f_name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��� � �� 礫����� ━� �����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF File_O(drv_list(drv_p)+wk_path+UPPER(ALLTRIM(f_name)))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ����� ���� 礒� メ碎, 磴��━? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN .F.
  ENDIF
  DELETE FILE (drv_list(drv_p)+wk_path+UPPER(ALLTRIM(f_name)))
ENDIF

RETURN Tst_F_Name()

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      �襤�� �牀痰���痰�� �� え瓷�.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 13.02.97 16:38:28 夕
PROCEDURE Show_Space

DO Disk_Space WITH drv_list(drv_p), sp_f, sp_h
@  9, 31 SAY sp_h PICTURE "99 999 999 999"
@ 10, 31 SAY sp_f PICTURE "99 999 999 999"
IF sp_f < sz_w
  SHOW WINDOW No_Space TOP
ELSE
  HIDE WINDOW No_Space
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 蹍������ �諷��� ├ 甎�瓷� え爛�皰爬�.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 13.02.97 13:43:53 夕
PROCEDURE Exit_P

PRIVATE prmpt, s

s = READKEY() % 256

IF s # 15
  RETURN .T.
ENDIF

prmpt = ALLTRIM(LEFT(wk_list(list_p),8))

DO CASE
CASE EMPTY(prmpt)      && ����� �� 甌ぅ爨�� え爛�皰爬�
  RETURN .T.
CASE prmpt == "."      && �����!
  wk_path = "\"
CASE prmpt == ".."     && �▲璢!
  wk_path = LEFT(wk_path, RAT("\", wk_path, 2))
OTHERWISE
  wk_path = wk_path+prmpt+"\"
ENDCASE

nm_w = drv_list(drv_p)+wk_path
DO Dir_List WITH nm_w
nm_w = nm_w+UPPER(ALLTRIM(f_name))
SHOW GET nm_w
list_p = 1
SHOW GET list_p
DO Show_Space

RETURN .F.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      蹍������ �諷��� ├ む����.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 13.02.97 13:59:53 夕
PROCEDURE Exit_D

IF drv_list(drv_p) $ "A:B:"
  IF .NOT. Disk_OK(drv_list(drv_p))
    drv_p = ASCAN(drv_list, drv)
    SHOW GET drv_p
    RETURN .F.
  ENDIF
ENDIF

IF drv # drv_list(drv_p)
  drv = drv_list(drv_p)
  list_p = 1
  wk_path = "\"
  nm_w = drv_list(drv_p)+wk_path
  SHOW GET nm_w
  DO Dir_List WITH nm_w
  list_p = 1
  SHOW GET list_p
ENDIF

DO Show_Space

RETURN .T.
