*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ ��ぃ�皰→� ����諷 か� �. �. � �. �.            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Sov_List

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※�∽┬
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

PRIVATE ex, dat_1, dat_2, dat_1b, dat_2b, nm_w, nm_d, q, p, cnt     &&
                     &&   ｡褊←錺� � ������錺� ���� ゛����
USE (base_path+"PARMS")
dat_1b = DAT_START
dat_2b = DATE()
dat_1  = MAX(dat_1b, dat_2b-91)
dat_2  = DATE()
ex   = 1             &&
nm_d = SPACE(40)     &&
nm_w = SPACE(40)     &&
IF File_O(prmo_path+"SOV_LIST.MEM")
  RESTORE FROM (prmo_path+"SOV_LIST.MEM") ADDITIVE
ENDIF
USE
*------------------------------------------------------------------------
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�    敖 ��痳�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕   �
*�    �            C  ��/��/����  ��  ��/��/����             �   �
*�    青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰   �
*� �ぅ 甎�甌� ��Ж罔�   ....:....1....:....2....:....3....:....4 �
*�                                                               �
*� �磴� ��茱� 爛с�赳�� ....:....1....:....2....:....3....:....4 �
*�                                                               �
*�              < OK Ctrl-W > < �皖���碎瘴 Esc >                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 11, 67, "��ぁ�爲� ��痳��� か� �. �. � �. �."
DO Sun_Bord WITH 2, 6, 4, 61
@ 2, 8 SAY " ��痳�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 19 SAY "C              ��"
@ 3, 22 GET dat_1 PICTURE "@D"
@ 3, 38 GET dat_2 PICTURE "@D"
@ 5, 3 SAY "�ぅ 甎�甌� ��Ж罔�  " GET nm_d PICTURE "@K"
@ 7, 3 SAY "�磴� ��茱� 爛с�赳��" GET nm_w PICTURE "@K"
@ 9, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Inp()

POP KEY
RELEASE WINDOW (win_name)

IF ex = 2
  RETURN
ENDIF
SAVE ALL LIKE nm_* TO (prmo_path+"SOV_LIST.MEM")
*--------------------------------------------------------------------------
nm_w = ALLTRIM(nm_w)
SET EXCLUSIVE ON
CREATE DBF (nm_w)   ;
    ( CODE   N( 7), ;
      PREFIX C( 4), ;
      NAME   C(35), ;
      PRODUCER C(8), ;
      PRICE  N(10,4), ;
      QNT    N( 6),  ;
      SUM    N(12,4),;
      BRON   N( 6),  ;
      SALED  N( 6),  ;
      MARK   C( 1)   )
USE (nm_w) ALIAS RESULT
SET EXCLUSIVE OFF


SELECT 0
USE (nm_d) ORDER TAG CODE ALIAS LIST

SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME
SET RELATION TO CODE INTO LIST
DO Wt_Mess WITH "��ぃ�皰→� ����諷..."

cnt = 0
SCAN FOR FOUND("LIST")

  SELECT RESULT
  APPEND BLANK
  REPLACE CODE   WITH STOCK.CODE,  ;
          PREFIX WITH STOCK.PREFIX,;
          NAME   WITH STOCK.NAME,  ;
          PRODUCER WITH STOCK.PRODUCER, ;
          QNT    WITH STOCK.QNT,   ;
          PRICE  WITH STOCK.PRICE, ;
          SALED  WITH 0,           ;
          BRON   WITH STOCK.QNT_SAVE+STOCK.QNT_REZ

  SELECT STOCK
  cnt = cnt+1
  @ WROWS()-1, 0 SAY PADC("���キ���矣�� "+ALLTRIM(STR(cnt,6)), WCOLS())
ENDSCAN

USE (base_path+"SALE") ORDER TAG CODE

SELECT RESULT
cnt = 0
SCAN FOR SEEK(CODE, "SALE")
  q = 0
  p = 0
  SELECT SALE
  SCAN REST WHILE CODE = RESULT.CODE FOR PRICE_R # 0
    IF BETWEEN(DOC_DATE, dat_1, dat_2)
      q = q+QNT
      p = p+PRICE_D*QNT
    ENDIF
  ENDSCAN
  SELECT RESULT
  REPLACE SALED WITH q, SUM WITH p
  cnt = cnt+1
  @ WROWS()-1, 0 SAY PADC("��痳�� "+ALLTRIM(STR(cnt,6)), WCOLS())
ENDSCAN

CLOSE DATABASES

DO Wt_Mess

PRIVATE l00, c00

l00 = FLOOR((WROWS("")-3)/2)
c00 = FLOOR((WCOLS("")-10)/2)

DEFINE WINDOW www FROM l00, c00 TO l00+2, c00+9 NONE SHADOW COLOR SCHEME 5
ACTIVATE WINDOW www

@ 1, 3 SAY "���!"

= INKEY(3, "M")
RELEASE WINDOW www

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ �牀▲爲� �諷��� ├ ゛����.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tst_Inp
PRIVATE mss, i

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF dat_1 > dat_2
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�����讚�� ���� ｀�跏� ���ョ���: ��� �� °��モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_1 < dat_1b
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�アム��� �����讚�� ����: ����襯 � ��痳�ぅ 縲��閧瘴 皰�讓� � "+TRANSFORM(dat_1b,"@E")+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_2 > dat_2b
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�アム��� ���ョ��� ����: 瓮��き� ラ� 皰�讓� "+TRANSFORM(dat_2b,"@E")+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(nm_w)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���-皰 �� �腑�� ���閧��, �磴� ����茱� 爛с�赳��? �����皀 ラ� ��� 竄�膈━!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
PRIVATE mmm, i, sw
mmm = ALLTRIM(nm_d)
IF .NOT. "." $ mmm
  mmm = mmm+".DBF"
ENDIF
IF .NOT. File_O(mmm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���� "+ALLTRIM(mmm)+" �� 痺薀痰≪モ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
USE (mmm)
i = 1
sw = .F.
DO WHILE .NOT. EMPTY(TAG(i))
  IF TAG(i) = "CODE"
    sw = .T.
    EXIT
  ENDIF
  i = i+1
ENDDO
USE
IF .NOT. sw
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���� ��Ж罔� �� 祚�瑙ぎ腑� �� �����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF File_O(ALLTRIM(nm_w))
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"���� "+ALLTRIM(nm_w)+" 礒� メ碎! �皰 ＜ぅ� ぅ��碎?"
  mss(3) = CHR(0)+"   [ ����━ ] [ ��キ━ ] [ �皖�Ε�瘴 ]"
  mss(4) = ""
  i = Out_Mess(7, "mss")
  IF i = 1
    DELETE FILE (ALLTRIM(nm_w))
    RETURN .T.
  ENDIF
  IF i = 3
    ex = 2
    RETURN .T.
  ENDIF
  RETURN .F.
ENDIF

RETURN .T.
