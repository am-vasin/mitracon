*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�              �ணࠬ�� �����⮢�� ������ ��� �. �. � �. �.            �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Sov_List

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

PRIVATE ex, dat_1, dat_2, dat_1b, dat_2b, nm_w, nm_d, q, p, cnt     &&
                     &&   ���塞 � ������塞 ���� ������
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
*���������������������������������������������������������������Ŀ
*�    �� ���室 ��������������������������������������������Ŀ   �
*�    �            C  ��/��/����  ��  ��/��/����             �   �
*�    ��������������������������������������������������������   �
*� ��� ᯨ᮪ ����権   ....:....1....:....2....:....3....:....4 �
*�                                                               �
*� �㤠 ��襬 १���� ....:....1....:....2....:....3....:....4 �
*�                                                               �
*�              < OK Ctrl-W > < �⪠������ Esc >                 �
*�����������������������������������������������������������������
PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 11, 67, "�����ઠ ��室� ��� �. �. � �. �."
DO Sun_Bord WITH 2, 6, 4, 61
@ 2, 8 SAY " ���室 "

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 19 SAY "C              ��"
@ 3, 22 GET dat_1 PICTURE "@D"
@ 3, 38 GET dat_2 PICTURE "@D"
@ 5, 3 SAY "��� ᯨ᮪ ����権  " GET nm_d PICTURE "@K"
@ 7, 3 SAY "�㤠 ��襬 १����" GET nm_w PICTURE "@K"
@ 9, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

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
DO Wt_Mess WITH "�����⮢�� ������..."

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
  @ WROWS()-1, 0 SAY PADC("����������� "+ALLTRIM(STR(cnt,6)), WCOLS())
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
  @ WROWS()-1, 0 SAY PADC("���室 "+ALLTRIM(STR(cnt,6)), WCOLS())
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

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�              �ணࠬ�� �஢�ન ��室� �� ������.                     �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Tst_Inp
PRIVATE mss, i

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF dat_1 > dat_2
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��砫쭠� ��� ����� ����筮�: ⠪ �� �뢠��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_1 < dat_1b
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����ୠ� ��砫쭠� ���: ����� � ��室� �࠭���� ⮫쪮 � "+TRANSFORM(dat_1b,"@E")+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_2 > dat_2b
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����ୠ� ����筠� ���: ᥣ���� �� ⮫쪮 "+TRANSFORM(dat_2b,"@E")+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(nm_w)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���-� �� �祭� ����⭮, �㤠 ����襬 १����? ������ �� ࠧ ��筨�!"
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
  mss(2) = CHR(0)+"���� "+ALLTRIM(mmm)+" �� �������!"
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
  mss(2) = CHR(0)+"���� ����権 �� 㯮�冷祭 �� �����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF File_O(ALLTRIM(nm_w))
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"���� "+ALLTRIM(nm_w)+" 㦥 ����! �� �㤥� ������?"
  mss(3) = CHR(0)+"   [ ������ ] [ ������ ] [ �⪠����� ]"
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