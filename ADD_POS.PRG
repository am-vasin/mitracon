*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Add_Pos      ��о�｀砒┴ ��むォ ��瓱�           07.05.97 13:21:35 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ����←キ┘ ��Ж罔� � ���キ���矣珮.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Add_Pos
PARAMETERS prf1, nm1, prod1, div1, is_s
EXTERNAL ARRAY d_names, d_codes

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

PRIVATE ex, prf_w, nm_w, prd_w, add1_w, add2_w
PRIVATE div_w, round_w, round_w_m, spec_w, stop_w, n_firm, n_div
PRIVATE sel_sav
sel_sav = SELECT()
SELECT 0
USE (base_path+"DEVELOP") ALIAS DEV_ADD AGAIN ORDER TAG B_NAME
SELECT 0
USE (base_path+"STOCK") ALIAS STO_ADD AGAIN

                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&

prf_w   = IIF(TYPE("prf1") = "C", prf1, "")
prf_w   = PADL(ALLTRIM(prf_w), FSIZE("PREFIX","STO_ADD"))
nm_w    = IIF(TYPE("nm1") = "C", nm1, "")
nm_w    = PADR(ALLTRIM(nm_w), FSIZE("NAME","STO_ADD"))
n_firm  = IIF(TYPE("prod1") = "C", prod1, "")
add1_w  = SPACE(FSIZE("ADDRESS","STO_ADD"))
add2_w  = SPACE(FSIZE("MEMO_ST","STO_ADD"))
div_w   = 1
IF TYPE("div1") = "N"
  div_w   = MAX(ASCAN(d_codes, div1), 1)
ENDIF
n_div   = d_names(div_w)
round_w = 1
round_w_m = 1
IF TYPE("is_s") = "C"
  spec_w  = IIF(is_s = "S", 2, 1)
ELSE
  spec_w  = 1
ENDIF
stop_w  = "��оヨキ�"
n_firm = SPACE(8)
prd_w = 1
IF SEEK(PADR(n_firm, FSIZE("B_NAME", "DEV_ADD")), "DEV_ADD")
  prd_w = DEV_ADD.CODE
ENDIF
SELECT DEV_ADD
USE
SELECT STO_ADD

*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*楽陳 ��爲�牀→� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳� ��爼� 陳朕�
*崖  臼臼 臼臼憶臼越臼臼憶臼越臼臼憶臼越臼臼� 臼臼憶臼  崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*楽� �むメ� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖 �甅�↓�� 臼臼憶�??  �������皀�讚覃 臼臼憶臼越臼臼憶 崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*���Гカ 臼臼憶臼越臼臼憶 �����→� 臼臼�  �皴竅� 臼臼憶臼�
*�   敖 ��ユ. ���モ�� 陳陳陳� 敖 蹍���� 陳陳陳陳陳陳�   �
*�   �  ( ) �皀�. 痰����珥  � �      ��оヨキ�       �   �
*�   �  ( ) ���. 痰����珥   � �  "蹍．�" - �ムオ�.  �   �
*�   青陳陳陳陳陳陳陳陳陳陳潰 青陳陳陳陳陳陳陳陳陳陳潰   �
*�           < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 15, 59, "�������皀 ��゛�聶"

*      �〓� ���ォ ゛����
*
DO Sun_Bord WITH 2, 2, 4, 56, "� ��爲�牀→� "
@  2, 51 SAY " ��爼� "
@  3, 10 GET prf_w PICTURE "@J"
@  3, 15 GET nm_w
@  3, 51 GET n_firm VALID Develop(prd_w)

DO Sun_Bord WITH 5, 2, 7, 55, " �むメ� "
@  6,  4 SAY "�甅�↓��" GET add1_w PICTURE Addr_Pic()
@  6, 24 SAY "�������皀�讚覃" GET add2_w

@  8,  2 SAY "��Гカ" GET n_div WHEN Ch_Line(2, div_w, d_names, "��Гカ�")
@  8, 26 SAY "�����→�" GET round_w PICTURE "99999"
@  8, 41 SAY " �皴竅�" GET round_w_m PICTURE "9999999"

DO Sun_Bord WITH 9, 6, 12, 28, " ��ユ. ���モ�� "
@ 10,  9 GET spec_w PICTURE "@*R ���. 痰����珥;�皀�. 痰����珥"
DO Sun_Bord WITH 9, 30, 12, 52, " 蹍���� "
@ 11, 33 SAY '"蹍．�" - �ムオ�.'
@ 10, 37 GET stop_w PICTURE "@M ��оヨキ�,���爛薀��" WHEN .F.

@ 13, 13 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blnk()
IF ex = 1
  ex = RECNO()
ELSE
  ex = 0
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

SELECT STO_ADD
USE
SELECT (sel_sav)

RETURN ex

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            �襦�� ├ 甎�瓷�.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.05.97 16:29:04 夕
PROCEDURE Ch_Line
PARAMETERS mode, p, a_lst, ttl
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

PRIVATE point, n_h, n_m, ex
*------------------------------------------------------------------------

n_h = ALEN(a_lst)
DIMENSION a_lst(n_h)
n_m = MIN(n_h, WROWS("")-8)
point = p
PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH n_m+3, LEN(a_lst(1))+4, ttl

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

DO WHILE .T.
  point = MAX(point,1)
  point = MIN(point,n_h)
  @ 1, 1 MENU a_lst, n_h, n_m
  READ MENU TO point
  ex = READKEY() % 256
  IF ex = 15
    p = point
    EXIT
  ENDIF
  IF ex = 12
    EXIT
  ENDIF
ENDDO

POP KEY
RELEASE WINDOW (win_name)

IF mode = 1
  n_firm = a_lst(p)
  SHOW GET n_firm
ELSE
  n_div = a_lst(p)
  SHOW GET n_div
ENDIF

RETURN .F.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      蹍������ �諷��� ├ ゛����.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 08.05.97 11:51:06 夕
PROCEDURE Tst_Blnk
PRIVATE mss, i0, i1, np
EXTERNAL ARRAY d_codes
IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰 �� ��Ж罔� � �竅硅� ━キガ? �� ���-�─磴� ゥ ��М※皀!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

*
*    蹍▲瑙ガ ����腮� � ���キ���矣爛
*
prf_w = PADL(ALLTRIM(prf_w), FSIZE("PREFIX","STO_ADD"))
SET ORDER TO TAG NAME_P
IF SEEK(nm_w+prf_w+LEFT(n_firm, FSIZE("PRODUCER")))
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ��Ж罔� 礒� 痺薀痰▲�! �����皀 艪�-�─磴� む磽��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

*  �襪カ錺� 罔籥�≪� ��痰�...
i0 = 1
DO WHILE i0 <= LEN(nm_w)
  IF SUBSTR(nm_w, i0, 1) $ "0123456789"
    EXIT
  ENDIF
  i0 = i0+1
ENDDO
IF i0 <= LEN(nm_w)
  i1 = LEN(nm_w)
  DO WHILE i1 > 0
    IF SUBSTR(nm_w, i1, 1) $ "0123456789"
      EXIT
    ENDIF
    i1 = i1-1
  ENDDO
  np = SUBSTR(nm_w, i0, i1-i0+1)
ELSE
  np = ""
ENDIF

SET ORDER TO TAG CODE
GO BOTT
mss = CODE+1
APPEND BLANK
REPLACE CODE      WITH mss,             ;
        PRO_CODE  WITH prd_w,           ;
        PRODUCER  WITH n_firm,          ;
        PREFIX    WITH prf_w,           ;
        NAME      WITH nm_w,            ;
        NUM_PART  WITH np,              ;
        ADDRESS   WITH add1_w,          ;
        PREFIX1   WITH SeekName(prf_w), ;
        NAME1     WITH SeekName(nm_w),  ;
        PRODUCER1 WITH SeekName(n_firm),;
        MEMO_ST   WITH add2_w,          ;
        DIV_CODE  WITH d_codes(div_w),  ;
        QNT_ROUND WITH round_w,         ;
        QNT_PACK  WITH round_w_m,       ;
        STOP_SALE WITH IIF(stop_w="��оヨキ�"," ","*"), ;
        STOP_PUR  WITH IIF(spec_w=1," ","S"), ;
        DATE_ON   WITH DATE(),          ;
        WHO_ADD   WITH user,            ;
        WHERE_ADD WITH arm
FLUSH

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG DUE_NAM ALIAS L_DUE AGAIN
IF SEEK(STO_ADD.NAME+STO_ADD.PREFIX+STO_ADD.PRODUCER)
  SCAN REST WHILE STO_ADD.NAME     = NAME   .AND. ;
                  STO_ADD.PREFIX   = PREFIX .AND. ;
                  STO_ADD.PRODUCER = PRODUCER
    REPLACE CODE WITH STO_ADD.CODE
  ENDSCAN
ENDIF
USE
SELECT STO_ADD

RETURN .T.
