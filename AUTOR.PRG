*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Autor        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ProductManager か� ��Ж罔�                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 12/18/2001 様�
PROCEDURE Autor
PARAMETERS pos_code

PRIVATE s_sav
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
*            PPPP nnnnnnnnnNnnnnnnnnnNnnnnn ffffFfff
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                                             �
*� Product manager: ....:....!....:....!....:....!....:....!.. �
*�                              ..:....!....:....!..           �
*� ���祚�� ....:....!....:....                                 �
*� ���� ��痰�→� ....:....!                                    �
*� ��Ж罔� 甅閧� � �牀├〓め癶�                                �
*�         < �ェム� > < ����腮� � 筥������ >  < Esc >          �
*�                                                             �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, ttl, tmpName, tmpSname, pur, obs, d_wait
ex = 3
pur = ""
obs = ""
d_wait = {}
*------------------------------------------------------------------------

s_sav = SELECT()

IF File_O(path_comm+"PURCHASE.DBF")
  SELECT 0
  USE (path_comm+"PURCHASE") ALIAS PUR_2702 ORDER TAG NAME_DATE AGAIN
  SET FILTER TO PURFLAG = "K"
ENDIF

SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ALIAS ST_2712 ORDER TAG CODE
SEEK pos_code
ttl = ALLTRIM(ST_2712.PRODUCER)
ttl = ALLTRIM(ST_2712.PREFIX)+" "+ ;
      ALLTRIM(ST_2712.NAME)+" "+ ;
      IIF(EMPTY(ttl), "", " /"+ttl+"/")

IF USED("PUR_2702")
  SELECT PUR_2702
  IF SEEK(ST_2712.NAME+ST_2712.PREFIX+ST_2712.PRODUCER, "PUR_2702")
    d_wait = PUR_2702.DATE
  ENDIF
ENDIF

SELECT 0
IF File_O(path_comm+"SUBJECT.DBF")
  USE (path_comm+"SUBJECT") SHARED AGAIN ALIAS SJ_2712 ORDER TAG STO_CODE
ELSE
  RETURN .F.
ENDIF  
SEEK pos_code
IF NOPUR = "X"
  pur = "�� ���祚�碎 �┴����"
ENDIF
IF NOPUR = "x"
  pur = "�� ���祚�碎 瓮����"
ENDIF
IF NOPUR = " "
  pur = "�モ ���爛��"
ENDIF
IF OBSOLETE
  obs = "��閧� � �牀├〓め癶�"
ENDIF
tmpName = SJ_2712.AUTHOR
*IF .NOT. EMPTY(tmpName)
*  SELECT 0
*  USE (base_path+"PERSONS") SHARED AGAIN ALIAS P_2712 ORDER TAG ALIAS
*  IF SEEK(tmpName)
*    tmpName = ALLTRIM(P_2712.FAMILY)+" "+ALLTRIM(P_2712.NAME)+" "+ALLTRIM(P_2712.S_NAME)
*  ELSE
*    tmpName = ""
*  ENDIF
*  USE
*ENDIF

SELECT ST_2712

IF EMPTY(tmpName)
  tmpName = "�モ!"
ENDIF
PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO D_Win_N WITH 11, 68, ttl

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3,  4 SAY "Product manager:"
@ 3, 21 SAY tmpName COLOR (SCHEME(14,2))
@ 5,  4 SAY "���祚��  "
@ 5, 14 SAY pur COLOR (SCHEME(14,2))
IF .NOT. EMPTY(d_wait)
  @ 7,  4 SAY "���� ��痰�→� "
  @ 7, 19 SAY DTOC(d_wait) COLOR (SCHEME(14,2))
ELSE
  @ 7,  4 SAY "��痰�→�"
  @ 7, 14 SAY "� ゛└��茱� ∇ガ� �� �Θ��モ瘴" COLOR (SCHEME(14,2))
ENDIF
IF .NOT. EMPTY(obs)
  @ 8,  3 SAY obs COLOR (SCHEME(14,2))
ENDIF
DO WHILE  .T.
  @ WROWS()-2, 13 GET ex PICTURE "@*HT \! �ェム� ;\? ����腮� � 筥������ ;\? �諷�� ESC"
 
  READ CYCLE
  IF LASTKEY() = 27
    EXIT
  ENDIF
  IF ex = 3
    EXIT
  ENDIF
  IF ex = 1
    DO Rep_Pos WITH 0
  ENDIF
  IF ex = 2
    DO Rep_Pos WITH 1
  ENDIF
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

SELECT ST_2712
USE
SELECT SJ_2712
USE
IF USED("PUR_2702")
  SELECT PUR_2702
  USE
ENDIF
SELECT (s_sav)
RETURN
