*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� GTD_Exp      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 ��瘉�爛�┘ ��� �� ≡� ��珥┬ ��Ж罔�.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 04.04.2000 様�
PROCEDURE GTD_Exp
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
* ��瘁����→� ���
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�   蹍甃�矗キ� 999999 ��Ж罔�      �
*�   ���ぅ�� ��� か�:                �
*�   999999 ��Ж罔�, 999999 ��珥┤   �
*�                                   �
*� < OK Ctrl-W > < �皖���碎瘴 Esc >  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 39, "��瘁����→� ���"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2,  5 SAY "蹍甃�矗キ�        ��Ж罔�"
@ 3,  5 SAY "���ぅ�� ��� か�:"
@ 4, 12 SAY "��Ж罔�,        ��珥┤"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  PRIVATE cnt1, cnt2, cnt3, g0, c0, cc0, l0, sw0, s_sav, r_sav
  s_sav = SELECT()
  STORE 0 TO cnt1, cnt2, cnt3
  DELETE FILE (tmpo_path+"GTD_EXP.DBF")
  SELECT 0
  CREATE DBF (tmpo_path+"GTD_EXP") ;
          (   MARK     L,     ;
              CODE     N( 7), ;
              PREFIX   C( 4), ;
              NAME     C(39), ;
              PRODUCER C( 8), ;
              GTD_NUM  C(26), ;
              GTD_LINE C( 4), ;
              COUNTRY  C(16), ;
              COUNTRYC C( 3)  )
  SELECT 0
  USE (base_path+"ACT_S") ORDER TAG CODE DESCENDING ALIAS P_0404 AGAIN
  SELECT 0
  USE (base_path+"STOCK") ORDER TAG NAME ALIAS ST_0404 AGAIN
  SET RELATION TO CODE INTO P_0404
  SCAN
    cnt1 = cnt1+1
    g0 = ""
    c0 = ""
    l0 = ""
    sw0 = .F.
    IF FOUND("P_0404")
      SELECT P_0404
      r_sav = RECNO()
      SCAN REST WHILE CODE = ST_0404.CODE
        IF EMPTY(g0) .AND. .NOT. EMPTY(GTD_NUM)
          g0 = GTD_NUM
          c0 = COUNTRY
          cc0 = COUNTRYC
          l0 = GTD_LINE
        ENDIF
        IF EMPTY(GTD_NUM)
          sw0 = .T.
          cnt3 = cnt3+1
        ENDIF
      ENDSCAN
      IF .NOT. EMPTY(g0) .AND. sw0
        cnt2 = cnt2+1
        SELECT GTD_EXP
        APPEND BLANK
        REPLACE MARK     WITH .T.,              ;
                CODE     WITH ST_0404.CODE,     ;
                PREFIX   WITH ST_0404.PREFIX,   ;
                NAME     WITH ST_0404.NAME,     ;
                PRODUCER WITH ST_0404.PRODUCER, ;
                GTD_NUM  WITH g0, ;
                GTD_LINE WITH l0, ;
                COUNTRY  WITH c0, ;
                COUNTRYC WITH cc0
      ENDIF
      SELECT ST_0404
    ENDIF
    @ 2, 17 SAY cnt1 PICTURE "@Z 999999"
    @ 4,  5 SAY cnt2 PICTURE "@Z 999999"
    @ 4, 21 SAY cnt3 PICTURE "@Z 999999"
  ENDSCAN
  @ WROWS()-2, 2 SAY PADC("��Μ�皀 艪�-�─磴�...", WCOLS()-4)
  = INKEY(0, "HM")
  USE
  SELECT GTD_EXP
  IF RECCOUNT() = 0
    USE
    DELETE FILE (tmpo_path+"GTD_EXP.DBF")
  ENDIF
  SELECT P_0404
  USE
  SELECT (s_sav)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF USED("GTD_EXP")
  DO Set_M
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ��瘁��←錺� ���? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 1
    DO Wt_Mess WITH "��瘁��←錺�..."
    SELECT 0
    USE (base_path+"ACT_S") ORDER TAG CODE ALIAS P_0405
    SELECT GTD_EXP
    SCAN FOR MARK
      SELECT P_0405
      SEEK GTD_EXP.CODE
      REPLACE REST WHILE CODE = GTD_EXP.CODE FOR EMPTY(GTD_NUM) ;
              GTD_NUM  WITH GTD_EXP.GTD_NUM, ;
              GTD_LINE WITH GTD_EXP.GTD_LINE,;
              COUNTRY  WITH GTD_EXP.COUNTRY, ;
              COUNTRYC WITH GTD_EXP.COUNTRYC
      SELECT GTD_EXP
    ENDSCAN
    SELECT P_0405
    USE
    DO Wt_Mess
  ENDIF
  SELECT GTD_EXP
  USE
  DELETE FILE (tmpo_path+"GTD_EXP.DBF")
ENDIF
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_Marks    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�            蹍甃�矗 �｀÷���諷 ��Ж罔� � 竅����→� �皙モ��.            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.04.2000 陳�
PROCEDURE Set_M

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
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT GTD_EXP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "�皙モ�碎" KEY Ctrl-A, "蹍．�"
ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 51

DO D_Wins WITH ln, wd, "���ぅ�� ��Ж罔�...", 0, 1
what_do = "List"
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�M AAAA 0000000000111111111122222 PPPPPPPP�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖���� N陳陳陳陳陳陳陳陳�矗���陳陳陳陳陳陳�
*�....:....!....:....!....:. ....:....!....�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
@ WROWS()-3, 2 SAY "陳��� N陳陳陳陳陳陳陳陳�矗���"
GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL SpaceBar DO Sw_Mode WITH "Mark"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS M000 = IIF(MARK, "�", " "):1:H="", ;
                  PREFIX:4:H="",   ;
                  NAME:35:H="",    ;
                  PRODUCER:8:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "Mark"    && �皙モ��

    REPLACE MARK WITH .NOT. MARK
    SKIP
    IF EOF()
      GO BOTTOM
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-2,  4 SAY GTD_NUM
@ WROWS()-2, 29 SAY LEFT(COUNTRY,14)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
