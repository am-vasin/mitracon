*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Set_Curs     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   �痰���→�/�牀甃�矗 �窶甌� ぎ�����.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 18.02.1999 様�
PROCEDURE Set_Curs
PARAMETERS let_corr

PRIVATE curs_c, curs_n
curs_c = ""
curs_n = ""
IF .NOT. Ch_Curs(curs_c, curs_n)
  RETURN
ENDIF

DO View_C

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_c       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �牀������� �窶��.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 18.02.1999 陳�
PROCEDURE View_c

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

DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_9218 AGAIN

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS A_9218 AGAIN

SELECT 0
USE (base_path+"CURS_SET") ORDER TAG HRON ALIAS C_9218 AGAIN
SET RELATION TO WHO INTO P_9218
SET RELATION TO WHERE INTO A_9218 ADDITIVE
GO BOTTOM

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Type OF (menu_name) PROMPT "�┓ �窶��" KEY Ctrl-A, "Tab"
ON SELECTION PAD Type OF (menu_name) DO Sw_Mode WITH "Type"

IF let_corr
  DEFINE PAD Corr OF (menu_name) PROMPT "�痰���→� �窶��" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"
ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
*敖陳����陳陳陳�爛�鐵陳�窶當��皰 竅����※�陳陳陳陳陳陳��｀腑� �メ皰陳朕
*���.��.���� ��:��:�� 99.999 NNNNNNNNNNNNNNNNNNNNNNNN AAAAAAAAAAAAAAAA�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ln, wd, w_c, w_n
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 68

DO D_Wins WITH ln, wd, "", 0, 0
@ 1, 2 SAY "陳�����陳陳陳�爛�鐵陳�窶當��皰 竅����※�陳陳陳陳陳陳��｀腑� �メ皰"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M")
    @ 0, 0 SAY PADC(ALLTRIM(curs_n), WCOLS()) COLOR SCHEME 5
    DO Prp_Nav_1
    IF let_corr
      ON KEY LABEL Enter KEYBOARD CHR(23)
    ENDIF
    ON KEY LABEL Tab DO Sw_Mode WITH "Type"

*
*    BROWSE - �キ�
*
    BROWSE KEY curs_c   ;
           FIELDS DATE_SET:H="",   ;
                  TIME_SET:H="",   ;
                  CURS:H="":P="99.999", ;
                  P_9218.FAMILY:H="",   ;
                  A_9218.ARM_N:H="",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && �痰���→� �窶��

    DO New_Val
    what_do = "List"

  CASE what_do = "Type"    && ��キ� 皋��

    w_c = curs_c
    w_n = curs_n
    IF Ch_Curs(w_c, w_n)
      curs_c = w_c
      curs_n = w_n
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
SELECT C_9218
USE
SELECT P_9218
USE
SELECT A_9218
USE
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� New_Val      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �痰���→� �窶��.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 18.02.1999 陳�
PROCEDURE New_Val
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
*�                                                 �
*� �オ竕ゥ Л�腑�┘ 99.999  ��〓� Л�腑�┘ 99.999  �
*�                                                 �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, c_flt, c_new
c_new = SELECT()
c_flt = Get_Curs(curs_c)
SET ORDER TO TAG HRON ASCENDING   && ����爬��!
c_new = 0
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 53, ALLTRIM(curs_n)

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3, 3 SAY "�オ竕ゥ Л�腑�┘ "+TRANSFORM(c_flt, "99.999")+"  ��〓� Л�腑�┘"  ;
      GET c_new PICTURE "99.999"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Curs()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  SELECT C_9218
  APPEND BLANK
  REPLACE B_NAME   WITH curs_c, ;
          DATE_SET WITH DATE(), ;
          TIME_SET WITH TIME(), ;
          CURS     WITH c_new,  ;
          WHO      WITH user,   ;
          WHERE    WITH arm
  IF ALLTRIM(curs_c) == "CURS"
    PRIVATE ss, cnt0
    ss = SELECT()
    SELECT 0
    USE (base_path+"STOCK") ORDER TAG CODE
    DO Wt_Mess WITH "�ムメ腑� 罐�..."
    cnt0 = 0
    SCAN
      cnt0 = cnt0+1
      IF cnt0 % 100 = 0
        @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt0)),WCOLS())
      ENDIF
      REPLACE PRICE_R    WITH Clc_Rub(PRICE, c_new),  ;
              PRICE_O_R  WITH Clc_Rub(PRICE_O, c_new),;
              PRICE_O1_R WITH Clc_Rub(PRICE_O1, c_new)
    ENDSCAN
    DO Wt_Mess
    USE
    SELECT (ss)
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_curs     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍▲爲� ��玻オ皚�痰� �窶��.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 18.02.1999 陳�
PROCEDURE Tst_curs
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF c_new <= 0
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"�牀膈� �÷�皋皀瘡 � ��о�｀砒┴�!"
  mss(3) = CHR(0)+"ｭ �祚�� ≡� ��荐 ぎ���琺 �� 竄牀キ����(!) �窶痺!"
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF c_flt # 0
  IF ABS(c_new-c_flt)/c_flt > .15
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+" �窶� ├�キ┼瘴 ｀�ゥ, 腑� �� 15%! �皰 ���※�讚�? [ �モ! ] [ �� ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

RETURN .T.
