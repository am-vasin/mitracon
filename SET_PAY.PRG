*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Set_Pay      ��о�｀砒┴ ��むォ ��瓱�           29.10.97 16:53:28 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           ��腮甄キ┘ ぅ�ィ.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Set_Pay
PARAMETERS p_code, ye_p, mn_p

PRIVATE sel_sav, is_dummy, lst_sav, s, ye, mn, mss, cur_year

sel_sav = SELECT()

SELECT 0
USE (base_path+"PARMS") ALIAS P_1405 AGAIN
cur_year = SLR_YEAR

USE (base_path+"MOULD") ORDER TAG MONTH ALIAS MLD_PAY AGAIN
IF RECCOUNT() < 2
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� 竄▲爨ぅ����� �メ閹�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT MLD_PAY
  USE
  SELECT (sel_sav)
  RETURN
ENDIF
  
IF TYPE("ye_p") = "N"
  ye = ye_p
ELSE
  ye = YEAR(DATE())
ENDIF

IF TYPE("mn_p") = "N"
  mn = mn_p
ELSE
  mn = MONTH(DATE())
ENDIF

SEEK STR(ye,4)+STR(mn,2)

IF .NOT. FOUND() .OR. RECNO() = RECCOUNT()
  GO BOTTOM
  SKIP -1
  mn = MONTH
  ye = YEAR
ENDIF

is_dummy = USED("DUMMY")
IF .NOT. is_dummy
  DO Use_Dummy
ENDIF

DIMENSION lst_sav(4)
IF USED("SLR_TYPE")
  lst_sav(1) = .T.
  SELECT SLR_TYPE
  lst_sav(2) = SET("FILTER")
  lst_sav(3) = SET("ORDER")
  lst_sav(4) = RECNO()
ELSE
  lst_sav(1) = .F.
  SELECT 0
  USE (base_path+"SLR_TYPE")
ENDIF
SET ORDER TO RANG
SET FILTER TO .NOT. EMPTY(TYPE)
IF EOF() .OR. BOF()
  GO TOP
ENDIF
IF EOF() .OR. BOF()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 甌Г��� �� �き��� 皋�� ��腮甄キ┤..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  IF lst_sav(1)
    s = lst_sav(2)
    SET FILTER TO &s
    s = lst_sav(3)
    SET ORDER TO &s
  ELSE
    USE
  ENDIF
  SELECT MLD_PAY
  USE
  RETURN
ENDIF

PUSH KEY CLEAR
IF .NOT. Chs_Type()
  IF lst_sav(1)
    s = lst_sav(2)
    SET FILTER TO &s
    s = lst_sav(3)
    SET ORDER TO &s
    IF BETWEEN(lst_sav(4), 1, RECCOUNT())
      GO lst_sav(4)
    ENDIF
  ELSE
    USE
  ENDIF
  IF .NOT. is_dummy
    SELECT DUMMY
    USE
  ENDIF
  SELECT MLD_PAY
  USE
  SELECT (sel_sav)
  POP KEY
  RETURN
ENDIF

POP KEY

DO CASE
CASE ALLTRIM(TYPE)=="1"
  DO Set_P_1 WITH p_code, ye, mn
CASE ALLTRIM(TYPE)=="2" .OR. ALLTRIM(TYPE)=="3"
  DO Set_P_23 WITH p_code, ye, mn
CASE ALLTRIM(TYPE)=="4" .OR. ALLTRIM(TYPE)=="5" .OR. ALLTRIM(TYPE)=="6"
  DO Set_P_456 WITH p_code, ye, mn
OTHERWISE
  DO (ALLTRIM(TYPE)) WITH p_code, ye, mn
ENDCASE

SELECT SLR_TYPE
IF lst_sav(1)
  s = lst_sav(2)
  SET FILTER TO &s
  s = lst_sav(3)
  SET ORDER TO &s
ELSE
  USE
ENDIF
IF .NOT. is_dummy
  SELECT DUMMY
  USE
ENDIF
SELECT MLD_PAY
USE
SELECT (sel_sav)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                         �襦�� 皋�� ��腮甄キ��.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 29.10.97 17:13:35 夕
PROCEDURE Chs_Type
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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, ex, rc
rc = RECNO()
ex = .F.
ln = 12
wd = 24

DO D_Wins WITH ln, wd, "�┓� ��腮甄キ┤", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS NAME:H="":24,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦��

    ex = .T.
    EXIT

  OTHERWISE

    SELECT SLR_TYPE
    IF BETWEEN(rc, 1, RECCOUNT())
      GO rc
    ENDIF
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN ex

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        ��腮甄キ┘ �ム〓�� 皋��.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 29.10.97 19:03:23 夕
PROCEDURE Set_P_1
PARAMETERS p_code, ye, mn

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
SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_PAY ORDER TAG CODE AGAIN
SEEK p_code
SELECT 0
USE (base_path+"PS_STAT") ALIAS PAY_STAT ORDER TAG CODE AGAIN
IF .NOT. SEEK(p_code)
  APPEND BLANK
  REPLACE CODE      WITH p_code, ;
          START     WITH Crpt_1(0,10,2), ;
          REST      WITH Crpt_1(0,10,2), ;
          SALARY    WITH PRS_PAY.SALARY, ;
          PREV_SUM  WITH Crpt_1(0,5),    ;
          PREV_SUMD WITH Crpt_1(0,5),    ;
          PAY_SUM   WITH Crpt_1(0,5),    ;
          PAY_VAL   WITH "�",            ;
          S_DATE    WITH PRS_PAY.DATE_IN

ENDIF

*                          < �┓ ��腮甄キ�� >
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��矗磴�┴ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!.... ....:....!....:. ....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�                                 敖陳陳陳陳朕敖陳陳朕               �
*�              �祠�� 999999.99 �� � ....:... 崖 1234 �               �
*�                                 青陳陳陳陳潰青陳陳潰               �
*�                敖 踸�ョ��┘ 陳陳陳陳陳陳陳陳陳陳朕                �
*�                �     ....:....!....:....!....     �                �
*�                青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰                �
*�                  < OK Ctrl-W > < �皖���碎瘴 Esc >                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, mn_list, ye_list, i, ye_numb, ye_w, mn_w, total, nt
ye_numb = 5
DIMENSION ye_list(ye_numb)
FOR i = 1 TO ye_numb
  ye_list(i) = STR(cur_year+i-ye_numb,4)
ENDFOR
nt = SPACE(24)

DIMENSION mn_list(12)
FOR i = 1 TO 12
  mn_list(i) = Month_R(i)
ENDFOR
mn_w = mn
ye_w = ASCAN(ye_list, STR(ye,4))
total= 0
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 13, 72, ALLTRIM(SLR_TYPE.NAME)
DO Sun_Bord WITH 2, 3, 4, 68, " ��矗磴�┴ "
DO Sun_Bord WITH WROWS()-5, 18, WROWS()-3, 53, " 踸�ョ��┘ "
@ 3, 5 SAY PRS_PAY.FAMILY-(" "+PRS_PAY.NAME)-(" "+PRS_PAY.S_NAME)
@ 6, 16 SAY "�祠�� 999999.99 ��"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 6, 22 GET total PICTURE "@Z 999999.99"
@ 5, 35 GET mn_w PICTURE "@^ " FROM mn_list SIZE 1, 12
@ 5, 47 GET ye_w PICTURE "@^ " FROM ye_list SIZE 1, 8
@ WROWS()-4, 24 GET nt
@ WROWS()-2, 20 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1 .AND. total # 0
  DO To_Inc WITH SLR_TYPE.CODE, p_code, 0, 0, total,   ;
                 SLR_TYPE.UNIT, VAL(ye_list(ye_w)), mn_w, nt
ENDIF
SELECT PRS_PAY
USE
SELECT PAY_STAT
USE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�              ��腮甄キ┘ ≒�牀�� � 矗モ譽�� 皋���.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 29.10.97 19:03:23 夕
PROCEDURE Set_P_23
PARAMETERS p_code, ye, mn

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
SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_PAY ORDER TAG CODE AGAIN
SEEK p_code
SELECT 0
USE (base_path+"PS_STAT") ALIAS PAY_STAT ORDER TAG CODE AGAIN
IF .NOT. SEEK(p_code)
  APPEND BLANK
  REPLACE CODE      WITH p_code, ;
          START     WITH Crpt_1(0,10,2), ;
          REST      WITH Crpt_1(0,10,2), ;
          SALARY    WITH PRS_PAY.SALARY, ;
          PREV_SUM  WITH Crpt_1(0,5),    ;
          PREV_SUMD WITH Crpt_1(0,5),    ;
          PAY_SUM   WITH Crpt_1(0,5),    ;
          PAY_VAL   WITH "�",            ;
          S_DATE    WITH PRS_PAY.DATE_IN

ENDIF

*                          < �┓ ��腮甄キ�� >
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��矗磴�┴ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!.... ....:....!....:. ....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� ��腮甄�碎 �� 99999.99 units_______  �� 9999999.99 �皰�� 9999999.99 �
*�                             敖陳陳陳陳朕敖陳陳朕                   �
*�                   �� �ム┏� � ....:... 崖 1234 �                   �
*�                             青陳陳陳陳潰青陳陳潰                   �
*�                敖 踸�ョ��┘ 陳陳陳陳陳陳陳陳陳陳朕                �
*�                �     ....:....!....:....!....     �                �
*�                青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰                �
*�                  < OK Ctrl-W > < �皖���碎瘴 Esc >                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ex, mn_list, ye_list, i, ye_numb, ye_w, mn_w, total, prc, q, nt
ye_numb = 5
DIMENSION ye_list(ye_numb)
FOR i = 1 TO ye_numb
  ye_list(i) = STR(cur_year+i-ye_numb,4)
ENDFOR
nt = SPACE(24)
total = 0
prc   = SLR_TYPE.PRICE
q     = 0
DIMENSION mn_list(12)
FOR i = 1 TO 12
  mn_list(i) = Month_R(i)
ENDFOR
mn_w = mn
ye_w = ASCAN(ye_list, STR(ye,4))
total= 0
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 14, 72, ALLTRIM(SLR_TYPE.NAME)
DO Sun_Bord WITH 2, 3, 4, 68, " ��矗磴�┴ "
DO Sun_Bord WITH WROWS()-5, 18, WROWS()-3, 53, " 踸�ョ��┘ "
@ 3, 5 SAY PRS_PAY.FAMILY-(" "+PRS_PAY.NAME)-(" "+PRS_PAY.S_NAME)
@ 5, 3 SAY "��腮甄�碎 �� 99999.99 units_______  �� 9999999.99 �皰��"
@ 5,25 SAY SLR_TYPE.UNIT
@ 7,21 SAY "�� �ム┏�"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 5, 16 GET q PICTURE "@Z 99999.99" VALID Set_Total()
@ 5, 42 GET prc PICTURE "@Z 9999999.99" VALID Set_Total() ;
              WHEN "3" $ SLR_TYPE.TYPE
@ 5, 59 GET total PICTURE "@Z 999999.99" WHEN .F.
@ 6, 31 GET mn_w PICTURE "@^ " FROM mn_list SIZE 1, 12
@ 6, 43 GET ye_w PICTURE "@^ " FROM ye_list SIZE 1, 8
@ WROWS()-4, 24 GET nt
@ WROWS()-2, 20 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1 .AND. total # 0
  DO To_Inc WITH SLR_TYPE.CODE, p_code, q, prc, total,   ;
                 SLR_TYPE.UNIT, VAL(ye_list(ye_w)), mn_w, nt
ENDIF
SELECT PRS_PAY
USE
SELECT PAY_STAT
USE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                             ��瘍モ 痺���.                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 29.10.97 21:02:46 夕
PROCEDURE Set_Total

total = q*prc
SHOW GET total

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�              ��腮甄キ┘ 腑癶ム皰�� - 茱痰��� 皋���.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 29.10.97 19:03:23 夕
PROCEDURE Set_P_456
PARAMETERS p_code, ye, mn

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
SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_PAY ORDER TAG CODE AGAIN
SEEK p_code
SELECT 0
USE (base_path+"PS_STAT") ALIAS PAY_STAT ORDER TAG CODE AGAIN
IF .NOT. SEEK(p_code)
  APPEND BLANK
  REPLACE CODE      WITH p_code, ;
          START     WITH Crpt_1(0,10,2), ;
          REST      WITH Crpt_1(0,10,2), ;
          SALARY    WITH PRS_PAY.SALARY, ;
          PREV_SUM  WITH Crpt_1(0,5),    ;
          PREV_SUMD WITH Crpt_1(0,5),    ;
          PAY_SUM   WITH Crpt_1(0,5),    ;
          PAY_VAL   WITH "�",            ;
          S_DATE    WITH RPS_PAY.DATE_IN

ENDIF

*                          < �┓ ��腮甄キ�� >
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��矗磴�┴ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!.... ....:....!....:. ....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�        敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕        �
*�        �  ｪ��� 99999    ���-〓 ��｀腮� きォ � �メ閹� 99  �        �
*�        青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰        �
*� ��腮甄�碎 �� 99999.99 units_______  �� 9999999.99 �皰�� 9999999.99 �
*�                             敖陳陳陳陳朕敖陳陳朕                   �
*�                   �� �ム┏� � ....:... 崖 1234 �                   �
*�                             青陳陳陳陳潰青陳陳潰                   �
*�                  < OK Ctrl-W > < �皖���碎瘴 Esc >                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ex, mn_list, ye_list, i, ye_numb, ye_w, mn_w, total, prc, q, nt
ye_numb = 5
DIMENSION ye_list(ye_numb)
FOR i = 1 TO ye_numb
  ye_list(i) = STR(cur_year+i-ye_numb,4)
ENDFOR
nt = SPACE(24)
total = 0
prc   = SLR_TYPE.PRICE
q     = 0
DIMENSION mn_list(12)
FOR i = 1 TO 12
  mn_list(i) = Month_R(i)
ENDFOR
mn_w = mn
ye_w = ASCAN(ye_list, STR(ye,4))
total= 0
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 17, 72, ALLTRIM(SLR_TYPE.NAME)
DO Sun_Bord WITH 2, 3, 4, 68, " ��矗磴�┴ "
DO Sun_Bord WITH 5, 10, 7, 61
DO Sun_Bord WITH WROWS()-5, 18, WROWS()-3, 53, " 踸�ョ��┘ "
@  3, 5 SAY PRS_PAY.FAMILY-(" "+PRS_PAY.NAME)-(" "+PRS_PAY.S_NAME)
@  8, 3 SAY "��腮甄�碎 �� 99999.99 units_______  �� 9999999.99 �皰��"
@  8,25 SAY SLR_TYPE.UNIT
@ 10,21 SAY "�� �ム┏�"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  8, 16 GET q PICTURE "@Z 99999.99" VALID Set_Total() WHEN Set_Pr()
@  8, 42 GET prc PICTURE "@Z 9999999.99" WHEN .F.
@  8, 59 GET total PICTURE "@Z 999999.99" WHEN .F.
@  9, 31 GET mn_w PICTURE "@^ " FROM mn_list SIZE 1, 12 VALID Set_Pr()
@  9, 43 GET ye_w PICTURE "@^ " FROM ye_list SIZE 1, 8 VALID Set_Pr()
@ WROWS()-4, 24 GET nt
@ WROWS()-2, 20 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1 .AND. total # 0
  DO To_Inc WITH SLR_TYPE.CODE, p_code, q, prc, total,   ;
                 SLR_TYPE.UNIT, VAL(ye_list(ye_w)), mn_w, nt
ENDIF
SELECT PRS_PAY
USE
SELECT PAY_STAT
USE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                  ��瘍モ 罐�� �� �甅����┬ ���キ��瑙.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 30.10.97 09:30:02 夕
PROCEDURE Set_Pr

PRIVATE sw, wg, d, s

d = 0
sw = .T.
IF .NOT. SEEK(ye_list(ye_w)+STR(mn_w,2), "MLD_PAY")
  sw = .F.
ELSE
  IF RECNO("MLD_PAY") = RECCOUNT("MLD_PAY")
    sw = .F.
  ELSE
    d = OCCURS("�", MLD_PAY.DAYS)
  ENDIF
ENDIF

wg = Crpt_2(PAY_STAT.SALARY)
IF sw .OR. "4" $ SLR_TYPE.TYPE
  prc = wg
  IF "5" $ SLR_TYPE.TYPE
    prc = ROUND(prc/d,2)
  ENDIF
  IF "6" $ SLR_TYPE.TYPE
    prc = ROUND(wg/d/8,2)
  ENDIF
  prc = prc*SLR_TYPE.KOEFF
ELSE
  prc = 0
  WAIT WINDOW "��������! ���キ��瑕 �� "+Month_R(mn_w)+" "+ye_list(ye_w)+ ;
              " �� 竄▲爨ぅ� ┼� �� 痺薀痰≪モ!"
ENDIF
SHOW GET prc
s = "ｪ��� "+ALLTRIM(STR(wg))+"    ���-〓 ��｀腮� きォ � �メ閹� "+ ;
    IIF(d = 0, "**", STR(d,2))
@ 6, 11 SAY PADC(s,50)
DO Set_Total

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                       ��腮甄キ┘ �皴竅��諷.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 29.10.97 19:03:23 夕
PROCEDURE Rest_Year
PARAMETERS p_code, ye, mn

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
SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_PAY ORDER TAG CODE AGAIN
SEEK p_code
SELECT 0
USE (base_path+"PS_STAT") ALIAS PAY_STAT ORDER TAG CODE AGAIN
IF .NOT. SEEK(p_code)
  APPEND BLANK
  REPLACE CODE      WITH p_code, ;
          START     WITH Crpt_1(0,10,2), ;
          REST      WITH Crpt_1(0,10,2), ;
          SALARY    WITH PRS_PAY.SALARY, ;
          PREV_SUM  WITH Crpt_1(0,5),    ;
          PREV_SUMD WITH Crpt_1(0,5),    ;
          PAY_SUM   WITH Crpt_1(0,5),    ;
          PAY_VAL   WITH "�",            ;
          S_DATE    WITH RPS_PAY.DATE_IN
ENDIF
IF EMPTY(S_DATE)
  REPLACE S_DATE WITH PRS_PAY.DATE_IN
ENDIF

*                          < �┓ ��腮甄キ�� >
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��矗磴�┴ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!.... ....:....!....:. ....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�   ｪ��� 99999 ��. ��痰� 999999.99 �皰�� 999999.99 �� ぅ�� 9999.99  �
*�                                                                    �
*�   ��腮甄�碎 �� 9999 ��� � ��.��.���� �� ��.��.���� �� 9999 きォ    �
*�                                                                    �
*�   �皴竅� 9999 きォ  �が. �皴竅� 9999 きォ  �皰�� 9999 きォ         �
*�                                                                    �
*�                  �皰�� �� 9999 きォ 9999999.99 $                   �
*�                                                                    �
*�                踸�ョ��┘ ....:....!....:....!....                 �
*�                                                                    �
*�                  < OK Ctrl-W > < �皖���碎瘴 Esc >                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ex, ye_w, total, prc, q, nt, slr_w, d1, d2, n_days1, n_days2, days1, days2
PRIVATE slr_w1, slr_w2
days1 = 0
days2 = 0
n_days2 = 0
slr_w1 = Crpt_2(PAY_STAT.SALARY)  && ｪ���
slr_w2 = Salary2(p_code, 3)       && �ぅ�讚�� ��痰�
slr_w  = slr_w1+slr_w2            && �皰��

d1    = PAY_STAT.S_DATE
d2    = Last_Day(p_code)          && ��甄イ�┤ ���琺硅� ぅ��
nt    = SPACE(24)

n_days1 = d2-d1+1
DO Adm WITH days1, days2, n_days2
q       = n_days1-n_days2
prc     = ROUND(slr_w/330,2)
ye_w    = YEAR(d2)
total   = q*prc

ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 17, 72, ALLTRIM(SLR_TYPE.NAME)
DO Sun_Bord WITH 2, 3, 4, 68, " ��矗磴�┴ "
@  3, 5 SAY PRS_PAY.FAMILY-(" "+PRS_PAY.NAME)-(" "+PRS_PAY.S_NAME)
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  5,  5 SAY "ｪ���" GET slr_w1 PICTURE "@Z 99999" VALID Calc_V()
@  5, 17 SAY "��. ��痰�" GET slr_w2 PICTURE "@Z 99999.99" VALID Calc_V()
@  5, 37 SAY "�皰��" GET slr_w PICTURE "@Z 99999.99" WHEN .F.
@  5, 53 SAY "�� ぅ��" GET prc PICTURE "@Z 9999.99" WHEN .F.
@  7,  5 SAY "��腮甄�碎 ��" GET ye_w PICTURE "@Z 9999" WHEN .F.
@  7, 23 SAY "��� �" GET d1 PICTURE "@D" WHEN .F.
@  7, 40 SAY "��" GET d2 PICTURE "@D" VALID Calc_V(.T.)
@  7, 54 SAY "��" GET n_days1 PICTURE "@Z 9999" WHEN .F.
@  7, 62 SAY "きォ"
@  9,  5 SAY "�皴竅�" GET days1 PICTURE "@Z 9999" WHEN .F.
@  9, 17 SAY "きォ  �が. �皴竅�" GET days2 PICTURE "@Z 9999" WHEN .F.
@  9, 40 SAY "きォ  �皰��" GET n_days2 PICTURE "@Z 9999" WHEN .F.
@  9, 57 SAY "きォ"
@ 11, 20 SAY "�皰�� ��" GET q PICTURE "@Z 9999" WHEN .F.
@ 11, 34 SAY "きォ" GET total PICTURE "@Z 9999999.99" WHEN .F.
@ 11, 50 SAY "$"
@ 13, 18 SAY "踸�ョ��┘" GET nt
@ WROWS()-2, 20 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1 .AND. total # 0
  REPLACE PAY_STAT.S_DATE WITH d2+1
  DO To_Inc WITH SLR_TYPE.CODE, p_code, q, prc, total,   ;
                 SLR_TYPE.UNIT, ye_w, 0, nt
ENDIF
SELECT PRS_PAY
USE
SELECT PAY_STAT
USE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            ��瘍モ Л�腑�┤.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 05.11.97 13:07:00 夕
PROCEDURE Calc_V
PARAMETER is_adm
n_days1 = d2-d1+1
SHOW GET n_days1
IF is_adm
  DO Adm WITH days1, days2, n_days2
  SHOW GET days1
  SHOW GET days2
  SHOW GET n_days2
ENDIF
slr_w   = slr_w1+slr_w2
q       = n_days1-n_days2
prc     = ROUND(slr_w/330,2)
ye_w    = YEAR(d2)
total = q*prc

SHOW GET slr_w1
SHOW GET slr_w2
SHOW GET slr_w
SHOW GET q
SHOW GET prc
SHOW GET ye_w
SHOW GET total

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�           ��瘍モ ����腑痰�� きォ �が┃�痰��皋↓��� �皴竅��.            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 05.11.97 13:09:57 夕
PROCEDURE Adm
PARAMETERS d_1, d_2, d_h

PRIVATE s_sav, r_sav, n_w, str_w, m_st, m_en

s_sav = SELECT()
SELECT LS
r_sav = RECNO()
SEEK STR(p_code,5)
m_st = YEAR(d1)*12+MONTH(d1)
m_en = YEAR(d2)*12+MONTH(d2)
d_1 = 0
d_2 = 0
SCAN REST WHILE PS_CODE = p_code FOR BETWEEN(YEAR*12+MONTH, m_st, m_en)
  str_w = DAYS
  IF YEAR*12+MONTH = m_en
    str_w = LEFT(str_w, DAY(d2)-1)
  ENDIF
  IF YEAR*12+MONTH = m_st
    str_w = SUBSTR(str_w, DAY(d1))
  ENDIF
  d_1 = d_1 + OCCURS("�", str_w)
  d_2 = d_2 + OCCURS("�", str_w)
ENDSCAN
d_h = d_1+d_2
RETURN
