*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� New_Year     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 �ムュ�� �� ���覃 ��� � ��痰� ��牆��硅.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 12.04.2001 様�
PROCEDURE New_Year

PRIVATE s_sav, tmpYear, s_otp, s_ill

s_sav = SELECT()
STORE 0 TO s_otp, s_ill
SELECT 0
USE (base_path+"PARMS") AGAIN ALIAS P_1412
tmpYear = SLR_YEAR
USE
SELECT (s_sav)

IF .NOT. Term_Y(tmpYear)
  RETURN
ENDIF

DELETE FILE (tmpo_path+"NEW_YEAR.DBF")
DELETE FILE (tmpo_path+"NEW_YEAR.CDX")
SELECT 0
CREATE DBF(tmpo_path+"NEW_YEAR.DBF") ;
  ( CODE     N( 5), ;
    FAMILY   C(24), ;
    NAME     C(16), ;
    S_NAME   C(20), ;
    SLR_0    N(7,2),;
    SLR_1    N(7,2),;
    FIO      C(28), ;
    ADM_D    N( 4), ;
    ILL_D    N( 4), ;
    ILL_M    N(8,2),;
    WAS_ILL  N( 3), ;
    DATE_IN  D,     ;
    DATE_OFF D,     ;
    OTP_D    N( 4), ;
    WAS_OTP  N( 3), ;
    OTP_M    N(8,2),;
    DEP_NO   N( 4), ;
    DEP_NAME C(16)  )

DO P_List       && ��ぃ�皰→� � ������キ┘ ∇ガキ���� �����

DO V_List       && ��ぃ�皰→� � ������キ┘ ∇ガキ���� �����

USE

SELECT (s_sav)
DELETE FILE (tmpo_path+"NEW_YEAR.DBF")
DELETE FILE (tmpo_path+"NEW_YEAR.CDX")
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Term_Y       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    蹍▲爲�: ≡� �� �メ閹� ���琺硅?                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.04.2001 陳�
PROCEDURE Term_Y
PARAMETERS y

PRIVATE s_sav, mss

RETURN .T.

s_sav = SELECT()
SELECT 0
USE (base_path+"MOULD") AGAIN ALIAS M1412
SET FILTER TO FLAG
GO BOTT
IF YEAR = y .AND. MONTH = 12
  USE
  SELECT (s_sav)
  RETURN .T.
ENDIF

USE
SELECT (s_sav)
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"�� ��▲琥キ� �÷�｀皖� ����!"
mss(3) = ""
DO Out_Mess WITH 7, "mss"

RETURN .F.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� P_List       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    ��爼�牀���┘ 甎�瓷� 甌矗磴�┴��.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.04.2001 陳�
PROCEDURE P_List

PRIVATE s_sav, sum_i, sum_o, sum_a, tmpDays, let_adm

let_adm = 2

DO Wt_Mess WITH "��ぃ�皰→� ����諷!"

s_sav = SELECT()

SELECT 0
USE (base_path+"DEPS") AGAIN ALIAS D_1413 ORDER TAG CODE
SELECT 0
USE (base_path+"PS_STAT") AGAIN ALIAS S_1413 ORDER TAG CODE
SELECT 0
USE (base_path+"PERSONS") AGAIN ALIAS P_1413
SET RELATION TO CODE INTO S_1413
SET RELATION TO DEP_NO INTO D_1413 ADDITIVE
SCAN FOR EMPTY(DATE_OFF) .OR. YEAR(DATE_OFF) = tmpYear
  SELECT NEW_YEAR
  APPEND BLANK
  REPLACE CODE     WITH P_1413.CODE,    ;
          FAMILY   WITH P_1413.FAMILY,  ;
          NAME     WITH P_1413.NAME,    ;
          S_NAME   WITH P_1413.S_NAME,  ;
          DATE_IN  WITH P_1413.DATE_IN, ;
          DATE_OFF WITH P_1413.DATE_OFF,;
          SLR_0    WITH Crpt_2(S_1413.SALARY),;
          SLR_1    WITH Salary2(P_1413.CODE, 3),;
          OTP_D    WITH S_1413.REST_OTP,;
          ILL_D    WITH S_1413.REST_ILL,;
          DEP_NO   WITH P_1413.DEP_NO,  ;
          DEP_NAME WITH D_1413.B_NAME,  ;
          FIO      WITH ALLTRIM(P_1413.FAMILY)+" "+ ;
                        LEFT(P_1413.NAME,1)   +" "+ ;
                        LEFT(P_1413.S_NAME,1)
  SELECT P_1413
ENDSCAN
USE
SELECT D_1413
USE
SELECT S_1413
USE (base_path+"LS_TABLE") ALIAS T_1413 AGAIN ORDER TAG PS_CODE

SELECT NEW_YEAR
INDEX ON SYS(15, lwr, FAMILY+NAME+S_NAME) TAG NAME
GO TOP

SCAN
  sum_i = 0
  sum_o = 0
  sum_a = 0
  IF SEEK(STR(CODE,5)+STR(tmpYear,4), "T_1413")
    SELECT T_1413
    SCAN REST WHILE PS_CODE = NEW_YEAR.CODE .AND. YEAR = tmpYear
      tmpDays = DAYS
      IF YEAR = YEAR(NEW_YEAR.DATE_IN) .AND. MONTH = MONTH(NEW_YEAR.DATE_IN)
        tmpDays = SPACE(DAY(NEW_YEAR.DATE_IN)-1)+SUBSTR(tmpDays, DAY(NEW_YEAR.DATE_IN))
      ENDIF
      IF YEAR = YEAR(NEW_YEAR.DATE_OFF) .AND. MONTH = MONTH(NEW_YEAR.DATE_OFF)
        tmpDays = LEFT(tmpDays, DAY(NEW_YEAR.DATE_OFF))
      ENDIF
      sum_i = sum_i + OCCURS("�", tmpDays)
      sum_o = sum_o + OCCURS("�", tmpDays)
      sum_a = sum_a + OCCURS("�", tmpDays)
    ENDSCAN
    SELECT NEW_YEAR
    REPLACE ADM_D WITH sum_a
    sum_a = MAX(sum_a-let_adm, 0)
*    sum_o = MAX(sum_o-sum_a, 0)
    REPLACE WAS_ILL WITH sum_i, ;
            WAS_OTP WITH sum_o, ;
            OTP_D   WITH MAX(OTP_D-sum_a, 0)
  ENDIF
  IF WAS_ILL = 0 .AND. ILL_D # 0 .AND. EMPTY(DATE_OFF)
    REPLACE ILL_M WITH ROUND((SLR_0+SLR_1)/30.42,2)*ILL_D
    s_ill = s_ill+ILL_M
  ENDIF
  IF OTP_D # 0 .AND. EMPTY(DATE_OFF)
    REPLACE OTP_M WITH ROUND((SLR_0+SLR_1)/30.42,2)*OTP_D
    s_otp = s_otp+OTP_M
  ENDIF
ENDSCAN

SELECT T_1413
USE
SELECT NEW_YEAR
GO TOP

DO Wt_Mess

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� V_List       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�               蹍甃�矗 甎�瓷� 甌矗磴�┴�� � ��腮甄キ┤.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.04.2001 陳�
PROCEDURE V_List

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
SELECT NEW_YEAR

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "��腮甄�碎" KEY Ctrl-A, "Tab"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳�" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*

* ==��矗磴�┴================= ==�〓�キ== ==���讚�膈覃=== ====�皴竅�===== 
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳��甎��痰陳�祠��陳�甎��痰陳�祠��朕
*�*******���****************** ��.��.���� 999 999 9999.99 999 999 9999.99�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�999999.99陳陳陳�999999.99�
*� ��牆����: ｪ��� 99999.99   �ぅ�讚�� ��痰� 99999.99   �� ぅ�� 9999.99  �
*�                        �發カ ....:....!....:.                         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 71

DO D_Wins WITH ln, wd, "��腮甄キ�� �� ��������� ����", 1, 2
@ 1,  2 SAY "  ��矗磴�┴                 " COLOR SCHEME 1
@ 1, 31 SAY "  �〓�キ  " COLOR SCHEME 1
@ 1, 42 SAY "  ���讚�膈覃   " COLOR SCHEME 1
@ 1, 58 SAY "    �皴竅�     " COLOR SCHEME 1

@ 2,  2 SAY ;
"陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�甎��痰陳�祠��陳�甎��痰陳�祠��"
@ WROWS()-4, 49 SAY s_ill PICTURE "99999.99"
@ WROWS()-4, 65 SAY s_otp PICTURE "99999.99"
@ WROWS()-3,  3 SAY "��牆����: ｪ��� 99999.99   �ぅ�讚�� ��痰� 99999.99   �� ぅ��"
@ WROWS()-2, 26 SAY "�發カ"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F6    DO Sw_Mode WITH "Filter"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Choose"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS FIO:28:H="",     ;
                  OUT = IIF(EMPTY(DATE_OFF), "", DTOC(DATE_OFF)):10:H="", ;
                  WAS_ILL:H="":P="@Z 999",  ;
                  ILL_D:H="":P="@Z 999",    ;
                  ILL_M:H="":P="@Z 9999.99",;
                  WAS_OTP:H="":P="@Z 999",  ;
                  OTP_D:H="":P="@Z 999",    ;
                  OTP_M:H="":P="@Z 9999.99",;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "List")
    ENDIF

  CASE what_do = "Choose"    && �ムュ�� �� ���覃 ���!

    DO Ch_Year
    EXIT

  CASE what_do = "Filter"    && �┼赳�

    IF EMPTY(SET("FILTER"))
      SET FILTER TO OTP_M # 0 .OR. ILL_M # 0
    ELSE
      SET FILTER TO
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
@ WROWS()-3, 19 SAY SLR_0 PICTURE "99999.99"
@ WROWS()-3, 45 SAY SLR_1 PICTURE "99999.99"
@ WROWS()-3, 64 SAY ROUND((SLR_0+SLR_1)/30.42,2) PICTURE "9999.99"
@ WROWS()-2, 32 SAY DEP_NAME

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_year      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         �ムュ�� �� ���覃 ���.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.04.2001 陳�
PROCEDURE Ch_year
PRIVATE p0, s_sav

DO Wt_Mess WITH "�ムュ�� �� ���覃 ���!"
SELECT 0
USE (base_path+"PS_STAT") ORDER TAG CODE AGAIN ALIAS PSS_1413

SELECT NEW_YEAR
SET FILTER TO

SCAN
  p0 = ROUND((SLR_0+SLR_1)/30.42,2)
  IF .NOT. EMPTY(OTP_M)
    DO To_Inc WITH 7, CODE, OTP_D, p0, OTP_M, "きォ", tmpYear, 0, ""
  ENDIF
  IF .NOT. EMPTY(ILL_M)
    DO To_Inc WITH 12, CODE, ILL_D, p0, ILL_M, "きォ", tmpYear, 0, ""
  ENDIF
  SELECT PSS_1413
  SEEK NEW_YEAR.CODE
  REPLACE REST_ILL WITH 7, ;
          REST_OTP WITH 30
  SELECT NEW_YEAR
ENDSCAN

SELECT PSS_1413
USE (base_path+"PARMS") AGAIN ALIAS PRMS_1413
REPLACE SLR_YEAR WITH SLR_YEAR+1
USE
SELECT NEW_YEAR
DO Wt_Mess

RETURN
