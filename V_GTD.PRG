*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� V_GTD        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             ��痰�鎰┘ ���                              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 09.08.2002 様�
PROCEDURE V_GTD
PARAMETERS d_num		&& ���ム ���
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

DELETE FILE (tmpo_path+"GTD_L.DBF")
DELETE FILE (tmpo_path+"GTD_L.CDX")
DELETE FILE (tmpo_path+"GTD_N.DBF")
DELETE FILE (tmpo_path+"GTD_N.CDX")
DELETE FILE (tmpo_path+"GTD_S.DBF")
DELETE FILE (tmpo_path+"GTD_S.CDX")
s_sav = SELECT()
DO Prp_Tmp WITH d_num

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
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
SELECT 0
DO Use_Dummy
SELECT 0

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Sw OF (menu_name) PROMPT "���/�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Sw OF (menu_name) DO Sw_Mode WITH "Sw"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

PRIVATE sw_mode
sw_mode = .T.

*
*   ��瘍モ ��Кム�� ����
*

*��矗.���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳�� ���陳��痰�皰�陳��蹍����朕
*�SSSS ....:....!....:....!....:....!....:.... 99999999 99999999 V 99999999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ln, wd
ln = WROWS("")-5   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 73

DO D_Wins WITH ln, wd, "��� � "+d_num, 0, 0
@ 1, 2 SAY ;
"�矗.���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳�� ���陳��痰�皰�陳��蹍����"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�
    IF sw_mode
      SELECT GTD_L
      SET RELATION TO GTD_LINE INTO GTD_N
      SET SKIP TO GTD_N
      statys_type = 3
      DO Prp_Nav_1
      ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
      BROWSE FIELDS GTD_L.GTD_LINE:4:H="",  ;
                    GTD_N.NAME:39:H="",     ;
                    GTD_L.QNT_GTD:H="":P="99999999",  ;
                    GTD_L.QNT_REST:H="":P="99999999", ;
                    DUMMY_F = " ":1:H="",             ;
                    GTD_L.QNT_SALE:H="":P="99999999", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ELSE
      SELECT GTD_L
      SET RELATION TO GTD_LINE INTO GTD_S
      SET SKIP TO GTD_S
      statys_type = 3
      DO Prp_Nav_1
      ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
      BROWSE FIELDS GTD_L.GTD_LINE:4:H="",  ;
                    GTD_S.NAME:39:H="",     ;
                    GTD_L.QNT_GTD:H="":P="99999999",  ;
                    GTD_S.QNT_REST:H="":P="99999999", ;
                    GTD_S.B_GTD:1:H="",               ;
                    GTD_S.QNT_SALE:H="":P="99999999", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ENDIF
    ON KEY
    
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Sw")
    ENDIF

  CASE what_do = "Sw"    && ��キ� 皋�� ��゛�肓

    sw_mode = .NOT. sw_mode    
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
SELECT GTD_L
USE
SELECT GTD_N
USE
SELECT GTD_S
USE
DELETE FILE (tmpo_path+"GTD_L.DBF")
DELETE FILE (tmpo_path+"GTD_L.CDX")
DELETE FILE (tmpo_path+"GTD_N.DBF")
DELETE FILE (tmpo_path+"GTD_N.CDX")
DELETE FILE (tmpo_path+"GTD_S.DBF")
DELETE FILE (tmpo_path+"GTD_S.CDX")

SELECT (s_sav)

RETURN


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Tmp      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ�諷 ������                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 09.08.2002 陳�
PROCEDURE Prp_Tmp
PARAMETER d_num
PRIVATE f_lst, g_dt, rc_w, d_sav

f_lst = ""
DO F_Lst

SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM SHARED AGAIN ALIAS GTD_2809

SELECT 0
CREATE DBF (tmpo_path+"GTD_L.DBF") ;
  ( GTD_LINE C(FSIZE("GTD_LINE", "GTD_2809")),;
    QNT_GTD  N( 8), ;
    QNT_REST N( 8), ;
    QNT_SALE N( 8)  )
INDEX ON GTD_LINE TAG GTD_LINE

SELECT 0
CREATE DBF (tmpo_path+"GTD_N.DBF") ;
  ( GTD_LINE C(FSIZE("GTD_LINE", "GTD_2809")),;
    NAME     C(39)  )
INDEX ON GTD_LINE TAG GTD_LINE

SELECT 0
CREATE DBF (tmpo_path+"GTD_S.DBF") ;
  ( GTD_LINE C(FSIZE("GTD_LINE", "GTD_2809")),;
    CODE     N( 7), ;
    NAME     C(39), ;
    QNT_REST N( 8), ;
    B_GTD    C( 1), ;
    QNT_SALE N( 8)  )
INDEX ON GTD_LINE+STR(CODE) TAG GTD_LINE

SELECT GTD_2809
SEEK d_num
g_dt = GTD_DATE
SCAN REST WHILE GTD_NUM = d_num
  SELECT GTD_N
  APPEND BLANK
  REPLACE GTD_LINE WITH GTD_2809.GTD_LINE, ;
          NAME     WITH GTD_2809.NAME
  SELECT GTD_L
  IF .NOT. SEEK(GTD_2809.GTD_LINE)
    APPEND BLANK
    REPLACE GTD_LINE WITH GTD_2809.GTD_LINE
  ENDIF
  REPLACE QNT_GTD WITH QNT_GTD+GTD_2809.QNT
  SELECT GTD_2809
ENDSCAN

SELECT GTD_2809

USE (base_path+"STOCK") SHARED AGAIN ALIAS ST_2809 ORDER TAG CODE
SELECT 0
USE (base_path+"ACT_S") SHARED AGAIN ALIAS P_2809
DO Wt_Mess WITH "��め腑� �痰�皖��!"
SCAN FOR GTD_NUM = d_num
  SELECT GTD_S
  IF .NOT. SEEK(P_2809.GTD_LINE+STR(P_2809.CODE))
    = SEEK(P_2809.CODE, "ST_2809")
    APPEND BLANK
    REPLACE GTD_LINE WITH P_2809.GTD_LINE,;
            CODE     WITH P_2809.CODE,    ;
            NAME     WITH ST_2809.PREFIX+" "+ST_2809.NAME+" "+ST_2809.PRODUCER
  ENDIF
  REPLACE QNT_REST WITH QNT_REST+P_2809.QNT_REST, ;
          B_GTD    WITH IIF(P_2809.B_GTD, "�", "")
  SELECT GTD_L
  IF .NOT. SEEK(P_2809.GTD_LINE)
    APPEND BLANK
    REPLACE GTD_LINE WITH P_2809.GTD_LINE
  ENDIF
  REPLACE QNT_REST WITH QNT_REST+P_2809.QNT_REST
  SELECT P_2809
ENDSCAN
USE
DO Wt_Mess
DO Wt_Mess WITH "��� � �牀�����..."

USE (base_path+"SALE") SHARED AGAIN ORDER TAG DOC_NUM ALIAS SD_2812
SELECT 0
USE (base_path+"SALE_TIT") SHARED AGAIN ORDER TAG HRON ALIAS ST_2812
IF .NOT. SEEK(DTOS(g_dt))
  rc_w = RECNO(0)
  IF BETWEEN(rc_w, 1, RECCOUNT())
    GO rc_w
  ENDIF
ENDIF

d_sav = {}
SCAN REST
  IF d_sav # DOC_DATE
    @ WROWS()-1, 3 SAY PADC(DTOC(DOC_DATE), WCOLS())
    d_sav = DOC_DATE
  ENDIF
  IF CUS_CODE < 0
    LOOP
  ENDIF
  IF .NOT. FIRM $ f_lst
    LOOP
  ENDIF
  SELECT SD_2812
  SEEK ST_2812.FIRM+LEFT(DTOS(ST_2812.DOC_DATE), 4)+ST_2812.DOC_NUM
  SCAN REST WHILE ST_2812.FIRM = FIRM .AND. ;
                  ST_2812.DOC_DATE = DOC_DATE .AND. ;
                  ST_2812.DOC_NUM = DOC_NUM
    IF GTD_NUM = d_num
      SELECT GTD_S
      IF .NOT. SEEK(SD_2812.GTD_LINE+STR(SD_2812.CODE))
        = SEEK(SD_2812.CODE, "ST_2809")
        APPEND BLANK
        REPLACE GTD_LINE WITH SD_2812.GTD_LINE,;
                CODE     WITH SD_2812.CODE,    ;
                NAME     WITH ST_2809.PREFIX+" "+ST_2809.NAME+" "+ST_2809.PRODUCER
      ENDIF
      REPLACE QNT_SALE WITH QNT_SALE+SD_2812.QNT
      SELECT GTD_L
      IF .NOT. SEEK(SD_2812.GTD_LINE)
        APPEND BLANK
        REPLACE GTD_LINE WITH SD_2812.GTD_LINE
      ENDIF
      REPLACE QNT_SALE WITH QNT_SALE+SD_2812.QNT
    ENDIF
    SELECT SD_2812
  ENDSCAN
  SELECT ST_2812
ENDSCAN

DO Wt_Mess

USE
SELECT SD_2812
USE
SELECT ST_2809
USE
SELECT GTD_L
GO TOP

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� F_Lst        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                  ���甌� �爛く爬閧┤ か� 竍モ� ���                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12.08.2002 陳�
PROCEDURE F_Lst

PRIVATE s_sav

s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") SHARED AGAIN ALIAS F_2812 
SCAN
  f_lst = f_lst+FIRM_CODE+CHR(13)
ENDSCAN
USE
SELECT (s_sav)

RETURN
