*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� OftenPos     ��о�｀砒┴ ���┃� ���矗┘���                 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�           ��゛��� ��痰�硅 �÷�薀�┤ � ��Ж罔鍖 ���キ���矣琺.           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 25.01.2000 様�
PROCEDURE OftenPos

PRIVATE d_list, d_codes, d_count, d_1, d_2, mss

USE (base_path+"POPUL")
d_count = 1

DIMENSION d_list(d_count), d_codes(d_count)
d_list(d_count)  = PADC("�瓮", FSIZE("NAME"))
d_codes(d_count) = ""
SCAN
  d_count = d_count + 1

  DIMENSION d_list(d_count), d_codes(d_count)
  d_list(d_count)  = NAME
  d_codes(d_count) = STR(CODE)
ENDSCAN
USE

d_1 = DATE() - 30
d_2 = DATE()
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  ｡��←錺� ����襯? [ �� ] [ �モ ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 1
  DO Prp_Oft
ELSE
  DO View_Oft
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_Oft      ��о�｀砒┴ ���┃� ���矗┘���                 �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                  ��ぃ�皰→� ����� ��痰�硅 �÷�薀�┤.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 25.01.2000 陳�
PROCEDURE Prp_Oft

PRIVATE is_near, err_cod
DIMENSION err_lst(1)

IF .NOT. G_Period(d_1, d_2)
  RETURN .F.
ENDIF

DELETE FILE (tmpo_path+"OFT_TMP.DBF")
DELETE FILE (tmpo_path+"OFT_TMP.CDX")

CREATE DBF (tmpo_path+"OFT_TMP")  ;
   ( CODE     N( 7), ;
     PREFIX   C( 4), ;
     NAME     C(35), ;
     PRODUCER C( 8), ;
     D_CODE   N( 2), ;
     N_SEEK   N( 5), ;
     N_POS    N( 6), ;
     RET_N    N( 5), ;
     RET_QNT  N( 6), ;
     S_PUR    C( 1), ;
     ADDR     C( 8)  )
INDEX ON CODE TAG CODE

SELECT 0
USE (base_path+"DIV_DET") ORDER TAG DIV ALIAS LNK_0125

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_0125
SET RELATION TO DIV_CODE INTO LNK_0125

SELECT 0
USE (base_path+"ORD_D") ORDER TAG DOC_NUM ALIAS DET_0125
SET RELATION TO CODE INTO STO_0125

SELECT 0
USE (base_path+"ORD_T") ORDER TAG HRON ALIAS TIT_0125

is_near = SET("NEAR") = "OFF"
SET NEAR ON
SEEK DTOS(d_1)
IF is_near
  SET NEAR OFF
ENDIF

SCAN REST WHILE ORD_DATE <= d_2
  SELECT DET_0125
  SEEK TIT_0125.LIST_NUM+STR(TIT_0125.ORD_NUM,3)
  SCAN REST WHILE TIT_0125.LIST_NUM = LIST_NUM .AND. TIT_0125.ORD_NUM = ORD_NUM
    SELECT OFT_TMP
    IF .NOT. SEEK(DET_0125.CODE)
      APPEND BLANK
      REPLACE CODE     WITH DET_0125.CODE,     ;
              PREFIX   WITH STO_0125.PREFIX,   ;
              NAME     WITH STO_0125.NAME,     ;
              PRODUCER WITH STO_0125.PRODUCER, ;
              D_CODE   WITH LNK_0125.SUP_DIV,  ;
              N_SEEK   WITH 1,                 ;
              N_POS    WITH ABS(DET_0125.QNT), ;
              ADDR     WITH STO_0125.ADDRESS,  ;
              S_PUR    WITH STO_0125.STOP_PUR, ;
              RET_N    WITH IIF(DET_0125.QNT<0,1,0), ;
              RET_QNT  WITH IIF(DET_0125.QNT<0,DET_0125.QNT,0)
    ELSE
      REPLACE N_SEEK  WITH N_SEEK+1, ;
              N_POS   WITH N_POS+ABS(DET_0125.QNT), ;
              RET_N   WITH IIF(DET_0125.QNT<0,RET_N+1,RET_N)  ;
              RET_QNT WITH IIF(DET_0125.QNT<0,RET_QNT+DET_0125.QNT,RET_QNT)
              
    ENDIF
    SELECT DET_0125
  ENDSCAN
  SELECT TIT_0125
ENDSCAN
USE
SELECT DET_0125
USE
SELECT LNK_0125
USE
SELECT STO_0125
USE

err_cod = 0 
err_lst(1) = 1705
SELECT 0
USE (base_path+"SPROS") ALIAS RES_0125 AGAIN EXCLUSIVE
IF err_cod # 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ���� ��痰�硅 �÷�薀�┤ ���閧 む磽━ ���讌���皀�ガ !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  ZAP
  APPEND BLANK
  REPLACE CODE WITH 0, NAME WITH DTOC(d_1)+"!"+DTOC(d_2)
  SKIP
  SELECT OFT_TMP
  SCAN
    SELECT RES_0125
    IF EOF() .OR. BOF()
      APPEND BLANK
    ENDIF
    REPLACE CODE     WITH OFT_TMP.CODE,    ;
            PREFIX   WITH OFT_TMP.PREFIX,  ;
            NAME     WITH OFT_TMP.NAME,    ;
            PRODUCER WITH OFT_TMP.PRODUCER,;
            SUP_DIV  WITH OFT_TMP.D_CODE,  ;
            DEMAND   WITH OFT_TMP.N_SEEK,  ;
            QNT      WITH OFT_TMP.N_POS,   ;
            RET_POS  WITH OFT_TMP.RET_N,   ;
            QNT_RET  WITH OFT_TMP.RET_QNT, ;
            ADDR     WITH OFT_TMP.ADDR,    ;
            STOP_PUR WITH OFT_TMP.S_PUR
    SKIP
    SELECT OFT_TMP
  ENDSCAN
  USE
  SELECT RES_0125
  REPLACE REST CODE WITH 0
  USE

  DO View_Oft

  DELETE FILE (tmpo_path+"OFT_TMP.DBF")
  DELETE FILE (tmpo_path+"OFT_TMP.CDX")

ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_Oft     ��о�｀砒┴ ���┃� ���矗┘���                 �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    蹍甃�矗 ����� ��痰�硅 �÷�薀�┤.                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 25.01.2000 陳�
PROCEDURE View_Oft

PRIVATE point, s
point = 1

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
PRIVATE flt
flt = .F.
*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
USE (base_path+"DIVISION") ORDER TAG CODE ALIAS D01_0125 AGAIN

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_0115 AGAIN
SET RELATION TO DIV_CODE INTO D01_0125

SELECT 0
USE (base_path+"POPUL") ORDER TAG CODE ALIAS DIV_0125 AGAIN

SELECT 0
USE (base_path+"SPROS") ALIAS POS_0115 AGAIN
s = AT("!", NAME)
s = LEFT(NAME, s-1)+" �� "+ALLTRIM(SUBSTR(NAME,s+1))
SET ORDER TO TAG ALL_DIVS
GO TOP
SET RELATION TO SUP_DIV INTO DIV_0125, CODE INTO STO_0115
*SET RELATION TO CODE INTO STO_0115 ADDITIVE
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD All OF (menu_name) PROMPT "�瓮/�皀腑痰▲��襯 ��Ж罔�" KEY Ctrl-A, "Tab"
ON SELECTION PAD All OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Div OF (menu_name) PROMPT "�祚ム��Гカ" KEY Ctrl-A, "F1"
ON SELECTION PAD Div OF (menu_name) DO Sw_Mode WITH "Div"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, pic_w
pic_w = Addr_Pic()

ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = LEN(Addr_Pic())+61
*   ��Гカ
*敖�むメ陳陳陳��Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳�N �÷������.������.�
*�AAAAAAAA PPPP NNNNNNNNNNNNNNNNNNNNNNNNN PPPPPPPP NNNNN NNNNNN NNNNNN�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
DO D_Wins WITH ln, wd, "��痰��� �÷�薀��� � ��Ж罔鍖 � "+s, 2, 3
@ 1, 4 SAY "�┼赳�"
@ 1, 30 SAY "�祚ム��Гカ"
@ 3, 2 SAY "��むメ�"+REPLICATE("�", LEN(Addr_Pic())-8) + ;
"陳陳���Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳�N �÷������.陳�����."
@ WROWS()-3,  3 SAY "��Гカ"
@ WROWS()-3, 20 SAY "�祚ム��Гカ"
@ WROWS()-3, 45 SAY "��Б��薀��"
@ WROWS()-2, 46 SAY "���"
@ WROWS()-2, 58 SAY "萵."
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Tab DO Sw_Mode WITH "Filter"
    ON KEY LABEL F1  DO Sw_Mode WITH "Div"
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 2, 30 SAY d_list(point)
*    BROWSE - �キ�
*
    BROWSE KEY d_codes(point)             ;
           FIELDS STO_0115.ADDRESS:H="":P=pic_w,  ;
                  STO_0115.PREFIX:H="",   ;
                  Brw_Name=Nm_Part(STO_0115.NAME):25:H="",     ;
                  STO_0115.PRODUCER:H="", ;
                  POS_0115.DEMAND:H="",   ;
                  POS_0115.QNT:H="",      ;
                  q_000=STO_0115.QNT+STO_0115.QNT_SAVE+STO_0115.QNT_REZ, ;
           DUMMY.F:H="" FREEZE DUMMY.F    ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "List")
    ENDIF


  CASE what_do = "Filter" && �瓮 ��Ж罔�/�皀腑痰▲���e
  
    IF .NOT. flt
      SET FILTER TO STOP_PUR = "S"
      @ 2, 4 SAY "�皀腑痰▲��襯"
      flt = .T.
    ELSE
      @ 2, 4 SAY "�瓮 ��Ж罔�  " 
      SET FILTER TO
      flt = .F.
    ENDIF
    what_do = "List"
    
  
  CASE what_do = "Div"    && ��キ� ��Гカ�

    DO Ch_Div
    IF point = 1
      SET ORDER TO TAG ALL_DIVS
    ELSE
      SET ORDER TO TAG D_DIV
    ENDIF
    GO TOP
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
USE
SELECT D01_0125
USE
SELECT STO_0115
USE
SELECT DIV_0125
USE
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
@ WROWS()-4,  3 SAY STO_0115.PREFIX+" "+STO_0115.NAME+" "+STO_0115.PRODUCER

@ WROWS()-2,  3 SAY D01_0125.B_NAME
@ WROWS()-2, 20 SAY DIV_0125.NAME
@ WROWS()-2, 40 SAY POS_0115.RET_POS
@ WROWS()-2, 50 SAY ABS(POS_0115.QNT_RET)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Div       ��о�｀砒┴ ���┃� ���矗┘���                 �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �襦�� 痺�ム��Гカ�.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 25.01.2000 陳�
PROCEDURE Ch_Div
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

PRIVATE ex
ex = point
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH MIN(ALEN(d_list)+3, WROWS("")-5), LEN(d_list(1))+4, "�祚ム��Гカ�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO WHILE .T.
  ex = MAX(ex, 1)
  ex = MIN(ex, ALEN(d_list))
  @ 1,1 MENU d_list, ALEN(d_list), WROWS()-3
  READ MENU TO ex
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    point = ex
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN