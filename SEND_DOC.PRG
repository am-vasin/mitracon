*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Send_Doc     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                �ムイ��� ぎ�祠キ皰� � 筥���� か� 瓣ム��.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 03.06.2000 様�
PROCEDURE Send_Doc
PARAMETERS fil_code, prmDate, prmPath

PRIVATE tmpDate, tmpPath, s_sav

s_sav = SELECT()
SELECT 0
IF TYPE("prmDate") = "C" .AND. TYPE("prmPatm") = "C"
  tmpDate = prmDate
  tmpPath = prmPath
ELSE
  tmpDate = DATE()-30
  tmpPath = SPACE(60)
  IF .NOT. Get_P()
    SELECT (s_sav)
    RETURN
  ENDIF
ENDIF

tmpPath = ALLTRIM(tmpPath)
CREATE DBF (tmpPath+"s_sale_t") ;
  (   DOC_NUM  C(10),  ;
      DOC_DATE D,      ;
      DOC_FIRM C( 3),  ;
      SUP_CODE N( 8),  ;
      CUS_CODE N( 8),  ;
      REF_NUM  C(10),  ;
      REF_DATE D       )
SELECT 0
CREATE DBF (tmpPath+"s_sale") ;
  (   DOC_NUM  C(10),  ;
      DOC_DATE D,      ;
      DOC_FIRM C( 3),  ;
      CODE     N( 7),  ;
      QNT      N( 8),  ;
      PRICE_R  N(15,5),;
      PRICE_D  N(13,4)  )
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS S_0603 AGAIN
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE ALIAS ST_0603 AGAIN
SEEK fil_code
SCAN REST WHILE fil_code = CUS_CODE ;
          FOR EMPTY(LIST_NUM) .AND. DOC_DATE >= tmpDate
  SELECT S_SALE_T
  APPEND BLANK
  REPLACE DOC_NUM  WITH ST_0603.DOC_NUM,  ;
          DOC_DATE WITH ST_0603.DOC_DATE, ;
          DOC_FIRM WITH ST_0603.FIRM,     ;
          SUP_CODE WITH sup_own,          ;
          CUS_CODE WITH fil_code,         ;
          REF_NUM  WITH ST_0603.REQ_DOC,  ;
          REF_DATE WITH ST_0603.REQ_DATE
  SELECT S_0603
  SEEK ST_0603.FIRM+LEFT(DTOS(ST_0603.DOC_DATE), 4)+ST_0603.DOC_NUM
  SCAN REST WHILE ST_0603.FIRM = FIRM .AND. ;
                  ST_0603.DOC_DATE = DOC_DATE .AND.  ;
                  ST_0603.DOC_NUM = DOC_NUM
    SELECT S_SALE
    APPEND BLANK
    REPLACE DOC_NUM  WITH ST_0603.DOC_NUM,  ;
            DOC_DATE WITH ST_0603.DOC_DATE, ;
            DOC_FIRM WITH ST_0603.FIRM,     ;
            CODE     WITH S_0603.CODE,      ;
            QNT      WITH S_0603.QNT,       ;
            PRICE_R  WITH S_0603.PRICE_R,   ;
            PRICE_D  WITH S_0603.PRICE_D
    SELECT S_0603
  ENDSCAN
  SELECT ST_0603
ENDSCAN

SELECT S_SALE_T
CREATE DBF (tmpPath+"s_ryn_t") ;
  (   DOC_NUM  C(10),  ;
      DOC_DATE D,      ;
      DOC_FIRM C( 3),  ;
      SUP_CODE N( 8),  ;
      CUS_CODE N( 8),  ;
      REF_NUM  C(10),  ;
      REF_DATE D       )
SELECT S_SALE
CREATE DBF (tmpPath+"s_ryn") ;
  (   DOC_NUM  C(10),  ;
      DOC_DATE D,      ;
      DOC_FIRM C( 3),  ;
      CODE     N( 7),  ;
      QNT      N( 8),  ;
      PRICE_R  N(15,5),;
      PRICE_D  N(13,4)  )
SELECT S_0603
USE (base_path+"RYN_M") ORDER TAG DOC_NUM ALIAS S_0603 AGAIN
SELECT ST_0603
USE (base_path+"RYN_M_T") ORDER TAG CUS_CODE ALIAS ST_0603 AGAIN
SEEK fil_code
SCAN REST WHILE fil_code = CUS_CODE
  SELECT S_RYN_T
  APPEND BLANK
  REPLACE DOC_NUM  WITH STR(ST_0603.DOC_NUM,10), ;
          DOC_DATE WITH ST_0603.DOC_DATE,        ;
          SUP_CODE WITH sup_own,                 ;
          CUS_CODE WITH fil_code
  SELECT S_0603
  SEEK LEFT(DTOS(ST_0603.DOC_DATE), 4)+STR(ST_0603.DOC_NUM,5)
  SCAN REST WHILE ST_0603.DOC_DATE = DOC_DATE .AND.  ;
                  ST_0603.DOC_NUM = DOC_NUM
    SELECT S_RYN
    APPEND BLANK
    REPLACE DOC_NUM  WITH STR(ST_0603.DOC_NUM,10),  ;
            DOC_DATE WITH ST_0603.DOC_DATE, ;
            CODE     WITH S_0603.CODE,      ;
            QNT      WITH S_0603.QNT
    SELECT S_0603
  ENDSCAN
  SELECT ST_0603
ENDSCAN
USE
SELECT S_0603
USE
SELECT S_RYN
USE
SELECT S_RYN_T
USE
SELECT (s_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_P        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ��皀���皋↓覃 □�� ����諷.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.06.2000 陳�
PROCEDURE Get_P
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 [ ] �磴� ���メ皋碎 爛с�赳�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �....:....!....:....!....:....!....:....!....:....!....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�                �ムイ�碎 ぎ�祠キ硅 � ��.��.����                 �
*�                < OK Ctrl-W > < �皖���碎瘴 Esc >                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, sw
ex = 1
sw = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 66, "�ムイ��� ぎ�祠キ皰� � 筥����"
DO Sun_Bord WITH  2,  2,  4, 63, " [ ] �磴� ���メ皋碎 爛с�赳�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2,  5 GET sw PICTURE "@*C �磴� ���メ皋碎 爛с�赳��" VALID G_Path()
@ 3,  3 GET tmpPath WHEN .F.
@ 5, 18 SAY "�ムイ�碎 ぎ�祠キ硅 �" GET tmpDate
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID TstBlank()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_Path       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �襦�� え爛�皰爬�                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.06.2000 陳�
PROCEDURE G_Path

PRIVATE tmp, mss, p

IF sw = .F.
  RETURN.T.
ENDIF

tmp = tmpPath
DO WHILE .T.
  tmp = Int_Dir(tmp, "�襦ム�皀 え爛�皰爬�")
  IF EMPTY(tmp)
    EXIT
  ENDIF
  IF File_O(tmp+"s_sale.dbf")   .OR. ;
     File_O(tmp+"s_sale_t.dbf") .OR. ;
     File_O(tmp+"s_ryn.dbf")    .OR. ;
     File_O(tmp+"s_ryn_t.dbf")
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"��� 礒� メ碎 ����� � ぎ�祠キ����!"
    mss(3) = CHR(0)+"   [ �����碎 ] [ ��キ�碎 え爛�皰爬� ] [ �皖���碎瘴 ]"
    mss(4) = ""
    p = Out_Mess(7, "mss")
    IF p = 3
      EXIT
    ENDIF
    IF p = 1
      DELETE FILE (tmp+"s_sale.dbf")
      DELETE FILE (tmp+"s_sale_t.dbf")
      DELETE FILE (tmp+"s_ryn.dbf")
      DELETE FILE (tmp+"s_ryn_t.dbf")
    ENDIF
    IF p = 2
      LOOP
    ENDIF
  ENDIF
  tmpPath = PADR(tmp, LEN(tmpPath))
  EXIT
ENDDO
sw = .F.
SHOW GET sw
SHOW GET tmpPath

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TstBlank     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �諷�� ├ ゛����.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.06.2000 陳�
PROCEDURE TstBlank
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(tmpPath)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� え爛�皰爬�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
