*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� For_Mst      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        ��゛�肓 か� � ��痰�〓��                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 03.07.2004 様�
PROCEDURE For_Mst

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
PRIVATE tbl_type, rez_file, rez_sw
m.tbl_type = 1
m.rez_file = m.tmpo_path+"result.dbf"
m.rez_sw   = .F.
m.rez_file = PADR(m.rez_file, 58)

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 �┓ ��゛�肓 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ( ) Einfo ( ) 蹍├〓え皀�� ( ) ���瘁� ( ) �キ� ( ) Comm � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� 敖 [ ] �磴� ���メ皋碎 爛с�赳�� 陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �                                                          � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�          < OK Ctrl-W > < �皖���碎瘴 Esc >                    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 66, "�砒モ�"
DO Sun_Bord WITH  2,  3,  4, 62, " �┓ ��゛�肓 "
DO Sun_Bord WITH  5,  3,  7, 62, " [ ] �磴� ���メ皋碎 爛с�赳�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  3,  5 GET m.tbl_type PICTURE "@*RH Einfo;蹍├〓え皀��;���瘁�;�キ�;Comm"
@  5,  6 GET m.rez_sw PICTURE "@*C �磴� ���メ皋碎 爛с�赳��" VALID Get_File()
@  6,  4 GET m.rez_file WHEN .F.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  DO CASE
  CASE m.tbl_type = 1
    DO Make_F1
  CASE m.tbl_type = 2
    DO Make_F2
  CASE m.tbl_type = 3
    DO Make_F3
  CASE m.tbl_type = 4
    DO Make_F4
  CASE m.tbl_type = 5
    DO Make_F5
  ENDCASE
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_File     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �襦�� ����� 爛с�赳���.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.07.2004 陳�
PROCEDURE Get_File

PRIVATE tmpPth, tmpFile, i
m.rez_sw = .F.

m.i = RAT("\", ALLTRIM(m.rez_file))
IF m.i = 0
  m.tmpFile = ""
  m.tmpPth = ALLTRIM(m.rez_file)
ELSE
  m.tmpFile = SUBSTR(ALLTRIM(m.rez_file), m.i+1)
  m.tmpPth = LEFT(m.rez_file, m.i-1)
ENDIF
m.rez_file = New_File(tmpPth, "dbf", "�����皀 ����", 0, m.tmpFile)
m.rez_file = PADR(m.rez_file, 58)

SHOW GET m.rez_sw
SHOW GET m.rez_file

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            蹍▲爲� ゛����.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.07.2004 陳�
PROCEDURE Tst_Blank

PRIVATE tmpPth, mss

IF m.ex # 1
  RETURN .T.
ENDIF
m.i = RAT("\", ALLTRIM(m.rez_file))
IF m.i = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� ━� �����"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ELSE
  m.tmpPth = LEFT(m.rez_file, m.i-1)
  IF .NOT. Is_Dir(m.tmpPth)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�オ�玻オ皚�� え爛�皰爬�"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Make_F1      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    ��Г��┘ ����� � 筮爼�皀 EInfo.                     �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.07.2004 陳�
PROCEDURE Make_F1

PRIVATE tmpCount, tmpStr
CREATE DBF (rez_file) ;
   (  PREFIX    C ( 4),;
      NAME      C (35),;
      PRODUCER  C ( 8),;
      CLASS     C (64),;
      FUNCTION  C (254),;
      CORPUS    C (16),;
      QNT       N (10),;
      QNT_C     N (10),;
      QNT_H     N (10),;
      PRICE_R_D N(9,2),;
      PRICE_R_R N(9,2)    )
USE (rez_file) ALIAS REZ_TABLE
SELECT 0
USE (base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (path_comm+"CORPUS") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (path_comm+"SUBJECT") SHARED AGAIN ORDER TAG STO_CODE
SET RELATION TO COR_CODE INTO CORPUS
SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ORDER TAG NAME
SET RELATION TO CODE INTO SUBJECT
SET RELATION TO CLASS INTO CLASSES ADDITIVE

DO Wt_Mess WITH "��ぃ�皰→� ����諷..."

m.tmpCount = 0
SCAN
  m.tmpCount = m.tmpCount+1
  IF m.tmpCount % 100 = 0
    m.tmpStr = PADC(ALLTRIM(STR(m.tmpCount)), WCOLS(WOUTPUT()))
    @ wROWS()-1, 0 SAY m.tmpStr
  ENDIF
  SELECT REZ_TABLE
  APPEND BLANK
  REPLACE PREFIX    WITH STOCK.PREFIX,     ;
          NAME      WITH STOCK.NAME,       ;
          PRODUCER  WITH STOCK.PRODUCER,   ;
          CLASS     WITH CLASSES.NAME,     ;
          FUNCTION  WITH SUBJECT.FUNCTION, ;
          CORPUS    WITH CORPUS.NAME,      ;
          QNT       WITH STOCK.QNT,        ;
          QNT_C     WITH STOCK.QNT_STORE,  ;
          QNT_H     WITH STOCK.QNT+STOCK.QNT_STORE,;
          PRICE_R_D WITH STOCK.PRICE,      ;
          PRICE_R_R WITH STOCK.PRICE_R
  SELECT STOCK
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Make_F2      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                  ��Г��┘ ����� 筥爼 - �牀├〓え皀�ォ.                 �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.07.2004 陳�
PROCEDURE Make_F2

CREATE DBF (rez_file) ;
   (  PRODUCER C(16), ;
      L_NAME   C(60), ;
      FOLDER   C( 8)  )
USE (rez_file) ALIAS REZ_TABLE
SELECT 0
USE (base_path+"DEVELOP") SHARED AGAIN ORDER TAG B_NAME
SCAN
  SELECT REZ_TABLE
  APPEND BLANK
  REPLACE PRODUCER WITH DEVELOP.B_NAME, ;
          L_NAME   WITH DEVELOP.L_NAME, ;
          FOLDER   WITH DEVELOP.FOLDER
  SELECT DEVELOP
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Make_F3      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                  ��Г��┘ ����� ���瘁�� ���キ���矣琺.                  �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.07.2004 陳�
PROCEDURE Make_F3

CREATE DBF (rez_file) ;
   (  CLASS        C(12), ;
      CLASS_NAME   C(64)  )
USE (rez_file) ALIAS REZ_TABLE
SELECT 0
USE (base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE
SCAN
  SELECT REZ_TABLE
  APPEND BLANK
  REPLACE CLASS      WITH CLASSES.CODE, ;
          CLASS_NAME WITH CLASSES.NAME
  SELECT CLASSES
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Make_F4      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    ��Г��┘ ����� か� 罐�┴��.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.07.2004 陳�
PROCEDURE Make_F4

PRIVATE tmpCount, tmpStr
CREATE DBF (rez_file) ;
   (  STO_CODE    N ( 7),;
      PREFIX      C ( 4),;
      NAME        C (35),;
      PRODUCER    C ( 8),;
      CLASS_NAME  C (64),;
      FUNCTION    C (254),;
      PRICE_R_R   N(9,2),;
      PRICE_R_D   N(9,2) )
      
USE (rez_file) ALIAS REZ_TABLE
SELECT 0
USE (base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (path_comm+"SUBJECT") SHARED AGAIN ORDER TAG STO_CODE
SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ORDER TAG NAME
SET RELATION TO CODE  INTO SUBJECT
SET RELATION TO CLASS INTO CLASSES ADDITIVE

DO Wt_Mess WITH "��ぃ�皰→� ����諷..."

m.tmpCount = 0
SCAN
  m.tmpCount = m.tmpCount+1
  IF m.tmpCount % 100 = 0
    m.tmpStr = PADC(ALLTRIM(STR(m.tmpCount)), WCOLS(WOUTPUT()))
    @ wROWS()-1, 0 SAY m.tmpStr
  ENDIF
  SELECT REZ_TABLE
  APPEND BLANK
  REPLACE STO_CODE    WITH STOCK.CODE,      ;
          PREFIX      WITH STOCK.PREFIX,    ;
          NAME        WITH STOCK.NAME,      ;
          PRODUCER    WITH STOCK.PRODUCER,  ;
          CLASS_NAME  WITH CLASSES.NAME,    ;
          FUNCTION    WITH SUBJECT.FUNCTION,;
          PRICE_R_R   WITH STOCK.PRICE_R,   ;
          PRICE_R_D   WITH STOCK.PRICE
  SELECT STOCK
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Make_F5      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        ��Г��┘ ����� COMM.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.07.2004 陳�
PROCEDURE Make_F5

PRIVATE tmpCount, tmpStr
CREATE DBF (rez_file) ;
   (  STO_CODE  N( 7), ;
      QNT_COMM  C(254)  )
USE (rez_file) ALIAS REZ_TABLE
SELECT 0
USE (path_comm+"SUBJECT") SHARED AGAIN ORDER TAG STO_CODE
SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ORDER TAG NAME
SET RELA TO CODE INTO SUBJECT

DO Wt_Mess WITH "��ぃ�皰→� ����諷..."

m.tmpCount = 0
SCAN
  m.tmpCount = m.tmpCount+1
  IF m.tmpCount % 100 = 0
    m.tmpStr = PADC(ALLTRIM(STR(m.tmpCount)), WCOLS(WOUTPUT()))
    @ wROWS()-1, 0 SAY m.tmpStr
  ENDIF
  SELECT REZ_TABLE
  APPEND BLANK
  REPLACE STO_CODE WITH STOCK.CODE, ;
          QNT_COMM WITH SUBJECT.FUNCTION
  SELECT STOCK
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

RETURN
