*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Cus_Name     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                          ��┃��� ━� ��┘���.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02.02.2007 様�
PROCEDURE Cus_Name
PARAMETERS prmCode

PRIVATE s_sav, tmpName, tmpShow, sName

m.s_sav = SELECT()
SELECT 0
USE m.base_path+"ACCOUNT" SHARED AGAIN ORDER TAG CUS_CODE ALIAS ACC_70202
SEEK m.prmCode
m.sName = ACC_70202.CUS_NAME
SELECT 0
USE m.base_path+"CUS_NAME" SHARED AGAIN ORDER TAG CUS_CODE ALIAS LONG_70202 IN 0

IF SEEK(m.prmCode)
  m.tmpName = ALLTRIM(CUS_NAME)
ELSE
  m.tmpName = ALLTRIM(ACC_70202.CUS_NAME)
ENDIF
m.tmpShow = m.tmpName

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�    ��┘�� ....:....!....:....!....:....!....:....!....:....!    �
*楽� ������ ━� ��┘��� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖....:....!....:....!....:....!....:....!....:....!....:....!...崖
*崖                                                               崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!...崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!...崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!...崖
*崖....:....!....:....!....:....!....:....!....:....!....:....!...崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰�
*�                < OK Ctrl-W > < �皖���碎瘴 Esc >                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 13, 69, "������ ━� ��┘���"
DO Sun_Bord WITH  3,  2, 10, 66, " ������ ━� ��┘��� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  2,  6 SAY "��┘��" GET m.sName WHEN .F.
@  4,  3 EDIT m.tmpName SIZE 1, 63, FSIZE("CUS_NAME") VALID S_Name() COLOR (SCHEME(1,1))
@  6,  3 EDIT m.tmpShow SIZE 4, 63 COLOR (SCHEME(1,1)) WHEN .F.

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Name()

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  SELECT LONG_70202
  DO CASE
  CASE EMPTY(m.tmpName)
    IF FOUND("LONG_70202")
      REPLACE CUS_NAME WITH "", CUS_CODE WITH 0
    ENDIF 
  CASE SYS(15, m.lwr, ALLTRIM(m.tmpName)) == SYS(15, m.lwr, ALLTRIM(ACC_70202.CUS_NAME))
    IF FOUND("LONG_70202")
      REPLACE CUS_NAME WITH "", CUS_CODE WITH 0
    ENDIF 
  OTHERWISE
    IF .NOT. FOUND("LONG_70202")
      GO TOP
      IF .NOT. EMPTY(CUS_CODE) .OR. BOF()
        APPEND BLANK
      ENDIF
      REPLACE CUS_CODE WITH ACC_70202.CUS_CODE
    ENDIF
    REPLACE CUS_NAME WITH ALLTRIM(m.tmpName)
  ENDCASE  
ENDIF
*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)
USE IN ACC_70202
USE IN LONG_70202
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� S_Name       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �磧�� ������� ━キ�                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 02.02.2007 陳�
PROCEDURE S_Name

m.tmpShow = m.tmpName
SHOW GET m.tmpShow

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Name     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍▲爲� ��玻オ皚�痰� ━キ�                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 02.02.2007 陳�
PROCEDURE Tst_Name

IF m.ex # 1
  RETURN .T.
ENDIF

IF EMPTY(m.tmpName)
  RETURN .T.
ENDIF

IF SYS(15, m.lwr, ALLTRIM(m.tmpName)) == SYS(15, m.lwr, ALLTRIM(ACC_70202.CUS_NAME))
  RETURN .T.
ENDIF

IF LEN(ALLTRIM(m.tmpName)) <= FSIZE("CUS_NAME", "ACC_70202")
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��� �� 痰��� か┃��� 艪� ° �甎��讌���碎 か┃��� ��━キ����┘"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
