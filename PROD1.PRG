*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Prod1        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      ��玻オ罔�/ぎ��←キ┘ ÷キ��                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 24.12.2005 様�
PROCEDURE Prod1
PARAMETERS prmCode	&& ���, メ�� �竅皰, 皰 ぎ��←キ┘

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                    ��━キ����┘ ....:...                     �
*楽� ������ ��━キ����┘ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖....:....!....:....!....:....!....:....!....:....!....:....!崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�   ����� � ├�÷. ....:...   �牀� ��痰�→� ....:....!....:.   �
*�              < OK Ctrl-W > < �皖���碎瘴 Esc >                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, s_sav
PRIVATE tmpCode, tmpBName, tmpLName, tmpFolder, tmpPeriod, savName
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"DEVELOP") SHARED AGAIN ALIAS BRND_5C24 ORDER TAG CODE
m.tmpCode = IIF(EMPTY(m.prmCode), 0, m.prmCode)

IF m.tmpCode = 0
  m.tmpBName  = SPACE(8)
  m.tmpLName  = SPACE(FSIZE("L_NAME"))
  m.tmpFolder = SPACE(FSIZE("FOLDER"))
  m.tmpPeriod = SPACE(FSIZE("PERIOD"))
ELSE
  SEEK m.tmpCode
  m.tmpBName  = LEFT(B_NAME, 8)
  m.tmpLName  = L_NAME
  m.tmpFolder = FOLDER
  m.tmpPeriod = PERIOD
ENDIF
m.savName = m.tmpBName
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 66, "��爼� �牀├〓え皀�� "+IIF(EMPTY(m.prmCode), "(ぎ��←キ┘)", "(��玻オ罔�)")
DO Sun_Bord WITH  3,  2,  5, 63, " ������ ��━キ����┘ "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  2, 22 SAY "��━キ����┘" GET m.tmpBName
@  4,  3 GET m.tmpLName
@  6,  5 SAY "����� � ├�÷." GET m.tmpFolder
@  6, 31 SAY "�牀� ��痰�→�" GET m.tmpPeriod
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID TstBrend()

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  IF m.tmpBName # m.savName .AND. m.tmpCode # 0
    SELECT 0
    USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG PRO_CODE ALIAS ST_B_5C24
    SEEK STR(m.tmpCode, 5)
    SCAN REST WHILE PRO_CODE = m.tmpCode
      REPLACE PRODUCER WITH m.tmpBName
    ENDSCAN
    USE
    SELECT BRND_5C24
  ENDIF
  IF m.tmpCode = 0
    SET ORDER TO TAG CODE
    GO BOTT
    m.tmpCode = MAX(CODE+1, 90000)
    APPEND BLANK
    REPLACE CODE    WITH m.tmpCode, ;
            DATE_ON WITH DATE(),    ;
            WHO_ADD WITH m.user
    IF TYPE("m.new_rec") = "N"
      m.new_rec = RECNO()
    ENDIF
  ENDIF
  REPLACE B_NAME WITH m.tmpBName, ;
          L_NAME WITH m.tmpLName, ;
          FOLDER WITH m.tmpFolder,;
          PERIOD WITH m.tmpPeriod
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
USE
SELECT (m.s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TstBrend     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍▲爲� ��玻オ皚�痰� ÷キ��                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 24.12.2005 陳�
PROCEDURE TstBrend

PRIVATE mss, s_sav

IF m.ex # 1
  RETURN .T.
ENDIF

IF EMPTY(m.tmpBName)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��Б��┘ �� ��Ε� °碎 �竅硅�..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(m.tmpLName)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"������ ��Б��┘ �� ��Ε� °碎 �竅硅�..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"DEVELOP") SHARED AGAIN ORDER TAG B_NAME ALIAS TMP_B_5C24
IF .NOT. EMPTY(m.tmpCode)
  SET FILTER TO CODE # m.tmpCode
ENDIF
IF SEEK(m.tmpBName)
  USE
  SELECT (m.s_sav)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��爼� � ���━ ��Б��┘� 礒� 痺薀痰≪モ..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

USE
SELECT (m.s_sav)
RETURN .T.
