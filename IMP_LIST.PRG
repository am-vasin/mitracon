*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Imp_List     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                ����珥 甎�瓷� ├ 艫Ξ� ��｀腑� �゛�痰�.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 07.09.2006 様�
PROCEDURE Imp_List
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

*
*    蹍▲瑙ガ, � ぎ痰祚�� �� 逾ム� 絎碎 艪�-�─磴�?
USE (m.base_path+"LET_LIST") SHARED AGAIN ORDER TAG USER_CODE ALIAS L_L_6915 IN 0
IF .NOT. SEEK(STR(m.user), "L_L_6915")
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��� �� ぎ痰祚�� 艫Θ� 甎�瓷�"
  mss[3] = ""
  USE IN L_L_6915
  DO Out_Mess WITH 7, "mss"
  RETURN 0
ENDIF
USE IN L_L_6915

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�  ( ) �� ���ム� 甎�瓷� 999999                    �
*�  ( ) �襦��碎 �� ��｀腑� �メ皀 ....:....!....:.  �
*�                                                 �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, tmpSw, tmpList, tmpArmC, tmpArmN, resList
m.resList = 0
m.ex = 1
m.tmpSw   = 1
m.tmpList = "      "
m.tmpArmC = "   "
m.tmpArmN = SPACE(16)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 53, "����珥 甎�瓷� � 艫Ξ�� ��｀腑�� �メ��"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  2,  4 GET m.tmpSw PICTURE "@*RV �� ���ム� 甎�瓷�;�襦��碎 �� ��｀腑� �メ皀" ;
                     VALID G_Arm()
@  2, 25 GET m.tmpList PICTURE "999999" WHEN m.tmpSw = 1
@  3, 33 GET m.tmpArmN WHEN .F.

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Ex_Blank()

*IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  
*ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.resList

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_Arm        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �襦�� ��｀腑�� �メ��                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 07.09.2006 陳�
PROCEDURE G_Arm

PRIVATE tmpC, tmpN

IF m.tmpSw = 1
  m.tmpArmC = "   "
  m.tmpArmN = SPACE(16)
ELSE
  m.tmpC = m.tmpArmC
  m.tmpN = m.tmpArmN
  m.tmpC = EnabList(.F., m.tmpN)
  IF .NOT. EMPTY(m.tmpC)
    m.tmpArmC = m.tmpC
    m.tmpArmN = m.tmpN
  ENDIF
  IF EMPTY(m.tmpArmC)
    m.tmpSw = 1
    m.tmpArmC = "   "
    m.tmpArmN = SPACE(16)
  ELSE
    m.tmpList = "      "
  ENDIF
ENDIF

SHOW GET m.tmpSw
SHOW GET m.tmpArmC
SHOW GET m.tmpArmN
SHOW GET m.tmpList

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ex_Blank     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �諷�� ├ ゛����                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 07.09.2006 陳�
PROCEDURE Ex_Blank
PRIVATE mss

IF m.ex = 2
  RETURN .T.
ENDIF

IF m.tmpSw = 1
  IF EMPTY(m.tmpList)
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"�� �� 礫����� ���ム 甎�瓷�!"
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS TMP_TIT_ ORDER TAG LST_NUM IN 0
  IF .NOT. SEEK(VAL(ALLTRIM(m.tmpList)), "TMP_TIT_")
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"���������� ���� 甎�瓷� �� 痺薀痰≪モ!"
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    USE IN TMP_TIT_
    RETURN .F.
  ENDIF
  USE IN TMP_TIT_
  m.resList = VAL(ALLTRIM(m.tmpList))
ELSE
  m.resList = Ch_List(m.tmpArmC)
  IF EMPTY(m.resList)
    m.resList = 0
    RETURN .F.
  ENDIF
  m.resList = VAL(ALLTRIM(m.resList))
ENDIF
IF .NOT. List_Info(m.resList)
  m.resList = 0
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� List_Info    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          ��筮爼�罔� � 甎�瓷�                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 07.09.2006 陳�
PROCEDURE List_Info
PARAMETERS n_list
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� ���甌� 甌Г�� ������������� 99.99.9999             �
*� ��｀ゥ �メ皰 ����������                            �
*楽� ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖....:....!....:....!....:....!....:....!....:....!崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�       �祠�� �� 甎�瓷� 99999999.9999                �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, tmpStr, is_enab

m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 56, "��筮爼�罔� � 甎�瓷� � "+ALLTRIM(STR(m.n_list))
DO Sun_Bord WITH  4,  2,  6, 53, " ��┘�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS TMP0_6907 IN 0
= SEEK(m.n_list, "TMP0_6907")
m.tmpStr = "�祠�� �� 甎�瓷� "+ALLTRIM(STR(TMP0_6907.R_TOTAL, 15, 4))+IIF(TMP0_6907.IS_USD, "$", "�")
@  7, 2 SAY PADC(m.tmpStr, WCOLS()-4)
m.is_enab = TMP0_6907.LST_ARM
USE (m.base_path+"LET_LIST") SHARED AGAIN ORDER TAG USER_CODE ALIAS TMP9_6907 IN 0
m.is_enab = SEEK(STR(m.user)+is_enab, "TMP9_6907")
USE IN TMP9_6907

USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS TMP1_6907 IN 0
= SEEK(TMP0_6907.WHO_CR, "TMP1_6907")
m.tmpStr = "���甌� 甌Г�� "+ALLTRIM(TMP1_6907.FAMILY)+" "+LEFT(TMP1_6907.NAME, 1)+" "+LEFT(TMP1_6907.S_NAME, 1)+"  "+;
           DTOC(TMP0_6907.CR_DAT)
@  2, 2 SAY PADC(m.tmpStr, WCOLS()-4)

USE (m.base_path+"ARM_LST") SHARED AGAIN ORDER TAG ARM_C ALIAS TMP1_6907 IN TMP1_6907
= SEEK(TMP0_6907.LST_ARM, "TMP1_6907")
m.tmpStr = "��｀腑� �メ皰 "+ALLTRIM(TMP1_6907.ARM_N)
@  3, 2 SAY PADC(m.tmpStr, WCOLS()-4)

USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS TMP1_6907 IN TMP1_6907
= SEEK(TMP0_6907.CLI_CODE, "TMP1_6907")
@  5, 3 SAY TMP1_6907.CUS_NAME
USE IN TMP1_6907
USE IN TMP0_6907

IF m.is_enab
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
  READ CYCLE
ELSE
  m.tmpStr = "�皰� 甎�甌� ��� �イ�痰祚キ. ����皀 艪�-�─磴�..."
  @ WROWS()-2, 2 SAY PADC(m.tmpStr, WCOLS()-4)
  m.ex = 2
  SET CURSOR OFF
  = INKEY(0)
  SET CURSOR ON
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.ex = 1
