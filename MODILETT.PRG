*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� ModiLett     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ��玻オ罔� ����皰→� ��瘡��                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 10.04.2006 様�
PROCEDURE ModiLett
PARAMETERS prmID

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

PRIVATE s_sav, tmpSub, tmpText, blnk, cnt, tmpID, ln, retRec
m.retRec = 0
IF EMPTY(m.prmID)
  m.tmpID = 0
ELSE
  m.tmpID = m.prmID
ENDIF
m.s_sav = SELECT()
SELECT 0
USE (base_path+"MAILTXT") SHARED AGAIN ALIAS ML1_6410 ORDER TAG LETTID
SCATTER TO blnk BLANK
m.tmpText = ""
IF EMPTY(tmpID)
  m.tmpSub = SPACE(FSIZE("SUBJECT"))
ELSE
  SEEK STR(m.tmpID)
  m.tmpSub = SUBJECT
  SCAN REST WHILE LETTID = m.tmpID
    m.tmpText = m.tmpText+TEXT
  ENDSCAN
  m.tmpText = ALLTRIM(m.tmpText)
ENDIF

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� �ガ� ....:....!....:....!....:....!....:....!....:....!....:....! �
*� 敖 �オ痰 ��瘡�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �                                                               � �
*� �                                                               � �
*� �                                                               � �
*� �                                                               � �
*� �                                                               � �
*� �                                                               � �
*� �                                                               � �
*� �                                                               � �
*� �                                                               � �
*� �                                                               � �
*� �                                                               � �
*� �                                                               � �
*� �                                                               � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�                 < OK Ctrl-W > < �皖���碎瘴 Esc >                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 20, 71, "����皰→� ��瘡��"
DO Sun_Bord WITH  3,  3, 17, 67, " �オ痰 ��瘡�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2, 3 SAY "�ガ�" GET m.tmpSub
@ 4, 4 EDIT m.tmpText SIZE 13, 63 COLOR (SCHEME(1,1))

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID TstBlank()

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  IF EMPTY(m.tmpID)
    GO BOTT
    m.tmpID = LETTID+1
  ELSE
    DO WHILE SEEK(STR(m.tmpID))
      GATHER FROM blnk
    ENDDO
  ENDIF
  m.cnt = 0
  m.tmpText = ALLTRIM(m.tmpText)
  m.ln = FSIZE("TEXT")
  DO WHILE .NOT. EMPTY(m.tmpText)
    m.cnt = m.cnt+1
    IF m.cnt = 0
      m.retRec = RECNO()
    ENDIF
    GO TOP
    IF .NOT. EMPTY(LETTID) .OR. BOF() .OR. EOF()
      APPEND BLANK
    ENDIF
    REPLACE WHO		WITH m.user,   ;
            LETTID	WITH m.tmpID,  ;
            SUBJECT	WITH m.tmpSub, ;
            N_LINE	WITH m.cnt,    ;
            TEXT	WITH LEFT(m.tmpText, m.ln)
    IF LEN(m.tmpText) < m.ln
      m.tmpText = ""
    ELSE
      m.tmpText = SUBSTR(m.tmpText, m.ln+1)
    ENDIF
  ENDDO
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

USE
SELECT (m.s_sav)

RETURN m.retRec

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TstBlank     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     蹍▲爲� ��玻オ皚�痰� ゛����.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10.04.2006 陳�
PROCEDURE TstBlank

PRIVATE mss

IF m.ex = 2
  RETURN .T.
ENDIF

IF EMPTY(m.tmpSub)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"�ガ� �� ��Ε� °碎 �竅皰�!"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(m.tmpText)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��ぅ爨━�� ��瘡�� �� ��Ε� °碎 �竅硅�!"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
