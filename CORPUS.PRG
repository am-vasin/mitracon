*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Corpus       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ��玻オ罔�/甌Г��┘ ��牆竅�                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 17.12.2005 様�
PROCEDURE Corpus
PARAMETERS tmpCode		&& ��� ��牆竅�, メ�� �竅皰 - 甌Г��┘

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                          �
*� ��Б��┘ ....:....!....:.   �メ 9999.999 �
*�                                          �
*�     < OK Ctrl-W > < �皖���碎瘴 Esc >     �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, s_sav, tmpN, tmpW, tmpC, newName
m.s_sav = SELECT()
SELECT 0
USE (m.path_comm+"CORPUS") SHARED AGAIN ALIAS CRP_5B17 ORDER TAG CODE
IF .NOT. EMPTY(m.tmpCode)
  SEEK m.tmpCode
  m.tmpN = NAME
  m.tmpW = WEIGHT
ELSE
  m.tmpN = SPACE(16)
  m.tmpW = 0
ENDIF
m.newName = ""
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 46, "��牆竅 "+IIF(EMPTY(m.tmpCode), "(甌Г��┘)", "(��玻オ罔�)")

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  3,  3 SAY "��Б��┘" GET m.tmpN
@  3, 31 SAY "�メ" GET m.tmpW PICTURE "9999.999"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID TstBlank()

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  IF EMPTY(m.tmpCode)
    GO BOTTOM
    m.tmpC = MAX(CODE+1, 9000000)
    APPEND BLANK
    REPLACE CODE WITH m.tmpC
    IF TYPE("m.new_rec") = "N"
      m.new_rec = RECNO()
    ENDIF
  ELSE
    IF m.tmpN # NAME
      m.newName = m.tmpN
    ENDIF
  ENDIF
  REPLACE NAME   WITH m.tmpN, ;
          WEIGHT WITH m.tmpW
  IF .NOT. EMPTY(m.newName)
    USE (m.path_comm+"SUBJECT") SHARED AGAIN ALIAS SBJ_5B17
    DO Wt_Mess WITH "｡��←錺� ��牆竅 � ���キ���矣爛"
    SCAN FOR COR_CODE = m.tmpCode
      REPLACE CORPUS WITH m.newName
    ENDSCAN
    DO Wt_Mess WITH "｡��←錺� ��牆竅 � ���キ���矣爛"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

USE
SELECT (m.s_sav)
POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TstBlank     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍▲爲� ��玻オ皚�痰� ゛����                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 17.12.2005 陳�
PROCEDURE TstBlank

IF m.ex # 1
  RETURN .T.
ENDIF

PRIVATE mss
IF EMPTY(m.tmpN)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��Б��┘ �� ��Ε� °碎 �竅硅�..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
