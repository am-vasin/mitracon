*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Own_Mail     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ��え※ゃ��讚襯 ��痰牀��� 蹕. ��艪�                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 05.02.2006 様�
PROCEDURE Own_Mail

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� ���讌���皀�� ....:....!....:....!....:....!....:....!....:.... �
*� 敖 ��� �皴��※皀�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �....:....!....:....!....:....!....:....!....:....!....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� 敖 E-Mail �皴��※皀�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �....:....!....:....!....:....!....:....!....:....!....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� 敖 ������珥�覃 皀�痰 ��瘡�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �....:....!....:....!....:....!....:....!....:....!....:....!� �
*� �....:....!....:....!....:....!....:....!....:....!....:....!� �
*� �....:....!....:....!....:....!....:....!....:....!....:....!� �
*� �....:....!....:....!....:....!....:....!....:....!....:....!� �
*� �....:....!....:....!....:....!....:....!....:....!....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�  ��痰�→�: ( ) �襪���碎 ≡ィ��  ( ) ���讓� �爬 �ギ≦�え��痰�   �
*�               [ ] �皴��←閧� ����� �� 瓣�� �むメ               �
*� ���♂. �� �皴��→�: ( ) �モ  ( ) ���讓� �爬 �ャ��腑  ( ) �瓮����
*�                   < OK Ctrl-W > < �皖���碎瘴 Esc >             �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, tmpAddr, tmpName, tmpText, tmpSw, s_sav, tmpUser, tmpCopy, tmpMess
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
m.s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") SHARED AGAIN ALIAS PRS6205 ORDER TAG CODE
SEEK m.user
m.tmpUser = ALLTRIM(FAMILY)+" "+ALLTRIM(NAME)+" "+ALLTRIM(S_NAME)
m.tmpUser = PADR(m.tmpUser, 49)
IF FSIZE("EML", "PRS6205") = 0
  m.tmpAddr = PADR(PersParm("MAILADDR"), 60)
ELSE
  m.tmpAddr = PRS6205.EML
ENDIF
m.tmpName = PADR(PersParm("MAILNAME"), 60)
m.tmpText = PersParm("MAILTEXT")
m.tmpSw   = VAL(PersParm("MAILMODE"))
m.tmpSw   = MAX(m.tmpSw, 1)
m.tmpMess = VAL(PersParm("MAILMESS"))
m.tmpMess = MAX(m.tmpMess, 1)
m.tmpCopy = .NOT. EMPTY(PersParm("MAILCOPY"))
DO Prp_Nav_2
DO D_Win_N WITH 21, 68, "��え※ゃ��讚襯 ��痰牀��� 蹕. ��艪�"
DO Sun_Bord WITH  3,  3,  5, 64, " ��� �皴��※皀�� "
DO Sun_Bord WITH  6,  3,  8, 64, " E-Mail �皴��※皀�� "
DO Sun_Bord WITH  9,  3, 15, 64, " ������珥�覃 皀�痰 ��瘡�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  2,  3 SAY "���讌���皀��" GET m.tmpUser WHEN .F.
@  4,  4 GET  m.tmpName
@  7,  4 GET  m.tmpAddr VALID Clr_Copy()
@ 10,  4 EDIT m.tmpText SIZE 5, 60 COLOR (SCHEME(1,1))
@ WROWS()-5,  4 SAY "��痰�→�:" GET m.tmpSw PICTURE "@*RH �襪���碎 ≡ィ�� ;���讓� �爬 �ギ≦�え��痰�"
@ WROWS()-4, 17 GET tmpCopy PICTURE "@*C �皴��←閧� ����� �� 瓣�� �むメ"  ;
                WHEN .NOT. EMPTY(m.tmpAddr)
@ WROWS()-3,  3 SAY "���♂. �� �皴��→�:" GET m.tmpMess ;
   PICTURE "@*RH �モ ;���讓� �爬 �ャ��腑 ;�瓮���"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  IF FSIZE("EML", "PRS6205") = 0
    DO PersParm WITH "MAILADDR", m.tmpAddr
  ELSE
    REPLACE EML WITH m.tmpAddr
  ENDIF
  DO PersParm WITH "MAILNAME", m.tmpName
  DO PersParm WITH "MAILTEXT", m.tmpText
  DO PersParm WITH "MAILMODE", STR(m.tmpSw)
  DO PersParm WITH "MAILMESS", STR(m.tmpMess)
  DO PersParm WITH "MAILCOPY", IIF(tmpCopy, "1", "")
ENDIF
*--------------------------------------------------------------------------

USE
SELECT (m.s_sav)
POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clr_Copy     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �諷�� ├ ���� �むメ�                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10.04.2006 陳�
PROCEDURE Clr_Copy

IF EMPTY(m.tmpAddr)
  m.tmpCopy = .F.
  SHOW GET m.tmpCopy
ENDIF

RETURN .T.
